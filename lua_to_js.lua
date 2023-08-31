#!/usr/bin/env lua
local parser = require 'parser'
local ast = require 'parser.ast'
local table = require 'ext.table'
local path = require 'ext.path'
local tolua = require 'ext.tolua'

local tabs = -1	-- because everything is in one block
function tab()
	return ('\t'):rep(tabs)
end
function tabblock(t)
	if #t == 0 then return '' end
	tabs = tabs + 1
	local s = table(t):mapi(function(expr)
		return tab() .. tostring(expr)
	end):concat';\n'
	tabs = tabs - 1
	return s..';\n'
end

-- tabblock() but wrapped in { }
function jsblock(t)
	if #t == 0 then
		return '{}'
	end
	return '{\n'
		.. tabblock(t)
		.. tab() .. '}'
end

-- make lua output the default for nodes' js output
local names = table()
for name,nc in pairs(ast) do
	if ast.node:isa(nc) then
		names:insert(name)
		nc.tostringmethods.js = nc.tostringmethods.lua
	end
end

-- ... then modify accordingly

-- JS undefined is what is returned in absence of anything, like Lua nil
-- JS 'null' is moreso a constant value that is used to determine empty, though it is not stored equivalent empty.
-- all in all JS is a mess.
ast._nil.tostringmethods.js = function(self)
	return 'lua_nil'
end

for _,op in ipairs{
	-- logical:
	'or',
	'and',
	-- metamethods:
	'add',
	'sub',
	'mul',
	'div',
	'mod',
	'pow',
	'concat',
	'eq',
	'ne',
	'lt',
	'gt',
	'le',
	'ge',
} do
	ast['_'..op].tostringmethods.js = function(self)
		local x, y = self.args:unpack()
		-- TODO when x and y are numbers, for ops that are 1:1 with JS ops (i.e. not modulo or power), just insert the JS op
		return 'lua_'..op..'('..x..', '..y..')'
	end
end

for _,op in ipairs{
	-- logical:
	'not',
	-- metamethods:
	'unm',
	'len',
} do
	ast['_'..op].tostringmethods.js = function(self)
		-- same as above, number optimization, esp with unm ...
		if op == 'unm'
		and ast._number:isa(self.arg)
		then
			return '-'..self.arg
		end
		return 'lua_'..op..'('..self.arg..')'
	end
end

ast._par.tostringmethods.js = function(self)
	if ast._vararg:isa(self.expr) then
		return 'vararg[0]'
	else
		return tostring(self.expr)
	end
end

--[[
TODO
when #vars > #exprs, the last+remaining vars are mult-assign from the last expr
and doubly TODO ... if 
--]]
local function multassign(vars, exprs, decl)
	local function assign(var, expr)
		if ast._index:isa(var) then
			return 'lua_newindex('..var.expr..', '..var.key..', '..expr..')'
		else
			return (decl and (decl..' ') or '') .. var .. ' = ' .. expr
		end
	end
	-- single assign of lvalue=rvalue with no table+key in lvalue ...
	if #vars == 1 
	and #exprs == 1 
	then
		return assign(vars[1], exprs[1])
	else
		-- TODO if all vars are _label's and esp not _index's
		-- then just use JS mult-assign

		local s = table()
		
		-- since we're using {}'s here, gotta declare vars beforehand...
		if decl then
			s:insert(decl .. ' ' .. vars:mapi(tostring):concat', '.. ';\n' .. tab())
			decl = nil
		end

		local needsUnpack = #exprs < #vars
			-- TODO and exprs:last() is either vararg or function-call
		-- multiple assigns, can't use JS multiple assign because I might need to invoke lua_newindex() in the event a lvalue is a table ...
		s:insert'{\n'
		tabs = tabs + 1
		for i,expr in ipairs(exprs) do
			-- if this is to be unpacked then store the array
			-- if it's not then store the unwrapped 1st arg
			-- TODO support extra ()'s to eliminate varargs
			if ast._call:isa(expr)	-- if it needs to be unpacked ...
			or ast._vararg:isa(expr)
			then
				if ast._vararg:isa(expr) then expr = 'vararg' end
				if i < #exprs then	-- if it's 1<->1 assignemnt
					s:insert(tab() .. 'const luareg'..i..' = '..expr..'[0];\n')
				else				-- if it's going to be unpacked
					s:insert(tab() .. 'const luareg'..i..' = '..expr..'; //storing param-pack\n')
				end
			else	-- 1<->1 assignment
				s:insert(tab() .. 'const luareg'..i..' = '..expr..';\n')
			end
		end
		for i=1,#vars do
			local var = vars[i]
			local expr = exprs[i]
			
			if #exprs < #vars
			and i == #exprs
			-- TODO support extra ()'s to eliminate varargs
			and (ast._call:isa(expr)	-- if it needs to be unpacked ...
				or ast._vararg:isa(expr)
			)
			then
				-- mult-ret assignment
				-- TODO this should only be applicable if the last expr is a function or param pack
				-- NOTICE if any of the lhs's matched are a table-key (requiring newindex) then this has to get complicated.
				-- otherwise I'll have to get multret return values and unpack them into the lhs's vars
				for j=0,#vars-#exprs do
					s:insert(tab() .. assign(vars[i+j], 'luareg'..i..'['..j..']; //unrolling param-pack assignment\n'))
				end
				break
			end
			
			local value = i <= #exprs and 'luareg'..i or 'lua_nil'
			s:insert(tab() .. assign(var, value) .. ';\n')
		end
		tabs = tabs - 1
		s:insert(tab()..'}')
		return s:concat()
	end
end


-- TODO if lhs is t[k] then insert a lua_newindex here
ast._assign.tostringmethods.js = function(self)
	return multassign(self.vars, self.exprs)
end

ast._table.tostringmethods.js = function(self)
	-- self.args is an array of {arg...}
	-- assign <-> arg.vars[1] is the key, arg.exprs[1] is the value
	-- otherwise <-> arg is the value
	if #self.args == 0 then
		return 'new lua_table()'
	else
		-- initialize with key-value pairs because we can't initialize with JS {} objs, because they can't handle keys of objects or functions.
		local s = table{'new lua_table([\n'}
		tabs = tabs + 1
		local i = 0
		for _,arg in ipairs(self.args) do
			if ast._assign:isa(arg) then
				assert(#arg.vars == 1)
				assert(#arg.exprs == 1)
				s:insert(tab()..'['..arg.vars[1]..', '..arg.exprs[1]..'],\n')
			else
				i = i + 1
				s:insert(tab()..'['..i..', '..arg..'],\n')
			end
		end
		tabs = tabs - 1
		s:insert(tab()..'])')
		return s:concat()
	end
end

ast._block.tostringmethods.js = function(self)
	return tabblock(self)
end

ast._call.tostringmethods.js = function(self)
	local func = self.func
	if func.type == 'indexself' then
		-- a:b(...)
		-- becomes:
		-- index(a,b)(a, ...)
		-- but that means '_indexself' needs to access the _call wrapping it ...
		-- ... and TODO what happens when 'a' is an expression whose value changes per-call?
		-- like "f():g()", where f() returns different values each time?
		-- in that case we need to insert code to cache f() ...	
		return 'lua_callself('
			..table{
				func.expr,
				tolua(func.key),
			}:append(self.args)
				:mapi(tostring)
				:concat', '
			..')'
	else
		return 'lua_call('
			..table{
				self.func
			}:append(self.args)
				:mapi(tostring)
				:concat', '
			..')'
	end
end

ast._foreq.tostringmethods.js = function(self)
	local s = 'for (let '..self.var..' = '..self.min..'; '..self.var..' < '..self.max..'; '
	if self.step then
		s = s .. self.var..' += '..self.step
	else
		s = s .. '++'..self.var
	end
	s = s ..') ' .. jsblock(self)
	return s
end

-- JS `for of` is made to work with iterators
ast._forin.tostringmethods.js = function(self)
	-- assuming it's just a `for k,v in pairs(...) do ...`
	--[[
	return 'for (const ['..table(self.vars):mapi(tostring):concat', '..'] of '..table(self.iterexprs):mapi(tostring):concat', '
		..') '
		.. jsblock(self)
	--]]
	-- the full general case
	-- [[
	local s = table()
	s:insert('{\n')
	tabs = tabs + 1
	s:insert(tab() .. multassign(
		table{ast._var'f', ast._var's', ast._var'v'},
		self.iterexprs,
		'let'	-- TODO 'const' for f and s, 'let' for v
	)..'\n')
	s:insert(tab() .. 'for(;;) {\n')
	tabs = tabs + 1
	s:insert(tab() .. multassign(
		self.vars,
		table{
			ast._call(
				ast._var'f',
				ast._var's',
				ast._var'v'
			)
		},
		'const'
	)..'\n')
	s:insert(tab() .. 'v = ' .. self.vars[1] .. ';\n')
	s:insert(tab() .. 'if (v === lua_nil) break;\n')
	s:insert(tab() .. jsblock(self)..'\n')
	tabs = tabs - 1
	s:insert(tab() .. '}\n')
	tabs = tabs - 1
	s:insert(tab() .. '}\n')
	return s:concat()
	--]]
end

local function fixname(name)
	if name == 'class' then
		-- and TODO make sure it's not used anywhere else
		return '_in_javascript_class_is_reserved'	
	elseif name == 'window' then
		return '_in_javascript_window_is_reserved'
	elseif name == '_G' then
		return 'window'
	else
		return name
	end
end

ast._do.tostringmethods.js = function(self)
	return jsblock(self)
end

ast._while.tostringmethods.js = function(self)
	return 'while ('..self.cond..') '..jsblock(self)
end

ast._repeat.tostringmethods.js = function(self)
	return 'do '
		.. jsblock(self)
		.. ' while (lua_not('..self.cond..'))'
end

ast._function.tostringmethods.js = function(self)
	if self.name then
		-- name can be _indexself ...
		-- in that case, we want to insert a 'self' param in the front
		if ast._indexself:isa(self.name) then
			return fixname(self.name.expr)..'.'..self.name.key..' = function(self, '
				..table(self.args):mapi(function(arg) 
					return tostring(arg) 
				end):concat', '..') '
				..jsblock(self)
		else
			return fixname(self.name)..' = function('
				..table(self.args):mapi(function(arg) 
					return tostring(arg) 
				end):concat', '..') '
				..jsblock(self)
		end
	else
		return 'function('..table(self.args):mapi(function(arg) 
			return tostring(arg)
		end):concat', '..') '
		..jsblock(self)
	end
end

-- in-argument use `...vararg`
-- in-code use `vararg`
ast._vararg.tostringmethods.js = function(self)
	return '...vararg'
end

ast._var.tostringmethods.js = function(self)
	return fixname(self.name)
end

ast._if.tostringmethods.js = function(self)
	local s = 'if (lua_toboolean('..self.cond..')) '
		..jsblock(self)
	for _,ei in ipairs(self.elseifs) do
		s = s .. ei
	end
	if self.elsestmt then
		s = s .. self.elsestmt
	end
	return s
end

ast._elseif.tostringmethods.js = function(self)
	return ' else if (lua_toboolean('..self.cond..')) '
		..jsblock(self)
end

ast._else.tostringmethods.js = function(self)
	return ' else '..jsblock(self)
end

ast._index.tostringmethods.js = function(self)
	return 'lua_index('..self.expr..', '..self.key..')'
end

ast._indexself.tostringmethods.js = function(self)
	error("handle this via _call or _function")
end

ast._local.tostringmethods.js = function(self)
	--[[
	this converts glboal-scope `local x = require 'y'` into import statements
	but maybe TODO for non-global-scope, do await import()?
	--]]
	if #self.exprs == 1
	and self.exprs[1].type == 'assign'
	then
		local assign = self.exprs[1]
		if #assign.vars == 1
		and #assign.exprs == 1
		and assign.exprs[1].type == 'call'
		then
			local call = assign.exprs[1]
			if call.func.type == 'var'
			and call.func.name == 'require'
			and #call.args == 1
			then
				local reqarg = call.args[1]
				if reqarg.type == 'string' then
					local reqpath = './'..reqarg.value:gsub('%.','/')..'.js'
					if self.parent.type == 'block'
					and self.parent.parent == nil
					then
						return 'import {_export as '..assign.vars[1].name..'} from "'..reqpath..'"'
					else
						-- TODO use the import() promise function here
					end
				end
			end
		end
	end



	local expr = self.exprs[1]
	if expr.type == 'function'
	or expr.type == 'assign'
	then
		assert(#self.exprs == 1)
		if expr.type == 'function' then
			return 'let '..expr
		else
			-- if exprs[1] is a multi-assign then an 'let' needs to prefix each new declaration
			assert(expr.type == 'assign')
			return multassign(expr.vars, expr.exprs, 'let')
		end
	else
		-- it'll be # > 1 if it's local defs without any values assigned
		-- if values are assigned (even multiple) then ti'll have a single _assign which itself contains the multiple names + values
		return 'let '..self.exprs:mapi(tostring):concat', '..' //multiple var decl\n'
	end
end

ast._return.tostringmethods.js = function(self)
	local s = table.mapi(self.exprs, tostring):concat', '	-- TODO make sure all self.exprs' are table metatable?
	
	-- javascript-specific, turn global return's into exports
	if not self.parent.parent then
		return 'export { _export = ['..s..']}'
	end
	
	-- javascript doesn't support multiple returns
	-- so I have to return multiple values as an array
	-- sooo TODO also track who is calling the function?
	-- because if the caller is assigning multiple values to an array-wrapped multiple return then - as long as we wrap the multiple-assign with a [] then JS won't poop its pants
	return 'return ['..s..']'
	-- then again ... we can avoid the conditional static analysis and instead just wrap all returns in []'s no matter if it's 1 or many
end

ast._string.tostringmethods.js = function(self)
	if self.value:find'\n' and #self.value > 10 then
		-- what gets escaped in multiline strings in javascript?
		-- the string endline ` needs to be escaped
		-- \r \n \t etc isn't
		return '`'..self.value:gsub('`', '\\`')..'`'
	else
		return ast._string.tostringmethods.lua(self)
	end
end

local srcdir = ...
assert(srcdir, "expected srcdir")
srcdir = path(srcdir)
assert(srcdir.path:sub(-1,-1) == '/')
print('srcdir', srcdir)

local dstdir = path'out'

local fns = srcdir:rdir()
	:mapi(function(fn) return path(fn).path end)
	:filter(function(fn) return fn:match'%.lua$' end)
	:mapi(function(fn)
		assert(fn:sub(1,#srcdir.path) == srcdir.path)
		return fn:sub(#srcdir.path+1)
	end)
print(fns:concat'\n')

ast.tostringmethod = 'js'

for _,fn in ipairs(fns) do
	local srcpath = srcdir/fn

	local luacode = srcpath:read()
	--print('original:')
	--print(luacode)

	local tree = parser.parse(luacode)
	--print('lua:')
	--print(tree)
	--print()

	local jscode = "import * as lua from './lua.js';\n"
					.."Object.entries(lua).forEach(([k,v]) => { window[k] = v; });\n"
					..tostring(tree)

	-- hmm, should :getdir() return a path?  or how about just '..' ?  does `path/filename/.. == path` make sense?  kind of?
	local dstpath = dstdir/fn:gsub('%.lua$', '.js')
	path((dstpath:getdir())):mkdir(true)
	print('writing to '..dstpath)
	dstpath:write(jscode)
end
