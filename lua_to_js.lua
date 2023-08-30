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
	tabs = tabs + 1
	local s = table(t):mapi(function(expr)
		return tab() .. tostring(expr)
	end):concat';\n'
	tabs = tabs - 1
	return s..';\n'
end

-- tabblock() but wrapped in { }
function jsblock(t)
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
		return 'lua_'..op..'('..self.args[1]..', '..self.args[2]..')'
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
		return 'lua_'..op..'('..self.arg..')'
	end
end

-- TODO if lhs is t[k] then insert a lua_newindex here
ast._assign.tostringmethods.js = function(self)
	-- single assign of lvalue=rvalue with no table+key in lvalue ...
	if #self.vars == 1 
	and #self.exprs == 1 
	then
		local var = self.vars[1]
		local expr = self.exprs[1]
		if ast._index:isa(var) then
			return 'lua_newindex('..var.expr..', '..var.key..', '..expr..')'
		else
			return var..' = '..expr
		end
	else
		-- multiple assigns, can't use JS multiple assign because I might need to invoke lua_newindex() in the event a lvalue is a table ...
		local s = table{'{\n'}
		tabs = tabs + 1
		for i,expr in ipairs(self.exprs) do
			s:insert(tab() .. 'const luareg'..i..' = '..expr..';\n')
		end
		for i,var in ipairs(self.vars) do
			local value = i <= #self.exprs and 'luareg'..i or 'lua_nil'
			if ast._index:isa(var) then
				s:insert(tab() .. 'lua_newindex('..var.expr..', '..var.key..', '..value..');\n')
			else
				s:insert(tab() .. var .. ' = '..value..';\n')
			end
		end
		tabs = tabs - 1
		s:insert(tab()..'}')
		return s:concat()
	end
end

ast._table.tostringmethods.js = function(self)
	-- self.args is an array of {arg...}
	-- assign <-> arg.vars[1] is the key, arg.exprs[1] is the value
	-- otherwise <-> arg is the value
	if #self.args == 0 then
		return 'new lua_table()'
	else
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
	return 'for (const ['..table(self.vars):mapi(tostring):concat', '..'] of '..table(self.iterexprs):mapi(tostring):concat', '
		..') '
		.. jsblock(self)
end

local function fixname(name)
	if name == 'class' then
		return '_javascript_cant_use_class'	-- and TODO make sure it's not used anywhere else
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

ast._vararg.tostringmethods.js = function(self)
	return '...args'
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



	if self.exprs[1].type == 'function'
	or self.exprs[1].type == 'assign'
	then
		assert(#self.exprs == 1)
		-- if exprs[1] is a multi-assign then an 'let' needs to prefix each new declaration
		return 'let '..self.exprs[1]
	else
		-- it'll be # > 1 if it's local defs without any values assigned
		-- if values are assigned (even multiple) then ti'll have a single _assign which itself contains the multiple names + values
		return 'let '..self.exprs:mapi(tostring):concat', '
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
	if #self.exprs > 1 then s = '['..s..']' end
	-- then again ... we can avoid the conditional static analysis and instead just wrap all returns in []'s no matter if it's 1 or many

	return 'return '..s
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

	local jscode = "import * from './lua.js';\n"
					..tostring(tree)

	-- hmm, should :getdir() return a path?  or how about just '..' ?  does `path/filename/.. == path` make sense?  kind of?
	local dstpath = dstdir/fn:gsub('%.lua$', '.js')
	path((dstpath:getdir())):mkdir(true)
	print('writing to '..dstpath)
	dstpath:write(jscode)
end
