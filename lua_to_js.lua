#!/usr/bin/env lua
local parser = require 'parser'
local ast = require 'parser.ast'
local table = require 'ext.table'
local path = require 'ext.path'

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

-- make lua output the default for nodes' js outputw
local names = table()
for name,nc in pairs(ast) do
	if ast.node:isa(nc) then
		names:insert(name)
		nc.tostringmethods.js = nc.tostringmethods.lua
	end
end
for _,info in ipairs{
	{'concat','+'},
	{'and','&&'},
	{'or','||'},
	{'ne','!='},
} do
	local name, op = table.unpack(info)
	ast['_'..name].tostringmethods.js = function(self) 
		return table(self.args):mapi(tostring):concat(' '..op..' ')
	end
end

-- 'not' in Lua returns 'true' if the value is 'false' or 'nil'
-- however in JS, '!' returns 'true' for 'false', 'null', 'undefined', '0', '""' ...
-- because JS is retarded
ast._not.tostringmethods.js = function(self)
	--[[
	return '!'..self.arg
	--]]
	-- [[
	return 'luaNot('..self.arg..')'
	--]]
end

ast._len.tostringmethods.js = function(self)
	return 'luaLen('..self.arg..')'
end

-- '%' in Lua is math modulo, which means negatives are made positive
-- '%' in JS is programmer modulo, which means negatives stay negative.
ast._mul.tostringmethods.js = function(self)
	local a, b = table.unpack(self.args)
	-- TODO make sure this works with negatives correctly
	return 'luaMod('..a..', '..b..')'
end

ast._assign.tostringmethods.js = function(self)
	local lhs = self.vars:mapi(tostring):concat', '
	if #self.vars > 1 then lhs = '['..lhs..']' end
	local rhs = self.exprs:mapi(tostring):concat', '
	if #self.exprs > 1 then rhs = '['..rhs..']' end
	return lhs..' = '..rhs
end

ast._block.tostringmethods.js = function(self)
	return tabblock(self)
end

ast._call.tostringmethods.js = function(self)
	return tostring(self.func)..'('..table(self.args):mapi(tostring):concat', '..')'
end

ast._foreq.tostringmethods.js = function(self)
	local s = 'for (let '..self.var..' = '..self.min..'; '..self.var..' < '..self.max..'; '
	if self.step then
		s = s .. self.var..' += '..self.step
	else
		s = s .. '++'..self.var
	end
	s = s ..') {\n' .. tabblock(self) .. tab() .. '}'
	return s
end

-- JS `for of` is made to work with iterators
ast._forin.tostringmethods.js = function(self)
	return 'for (const ['..table(self.vars):mapi(tostring):concat', '..'] of '..table(self.iterexprs):mapi(tostring):concat', '..') {\n' .. tabblock(self) .. tab() .. '}'
end

local function fixname(name)
	if name == 'self' then
		return 'this' 
	elseif name == 'this' then
		return 'self'
	elseif name == 'class' then
		return '_javascript_cant_use_class'	-- and TODO make sure it's not used anywhere else
	else
		return name
	end
end

ast._function.tostringmethods.js = function(self)
	if self.name then
		return fixname(self.name)..' = function('..table(self.args):mapi(function(arg) 
			return tostring(arg) 
		end):concat', '..') {\n' .. tabblock(self) .. tab() .. '}'
	else
		return 'function('..table(self.args):mapi(function(arg) 
			return tostring(arg)
		end):concat', '..') {\n' .. tabblock(self) .. tab() .. '}'
	end
end

ast._vararg.tostringmethods.js = function(self)
	return '...args'
end

ast._var.tostringmethods.js = function(self)
	return fixname(self.name)
end

ast._if.tostringmethods.js = function(self)
	local s = 'if ('..self.cond..') {\n' .. tabblock(self) .. tab() .. '}'
	for _,ei in ipairs(self.elseifs) do
		s = s .. ei
	end
	if self.elsestmt then s = s .. self.elsestmt end
	return s
end

ast._elseif.tostringmethods.js = function(self)
	return ' else if ('..self.cond..') {\n' .. tabblock(self) .. tab() .. '}'
end

ast._else.tostringmethods.js = function(self)
	return ' else {\n' .. tabblock(self) .. tab() .. '}'
end

ast._indexself.tostringmethods.js = function(self)
	return self.expr..'.'..self.key
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
		return 'export { _export = ['..s..']};'
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

	local jscode = tostring(tree)

	-- hmm, should :getdir() return a path?  or how about just '..' ?  does `path/filename/.. == path` make sense?  kind of?
	local dstpath = dstdir/fn:gsub('%.lua$', '.js')
	path((dstpath:getdir())):mkdir(true)
	print('writing to '..dstpath)
	dstpath:write(jscode)	
end
