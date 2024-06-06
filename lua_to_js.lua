#!/usr/bin/env lua
local LuaParser = require 'parser.lua.parser'
local ast = require 'parser.lua.ast'
local table = require 'ext.table'
local path = require 'ext.path'
local tolua = require 'ext.tolua'

local tabs = -1	-- because everything is in one block
function tab()
	return ('\t'):rep(tabs)
end
function tabblock(t, apply)
	if #t == 0 then return '' end
	tabs = tabs + 1
	local s = table.mapi(t, function(ti)
		return tab() .. apply(ti)
	end):concat';\n'
	tabs = tabs - 1
	return s..';\n'
end

local function toJS(x)
	if x.toJS then return x:toJS(toJS) end
	return x:serialize(toJS)
end

for _,cl in ipairs(ast.allclasses) do
	function cl:toJS(apply)
		return self:serialize(apply)
	end
end

--[[
scope stack
start it as empty <-> global-scope (or whatever global fenv/_ENV is)
as we encounter function, for, do, while, repeat
	then we add a table to the stack
function-args, locals, for-vars are set as keys the top stack.
then when assigning a var, we can determine if it belongs to a local or to the fenv/global env.

coincidentally this list of stmts are exactly those that call 'jsblock' for generating their own stmt blocks...
maybe I will piggyback?
--]]
local scope = table()

-- this is the same as tabblock() but wrapped in { }
-- 't' is the list of stmts
-- 'varlist' is optional key-set of variable-names available to this scope block
-- 	(populate it with function args and for vars)
function jsblock(t, varlist, apply)
	if #t == 0 then
		return '{}'
	end
	local s = table()
	s:insert'{\n'

	local varset = {}
	if varlist then
		for _,var in ipairs(varlist) do
			if not ast._vararg:isa(var) then
				assert(ast._var:isa(var))
				varset[var.name] = true
			end
		end
	end
	scope:insert(varset)	-- add new local scope block
	s:insert(tabblock(t, apply))
	scope:remove()	-- remove it
	
	s:insert(tab() .. '}')
	return s:concat()
end

--local nilname = 'lua_nil'
local nilname = 'undefined'
-- TODO make sure this doesn't collide with any variable names
local varargname = 'vararg'

-- ... then modify accordingly

-- JS undefined is what is returned in absence of anything, like Lua nil
-- JS 'null' is moreso a constant value that is used to determine empty, though it is not stored equivalent empty.
-- all in all JS is a mess.
function ast._nil:toJS(apply)
	return nilname
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
	ast['_'..op].toJS = function(self, apply)
		local x, y = table.unpack(self.args:mapi(apply))
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
	ast['_'..op].toJS = function(self, apply)
		-- same as above, number optimization, esp with unm ...
		if op == 'unm'
		and ast._number:isa(self.arg)
		then
			return '-'..apply(self.arg)
		end
		return 'lua_'..op..'('..apply(self.arg)..')'
	end
end

-- search for any # of (( )) nestings
-- used for determining when varargs should expand and when not to
local function isParAroundMultRet(arg, nestedClass)
	if not ast._par:isa(arg) then return false end
	while ast._par:isa(arg) do
		arg = arg.expr
	end
	return nestedClass:isa(arg)
end

function ast._par:toJS(apply)
	if isParAroundMultRet(self, ast._vararg) then
		return varargname..'[0]'
	else
		return apply(self.expr)
	end
end

-- TODO this wraps in []
-- but some callers also wrap in ...
-- so maybe this shouldn't wrap in [], and leave that to whoever calls it to wrap
-- however some callers expect that array opt for when #exprs==1
-- so ... one way or the other ...
local function luaArgListToJSArray(apply, exprs)
	if #exprs == 1 then
		local expr = exprs[1]
		if ast._call:isa(expr)
		or ast._vararg:isa(expr)
		then
			return apply(expr)
		end
	end
	return '['..table.mapi(exprs, function(e,i)
		if ast._vararg:isa(e)
		or ast._call:isa(e)
		then
			if i < #exprs then
				return e..'[0]'
			else
				return '...'..e
			end
		end
		if isParAroundMultRet(e, ast._call) then
			return e..'[0]'
		end
		return apply(e)
	end):concat', '..']'
end

--[[
TODO
when #vars > #exprs, the last+remaining vars are mult-assign from the last expr
and doubly TODO ... if 
--]]
local function multassign(apply, vars, exprs, decl)
	-- TODO presence of 'decl' isn't dependable for global test
	-- it will be hit for `t.k = v`, even when 't' has been previously defined locally.
	--  (and it will fail in JS , since Lua allows repeated local defs, while JS doesn't like repeated 'let' defs)
	-- instead, we need a function for tracking/searching current lookup scope,
	-- and determining what scope a var is / global otherwise.
	local function assign(var, expr)
		if ast._index:isa(var) then
			return 'lua_newindex('..apply(var.expr)..', '..apply(var.key)..', '..apply(expr)..')'
		else
			return (decl and (decl..' ') or '') .. apply(var) .. ' = ' .. apply(expr)
		end
	end
	
	-- single assign of lvalue=rvalue with no table+key in lvalue ...
	if #vars == 1 
	and #exprs == 1 
	then
		return assign(vars[1], exprs[1])
	end
		
	-- [[
	-- if all vars are _label's and esp not _index's
	-- then just use JS mult-assign
	local noVarsAreIndex = true
	for _,var in ipairs(vars) do
		if ast._index:isa(var) then
			noVarsAreIndex = false
			break
		end
	end
	if noVarsAreIndex then
		return 
			(decl and (decl..' ') or '')
			..'['..vars:mapi(apply):concat', '..'] = '
			..luaArgListToJSArray(apply, exprs)..'; //mult-assign, none are indexes'
	end
	--]]

	local s = table()
	
	-- since we're using {}'s here, gotta declare vars beforehand...
	if decl then
		s:insert(decl .. ' ' .. vars:mapi(apply):concat', '.. ';\n' .. tab())
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
			if ast._vararg:isa(expr) then expr = varargname end
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
			local anyAreIndex
			for j=0,#vars-#exprs do
				if ast._index:isa(vars[i+j]) then
					anyAreIndex = true
					break
				end
			end
			if anyAreIndex then
				for j=0,#vars-#exprs do
					s:insert(tab() .. assign(vars[i+j], 'luareg'..i..'['..j..']')..'; // unrolling param-pack assignment\n')
				end
			else
				s:insert(tab() .. '[' 
					.. vars:sub(i,i+#vars-#exprs):mapi(function(v)
						assert(not ast._index:isa(v))
						return apply(v)
					end):concat', '..'] = luareg'..i..'; //unrolling param-pack assignment\n')
			end
			break
		end
		
		local value = i <= #exprs and 'luareg'..i or nilname
		s:insert(tab() .. assign(var, value) .. ';\n')
	end
	tabs = tabs - 1
	s:insert(tab()..'}')
	return s:concat()
end


-- TODO if lhs is t[k] then insert a lua_newindex here
function ast._assign:toJS(apply)
	-- TODO HERE
	-- if the parent isn't a _local
	-- then this is a global assign (right? maybe?)
	-- and in that case , insert a lua_newindex(_G, name, expr)
	-- in fact, maybe that behavior belongs in 'multassign' ....
	return multassign(apply, self.vars, self.exprs)
end

function ast._table:toJS(apply)
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
				s:insert(tab()..'['..apply(arg.vars[1])..', '..apply(arg.exprs[1])..'],\n')
			else
				i = i + 1
				s:insert(tab()..'['..i..', '..apply(arg)..'],\n')
			end
		end
		tabs = tabs - 1
		s:insert(tab()..'])')
		return s:concat()
	end
end

function ast._block:toJS(apply)
	return tabblock(self, apply)
end

function ast._call:toJS(apply)
	local func = self.func
	local args
	if #self.args == 0 then
		args = ''
	else
		
		args = '...'..luaArgListToJSArray(apply, self.args)
		if args:match'^%.%.%.%[.*%]$' then
			args = args:sub(5,-2)
		end
		args =  ', '..args
		--[[
		..table(self.args)
			:mapi(function(arg,i)
				-- TODO here:
				-- if it's a function call then expand it.
				if ast._call:isa(arg) then
					if i < #self.args then
						return arg..'[0]'
					else
						return '...'..arg
					end
				end

				-- TODO flatten all par(par(call)) to par(call)
				if isParAroundMultRet(arg, ast._call) then
					return arg..'[0]'
				end
				return apply(arg)
			end)
			:concat', '
		--]]
	end
	if func.type == 'indexself' then
		-- a:b(...)
		-- becomes:
		-- index(a,b)(a, ...)
		-- but that means '_indexself' needs to access the _call wrapping it ...
		-- ... and TODO what happens when 'a' is an expression whose value changes per-call?
		-- like "f():g()", where f() returns different values each time?
		-- in that case we need to insert code to cache f() ...	
		return 'lua_callself('
				..apply(func.expr)..', '
				..tolua(func.key)
				..args
			..')'
	else
		return 'lua_call('
				..apply(self.func)
				..args
			..')'
	end
end

function ast._foreq:toJS(apply)
	local s = 'for (let '..apply(self.var)..' = '..apply(self.min)..'; '..apply(self.var)..' <= '..apply(self.max)..'; '
	if self.step then
		s = s .. apply(self.var)..' += '..apply(self.step)
	else
		s = s .. '++'..apply(self.var)
	end
	s = s ..') ' .. jsblock(self, table{self.var}, apply)
	return s
end

-- JS `for of` is made to work with iterators
function ast._forin:toJS(apply)
	-- [[ shortcut `for k,v in pairs/ipairs(t)`
	if #self.iterexprs == 1 then
		local iterexpr = self.iterexprs[1]
		if iterexpr.type == 'call' then
			local func = iterexpr.func
			if func.type == 'var' then
				if func.name == 'pairs' then
					return tab() .. 'for (const ['
						..self.vars:mapi(apply):concat', '
						..'] of lua_assertIsTable('
						-- TODO wouldn't hurt to verify this is a map and throw a pairs() error if it's not
						..apply(iterexpr.args[1])
						..').t) '
						.. jsblock(self, self.vars, apply)
				elseif func.name == 'ipairs' then
					-- in Lua you need at least one var, so ... 
					local var = self.vars[1]
					local s = table()
					s:insert(tab() .. 'for (let '..apply(var)..' = 1; '..apply(var)..' <= lua_len(lua_assertIsTable('..apply(iterexpr.args[1])..')); ++'..apply(var)..') {\n')
					tabs = tabs + 1
					if #self.vars > 1 then
						s:insert(tab() .. 'const '..apply(self.vars[2])..' = lua_rawget('..apply(iterexpr.args[1])..', '..apply(var)..');\n')
					end
					-- hmm do I really need extra {}'s here?
					s:insert(tab() .. jsblock(self, self.vars)..'\n', apply)
					tabs = tabs - 1
					s:insert(tab()..'}')
					return s:concat()
				end
			end
		end
	end
	--]]

	--[[ TODO if it's just a `for k,v in pairs(...) do ...`
	-- ... then replace it with an Object.entries(...).forEach(...)
	-- same with ipairs() and .forEach
	return 'for (const ['..table(self.vars):mapi(apply):concat', '..'] of '..table(self.iterexprs):mapi(apply):concat', '
		..') '
		.. jsblock(self, nil, apply)
	--]]
	-- the full general case
	-- [[
	local s = table()
	s:insert('{\n')
	tabs = tabs + 1
	s:insert(tab() .. multassign(
		apply,
		table{ast._var'f', ast._var's', ast._var'v'},
		self.iterexprs,
		'let'	-- TODO 'const' for f and s, 'let' for v
	)..'\n')
	s:insert(tab() .. 'for(;;) {\n')
	tabs = tabs + 1
	s:insert(tab() .. multassign(
		apply,
		self.vars,
		table{
			ast._call(
				ast._var'f',
				ast._var's',
				ast._var'v'
			)
		},
		'let'
	)..'\n')
	s:insert(tab() .. 'v = ' .. self.vars[1] .. ';\n')
	s:insert(tab() .. 'if (v === '..nilname..') break;\n')
	s:insert(tab() .. jsblock(self, self.vars, apply)..'\n')
	tabs = tabs - 1
	s:insert(tab() .. '}\n')
	tabs = tabs - 1
	s:insert(tab() .. '}')
	return s:concat()
	--]]
end

--[[
Lua reserved words:

JS reserved words:


JS reserved words that are not Lua reserved words:

--]]

local replaceName = {
	-- replace Lua name with JS name for when the name is reserved in JS
	-- JS has 64 reserved words while Lua has just 21 
	['class'] = '_js_class',
	['window'] = '_js_window',
	['abstract'] = '_js_abstract',
	['arguments'] = '_js_arguments',
	['boolean'] = '_js_boolean',
	['break'] = '_js_break',
	['byte'] = '_js_byte',
	['case'] = '_js_case',
	['catch'] = '_js_catch',
	['char'] = '_js_char',
	['const'] = '_js_const',
	['continue'] = '_js_continue',
	['debugger'] = '_js_debugger',
	['default'] = '_js_default',
	['delete'] = '_js_delete',
	['do'] = '_js_do',
	['double'] = '_js_double',
	['else'] = '_js_else',
	['eval'] = '_js_eval',
	['false'] = '_js_false',
	['final'] = '_js_final',
	['finally'] = '_js_finally',
	['float'] = '_js_float',
	['for'] = '_js_for',
	['function'] = '_js_function',
	['goto'] = '_js_goto',
	['if'] = '_js_if',
	['implements'] = '_js_implements',
	['in'] = '_js_in',
	['instanceof'] = '_js_instanceof',
	['int'] = '_js_int',
	['interface'] = '_js_interface',
	['long'] = '_js_long',
	['native'] = '_js_native',
	['new'] = '_js_new',
	['null'] = '_js_null',
	['package'] = '_js_package',
	['private'] = '_js_private',
	['protected'] = '_js_protected',
	['public'] = '_js_public',
	['return'] = '_js_return',
	['short'] = '_js_short',
	['static'] = '_js_static',
	['switch'] = '_js_switch',
	['synchronized'] = '_js_synchronized',
	['this'] = '_js_this',
	['throw'] = '_js_throw',
	['throws'] = '_js_throws',
	['transient'] = '_js_transient',
	['true'] = '_js_true',
	['try'] = '_js_try',
	['typeof'] = '_js_typeof',
	['var'] = '_js_var',
	['void'] = '_js_void',
	['volatile'] = '_js_volatile',
	['while'] = '_js_while',
	['with'] = '_js_with',
	['yield'] = '_js_yield',
	-- star-suffix:
	['await'] = '_js_await',
	--class*
	['enum'] = '_js_enum',
	['export'] = '_js_export',
	['extends'] = '_js_extends',
	['import'] = '_js_import',
	['let'] = '_js_let',
	['super'] = '_js_super',
}

local reservedNames = {
	-- list of all names used in the generated code
	[nilname] = true,
	[varargname] = true,

	-- list of all names used in builtin code
	lua_toboolean = true,
	lua_table = true,
	lua_assertIsTable = true,	-- used by optimized iterators
	-- operators <-> metamethods <-> names
	lua_or = true,
	lua_and = true,
	lua_add = true,
	lua_sub = true,
	lua_mul = true,
	lua_div = true,
	lua_mod = true,
	lua_pow = true,
	lua_unm = true,
	lua_concat = true,
	lua_len = true,
	lua_eq = true,
	lua_not = true,
	lua_lt = true,
	lua_gt = true,
	lua_le = true,
	lua_ge = true,
	lua_index = true,
	lua_newindex = true,
	lua_call = true,
	lua_callself = true,
	--[[ builtin functions ... or these aren't reserved, are they
	assert = true,
	collectgarbage = true,
	dofile = true,
	error = true,
	getfenv = true,
	getmetatable = true,
	ipairs = true,
	load = true,
	loadfile = true,
	loadstring = true,
	next = true,
	pairs = true,
	pcall = true,
	print = true,
	rawequal = true,
	rawget = true,
	rawset = true,
	select = true,
	setfenv = true,
	tonumber = true,
	tostring = true,
	type = true,
	unpack = true,
	_VERSION = true,
	xpcall = true,
	--]]
}
-- reserve these, if no other keys map into this
for k,v in pairs(replaceName) do
	reservedNames[v] = true
end

-- I don't want 'window' added to the list of values that get excluded,
-- since it's going to be mapped
replaceName._G = 'window'

local function fixname(name)
	if reservedNames[name] then
		error("name collision: "..tostring(name))
	end
	return replaceName[name] or name
end

function ast._do:toJS(apply)
	return jsblock(self, nil, apply)
end

function ast._while:toJS(apply)
	return 'while ('..apply(self.cond)..') '..jsblock(self, nil, apply)
end

function ast._repeat:toJS(apply)
	return 'do '
		.. jsblock(self, nil, apply)
		.. ' while (lua_not('..apply(self.cond)..'))'
end

function ast._function:toJS(apply)
	local argstr = table(self.args):mapi(function(arg) 
		if ast._vararg:isa(arg) then
			return '...'..apply(arg)
		end
		return apply(arg) 
	end):concat', '

	if self.name then
		-- name can be _indexself ...
		-- in that case, we want to insert a 'self' param in the front
		if ast._indexself:isa(self.name) then
			return fixname(self.name.expr)..'.'..self.name.key
				..' = (self, '..argstr..') => '
				..jsblock(self, self.args, apply)
		else
			return fixname(self.name)
				..' = ('..argstr..') => '
				..jsblock(self, self.args, apply)
		end
	else
		return '('..argstr..') => '
			..jsblock(self, self.args, apply)
	end
end

-- in-argument use `...vararg`
-- in-code use `vararg`
function ast._vararg:toJS(apply)
	return varargname
end

function ast._var:toJS(apply)
	return fixname(self.name)
end

function ast._if:toJS(apply)
	local s = 'if (lua_toboolean('..self.cond..')) '
		..jsblock(self, nil, apply)
	for _,ei in ipairs(self.elseifs) do
		s = s .. apply(ei)
	end
	if self.elsestmt then
		s = s .. apply(self.elsestmt)
	end
	return s
end

function ast._elseif:toJS(apply)
	return ' else if (lua_toboolean('..apply(self.cond)..')) '
		..jsblock(self, nil, apply)
end

function ast._else:toJS(apply)
	return ' else '..jsblock(self, nil, apply)
end

function ast._index:toJS(apply)
	return 'lua_index('..apply(self.expr)..', '..apply(self.key)..')'
end

function ast._indexself:toJS(apply)
	error("handle this via _call or _function")
end

function ast._local:toJS(apply)
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
			return 'let '..apply(expr)
		else
			-- if exprs[1] is a multi-assign then an 'let' needs to prefix each new declaration
			assert(expr.type == 'assign')
			return multassign(apply, expr.vars, expr.exprs, 'let')
		end
	else
		-- it'll be # > 1 if it's local defs without any values assigned
		-- if values are assigned (even multiple) then ti'll have a single _assign which itself contains the multiple names + values
		return 'let '..self.exprs:mapi(apply):concat', '..' //multiple var decl\n'
	end
end

function ast._return:toJS(apply)
	local s = luaArgListToJSArray(apply, self.exprs)
	
	-- javascript-specific, turn global return's into exports
	if not self.parent.parent then
		return 'export { _export = '..s..'}'
	end
	
	-- javascript doesn't support multiple returns
	-- so I have to return multiple values as an array
	-- sooo TODO also track who is calling the function?
	-- because if the caller is assigning multiple values to an array-wrapped multiple return then - as long as we wrap the multiple-assign with a [] then JS won't poop its pants
	return 'return '..s
	-- then again ... we can avoid the conditional static analysis and instead just wrap all returns in []'s no matter if it's 1 or many
end

function ast._string:toJS(apply)
	if self.value:find'\n' and #self.value > 10 then
		-- what gets escaped in multiline strings in javascript?
		-- the string endline ` needs to be escaped
		-- \r \n \t etc isn't
		return '`'..self.value:gsub('`', '\\`')..'`'
	else
		return self:toLua(apply)
	end
end

local srcdir = ...
assert(srcdir, "expected srcdir")
srcdir = path(srcdir)
assert(srcdir.path:sub(-1,-1) == '/')
print('srcdir', srcdir)

local dstdir = path'out'

local fns = table()
for f in srcdir:rdir() do fns:insert(f) end
fns = fns
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

	local tree = LuaParser.parse(luacode)
	--print('lua:')
	--print(tree)
	--print()

	local jscode = "import * as lua from './lua.js';\n"
					.."Object.entries(lua).forEach(([k,v]) => { window[k] = v; });\n\n"
					..tostring(tree)

	local dstpath = dstdir/fn:gsub('%.lua$', '.js')
	dstpath:getdir():mkdir(true)
	print('writing to '..dstpath)
	dstpath:write(jscode)
end
