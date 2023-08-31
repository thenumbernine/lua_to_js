/* 
This file holds functions necessary for glue code.

Right now I'm prefixing all functions with `lua_`
This will probably collide with generated code.
For namespace collision's sake it is probably best to put these all into their own table,
but for performance's sake I'm thinking it's not.
Maybe later I'll make the prefix interchangeable, or write an `import` call generator which does exactly this.

Lua types:
	nil <-> undefined
	boolean <-> boolean
	number <-> number
	string <-> string
	function <-> function ... or function* whatever the non-"this" functions in JS are.
	table <-> LuaTable
	userdata <-> hmmm....
	thread <-> also hmmm....
	cdata <-> also hmmmm....
*/

//ok this is laughable about JS:
// you can't modify an imported function ...
// but you can modify builtin tables like Math:
Math.huge = Infinity;

const lua_nil = undefined;

export error = (x) => {
	throw x;
};

const lua_mappedType = {
	'undefined':'nil',
	'boolean':'boolean',
	'number':'number',
	'string':'string',
	'function':'function',
	'object':'table',
};

const internal_type = (x) => {
	const xJSType = typeof(x);
	const mappedType = lua_mappedType[xJSType];
	if (mappedType !== undefined) return mappedType;
	error("unknown JS type: "+xJSType);
};

export const type = (x) => {
	return [internal_type(x)];
};

// in Lua, only false and nil evaluate to false within conditions
// in JS, so does 0 and ''
export const lua_toboolean = (a) => {
	return a !== undefined && a !== null && a !== false;
};

export const lua_or = (a,b) => {
	return lua_toboolean(a) ? a : b;
};

export const lua_and = (a,b) => {
	//return lua_not(lua_or(a,b));
	return lua_toboolean(a) ? b : a;
};

export const internal_assert = (expr, msg, ...rest) => {
	if (!lua_toboolean(expr)) {
		error(msg || "assertion failed!");
	}
};

export const assert = (...vararg) => {
	internal_assert(...vararg);
	return vararg;
};

export class lua_table {
	constructor(kvs, mt) {
		internal_assert(mt === lua_nil || mt instanceof lua_table);
		this.mt = mt;
		
		// JS tables cannot be key'd by objects or functions, just strings and numbers (which are probably converted to strings under the hood anyways)
		// so for functional compatability's sake, lets just store as a list of key-value pairs here.
		this.t = new Map();
		if (kvs) {
			kvs.forEach(([k,v]) => {
				this.t.set(k, v);
			});
		}
	}

	len() {
		//TODO lua approx len method
		// what do they do? double + bisection to find the len in log(n) time?
		// until then ...
		let max = 0;
		for (let k of this.t.keys()) {
			if (internal_type(k) === 'number') {
				max = Math.max(max, k);
			}
		}
		return max;
	}
}

const internal_assertIsTable = (x) => {
	if (!(x instanceof lua_table)) {
		error("got a JS object that wasn't a lua_table");
	}
};

//set these via debug.setmetatable()
const internal_globalMetatables = {
	'nil':lua_nil,
	'boolean':lua_nil,
	'number':lua_nil,
	'string':new lua_table(),	// TODO i think lua_load(string) is what does this.
	'function':lua_nil,
	'table':lua_nil,	// can't debug.setmetatable this because every table has a different m.t., so all tables are initially no m.t.
	'userdata':lua_nil,	// not sure if there's a default userdata metatable, or like table it is per-table only ...
	'thread':lua_nil,
	'cdata':lua_nil,
};

//index is 0-based
const internal_assertArgIsType = (args, index, type, name) => {
	const argtype = internal_type(args[index]);
	if (argtype !== type) {
		error("bad argument #"+(index+1)+" to '"+name+"' ("+type+" expected, got "+argtype+")");
	}
};

const internal_assertArgIsTypeOrNil = (args, index, type, name) => {
	const argtype = internal_type(args[index]);
	if (argtype !== type && argtype !== 'nil') {
		error("bad argument #"+(index+1)+" to '"+name+"' ("+type+" or nil expected)");
	}
	return argtype;
};

const internal_rawget = (...vararg) => {
	internal_assertArgIsType(vararg, 0, 'table', 'rawget');
	const [table, key] = vararg;
	internal_assertIsTable(table);
	return table.t.get(key);
};

const internal_getmetatable(x) => {
	const xtype = internal_type(x);
	// hmm, userdata ... thread ... table ... cdata ...
	if (xtype === 'table') {
		//only lua_table objects can be used, because they will hold our .metatable etc
		internal_assertIsTable(x);
		return x.mt;
	}
	const mt = internal_globalMetatables[xtype];
	if (mt !== lua_nil) {
		const mtmt = internal_rawget(mt, '__metatable');
		if (mtmt !== lua_nil) return mtmt;
	}
	return mt;
};

export const getmetatable = (...vararg) => {
	return [internal_getmetatable(...vararg)];
};

export const setmetatable = (...vararg) => {
	internal_assertArgIsType(vararg, 0, 'table', 'setmetatble');
	internal_assertArgIsTypeOrNil(vararg, 1, 'table', 'setmetatble');
	const [t, mt] = vararg;
	const prevmt = t.mt;
	if (prevmt !== lua_nil) {
		const prevmtmt = internal_rawget(prevmt, '__metatable');
		if (prevmtmt !== lua_nil) {
			error("cannot change a protected metatable");
		}
	}
	t.mt = mt;
	return [table];
};

export const rawget = (...vararg) => {
	return [internal_rawget(...vararg)];
};

const internal_rawset = (...vararg) => {
	internal_assertArgIsType(vararg, 0, 'table', 'rawset');
	const [table, key, value] = vararg;
	internal_assertIsTable(table);
	// TODO should I map.delete if value is lua_nil?
	// fwiw in the Lua code, nil values persist in tables.
	table.t.set(key, value);
	return table;
};

export const rawset = (...vararg) => {
	return [internal_rawset(...vararg)];
};

export const rawequal = (op1, op2) => {
	return [op1 === op2];
};

const internal_tonumber = (...vararg) => {
	const [x, base] = vararg;
	const xtype = internal_type(x);
	if (xtype === 'string') {
		const basetype = internal_assertArgIsTypeOrNil(vararg, 1, 'number', 'tonumber');
		// strange enough, JS parseInt only supports multiple bases while parseFloat doesn't
		// and likewise tonumber() with a string of a decimal + non-10 base returns nil, but for non-10 base returns fine for integers. 
		let result;
		if (basetype === 'number') {
			result = parseInt(x, base);
		} else {
			result = parseFloat(x);
		}
		// JS parseInt and parseFloat failure returns NaN
		if (result !== result) return lua_nil;
		return result;
	} else if (xtype === 'number') {
		return x;
	} else {
		return lua_nil;
	}
};

export const tonumber = (...vararg) => {
	return [internal_tonumber(...vararg)];
};

// If a metatable's metatable's __index is set then does the metatable call it?  NO.
// so use rawget(mt, e)
const internal_metaop = (o, e) => {
	const mt = internal_getmetatable(o);
	if (mt === lua_nil) return lua_nil;
	return internal_rawget(mt, e);
};

const internal_getbinhandler = (op1, op2, event) => {
	// TODO does js || short-circuit work like Lua does?
	// I know it doesn't for == comparison
	// real question: is anything but functions going to appear in metatables?
	// yes ... empty strings will make JS || retarded.
	return lua_or(
		internal_metaop(op1, event),
		internal_metaop(op2, event)
	);
};

const internal_arith = (op1, op2, func, event) => {
	const o1 = internal_tonumber(op1);
	const o2 = internal_tonumber(op2);
	if (o1 !== lua_nil && o2 !== lua_nil) {
		return func(o1, o2);
	} else {
		const h = internal_getbinhandler(op1, op2, event);
		if (h !== lua_nil) {
			return ...lua_call(h, op1, op2);
		} else {
			error("attempt to perform arithmetic on a "+internal_type(op1)+" value");
		}
	}
};

//returns a single value
export const lua_add = (op1, op2) => {
	return internal_arith(op1, op2, (x,y) => { return x + y; }, '__add');
};

//returns a single value
export const lua_sub = (op1, op2) => {
	return internal_arith(op1, op2, (x,y) => { return x - y; }, '__sub');
};

//returns a single value
export const lua_mul = (op1, op2) => {
	return internal_arith(op1, op2, (x,y) => { return x * y; }, '__mul');
};

//returns a single value
export const lua_div = (op1, op2) => {
	return internal_arith(op1, op2, (x,y) => { return x / y; }, '__div');
};

//returns a single value
// '%' in Lua is math modulo, which means negatives are made positive
// '%' in JS is programmer modulo, which means negatives stay negative.
export const lua_mod = (op1, op2) => {
	return internal_arith(op1, op2, (x,y) => { return x - Math.floor(x/y)*y; }, '__mod');
};

//returns a single value
export const lua_pow = (op1, op2) => {
	return internal_arith(op1, op2, (x,y) => { return Math.pow(x,y); }, '__pow');
};

//returns a single value
export const lua_unm = (op) => {
	const o = internal_tonumber(op);
	if (o !== lua_nil) {
		return -o;
	} else {
		const h = internal_metaop(op, '__unm');
		if (h !== lua_nil) {
			return ...lua_call(h, op);
		} else {
			error("attempt to perform arithmetic on a "+internal_type(op)+" value");
		}
	}
};

//returns a single value
const internal_tostring = (op) => {
	const h = internal_metaop(op, '__tostring');
	if (h !== lua_nil) {
		return ...lua_call(h, op);
	}

	const optype = internal_type(op);
	if (optype === 'nil') {
		return 'nil';
	} else if (optype === 'boolean') {
		return op.toString();
	} else if (optype === 'number') {
		return op.toString();
	} else if (optype === 'string') {
		return op;
	} else if (optype === 'function') {
		return 'function: 0x0';
	} else if (optype === 'table') {
		return 'table: 0x0';
	} else if (optype === 'userdata') {
		return 'userdata: 0x0';
	} else if (optype === 'thread') {
		return 'thread: 0x0';
	} else if (optype === 'cdata') {
		return 'cdata: 0x0';
	}
	error("here");
};

//returns an array <-> multret
export const tostring = (...vararg) => {
	return [internal_tostring(...vararg)];
};

//returns a single value
export const lua_concat = (op1, op2) => {
	const op1type = internal_type(op1);
	const op2type = internal_type(op2);
	if ((op1type === 'string' || op1type === 'number') &&
		(op2type === 'string' || op2type === 'number')
	) {
		return internal_tostring(op1) + internal_tostring(op2);
	} else {
		const h = internal_getbinhandler(op1, op2, '__concat');
		if (h !== lua_nil) {
			return ...lua_call(h, op1, op2);
		} else {
			error("attempt to concatenate a "+internal_type(op1)+" value");
		}
	}
};

export const lua_len = (op) {
	const optype = internal_type(op);
	if (optype === 'string') {
		return op.length;
	} else if (optype == 'table') {
		return op.len();
	} else {
		const h = internal_metaop(op, '__len');
		if (h !== lua_nil) {
			return ...lua_call(h, op);
		} else {
			error("attempted to get length of a "+optype+" value");
		}
	}
};

// assert type(op1) == type(op2)
const internal_getcomphandler = (op1, op2, event) => {
	const mm1 = internal_metaop(op1, event);
	const mm2 = internal_metaop(op2, event);
	if (mm1 === mm2) return mm1;
	return lua_nil;
};

export const lua_eq = (op1,op2) => {
	const op1type = internal_type(op1);
	const op2type = internal_type(op2);
	if (op1type !== op2type) {
		return false;
	}
	if (op1 === op2) {
		return true;
	}
	const h = internal_getcomphandler(op1, op2, '__eq');
	if (h !== lua_nil) {
		return ...lua_call(h, op1, op2);
	} else {
		return false;
	}
};

// 'not' in Lua returns true when the value is either 'false' or 'nil'.
// not zero, not empty strings, not other weird javascript nonsense that would evaluate to 'false' under == comparison.
// 'not' in Lua returns 'true' if the value is 'false' or 'nil'
// however in JS, '!' returns 'true' for 'false', 'null', 'undefined', '0', '""' ...
// because JS is retarded
export const lua_not = (a) => {
	return !lua_toboolean(a);
};

export const lua_ne = (op1, op2) => {
	return lua_not(lua_eq(op1, op2));
};

export const lua_lt = (op1, op2) => {
	const op1type = internal_type(op1);
	const op2type = internal_type(op2);
	if (op1type === 'number' && op2type === 'number') {
		return op1 < op2;
	} else if (op1type === 'string' && op2type === 'string') {
		return op1 < op2;
	} else {
		const h = internal_getcomphandler(op1, op2, '__lt');
		if (h !== lua_nil) {
			return ...lua_call(h, op1, op2);
		} else {
			error("attempt to compare a "+op1type+" value and a "+op2type+" value");
		}
	}
};

export const lua_gt = (op1, op2) => {
	return lua_lt(op2, op1);
};

export const lua_le = (op1, op2) => {
	const op1type = internal_type(op1);
	const op2type = internal_type(op2);
	if (op1type === 'number' && op2type === 'number') {
		return op1 <= op2;
	} else if (op1type === 'string' && op2type === 'string') {
		return op1 <= op2;
	} else {
		const h = internal_getcomphandler(op1, op2, '__le');
		if (h !== lua_nil) {
			return ...lua_call(h, op1, op2);
		} else {
			const h = internal_getcomphandler(op1, op2, '__lt');
			if (h !== lua_nil) {
				return lua_not(op2, op1);
			} else {
				error("attempt to compare a "+op1type+" value and a "+op2type+" value");
			}
		}
	}
};

export const lua_ge = (op1, op2) => {
	return lua_le(op2, op1);
};

export const lua_index = (table, key) => {
	const tabletype = internal_type(table);
	let h;
	if (tabletype === 'table') {
		const v = internal_rawget(table, key);
		if (v !== lua_nil) return v;
		h = internal_metaop(table, '__index');
		if (h === lua_nil) return lua_nil;
	} else {
		h = internal_metaop(table, '__index');
		if (h === lua_nil) {
			error("attempt to index a "+tabletype+" value");
		}
	}
	if (internal_type(h) === 'function') {
		return ...lua_call(h, table, key);
	} else {
		return lua_index(h, key);
	}
};

export const lua_newindex = (table, key, value) => {
	const tabletype = internal_type(table);
	let h;
	if (tabletype === 'table') {
		const v = internal_rawget(table, key);
		if (v !== lua_nil) {
			internal_rawset(table, key, value);
			return;
		}
		h = internal_metaop(table, '__newindex');
		if (h === lua_nil) {
			internal_rawset(table, key, value);
		}
	} else {
		h = internal_metaop(table, '__newindex');
		if (h === lua_nil) {
			error("attempt to index a "+tabletype+" value");
		}
	}
	if (internal_type(h) === 'function') {
		...lua_call(h, table, key, value);
	} else {
		//h[key] = value;
		lua_newindex(h, key, value);
	}
};

//return an array <-> Lua mult-ret
export const lua_call = (func, ...vararg) => {
	const functype = internal_type(func);
	if (functype === 'function') {
		// func also needs to return an array
		return func(...vararg);
	} else {
		const h = internal_metaop(func, '__call');
		if (h !== lua_nil) {
			//recursive ... if __call isn't a function but is a table with a __call metamethod ...
			// this risks infinite loop
			return lua_call(h, func, ...vararg);
		} else {
			error("attemp to call a "+functype+" value");
		}
	}
	return [];
};

// (obj):key(...)
export const lua_callself = (obj, key, ...vararg) => {
	return lua_call(lua_index(obj, key), obj, ...vararg);
};

export const next = (...vararg) => {
	internal_assertArgIsType(vararg, 0, 'table', 'next');
	const table = vararg[0];
	internal_assertIsTable(table);
	const keys = table.map.keys();
	const index = vararg.length > 1 ? vararg[1] : lua_nil;
	// 1) enumerate all keys (or should I cache them enumerated? hmm)
	// 2) find the next key
	// 3) return it
	let result = keys.next();
	for (; !result.done; result = keys.next()) {
		if (result.value === index) {
			result = keys.next();
			break;
		}
	}
	if (result.done) return;
	return [result.value, lua_index(table, result.value)];
};

const internal_inext = (...vararg) => {
	const table = vararg[0];
	const index = (vararg.length > 1) 
		? vararg[1]
		: 0;
	++index;
	const elem = lua_index(table, index);
	if (elem === lua_nil) {
		return;
	} else {
		return [index, elem];
	}
};

// TODO make this work for Lua for-loop iterators
export const pairs = (...vararg) => {
	internal_assertArgIsType(vararg, 0, 'table', 'pairs');
	const table = vararg[0];
	internal_assertIsTable(table);
	return [next, table];
};

// TODO don't use 'next', instead use something to track iteration using JS iterators
export const ipairs = (t) => {
	internal_assertArgIsType(vararg, 0, 'table', 'ipairs');
	const table = vararg[0];
	internal_assertIsTable(table);
	return [internal_inext, table];
};

export const select = (i, ...rest) => {
	if (i > 0) {
		return rest.slice(i-1);
	} else if (i < 0) {
		return rest.slice(i);
	} else {
		error("bad argument #1 to 'select' (index out of range)");
	}
};

export const unpack = (...vararg) => {
	internal_assertArgIsType(vararg, 0, 'table', 'unpack');
	internal_assertArgIsTypeOrNil(vararg, 1, 'number', 'unpack');
	internal_assertArgIsTypeOrNil(vararg, 2, 'number', 'unpack');
	let [table, i, j] = vararg;
	if (vararg.length < 2) i = 1;
	if (vararg.length < 3) j = lua_len(table);
	const result = [];
	for (let k = i; k <= j; ++k) {
		result[k-i] = internal_rawget(table, k);
	}
	return result;
};

export const print = (...vararg) => {
	console.log(...vararg);
};

export const xpcall = (f, cb, ...vars) => {
	try {
		return [true, ...f(...vars)];
	} catch (e) {
		// TODO can 'e' be not a string, like an object?  if so then translate to Lua types.
		e = ''+e;
		return [false, ...cb(e)];
	}
};

export const pcall = (f, ...vars) => {
	try {
		return [true, ...f(...vars)];
	} catch (e) {
		// TODO can 'e' be not a string, like an object?  if so then translate to Lua types.
		e = ''+e;
		return [false, e];
	}
}

export const collectgarbage = () => {};

export const getfenv = (...vararg) => { throw 'TODO'; };
export const setfenv = (...vararg) => { throw 'TODO'; };

export const dofile = (...vararg) => { throw 'TODO'; };
export const load = (...vararg) => { throw 'TODO'; };
export const loadfile = (...vararg) => { throw 'TODO'; };
export const loadstring = (...vararg) => { throw 'TODO'; };

// can I export _G = window; ?
export _VERSION = 'Lua 5.1';

const coroutine = new lua_table({
	['create', (f) => {
		// wait until the first resume i.e. call
		return () => { return f(); }	// JS iterable works as Lua coroutines.
	}],
	['resume', (co, ...vars) => {
		return co.next(...vars);	//TODO ...
	}],
	['running', () => {
		return [!co.done];
	}],
	['status', (co) => {
		// TODO if we're the calling thread then return 'running'
		// otherwise if we're not done then return 'suspended'
		// unless we're resuming anotehr but waiting for that to yield, then return 'normal'	
		// then if done, then return 'dead'
		throw 'TODO';
	}],
	['wrap', (f) => {
		throw 'TODO';
	}],
	['yield', (f) => {
		throw 'TODO';
	}],
]);

// Lua 5.1
const module = (name, ...vars) => {
	throw 'TODO';
};

// package
const _js_package = new lua_table([
	['cpath', ';;'],
	['loaded', new lua_table([
		['_G', window],	//TODO this won't work, 'window' is not a 'lua_table'
	])],
	['loaders', new lua_table([
		[1, () => {
			//TODO search preload
			throw 'HERE';
		}],
		[2, () => {
			//TODO search package.path
			throw 'HERE';
		}],
		[3, () => {
			//TODO search package.cpath
			throw 'HERE';
		}],
		[4, () => {
			//TODO search all_in_one loader
			throw 'HERE';
		}],
	])],
	//TODO loadlib
	//TODO path
	//TODO preload
	//TODO seeall
]);

//package.package = package
internal_rawset(
	internal_rawget(_js_package, 'loaded'),
	'package',
	_js_package);

const require = (modname) => {
	const t = lua_index(_js_package, 'loaded');
};

// set to global namespace:
export coroutine;
export module;
export _js_package;	//package 
export require;
