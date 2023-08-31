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
// but you can modify Math:
Math.huge = Infinity;

const lua_nil = undefined;

export function lua_error(x) {
	throw x;
}

const lua_mappedType = {
	'undefined':'nil',
	'boolean':'boolean',
	'number':'number',
	'string':'string',
	'function':'function',
	'object':'table',
};

export function lua_type(x) {
	const xJSType = typeof(x);
	const mappedType = lua_mappedType[xJSType];
	if (mappedType !== undefined) return mappedType;
	lua_error("unknown JS type: "+xJSType);
}

// in Lua, only false and nil evaluate to false within conditions
// in JS, so does 0 and ''
export function lua_toboolean(a) {
	return a !== undefined && a !== null && a !== false;
}

export function lua_or(a,b) {
	return lua_toboolean(a) ? a : b;
}

export function lua_and(a,b) {
	//return lua_not(lua_or(a,b));
	return lua_toboolean(a) ? b : a;
}

export function assert(expr, msg, ...rest) {
	if (!lua_toboolean(expr)) {
		lua_error(msg || "assertion failed");
	}
	// TODO proper mult-ret ...
	return [expr, msg, ...rest];
}

export class lua_table {
	constructor(kvs, mt) {
		assert(mt === lua_nil || mt instanceof lua_table);
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
			if (lua_type(k) === 'number') {
				max = Math.max(max, k);
			}
		}
		return max;
	}
}

function lua_assertIsTable(x) {
	if (!(x instanceof lua_table)) {
		lua_error("got a JS object that wasn't a lua_table");
	}
}

//set these via debug.setmetatable()
const lua_globalMetatables = {
	'nil':lua_nil,
	'boolean':lua_nil,
	'number':lua_nil,
	'string':new lua_table(),	// TODO i think lua_load(string) is what does this.
	'function':lua_nil,
	//'table':lua_nil,	// can't debug.setmetatable this because every table has a different m.t., so all tables are initially no m.t.
	//'userdata':lua_nil,	// not sure if there's a default userdata metatable, or like table it is per-table only ...
	'thread':lua_nil,
	//'cdata':lua_nil,
};

export function lua_getmetatable(x) {
	const xtype = lua_type(x);
	// hmm, userdata ... thread ... table ... cdata ...
	if (xtype === 'table') {
		//only lua_table objects can be used, because they will hold our .metatable etc
		lua_assertIsTable(x);
		return x.mt;
	}
	const mt = lua_globalMetatables[xtype];
	return mt;
}

//index is 0-based
function lua_assertArgIsType(args, index, type, name) {
	const argtype = lua_type(args[index]);
	if (argtype !== type) {
		lua_error("bad argument #"+(index+1)+" to '"+name+"' ("+type+" expected, got "+argtype+")");
	}
}

export function lua_rawget(...vararg) {
	lua_assertArgIsType(vararg, 0, 'table', 'rawget');
	const [table, key] = vararg;
	lua_assertIsTable(table);
	return table.t.get(key);
}

export function lua_rawset(...vararg) {
	lua_assertArgIsType(vararg, 0, 'table', 'rawset');
	const [table, key, value] = vararg;
	lua_assertIsTable(table);
	// TODO should I map.delete if value is lua_nil?
	// fwiw in the Lua code, nil values persist in tables.
	table.t.set(key, value);
}


// TODO if a metatable's metatable's __index is set then does the metatable call it?
function lua_metaop(o, e) {
	const m = lua_getmetatable(o);
	if (m === lua_nil) return lua_nil;
	return m[e];
}

export function lua_tonumber(x, base) {
	const xtype = lua_type(x);
	if (xtype === 'string') {
		// strange enough, JS parseInt only supports multiple bases while parseFloat doesn't
		// and likewise tonumber() with a string of a decimal + non-10 base returns nil, but for non-10 base returns fine for integers. 
		const basetype = lua_type(base);
		assert(basetype === 'number' || basetype === 'nil');
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
}

function lua_getbinhandler(op1, op2, event) {
	// TODO does js || short-circuit work like Lua does?
	// I know it doesn't for == comparison
	// real question: is anything but functions going to appear in metatables?
	// yes ... empty strings will make JS || retarded.
	return lua_or(lua_metaop(op1, event), lua_metaop(op2, event));
}

export function lua_arith(op1, op2, func, event) {
	const o1 = lua_tonumber(op1);
	const o2 = lua_tonumber(op2);
	if (o1 !== lua_nil && o2 !== lua_nil) {
		return func(o1, o2);
	} else {
		const h = lua_getbinhandler(op1, op2, event);
		if (h !== lua_nil) {
			return h(op1, op2);
		} else {
			lua_error("attempt to perform arithmetic on a "+lua_type(op1)+" value");
		}
	}
}

export function lua_add(op1, op2) {
	return lua_arith(op1, op2, (x,y) => { return x + y; }, '__add');
}
export function lua_sub(op1, op2) {
	return lua_arith(op1, op2, (x,y) => { return x - y; }, '__sub');
}
export function lua_mul(op1, op2) {
	return lua_arith(op1, op2, (x,y) => { return x * y; }, '__mul');
}
export function lua_div(op1, op2) {
	return lua_arith(op1, op2, (x,y) => { return x / y; }, '__div');
}
// '%' in Lua is math modulo, which means negatives are made positive
// '%' in JS is programmer modulo, which means negatives stay negative.
export function lua_mod(op1, op2) {
	return lua_arith(op1, op2, (x,y) => { return x - Math.floor(x/y)*y; }, '__mod');
}
export function lua_pow(op1, op2) {
	return lua_arith(op1, op2, (x,y) => { return Math.pow(x,y); }, '__pow');
}

export function lua_unm(op) {
	const o = lua_tonumber(op);
	if (o !== lua_nil) {
		return -o;
	} else {
		const h = lua_metaop(op, '__unm');
		if (h !== lua_nil) {
			return h(op);
		} else {
			lua_error("attempt to perform arithmetic on a "+lua_type(op)+" value");
		}
	}
}

export function lua_concat(op1, op2) {
	const op1type = lua_type(op1);
	const op2type = lua_type(op2);
	if ((op1type === 'string' || op1type === 'number') &&
		(op2type === 'string' || op2type === 'number')
	) {
		return lua_tostring(op1)+lua_tostring(op2);
	} else {
		const h = lua_getbinhandler(op1, op2, '__concat');
		if (h !== lua_nil) {
			return h(op1, op2);
		} else {
			lua_error("attempt to concatenate a "+lua_type(op1)+" value");
		}
	}
}

export function lua_len(op) {
	const optype = lua_type(op);
	if (optype === 'string') {
		return op.length;
	} else if (optype == 'table') {
		return op.len();
	} else {
		const h = lua_metaop(op, '__len');
		if (h !== lua_nil) {
			return h(op);
		} else {
			lua_error("attempted to get length of a "+optype+" value");
		}
	}
}

// assert lua_type(op1) == lua_type(op2)
function lua_getcomphandler(op1, op2, event) {
	const mm1 = lua_metaop(op1, event);
	const mm2 = lua_metaop(op2, event);
	if (mm1 === mm2) return mm1;
	return lua_nil;
}

export function lua_eq(op1,op2) {
	const op1type = lua_type(op1);
	const op2type = lua_type(op2);
	if (op1type !== op2type) {
		return false;
	}
	if (op1 === op2) {
		return true;
	}
	const h = lua_getcomphandler(op1, op2, '__eq');
	if (h !== lua_nil) {
		return h(op1, op2);
	} else {
		return false;
	}
}

// 'not' in Lua returns true when the value is either 'false' or 'nil'.
// not zero, not empty strings, not other weird javascript nonsense that would evaluate to 'false' under == comparison.
// 'not' in Lua returns 'true' if the value is 'false' or 'nil'
// however in JS, '!' returns 'true' for 'false', 'null', 'undefined', '0', '""' ...
// because JS is retarded
export function lua_not(a) {
	return !lua_toboolean(a);
}

export function lua_ne(op1, op2) {
	return lua_not(lua_eq(op1, op2));
}

export function lua_lt(op1, op2) {
	const op1type = lua_type(op1);
	const op2type = lua_type(op2);
	if (op1type === 'number' && op2type === 'number') {
		return op1 < op2;
	} else if (op1type === 'string' && op2type === 'string') {
		return op1 < op2;
	} else {
		const h = lua_getcomphandler(op1, op2, '__lt');
		if (h !== lua_nil) {
			return h(op1, op2);
		} else {
			lua_error("attempt to compare a "+op1type+" value and a "+op2type+" value");
		}
	}
}

export function lua_gt(op1, op2) {
	return lua_lt(op2, op1);
}

export function lua_le(op1, op2) {
	const op1type = lua_type(op1);
	const op2type = lua_type(op2);
	if (op1type === 'number' && op2type === 'number') {
		return op1 <= op2;
	} else if (op1type === 'string' && op2type === 'string') {
		return op1 <= op2;
	} else {
		const h = lua_getcomphandler(op1, op2, '__le');
		if (h !== lua_nil) {
			return h(op1, op2);
		} else {
			const h = lua_getcomphandler(op1, op2, '__lt');
			if (h !== lua_nil) {
				return lua_not(op2, op1);
			} else {
				lua_error("attempt to compare a "+op1type+" value and a "+op2type+" value");
			}
		}
	}
}

export function lua_ge(op1, op2) {
	return lua_le(op2, op1);
}

export function lua_index(table, key) {
	const tabletype = lua_type(table);
	let h;
	if (tabletype === 'table') {
		const v = lua_rawget(table, key);
		if (v !== lua_nil) return v;
		h = lua_metaop(table, '__index');
		if (h === lua_nil) return lua_nil;
	} else {
		h = lua_metaop(table, '__index');
		if (h === lua_nil) {
			lua_error("attempt to index a "+tabletype+" value");
		}
	}
	if (lua_type(h) === 'function') {
		return h(table, key);
	} else {
		return lua_index(h, key);
	}
}

export function lua_newindex(table, key, value) {
	const tabletype = lua_type(table);
	let h;
	if (tabletype === 'table') {
		const v = lua_rawget(table, key);
		if (v !== lua_nil) {
			lua_rawset(table, key, value);
			return;
		}
		h = lua_metaop(table, '__newindex');
		if (h === lua_nil) lua_rawset(table, key, value);
	} else {
		h = lua_metaop(table, '__newindex');
		if (h === lua_nil) {
			lua_error("attempt to index a "+tabletype+" value");
		}
	}
	if (lua_type(h) === 'function') {
		h(table, key, value);
	} else {
		//h[key] = value;
		lua_newindex(h, key, value);
	}
}

// TODO Here ... for the sake of multret, I think I need to wrap all functions and make sure they all return []'s
export function lua_call(func, ...vararg) {
	const functype = lua_type(func);
	if (functype === 'function') {
		return func(...vararg);
	} else {
		const h = lua_metaop(func, '__call');
		if (h !== lua_nil) {
			return h(func, ...vararg);
		} else {
			lua_error("attemp to call a "+functype+" value");
		}
	}
}

// (obj):key(...)
export function lua_callself(obj, key, ...vararg) {
	return lua_call(lua_index(obj, key), obj, ...vararg);
}

export function next(...vararg) {
	lua_assertArgIsType(vararg, 0, 'table', 'next');
	const table = vararg[0];
	lua_assertIsTable(table);
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
}

function inext(...vararg) {
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
}

// TODO make this work for Lua for-loop iterators
export function pairs(...vararg) {
	lua_assertArgIsType(vararg, 0, 'table', 'pairs');
	const table = vararg[0];
	lua_assertIsTable(table);
	return [next, table];
}

// TODO don't use 'next', instead use something to track iteration using JS iterators
export function ipairs(t) {
	lua_assertArgIsType(vararg, 0, 'table', 'ipairs');
	const table = vararg[0];
	lua_assertIsTable(table);
	return [inext, table];
}

export function select(i, ...rest) {
	return rest.slice(i-1);
}

export function print(...vararg) {
	console.log(...vararg);
}
