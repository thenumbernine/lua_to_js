// some builtins necessary for glue code

export function luaLen(a) {
	// works for strings and arrays
	// but not for tables ...
	return a.length;
}

// 'not' in Lua returns true when the value is either 'false' or 'nil'.
// not zero, not empty strings, not other weird javascript nonsense that would evaluate to 'false' under == comparison.
export function luaNot(a) {
	return a === false || a === null || a === undefined;
}

export function luaMod(a,b) {
	return ((a % b) + b) % b;
}

export function* pairs(t) {
	for (let k in t) {
		yield([k, t[k]]);
	}
}

export function* ipairs(t) {
	for (let i = 0; i < tlen; ++i) {
		yield([i, t[i]]);
	}
}

export function select(i, ...rest) {
	return rest.slice(i-1);
}
