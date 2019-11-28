try {
	(function(scope) {
	  "use strict";

	  function F(arity, fun, wrapper) {
		wrapper.a = arity;
		wrapper.f = fun;
		return wrapper;
	  }

	  function F2(fun) {
		return F(2, fun, function(a) {
		  return function(b) {
			return fun(a, b);
		  };
		});
	  }
	  function F3(fun) {
		return F(3, fun, function(a) {
		  return function(b) {
			return function(c) {
			  return fun(a, b, c);
			};
		  };
		});
	  }
	  function F4(fun) {
		return F(4, fun, function(a) {
		  return function(b) {
			return function(c) {
			  return function(d) {
				return fun(a, b, c, d);
			  };
			};
		  };
		});
	  }
	  function F5(fun) {
		return F(5, fun, function(a) {
		  return function(b) {
			return function(c) {
			  return function(d) {
				return function(e) {
				  return fun(a, b, c, d, e);
				};
			  };
			};
		  };
		});
	  }
	  function F6(fun) {
		return F(6, fun, function(a) {
		  return function(b) {
			return function(c) {
			  return function(d) {
				return function(e) {
				  return function(f) {
					return fun(a, b, c, d, e, f);
				  };
				};
			  };
			};
		  };
		});
	  }
	  function F7(fun) {
		return F(7, fun, function(a) {
		  return function(b) {
			return function(c) {
			  return function(d) {
				return function(e) {
				  return function(f) {
					return function(g) {
					  return fun(a, b, c, d, e, f, g);
					};
				  };
				};
			  };
			};
		  };
		});
	  }
	  function F8(fun) {
		return F(8, fun, function(a) {
		  return function(b) {
			return function(c) {
			  return function(d) {
				return function(e) {
				  return function(f) {
					return function(g) {
					  return function(h) {
						return fun(a, b, c, d, e, f, g, h);
					  };
					};
				  };
				};
			  };
			};
		  };
		});
	  }
	  function F9(fun) {
		return F(9, fun, function(a) {
		  return function(b) {
			return function(c) {
			  return function(d) {
				return function(e) {
				  return function(f) {
					return function(g) {
					  return function(h) {
						return function(i) {
						  return fun(a, b, c, d, e, f, g, h, i);
						};
					  };
					};
				  };
				};
			  };
			};
		  };
		});
	  }

	  function A2(fun, a, b) {
		return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
	  }
	  function A3(fun, a, b, c) {
		return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
	  }
	  function A4(fun, a, b, c, d) {
		return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
	  }
	  function A5(fun, a, b, c, d, e) {
		return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
	  }
	  function A6(fun, a, b, c, d, e, f) {
		return fun.a === 6
		  ? fun.f(a, b, c, d, e, f)
		  : fun(a)(b)(c)(d)(e)(f);
	  }
	  function A7(fun, a, b, c, d, e, f, g) {
		return fun.a === 7
		  ? fun.f(a, b, c, d, e, f, g)
		  : fun(a)(b)(c)(d)(e)(f)(g);
	  }
	  function A8(fun, a, b, c, d, e, f, g, h) {
		return fun.a === 8
		  ? fun.f(a, b, c, d, e, f, g, h)
		  : fun(a)(b)(c)(d)(e)(f)(g)(h);
	  }
	  function A9(fun, a, b, c, d, e, f, g, h, i) {
		return fun.a === 9
		  ? fun.f(a, b, c, d, e, f, g, h, i)
		  : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
	  }

	  console.warn(
		"Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets."
	  );

	  // EQUALITY

	  function _Utils_eq(x, y) {
		for (
		  var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		  isEqual && (pair = stack.pop());
		  isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		) {}

		return isEqual;
	  }

	  function _Utils_eqHelp(x, y, depth, stack) {
		if (depth > 100) {
		  stack.push(_Utils_Tuple2(x, y));
		  return true;
		}

		if (x === y) {
		  return true;
		}

		if (typeof x !== "object" || x === null || y === null) {
		  typeof x === "function" && _Debug_crash(5);
		  return false;
		}

		/**/
		if (x.$ === "Set_elm_builtin") {
		  x = $elm$core$Set$toList(x);
		  y = $elm$core$Set$toList(y);
		}
		if (x.$ === "RBNode_elm_builtin" || x.$ === "RBEmpty_elm_builtin") {
		  x = $elm$core$Dict$toList(x);
		  y = $elm$core$Dict$toList(y);
		}
		//*/

		/**_UNUSED/
if (x.$ < 0)
{
	x = $elm$core$Dict$toList(x);
	y = $elm$core$Dict$toList(y);
}
//*/

		for (var key in x) {
		  if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack)) {
			return false;
		  }
		}
		return true;
	  }

	  var _Utils_equal = F2(_Utils_eq);
	  var _Utils_notEqual = F2(function(a, b) {
		return !_Utils_eq(a, b);
	  });

	  // COMPARISONS

	  // Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
	  // the particular integer values assigned to LT, EQ, and GT.

	  function _Utils_cmp(x, y, ord) {
		if (typeof x !== "object") {
		  return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
		}

		/**/
		if (x instanceof String) {
		  var a = x.valueOf();
		  var b = y.valueOf();
		  return a === b ? 0 : a < b ? -1 : 1;
		}
		//*/

		/**_UNUSED/
if (typeof x.$ === 'undefined')
//*/
		/**/
		if (x.$[0] === "#") {
		  //*/
		  return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
			? ord
			: _Utils_cmp(x.c, y.c);
		}

		// traverse conses until end of a list or a mismatch
		for (
		  ;
		  x.b && y.b && !(ord = _Utils_cmp(x.a, y.a));
		  x = x.b, y = y.b
		) {} // WHILE_CONSES
		return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
	  }

	  var _Utils_lt = F2(function(a, b) {
		return _Utils_cmp(a, b) < 0;
	  });
	  var _Utils_le = F2(function(a, b) {
		return _Utils_cmp(a, b) < 1;
	  });
	  var _Utils_gt = F2(function(a, b) {
		return _Utils_cmp(a, b) > 0;
	  });
	  var _Utils_ge = F2(function(a, b) {
		return _Utils_cmp(a, b) >= 0;
	  });

	  var _Utils_compare = F2(function(x, y) {
		var n = _Utils_cmp(x, y);
		return n < 0
		  ? $elm$core$Basics$LT
		  : n
		  ? $elm$core$Basics$GT
		  : $elm$core$Basics$EQ;
	  });

	  // COMMON VALUES

	  var _Utils_Tuple0_UNUSED = 0;
	  var _Utils_Tuple0 = { $: "#0" };

	  function _Utils_Tuple2_UNUSED(a, b) {
		return { a: a, b: b };
	  }
	  function _Utils_Tuple2(a, b) {
		return { $: "#2", a: a, b: b };
	  }

	  function _Utils_Tuple3_UNUSED(a, b, c) {
		return { a: a, b: b, c: c };
	  }
	  function _Utils_Tuple3(a, b, c) {
		return { $: "#3", a: a, b: b, c: c };
	  }

	  function _Utils_chr_UNUSED(c) {
		return c;
	  }
	  function _Utils_chr(c) {
		return new String(c);
	  }

	  // RECORDS

	  function _Utils_update(oldRecord, updatedFields) {
		var newRecord = {};

		for (var key in oldRecord) {
		  newRecord[key] = oldRecord[key];
		}

		for (var key in updatedFields) {
		  newRecord[key] = updatedFields[key];
		}

		return newRecord;
	  }

	  // APPEND

	  var _Utils_append = F2(_Utils_ap);

	  function _Utils_ap(xs, ys) {
		// append Strings
		if (typeof xs === "string") {
		  return xs + ys;
		}

		// append Lists
		if (!xs.b) {
		  return ys;
		}
		var root = _List_Cons(xs.a, ys);
		xs = xs.b;
		for (
		  var curr = root;
		  xs.b;
		  xs = xs.b // WHILE_CONS
		) {
		  curr = curr.b = _List_Cons(xs.a, ys);
		}
		return root;
	  }

	  var _List_Nil_UNUSED = { $: 0 };
	  var _List_Nil = { $: "[]" };

	  function _List_Cons_UNUSED(hd, tl) {
		return { $: 1, a: hd, b: tl };
	  }
	  function _List_Cons(hd, tl) {
		return { $: "::", a: hd, b: tl };
	  }

	  var _List_cons = F2(_List_Cons);

	  function _List_fromArray(arr) {
		var out = _List_Nil;
		for (var i = arr.length; i--; ) {
		  out = _List_Cons(arr[i], out);
		}
		return out;
	  }

	  function _List_toArray(xs) {
		for (
		  var out = [];
		  xs.b;
		  xs = xs.b // WHILE_CONS
		) {
		  out.push(xs.a);
		}
		return out;
	  }

	  var _List_map2 = F3(function(f, xs, ys) {
		for (
		  var arr = [];
		  xs.b && ys.b;
		  xs = xs.b, ys = ys.b // WHILE_CONSES
		) {
		  arr.push(A2(f, xs.a, ys.a));
		}
		return _List_fromArray(arr);
	  });

	  var _List_map3 = F4(function(f, xs, ys, zs) {
		for (
		  var arr = [];
		  xs.b && ys.b && zs.b;
		  xs = xs.b, ys = ys.b, zs = zs.b // WHILE_CONSES
		) {
		  arr.push(A3(f, xs.a, ys.a, zs.a));
		}
		return _List_fromArray(arr);
	  });

	  var _List_map4 = F5(function(f, ws, xs, ys, zs) {
		for (
		  var arr = [];
		  ws.b && xs.b && ys.b && zs.b;
		  ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b // WHILE_CONSES
		) {
		  arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
		}
		return _List_fromArray(arr);
	  });

	  var _List_map5 = F6(function(f, vs, ws, xs, ys, zs) {
		for (
		  var arr = [];
		  vs.b && ws.b && xs.b && ys.b && zs.b;
		  vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b // WHILE_CONSES
		) {
		  arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
		}
		return _List_fromArray(arr);
	  });

	  var _List_sortBy = F2(function(f, xs) {
		return _List_fromArray(
		  _List_toArray(xs).sort(function(a, b) {
			return _Utils_cmp(f(a), f(b));
		  })
		);
	  });

	  var _List_sortWith = F2(function(f, xs) {
		return _List_fromArray(
		  _List_toArray(xs).sort(function(a, b) {
			var ord = A2(f, a, b);
			return ord === $elm$core$Basics$EQ
			  ? 0
			  : ord === $elm$core$Basics$LT
			  ? -1
			  : 1;
		  })
		);
	  });

	  var _JsArray_empty = [];

	  function _JsArray_singleton(value) {
		return [value];
	  }

	  function _JsArray_length(array) {
		return array.length;
	  }

	  var _JsArray_initialize = F3(function(size, offset, func) {
		var result = new Array(size);

		for (var i = 0; i < size; i++) {
		  result[i] = func(offset + i);
		}

		return result;
	  });

	  var _JsArray_initializeFromList = F2(function(max, ls) {
		var result = new Array(max);

		for (var i = 0; i < max && ls.b; i++) {
		  result[i] = ls.a;
		  ls = ls.b;
		}

		result.length = i;
		return _Utils_Tuple2(result, ls);
	  });

	  var _JsArray_unsafeGet = F2(function(index, array) {
		return array[index];
	  });

	  var _JsArray_unsafeSet = F3(function(index, value, array) {
		var length = array.length;
		var result = new Array(length);

		for (var i = 0; i < length; i++) {
		  result[i] = array[i];
		}

		result[index] = value;
		return result;
	  });

	  var _JsArray_push = F2(function(value, array) {
		var length = array.length;
		var result = new Array(length + 1);

		for (var i = 0; i < length; i++) {
		  result[i] = array[i];
		}

		result[length] = value;
		return result;
	  });

	  var _JsArray_foldl = F3(function(func, acc, array) {
		var length = array.length;

		for (var i = 0; i < length; i++) {
		  acc = A2(func, array[i], acc);
		}

		return acc;
	  });

	  var _JsArray_foldr = F3(function(func, acc, array) {
		for (var i = array.length - 1; i >= 0; i--) {
		  acc = A2(func, array[i], acc);
		}

		return acc;
	  });

	  var _JsArray_map = F2(function(func, array) {
		var length = array.length;
		var result = new Array(length);

		for (var i = 0; i < length; i++) {
		  result[i] = func(array[i]);
		}

		return result;
	  });

	  var _JsArray_indexedMap = F3(function(func, offset, array) {
		var length = array.length;
		var result = new Array(length);

		for (var i = 0; i < length; i++) {
		  result[i] = A2(func, offset + i, array[i]);
		}

		return result;
	  });

	  var _JsArray_slice = F3(function(from, to, array) {
		return array.slice(from, to);
	  });

	  var _JsArray_appendN = F3(function(n, dest, source) {
		var destLen = dest.length;
		var itemsToCopy = n - destLen;

		if (itemsToCopy > source.length) {
		  itemsToCopy = source.length;
		}

		var size = destLen + itemsToCopy;
		var result = new Array(size);

		for (var i = 0; i < destLen; i++) {
		  result[i] = dest[i];
		}

		for (var i = 0; i < itemsToCopy; i++) {
		  result[i + destLen] = source[i];
		}

		return result;
	  });

	  // LOG

	  var _Debug_log_UNUSED = F2(function(tag, value) {
		return value;
	  });

	  var _Debug_log = F2(function(tag, value) {
		console.log(tag + ": " + _Debug_toString(value));
		return value;
	  });

	  // TODOS

	  function _Debug_todo(moduleName, region) {
		return function(message) {
		  _Debug_crash(8, moduleName, region, message);
		};
	  }

	  function _Debug_todoCase(moduleName, region, value) {
		return function(message) {
		  _Debug_crash(9, moduleName, region, value, message);
		};
	  }

	  // TO STRING

	  function _Debug_toString_UNUSED(value) {
		return "<internals>";
	  }

	  function _Debug_toString(value) {
		return _Debug_toAnsiString(false, value);
	  }

	  function _Debug_toAnsiString(ansi, value) {
		if (typeof value === "function") {
		  return _Debug_internalColor(ansi, "<function>");
		}

		if (typeof value === "boolean") {
		  return _Debug_ctorColor(ansi, value ? "True" : "False");
		}

		if (typeof value === "number") {
		  return _Debug_numberColor(ansi, value + "");
		}

		if (value instanceof String) {
		  return _Debug_charColor(
			ansi,
			"'" + _Debug_addSlashes(value, true) + "'"
		  );
		}

		if (typeof value === "string") {
		  return _Debug_stringColor(
			ansi,
			'"' + _Debug_addSlashes(value, false) + '"'
		  );
		}

		if (typeof value === "object" && "$" in value) {
		  var tag = value.$;

		  if (typeof tag === "number") {
			return _Debug_internalColor(ansi, "<internals>");
		  }

		  if (tag[0] === "#") {
			var output = [];
			for (var k in value) {
			  if (k === "$") continue;
			  output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return "(" + output.join(",") + ")";
		  }

		  if (tag === "Set_elm_builtin") {
			return (
			  _Debug_ctorColor(ansi, "Set") +
			  _Debug_fadeColor(ansi, ".fromList") +
			  " " +
			  _Debug_toAnsiString(ansi, $elm$core$Set$toList(value))
			);
		  }

		  if (
			tag === "RBNode_elm_builtin" ||
			tag === "RBEmpty_elm_builtin"
		  ) {
			return (
			  _Debug_ctorColor(ansi, "Dict") +
			  _Debug_fadeColor(ansi, ".fromList") +
			  " " +
			  _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value))
			);
		  }

		  if (tag === "Array_elm_builtin") {
			return (
			  _Debug_ctorColor(ansi, "Array") +
			  _Debug_fadeColor(ansi, ".fromList") +
			  " " +
			  _Debug_toAnsiString(ansi, $elm$core$Array$toList(value))
			);
		  }

		  if (tag === "::" || tag === "[]") {
			var output = "[";

			value.b &&
			  ((output += _Debug_toAnsiString(ansi, value.a)),
			  (value = value.b));

			for (
			  ;
			  value.b;
			  value = value.b // WHILE_CONS
			) {
			  output += "," + _Debug_toAnsiString(ansi, value.a);
			}
			return output + "]";
		  }

		  var output = "";
		  for (var i in value) {
			if (i === "$") continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless =
			  c0 === "{" ||
			  c0 === "(" ||
			  c0 === "[" ||
			  c0 === "<" ||
			  c0 === '"' ||
			  str.indexOf(" ") < 0;
			output += " " + (parenless ? str : "(" + str + ")");
		  }
		  return _Debug_ctorColor(ansi, tag) + output;
		}

		if (typeof DataView === "function" && value instanceof DataView) {
		  return _Debug_stringColor(
			ansi,
			"<" + value.byteLength + " bytes>"
		  );
		}

		if (typeof File === "function" && value instanceof File) {
		  return _Debug_internalColor(ansi, "<" + value.name + ">");
		}

		if (typeof value === "object") {
		  var output = [];
		  for (var key in value) {
			var field = key[0] === "_" ? key.slice(1) : key;
			output.push(
			  _Debug_fadeColor(ansi, field) +
				" = " +
				_Debug_toAnsiString(ansi, value[key])
			);
		  }
		  if (output.length === 0) {
			return "{}";
		  }
		  return "{ " + output.join(", ") + " }";
		}

		return _Debug_internalColor(ansi, "<internals>");
	  }

	  function _Debug_addSlashes(str, isChar) {
		var s = str
		  .replace(/\\/g, "\\\\")
		  .replace(/\n/g, "\\n")
		  .replace(/\t/g, "\\t")
		  .replace(/\r/g, "\\r")
		  .replace(/\v/g, "\\v")
		  .replace(/\0/g, "\\0");

		if (isChar) {
		  return s.replace(/\'/g, "\\'");
		} else {
		  return s.replace(/\"/g, '\\"');
		}
	  }

	  function _Debug_ctorColor(ansi, string) {
		return ansi ? "\x1b[96m" + string + "\x1b[0m" : string;
	  }

	  function _Debug_numberColor(ansi, string) {
		return ansi ? "\x1b[95m" + string + "\x1b[0m" : string;
	  }

	  function _Debug_stringColor(ansi, string) {
		return ansi ? "\x1b[93m" + string + "\x1b[0m" : string;
	  }

	  function _Debug_charColor(ansi, string) {
		return ansi ? "\x1b[92m" + string + "\x1b[0m" : string;
	  }

	  function _Debug_fadeColor(ansi, string) {
		return ansi ? "\x1b[37m" + string + "\x1b[0m" : string;
	  }

	  function _Debug_internalColor(ansi, string) {
		return ansi ? "\x1b[94m" + string + "\x1b[0m" : string;
	  }

	  function _Debug_toHexDigit(n) {
		return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
	  }

	  // CRASH

	  function _Debug_crash_UNUSED(identifier) {
		throw new Error(
		  "https://github.com/elm/core/blob/1.0.0/hints/" +
			identifier +
			".md"
		);
	  }

	  function _Debug_crash(identifier, fact1, fact2, fact3, fact4) {
		switch (identifier) {
		  case 0:
			throw new Error(
			  'What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.'
			);

		  case 1:
			throw new Error(
			  "Browser.application programs cannot handle URLs like this:\n\n    " +
				document.location.href +
				"\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server."
			);

		  case 2:
			var jsonErrorString = fact1;
			throw new Error(
			  "Problem with the flags given to your Elm program on initialization.\n\n" +
				jsonErrorString
			);

		  case 3:
			var portName = fact1;
			throw new Error(
			  "There can only be one port named `" +
				portName +
				"`, but your program has multiple."
			);

		  case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error(
			  "Trying to send an unexpected type of value through port `" +
				portName +
				"`:\n" +
				problem
			);

		  case 5:
			throw new Error(
			  'Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.'
			);

		  case 6:
			var moduleName = fact1;
			throw new Error(
			  "Your page is loading multiple Elm scripts with a module named " +
				moduleName +
				". Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!"
			);

		  case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error(
			  "TODO in module `" +
				moduleName +
				"` " +
				_Debug_regionToString(region) +
				"\n\n" +
				message
			);

		  case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
			  "TODO in module `" +
				moduleName +
				"` from the `case` expression " +
				_Debug_regionToString(region) +
				"\n\nIt received the following value:\n\n    " +
				_Debug_toString(value).replace("\n", "\n    ") +
				"\n\nBut the branch that handles it says:\n\n    " +
				message.replace("\n", "\n    ")
			);

		  case 10:
			throw new Error(
			  "Bug in https://github.com/elm/virtual-dom/issues"
			);

		  case 11:
			throw new Error(
			  "Cannot perform mod 0. Division by zero error."
			);
		}
	  }

	  function _Debug_regionToString(region) {
		if (region.start.line === region.end.line) {
		  return "on line " + region.start.line;
		}
		return (
		  "on lines " + region.start.line + " through " + region.end.line
		);
	  }

	  // MATH

	  var _Basics_add = F2(function(a, b) {
		return a + b;
	  });
	  var _Basics_sub = F2(function(a, b) {
		return a - b;
	  });
	  var _Basics_mul = F2(function(a, b) {
		return a * b;
	  });
	  var _Basics_fdiv = F2(function(a, b) {
		return a / b;
	  });
	  var _Basics_idiv = F2(function(a, b) {
		return (a / b) | 0;
	  });
	  var _Basics_pow = F2(Math.pow);

	  var _Basics_remainderBy = F2(function(b, a) {
		return a % b;
	  });

	  // https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
	  var _Basics_modBy = F2(function(modulus, x) {
		var answer = x % modulus;
		return modulus === 0
		  ? _Debug_crash(11)
		  : (answer > 0 && modulus < 0) || (answer < 0 && modulus > 0)
		  ? answer + modulus
		  : answer;
	  });

	  // TRIGONOMETRY

	  var _Basics_pi = Math.PI;
	  var _Basics_e = Math.E;
	  var _Basics_cos = Math.cos;
	  var _Basics_sin = Math.sin;
	  var _Basics_tan = Math.tan;
	  var _Basics_acos = Math.acos;
	  var _Basics_asin = Math.asin;
	  var _Basics_atan = Math.atan;
	  var _Basics_atan2 = F2(Math.atan2);

	  // MORE MATH

	  function _Basics_toFloat(x) {
		return x;
	  }
	  function _Basics_truncate(n) {
		return n | 0;
	  }
	  function _Basics_isInfinite(n) {
		return n === Infinity || n === -Infinity;
	  }

	  var _Basics_ceiling = Math.ceil;
	  var _Basics_floor = Math.floor;
	  var _Basics_round = Math.round;
	  var _Basics_sqrt = Math.sqrt;
	  var _Basics_log = Math.log;
	  var _Basics_isNaN = isNaN;

	  // BOOLEANS

	  function _Basics_not(bool) {
		return !bool;
	  }
	  var _Basics_and = F2(function(a, b) {
		return a && b;
	  });
	  var _Basics_or = F2(function(a, b) {
		return a || b;
	  });
	  var _Basics_xor = F2(function(a, b) {
		return a !== b;
	  });

	  var _String_cons = F2(function(chr, str) {
		return chr + str;
	  });

	  function _String_uncons(string) {
		var word = string.charCodeAt(0);
		return word
		  ? $elm$core$Maybe$Just(
			  0xd800 <= word && word <= 0xdbff
				? _Utils_Tuple2(
					_Utils_chr(string[0] + string[1]),
					string.slice(2)
				  )
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
			)
		  : $elm$core$Maybe$Nothing;
	  }

	  var _String_append = F2(function(a, b) {
		return a + b;
	  });

	  function _String_length(str) {
		return str.length;
	  }

	  var _String_map = F2(function(func, string) {
		var len = string.length;
		var array = new Array(len);
		var i = 0;
		while (i < len) {
		  var word = string.charCodeAt(i);
		  if (0xd800 <= word && word <= 0xdbff) {
			array[i] = func(_Utils_chr(string[i] + string[i + 1]));
			i += 2;
			continue;
		  }
		  array[i] = func(_Utils_chr(string[i]));
		  i++;
		}
		return array.join("");
	  });

	  var _String_filter = F2(function(isGood, str) {
		var arr = [];
		var len = str.length;
		var i = 0;
		while (i < len) {
		  var char = str[i];
		  var word = str.charCodeAt(i);
		  i++;
		  if (0xd800 <= word && word <= 0xdbff) {
			char += str[i];
			i++;
		  }

		  if (isGood(_Utils_chr(char))) {
			arr.push(char);
		  }
		}
		return arr.join("");
	  });

	  function _String_reverse(str) {
		var len = str.length;
		var arr = new Array(len);
		var i = 0;
		while (i < len) {
		  var word = str.charCodeAt(i);
		  if (0xd800 <= word && word <= 0xdbff) {
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		  } else {
			arr[len - i] = str[i];
			i++;
		  }
		}
		return arr.join("");
	  }

	  var _String_foldl = F3(function(func, state, string) {
		var len = string.length;
		var i = 0;
		while (i < len) {
		  var char = string[i];
		  var word = string.charCodeAt(i);
		  i++;
		  if (0xd800 <= word && word <= 0xdbff) {
			char += string[i];
			i++;
		  }
		  state = A2(func, _Utils_chr(char), state);
		}
		return state;
	  });

	  var _String_foldr = F3(function(func, state, string) {
		var i = string.length;
		while (i--) {
		  var char = string[i];
		  var word = string.charCodeAt(i);
		  if (0xdc00 <= word && word <= 0xdfff) {
			i--;
			char = string[i] + char;
		  }
		  state = A2(func, _Utils_chr(char), state);
		}
		return state;
	  });

	  var _String_split = F2(function(sep, str) {
		return str.split(sep);
	  });

	  var _String_join = F2(function(sep, strs) {
		return strs.join(sep);
	  });

	  var _String_slice = F3(function(start, end, str) {
		return str.slice(start, end);
	  });

	  function _String_trim(str) {
		return str.trim();
	  }

	  function _String_trimLeft(str) {
		return str.replace(/^\s+/, "");
	  }

	  function _String_trimRight(str) {
		return str.replace(/\s+$/, "");
	  }

	  function _String_words(str) {
		return _List_fromArray(str.trim().split(/\s+/g));
	  }

	  function _String_lines(str) {
		return _List_fromArray(str.split(/\r\n|\r|\n/g));
	  }

	  function _String_toUpper(str) {
		return str.toUpperCase();
	  }

	  function _String_toLower(str) {
		return str.toLowerCase();
	  }

	  var _String_any = F2(function(isGood, string) {
		var i = string.length;
		while (i--) {
		  var char = string[i];
		  var word = string.charCodeAt(i);
		  if (0xdc00 <= word && word <= 0xdfff) {
			i--;
			char = string[i] + char;
		  }
		  if (isGood(_Utils_chr(char))) {
			return true;
		  }
		}
		return false;
	  });

	  var _String_all = F2(function(isGood, string) {
		var i = string.length;
		while (i--) {
		  var char = string[i];
		  var word = string.charCodeAt(i);
		  if (0xdc00 <= word && word <= 0xdfff) {
			i--;
			char = string[i] + char;
		  }
		  if (!isGood(_Utils_chr(char))) {
			return false;
		  }
		}
		return true;
	  });

	  var _String_contains = F2(function(sub, str) {
		return str.indexOf(sub) > -1;
	  });

	  var _String_startsWith = F2(function(sub, str) {
		return str.indexOf(sub) === 0;
	  });

	  var _String_endsWith = F2(function(sub, str) {
		return (
		  str.length >= sub.length &&
		  str.lastIndexOf(sub) === str.length - sub.length
		);
	  });

	  var _String_indexes = F2(function(sub, str) {
		var subLen = sub.length;

		if (subLen < 1) {
		  return _List_Nil;
		}

		var i = 0;
		var is = [];

		while ((i = str.indexOf(sub, i)) > -1) {
		  is.push(i);
		  i = i + subLen;
		}

		return _List_fromArray(is);
	  });

	  // TO STRING

	  function _String_fromNumber(number) {
		return number + "";
	  }

	  // INT CONVERSIONS

	  function _String_toInt(str) {
		var total = 0;
		var code0 = str.charCodeAt(0);
		var start = code0 == 0x2b /* + */ || code0 == 0x2d /* - */ ? 1 : 0;

		for (var i = start; i < str.length; ++i) {
		  var code = str.charCodeAt(i);
		  if (code < 0x30 || 0x39 < code) {
			return $elm$core$Maybe$Nothing;
		  }
		  total = 10 * total + code - 0x30;
		}

		return i == start
		  ? $elm$core$Maybe$Nothing
		  : $elm$core$Maybe$Just(code0 == 0x2d ? -total : total);
	  }

	  // FLOAT CONVERSIONS

	  function _String_toFloat(s) {
		// check if it is a hex, octal, or binary number
		if (s.length === 0 || /[\sxbo]/.test(s)) {
		  return $elm$core$Maybe$Nothing;
		}
		var n = +s;
		// faster isNaN check
		return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
	  }

	  function _String_fromList(chars) {
		return _List_toArray(chars).join("");
	  }

	  function _Char_toCode(char) {
		var code = char.charCodeAt(0);
		if (0xd800 <= code && code <= 0xdbff) {
		  return (
			(code - 0xd800) * 0x400 + char.charCodeAt(1) - 0xdc00 + 0x10000
		  );
		}
		return code;
	  }

	  function _Char_fromCode(code) {
		return _Utils_chr(
		  code < 0 || 0x10ffff < code
			? "\uFFFD"
			: code <= 0xffff
			? String.fromCharCode(code)
			: ((code -= 0x10000),
			  String.fromCharCode(
				Math.floor(code / 0x400) + 0xd800,
				(code % 0x400) + 0xdc00
			  ))
		);
	  }

	  function _Char_toUpper(char) {
		return _Utils_chr(char.toUpperCase());
	  }

	  function _Char_toLower(char) {
		return _Utils_chr(char.toLowerCase());
	  }

	  function _Char_toLocaleUpper(char) {
		return _Utils_chr(char.toLocaleUpperCase());
	  }

	  function _Char_toLocaleLower(char) {
		return _Utils_chr(char.toLocaleLowerCase());
	  }

	  /**/
	  function _Json_errorToString(error) {
		return $elm$json$Json$Decode$errorToString(error);
	  }
	  //*/

	  // CORE DECODERS

	  function _Json_succeed(msg) {
		return {
		  $: 0,
		  a: msg
		};
	  }

	  function _Json_fail(msg) {
		return {
		  $: 1,
		  a: msg
		};
	  }

	  function _Json_decodePrim(decoder) {
		return { $: 2, b: decoder };
	  }

	  var _Json_decodeInt = _Json_decodePrim(function(value) {
		return typeof value !== "number"
		  ? _Json_expecting("an INT", value)
		  : -2147483647 < value &&
			value < 2147483647 &&
			(value | 0) === value
		  ? $elm$core$Result$Ok(value)
		  : isFinite(value) && !(value % 1)
		  ? $elm$core$Result$Ok(value)
		  : _Json_expecting("an INT", value);
	  });

	  var _Json_decodeBool = _Json_decodePrim(function(value) {
		return typeof value === "boolean"
		  ? $elm$core$Result$Ok(value)
		  : _Json_expecting("a BOOL", value);
	  });

	  var _Json_decodeFloat = _Json_decodePrim(function(value) {
		return typeof value === "number"
		  ? $elm$core$Result$Ok(value)
		  : _Json_expecting("a FLOAT", value);
	  });

	  var _Json_decodeValue = _Json_decodePrim(function(value) {
		return $elm$core$Result$Ok(_Json_wrap(value));
	  });

	  var _Json_decodeString = _Json_decodePrim(function(value) {
		return typeof value === "string"
		  ? $elm$core$Result$Ok(value)
		  : value instanceof String
		  ? $elm$core$Result$Ok(value + "")
		  : _Json_expecting("a STRING", value);
	  });

	  function _Json_decodeList(decoder) {
		return { $: 3, b: decoder };
	  }
	  function _Json_decodeArray(decoder) {
		return { $: 4, b: decoder };
	  }

	  function _Json_decodeNull(value) {
		return { $: 5, c: value };
	  }

	  var _Json_decodeField = F2(function(field, decoder) {
		return {
		  $: 6,
		  d: field,
		  b: decoder
		};
	  });

	  var _Json_decodeIndex = F2(function(index, decoder) {
		return {
		  $: 7,
		  e: index,
		  b: decoder
		};
	  });

	  function _Json_decodeKeyValuePairs(decoder) {
		return {
		  $: 8,
		  b: decoder
		};
	  }

	  function _Json_mapMany(f, decoders) {
		return {
		  $: 9,
		  f: f,
		  g: decoders
		};
	  }

	  var _Json_andThen = F2(function(callback, decoder) {
		return {
		  $: 10,
		  b: decoder,
		  h: callback
		};
	  });

	  function _Json_oneOf(decoders) {
		return {
		  $: 11,
		  g: decoders
		};
	  }

	  // DECODING OBJECTS

	  var _Json_map1 = F2(function(f, d1) {
		return _Json_mapMany(f, [d1]);
	  });

	  var _Json_map2 = F3(function(f, d1, d2) {
		return _Json_mapMany(f, [d1, d2]);
	  });

	  var _Json_map3 = F4(function(f, d1, d2, d3) {
		return _Json_mapMany(f, [d1, d2, d3]);
	  });

	  var _Json_map4 = F5(function(f, d1, d2, d3, d4) {
		return _Json_mapMany(f, [d1, d2, d3, d4]);
	  });

	  var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5) {
		return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
	  });

	  var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6) {
		return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
	  });

	  var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7) {
		return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
	  });

	  var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
	  });

	  // DECODE

	  var _Json_runOnString = F2(function(decoder, string) {
		try {
		  var value = JSON.parse(string);
		  return _Json_runHelp(decoder, value);
		} catch (e) {
		  return $elm$core$Result$Err(
			A2(
			  $elm$json$Json$Decode$Failure,
			  "This is not valid JSON! " + e.message,
			  _Json_wrap(string)
			)
		  );
		}
	  });

	  var _Json_run = F2(function(decoder, value) {
		return _Json_runHelp(decoder, _Json_unwrap(value));
	  });

	  function _Json_runHelp(decoder, value) {
		switch (decoder.$) {
		  case 2:
			return decoder.b(value);

		  case 5:
			return value === null
			  ? $elm$core$Result$Ok(decoder.c)
			  : _Json_expecting("null", value);

		  case 3:
			if (!_Json_isArray(value)) {
			  return _Json_expecting("a LIST", value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		  case 4:
			if (!_Json_isArray(value)) {
			  return _Json_expecting("an ARRAY", value);
			}
			return _Json_runArrayDecoder(
			  decoder.b,
			  value,
			  _Json_toElmArray
			);

		  case 6:
			var field = decoder.d;
			if (
			  typeof value !== "object" ||
			  value === null ||
			  !(field in value)
			) {
			  return _Json_expecting(
				"an OBJECT with a field named `" + field + "`",
				value
			  );
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return $elm$core$Result$isOk(result)
			  ? result
			  : $elm$core$Result$Err(
				  A2($elm$json$Json$Decode$Field, field, result.a)
				);

		  case 7:
			var index = decoder.e;
			if (!_Json_isArray(value)) {
			  return _Json_expecting("an ARRAY", value);
			}
			if (index >= value.length) {
			  return _Json_expecting(
				"a LONGER array. Need index " +
				  index +
				  " but only see " +
				  value.length +
				  " entries",
				value
			  );
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return $elm$core$Result$isOk(result)
			  ? result
			  : $elm$core$Result$Err(
				  A2($elm$json$Json$Decode$Index, index, result.a)
				);

		  case 8:
			if (
			  typeof value !== "object" ||
			  value === null ||
			  _Json_isArray(value)
			) {
			  return _Json_expecting("an OBJECT", value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value) {
			  if (value.hasOwnProperty(key)) {
				var result = _Json_runHelp(decoder.b, value[key]);
				if (!$elm$core$Result$isOk(result)) {
				  return $elm$core$Result$Err(
					A2($elm$json$Json$Decode$Field, key, result.a)
				  );
				}
				keyValuePairs = _List_Cons(
				  _Utils_Tuple2(key, result.a),
				  keyValuePairs
				);
			  }
			}
			return $elm$core$Result$Ok(
			  $elm$core$List$reverse(keyValuePairs)
			);

		  case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++) {
			  var result = _Json_runHelp(decoders[i], value);
			  if (!$elm$core$Result$isOk(result)) {
				return result;
			  }
			  answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		  case 10:
			var result = _Json_runHelp(decoder.b, value);
			return !$elm$core$Result$isOk(result)
			  ? result
			  : _Json_runHelp(decoder.h(result.a), value);

		  case 11:
			var errors = _List_Nil;
			for (
			  var temp = decoder.g;
			  temp.b;
			  temp = temp.b // WHILE_CONS
			) {
			  var result = _Json_runHelp(temp.a, value);
			  if ($elm$core$Result$isOk(result)) {
				return result;
			  }
			  errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err(
			  $elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors))
			);

		  case 1:
			return $elm$core$Result$Err(
			  A2(
				$elm$json$Json$Decode$Failure,
				decoder.a,
				_Json_wrap(value)
			  )
			);

		  case 0:
			return $elm$core$Result$Ok(decoder.a);
		}
	  }

	  function _Json_runArrayDecoder(decoder, value, toElmValue) {
		var len = value.length;
		var array = new Array(len);
		for (var i = 0; i < len; i++) {
		  var result = _Json_runHelp(decoder, value[i]);
		  if (!$elm$core$Result$isOk(result)) {
			return $elm$core$Result$Err(
			  A2($elm$json$Json$Decode$Index, i, result.a)
			);
		  }
		  array[i] = result.a;
		}
		return $elm$core$Result$Ok(toElmValue(array));
	  }

	  function _Json_isArray(value) {
		return (
		  Array.isArray(value) ||
		  (typeof FileList !== "undefined" && value instanceof FileList)
		);
	  }

	  function _Json_toElmArray(array) {
		return A2($elm$core$Array$initialize, array.length, function(i) {
		  return array[i];
		});
	  }

	  function _Json_expecting(type, value) {
		return $elm$core$Result$Err(
		  A2(
			$elm$json$Json$Decode$Failure,
			"Expecting " + type,
			_Json_wrap(value)
		  )
		);
	  }

	  // EQUALITY

	  function _Json_equality(x, y) {
		if (x === y) {
		  return true;
		}

		if (x.$ !== y.$) {
		  return false;
		}

		switch (x.$) {
		  case 0:
		  case 1:
			return x.a === y.a;

		  case 2:
			return x.b === y.b;

		  case 5:
			return x.c === y.c;

		  case 3:
		  case 4:
		  case 8:
			return _Json_equality(x.b, y.b);

		  case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		  case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		  case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		  case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		  case 11:
			return _Json_listEquality(x.g, y.g);
		}
	  }

	  function _Json_listEquality(aDecoders, bDecoders) {
		var len = aDecoders.length;
		if (len !== bDecoders.length) {
		  return false;
		}
		for (var i = 0; i < len; i++) {
		  if (!_Json_equality(aDecoders[i], bDecoders[i])) {
			return false;
		  }
		}
		return true;
	  }

	  // ENCODE

	  var _Json_encode = F2(function(indentLevel, value) {
		return JSON.stringify(_Json_unwrap(value), null, indentLevel) + "";
	  });

	  function _Json_wrap(value) {
		return { $: 0, a: value };
	  }
	  function _Json_unwrap(value) {
		return value.a;
	  }

	  function _Json_wrap_UNUSED(value) {
		return value;
	  }
	  function _Json_unwrap_UNUSED(value) {
		return value;
	  }

	  function _Json_emptyArray() {
		return [];
	  }
	  function _Json_emptyObject() {
		return {};
	  }

	  var _Json_addField = F3(function(key, value, object) {
		object[key] = _Json_unwrap(value);
		return object;
	  });

	  function _Json_addEntry(func) {
		return F2(function(entry, array) {
		  array.push(_Json_unwrap(func(entry)));
		  return array;
		});
	  }

	  var _Json_encodeNull = _Json_wrap(null);

	  // TASKS

	  function _Scheduler_succeed(value) {
		return {
		  $: 0,
		  a: value
		};
	  }

	  function _Scheduler_fail(error) {
		return {
		  $: 1,
		  a: error
		};
	  }

	  function _Scheduler_binding(callback) {
		return {
		  $: 2,
		  b: callback,
		  c: null
		};
	  }

	  var _Scheduler_andThen = F2(function(callback, task) {
		return {
		  $: 3,
		  b: callback,
		  d: task
		};
	  });

	  var _Scheduler_onError = F2(function(callback, task) {
		return {
		  $: 4,
		  b: callback,
		  d: task
		};
	  });

	  function _Scheduler_receive(callback) {
		return {
		  $: 5,
		  b: callback
		};
	  }

	  // PROCESSES

	  var _Scheduler_guid = 0;

	  function _Scheduler_rawSpawn(task) {
		var proc = {
		  $: 0,
		  e: _Scheduler_guid++,
		  f: task,
		  g: null,
		  h: []
		};

		_Scheduler_enqueue(proc);

		return proc;
	  }

	  function _Scheduler_spawn(task) {
		return _Scheduler_binding(function(callback) {
		  callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
		});
	  }

	  function _Scheduler_rawSend(proc, msg) {
		proc.h.push(msg);
		_Scheduler_enqueue(proc);
	  }

	  var _Scheduler_send = F2(function(proc, msg) {
		return _Scheduler_binding(function(callback) {
		  _Scheduler_rawSend(proc, msg);
		  callback(_Scheduler_succeed(_Utils_Tuple0));
		});
	  });

	  function _Scheduler_kill(proc) {
		return _Scheduler_binding(function(callback) {
		  var task = proc.f;
		  if (task.$ === 2 && task.c) {
			task.c();
		  }

		  proc.f = null;

		  callback(_Scheduler_succeed(_Utils_Tuple0));
		});
	  }

	  /* STEP PROCESSES

type alias Process =
{ $ : tag
, id : unique_id
, root : Task
, stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
, mailbox : [msg]
}

*/

	  var _Scheduler_working = false;
	  var _Scheduler_queue = [];

	  function _Scheduler_enqueue(proc) {
		_Scheduler_queue.push(proc);
		if (_Scheduler_working) {
		  return;
		}
		_Scheduler_working = true;
		while ((proc = _Scheduler_queue.shift())) {
		  _Scheduler_step(proc);
		}
		_Scheduler_working = false;
	  }

	  function _Scheduler_step(proc) {
		while (proc.f) {
		  var rootTag = proc.f.$;
		  if (rootTag === 0 || rootTag === 1) {
			while (proc.g && proc.g.$ !== rootTag) {
			  proc.g = proc.g.i;
			}
			if (!proc.g) {
			  return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		  } else if (rootTag === 2) {
			proc.f.c = proc.f.b(function(newRoot) {
			  proc.f = newRoot;
			  _Scheduler_enqueue(proc);
			});
			return;
		  } else if (rootTag === 5) {
			if (proc.h.length === 0) {
			  return;
			}
			proc.f = proc.f.b(proc.h.shift());
		  } // if (rootTag === 3 || rootTag === 4)
		  else {
			proc.g = {
			  $: rootTag === 3 ? 0 : 1,
			  b: proc.f.b,
			  i: proc.g
			};
			proc.f = proc.f.d;
		  }
		}
	  }

	  function _Process_sleep(time) {
		return _Scheduler_binding(function(callback) {
		  var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		  }, time);

		  return function() {
			clearTimeout(id);
		  };
		});
	  }

	  // PROGRAMS

	  var _Platform_worker = F4(function(
		impl,
		flagDecoder,
		debugMetadata,
		args
	  ) {
		return _Platform_initialize(
		  flagDecoder,
		  args,
		  impl.init,
		  impl.update,
		  impl.subscriptions,
		  function() {
			return function() {};
		  }
		);
	  });

	  // INITIALIZE A PROGRAM

	  function _Platform_initialize(
		flagDecoder,
		args,
		init,
		update,
		subscriptions,
		stepperBuilder
	  ) {
		var result = A2(
		  _Json_run,
		  flagDecoder,
		  _Json_wrap(args ? args["flags"] : undefined)
		);
		$elm$core$Result$isOk(result) ||
		  _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
		var managers = {};
		result = init(result.a);
		var model = result.a;
		var stepper = stepperBuilder(sendToApp, model);
		var ports = _Platform_setupEffects(managers, sendToApp);

		function sendToApp(msg, viewMetadata) {
		  result = A2(update, msg, model);
		  stepper((model = result.a), viewMetadata);
		  _Platform_dispatchEffects(
			managers,
			result.b,
			subscriptions(model)
		  );
		}

		_Platform_dispatchEffects(managers, result.b, subscriptions(model));

		return ports ? { ports: ports } : {};
	  }

	  // TRACK PRELOADS
	  //
	  // This is used by code in elm/browser and elm/http
	  // to register any HTTP requests that are triggered by init.
	  //

	  var _Platform_preload;

	  function _Platform_registerPreload(url) {
		_Platform_preload.add(url);
	  }

	  // EFFECT MANAGERS

	  var _Platform_effectManagers = {};

	  function _Platform_setupEffects(managers, sendToApp) {
		var ports;

		// setup all necessary effect managers
		for (var key in _Platform_effectManagers) {
		  var manager = _Platform_effectManagers[key];

		  if (manager.a) {
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		  }

		  managers[key] = _Platform_instantiateManager(manager, sendToApp);
		}

		return ports;
	  }

	  function _Platform_createManager(
		init,
		onEffects,
		onSelfMsg,
		cmdMap,
		subMap
	  ) {
		return {
		  b: init,
		  c: onEffects,
		  d: onSelfMsg,
		  e: cmdMap,
		  f: subMap
		};
	  }

	  function _Platform_instantiateManager(info, sendToApp) {
		var router = {
		  g: sendToApp,
		  h: undefined
		};

		var onEffects = info.c;
		var onSelfMsg = info.d;
		var cmdMap = info.e;
		var subMap = info.f;

		function loop(state) {
		  return A2(
			_Scheduler_andThen,
			loop,
			_Scheduler_receive(function(msg) {
			  var value = msg.a;

			  if (msg.$ === 0) {
				return A3(onSelfMsg, router, value, state);
			  }

			  return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
			})
		  );
		}

		return (router.h = _Scheduler_rawSpawn(
		  A2(_Scheduler_andThen, loop, info.b)
		));
	  }

	  // ROUTING

	  var _Platform_sendToApp = F2(function(router, msg) {
		return _Scheduler_binding(function(callback) {
		  router.g(msg);
		  callback(_Scheduler_succeed(_Utils_Tuple0));
		});
	  });

	  var _Platform_sendToSelf = F2(function(router, msg) {
		return A2(_Scheduler_send, router.h, {
		  $: 0,
		  a: msg
		});
	  });

	  // BAGS

	  function _Platform_leaf(home) {
		return function(value) {
		  return {
			$: 1,
			k: home,
			l: value
		  };
		};
	  }

	  function _Platform_batch(list) {
		return {
		  $: 2,
		  m: list
		};
	  }

	  var _Platform_map = F2(function(tagger, bag) {
		return {
		  $: 3,
		  n: tagger,
		  o: bag
		};
	  });

	  // PIPE BAGS INTO EFFECT MANAGERS

	  function _Platform_dispatchEffects(managers, cmdBag, subBag) {
		var effectsDict = {};
		_Platform_gatherEffects(true, cmdBag, effectsDict, null);
		_Platform_gatherEffects(false, subBag, effectsDict, null);

		for (var home in managers) {
		  _Scheduler_rawSend(managers[home], {
			$: "fx",
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		  });
		}
	  }

	  function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers) {
		switch (bag.$) {
		  case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(
			  isCmd,
			  effect,
			  effectsDict[home]
			);
			return;

		  case 2:
			for (
			  var list = bag.m;
			  list.b;
			  list = list.b // WHILE_CONS
			) {
			  _Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		  case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
			  p: bag.n,
			  q: taggers
			});
			return;
		}
	  }

	  function _Platform_toEffect(isCmd, home, taggers, value) {
		function applyTaggers(x) {
		  for (var temp = taggers; temp; temp = temp.q) {
			x = temp.p(x);
		  }
		  return x;
		}

		var map = isCmd
		  ? _Platform_effectManagers[home].e
		  : _Platform_effectManagers[home].f;

		return A2(map, applyTaggers, value);
	  }

	  function _Platform_insert(isCmd, newEffect, effects) {
		effects = effects || { i: _List_Nil, j: _List_Nil };

		isCmd
		  ? (effects.i = _List_Cons(newEffect, effects.i))
		  : (effects.j = _List_Cons(newEffect, effects.j));

		return effects;
	  }

	  // PORTS

	  function _Platform_checkPortName(name) {
		if (_Platform_effectManagers[name]) {
		  _Debug_crash(3, name);
		}
	  }

	  // OUTGOING PORTS

	  function _Platform_outgoingPort(name, converter) {
		_Platform_checkPortName(name);
		_Platform_effectManagers[name] = {
		  e: _Platform_outgoingPortMap,
		  r: converter,
		  a: _Platform_setupOutgoingPort
		};
		return _Platform_leaf(name);
	  }

	  var _Platform_outgoingPortMap = F2(function(tagger, value) {
		return value;
	  });

	  function _Platform_setupOutgoingPort(name) {
		var subs = [];
		var converter = _Platform_effectManagers[name].r;

		// CREATE MANAGER

		var init = _Process_sleep(0);

		_Platform_effectManagers[name].b = init;
		_Platform_effectManagers[name].c = F3(function(
		  router,
		  cmdList,
		  state
		) {
		  for (
			;
			cmdList.b;
			cmdList = cmdList.b // WHILE_CONS
		  ) {
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++) {
			  currentSubs[i](value);
			}
		  }
		  return init;
		});

		// PUBLIC API

		function subscribe(callback) {
		  subs.push(callback);
		}

		function unsubscribe(callback) {
		  // copy subs into a new array in case unsubscribe is called within a
		  // subscribed callback
		  subs = subs.slice();
		  var index = subs.indexOf(callback);
		  if (index >= 0) {
			subs.splice(index, 1);
		  }
		}

		return {
		  subscribe: subscribe,
		  unsubscribe: unsubscribe
		};
	  }

	  // INCOMING PORTS

	  function _Platform_incomingPort(name, converter) {
		_Platform_checkPortName(name);
		_Platform_effectManagers[name] = {
		  f: _Platform_incomingPortMap,
		  r: converter,
		  a: _Platform_setupIncomingPort
		};
		return _Platform_leaf(name);
	  }

	  var _Platform_incomingPortMap = F2(function(tagger, finalTagger) {
		return function(value) {
		  return tagger(finalTagger(value));
		};
	  });

	  function _Platform_setupIncomingPort(name, sendToApp) {
		var subs = _List_Nil;
		var converter = _Platform_effectManagers[name].r;

		// CREATE MANAGER

		var init = _Scheduler_succeed(null);

		_Platform_effectManagers[name].b = init;
		_Platform_effectManagers[name].c = F3(function(
		  router,
		  subList,
		  state
		) {
		  subs = subList;
		  return init;
		});

		// PUBLIC API

		function send(incomingValue) {
		  var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		  $elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		  var value = result.a;
		  for (
			var temp = subs;
			temp.b;
			temp = temp.b // WHILE_CONS
		  ) {
			sendToApp(temp.a(value));
		  }
		}

		return { send: send };
	  }

	  // EXPORT ELM MODULES
	  //
	  // Have DEBUG and PROD versions so that we can (1) give nicer errors in
	  // debug mode and (2) not pay for the bits needed for that in prod mode.
	  //

	  function _Platform_export_UNUSED(exports) {
		scope["Elm"]
		  ? _Platform_mergeExportsProd(scope["Elm"], exports)
		  : (scope["Elm"] = exports);
	  }

	  function _Platform_mergeExportsProd(obj, exports) {
		for (var name in exports) {
		  name in obj
			? name == "init"
			  ? _Debug_crash(6)
			  : _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
		}
	  }

	  function _Platform_export(exports) {
		scope["Elm"]
		  ? _Platform_mergeExportsDebug("Elm", scope["Elm"], exports)
		  : (scope["Elm"] = exports);
	  }

	  function _Platform_mergeExportsDebug(moduleName, obj, exports) {
		for (var name in exports) {
		  name in obj
			? name == "init"
			  ? _Debug_crash(6, moduleName)
			  : _Platform_mergeExportsDebug(
				  moduleName + "." + name,
				  obj[name],
				  exports[name]
				)
			: (obj[name] = exports[name]);
		}
	  }

	  // HELPERS

	  var _VirtualDom_divertHrefToApp;

	  var _VirtualDom_doc = typeof document !== "undefined" ? document : {};

	  function _VirtualDom_appendChild(parent, child) {
		parent.appendChild(child);
	  }

	  var _VirtualDom_init = F4(function(
		virtualNode,
		flagDecoder,
		debugMetadata,
		args
	  ) {
		// NOTE: this function needs _Platform_export available to work

		/**_UNUSED/
var node = args['node'];
//*/
		/**/
		var node = args && args["node"] ? args["node"] : _Debug_crash(0);
		//*/

		node.parentNode.replaceChild(
		  _VirtualDom_render(virtualNode, function() {}),
		  node
		);

		return {};
	  });

	  // TEXT

	  function _VirtualDom_text(string) {
		return {
		  $: 0,
		  a: string
		};
	  }

	  // NODE

	  var _VirtualDom_nodeNS = F2(function(namespace, tag) {
		return F2(function(factList, kidList) {
		  for (
			var kids = [], descendantsCount = 0;
			kidList.b;
			kidList = kidList.b // WHILE_CONS
		  ) {
			var kid = kidList.a;
			descendantsCount += kid.b || 0;
			kids.push(kid);
		  }
		  descendantsCount += kids.length;

		  return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		  };
		});
	  });

	  var _VirtualDom_node = _VirtualDom_nodeNS(undefined);

	  // KEYED NODE

	  var _VirtualDom_keyedNodeNS = F2(function(namespace, tag) {
		return F2(function(factList, kidList) {
		  for (
			var kids = [], descendantsCount = 0;
			kidList.b;
			kidList = kidList.b // WHILE_CONS
		  ) {
			var kid = kidList.a;
			descendantsCount += kid.b.b || 0;
			kids.push(kid);
		  }
		  descendantsCount += kids.length;

		  return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		  };
		});
	  });

	  var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);

	  // CUSTOM

	  function _VirtualDom_custom(factList, model, render, diff) {
		return {
		  $: 3,
		  d: _VirtualDom_organizeFacts(factList),
		  g: model,
		  h: render,
		  i: diff
		};
	  }

	  // MAP

	  var _VirtualDom_map = F2(function(tagger, node) {
		return {
		  $: 4,
		  j: tagger,
		  k: node,
		  b: 1 + (node.b || 0)
		};
	  });

	  // LAZY

	  function _VirtualDom_thunk(refs, thunk) {
		return {
		  $: 5,
		  l: refs,
		  m: thunk,
		  k: undefined
		};
	  }

	  var _VirtualDom_lazy = F2(function(func, a) {
		return _VirtualDom_thunk([func, a], function() {
		  return func(a);
		});
	  });

	  var _VirtualDom_lazy2 = F3(function(func, a, b) {
		return _VirtualDom_thunk([func, a, b], function() {
		  return A2(func, a, b);
		});
	  });

	  var _VirtualDom_lazy3 = F4(function(func, a, b, c) {
		return _VirtualDom_thunk([func, a, b, c], function() {
		  return A3(func, a, b, c);
		});
	  });

	  var _VirtualDom_lazy4 = F5(function(func, a, b, c, d) {
		return _VirtualDom_thunk([func, a, b, c, d], function() {
		  return A4(func, a, b, c, d);
		});
	  });

	  var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e) {
		return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		  return A5(func, a, b, c, d, e);
		});
	  });

	  var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f) {
		return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		  return A6(func, a, b, c, d, e, f);
		});
	  });

	  var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g) {
		return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		  return A7(func, a, b, c, d, e, f, g);
		});
	  });

	  var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h) {
		return _VirtualDom_thunk(
		  [func, a, b, c, d, e, f, g, h],
		  function() {
			return A8(func, a, b, c, d, e, f, g, h);
		  }
		);
	  });

	  // FACTS

	  var _VirtualDom_on = F2(function(key, handler) {
		return {
		  $: "a0",
		  n: key,
		  o: handler
		};
	  });
	  var _VirtualDom_style = F2(function(key, value) {
		return {
		  $: "a1",
		  n: key,
		  o: value
		};
	  });
	  var _VirtualDom_property = F2(function(key, value) {
		return {
		  $: "a2",
		  n: key,
		  o: value
		};
	  });
	  var _VirtualDom_attribute = F2(function(key, value) {
		return {
		  $: "a3",
		  n: key,
		  o: value
		};
	  });
	  var _VirtualDom_attributeNS = F3(function(namespace, key, value) {
		return {
		  $: "a4",
		  n: key,
		  o: { f: namespace, o: value }
		};
	  });

	  // XSS ATTACK VECTOR CHECKS

	  function _VirtualDom_noScript(tag) {
		return tag == "script" ? "p" : tag;
	  }

	  function _VirtualDom_noOnOrFormAction(key) {
		return /^(on|formAction$)/i.test(key) ? "data-" + key : key;
	  }

	  function _VirtualDom_noInnerHtmlOrFormAction(key) {
		return key == "innerHTML" || key == "formAction"
		  ? "data-" + key
		  : key;
	  }

	  function _VirtualDom_noJavaScriptUri_UNUSED(value) {
		return /^javascript:/i.test(value.replace(/\s/g, "")) ? "" : value;
	  }

	  function _VirtualDom_noJavaScriptUri(value) {
		return /^javascript:/i.test(value.replace(/\s/g, ""))
		  ? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		  : value;
	  }

	  function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value) {
		return /^\s*(javascript:|data:text\/html)/i.test(value)
		  ? ""
		  : value;
	  }

	  function _VirtualDom_noJavaScriptOrHtmlUri(value) {
		return /^\s*(javascript:|data:text\/html)/i.test(value)
		  ? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		  : value;
	  }

	  // MAP FACTS

	  var _VirtualDom_mapAttribute = F2(function(func, attr) {
		return attr.$ === "a0"
		  ? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		  : attr;
	  });

	  function _VirtualDom_mapHandler(func, handler) {
		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		return {
		  $: handler.$,
		  a: !tag
			? A2($elm$json$Json$Decode$map, func, handler.a)
			: A3(
				$elm$json$Json$Decode$map2,
				tag < 3
				  ? _VirtualDom_mapEventTuple
				  : _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			  )
		};
	  }

	  var _VirtualDom_mapEventTuple = F2(function(func, tuple) {
		return _Utils_Tuple2(func(tuple.a), tuple.b);
	  });

	  var _VirtualDom_mapEventRecord = F2(function(func, record) {
		return {
		  message: func(record.message),
		  stopPropagation: record.stopPropagation,
		  preventDefault: record.preventDefault
		};
	  });

	  // ORGANIZE FACTS

	  function _VirtualDom_organizeFacts(factList) {
		for (
		  var facts = {};
		  factList.b;
		  factList = factList.b // WHILE_CONS
		) {
		  var entry = factList.a;

		  var tag = entry.$;
		  var key = entry.n;
		  var value = entry.o;

		  if (tag === "a2") {
			key === "className"
			  ? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
			  : (facts[key] = _Json_unwrap(value));

			continue;
		  }

		  var subFacts = facts[tag] || (facts[tag] = {});
		  tag === "a3" && key === "class"
			? _VirtualDom_addClass(subFacts, key, value)
			: (subFacts[key] = value);
		}

		return facts;
	  }

	  function _VirtualDom_addClass(object, key, newClass) {
		var classes = object[key];
		object[key] = classes ? classes + " " + newClass : newClass;
	  }

	  // RENDER

	  function _VirtualDom_render(vNode, eventNode) {
		var tag = vNode.$;

		if (tag === 5) {
		  return _VirtualDom_render(
			vNode.k || (vNode.k = vNode.m()),
			eventNode
		  );
		}

		if (tag === 0) {
		  return _VirtualDom_doc.createTextNode(vNode.a);
		}

		if (tag === 4) {
		  var subNode = vNode.k;
		  var tagger = vNode.j;

		  while (subNode.$ === 4) {
			typeof tagger !== "object"
			  ? (tagger = [tagger, subNode.j])
			  : tagger.push(subNode.j);

			subNode = subNode.k;
		  }

		  var subEventRoot = { j: tagger, p: eventNode };
		  var domNode = _VirtualDom_render(subNode, subEventRoot);
		  domNode.elm_event_node_ref = subEventRoot;
		  return domNode;
		}

		if (tag === 3) {
		  var domNode = vNode.h(vNode.g);
		  _VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		  return domNode;
		}

		// at this point `tag` must be 1 or 2

		var domNode = vNode.f
		  ? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		  : _VirtualDom_doc.createElement(vNode.c);

		if (_VirtualDom_divertHrefToApp && vNode.c == "a") {
		  domNode.addEventListener(
			"click",
			_VirtualDom_divertHrefToApp(domNode)
		  );
		}

		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

		for (var kids = vNode.e, i = 0; i < kids.length; i++) {
		  _VirtualDom_appendChild(
			domNode,
			_VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode)
		  );
		}

		return domNode;
	  }

	  // APPLY FACTS

	  function _VirtualDom_applyFacts(domNode, eventNode, facts) {
		for (var key in facts) {
		  var value = facts[key];

		  key === "a1"
			? _VirtualDom_applyStyles(domNode, value)
			: key === "a0"
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			: key === "a3"
			? _VirtualDom_applyAttrs(domNode, value)
			: key === "a4"
			? _VirtualDom_applyAttrsNS(domNode, value)
			: ((key !== "value" && key !== "checked") ||
				domNode[key] !== value) &&
			  (domNode[key] = value);
		}
	  }

	  // APPLY STYLES

	  function _VirtualDom_applyStyles(domNode, styles) {
		var domNodeStyle = domNode.style;

		for (var key in styles) {
		  domNodeStyle[key] = styles[key];
		}
	  }

	  // APPLY ATTRS

	  function _VirtualDom_applyAttrs(domNode, attrs) {
		for (var key in attrs) {
		  var value = attrs[key];
		  typeof value !== "undefined"
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
		}
	  }

	  // APPLY NAMESPACED ATTRS

	  function _VirtualDom_applyAttrsNS(domNode, nsAttrs) {
		for (var key in nsAttrs) {
		  var pair = nsAttrs[key];
		  var namespace = pair.f;
		  var value = pair.o;

		  typeof value !== "undefined"
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
		}
	  }

	  // APPLY EVENTS

	  function _VirtualDom_applyEvents(domNode, eventNode, events) {
		var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

		for (var key in events) {
		  var newHandler = events[key];
		  var oldCallback = allCallbacks[key];

		  if (!newHandler) {
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		  }

		  if (oldCallback) {
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$) {
			  oldCallback.q = newHandler;
			  continue;
			}
			domNode.removeEventListener(key, oldCallback);
		  }

		  oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		  domNode.addEventListener(
			key,
			oldCallback,
			_VirtualDom_passiveSupported && {
			  passive:
				$elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2
			}
		  );
		  allCallbacks[key] = oldCallback;
		}
	  }

	  // PASSIVE EVENTS

	  var _VirtualDom_passiveSupported;

	  try {
		window.addEventListener(
		  "t",
		  null,
		  Object.defineProperty({}, "passive", {
			get: function() {
			  _VirtualDom_passiveSupported = true;
			}
		  })
		);
	  } catch (e) {}

	  // EVENT HANDLERS

	  function _VirtualDom_makeCallback(eventNode, initialHandler) {
		function callback(event) {
		  var handler = callback.q;
		  var result = _Json_runHelp(handler.a, event);

		  if (!$elm$core$Result$isOk(result)) {
			return;
		  }

		  var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		  // 0 = Normal
		  // 1 = MayStopPropagation
		  // 2 = MayPreventDefault
		  // 3 = Custom

		  var value = result.a;
		  var message = !tag ? value : tag < 3 ? value.a : value.message;
		  var stopPropagation =
			tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		  var currentEventNode =
			(stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) &&
			  event.preventDefault(),
			eventNode);
		  var tagger;
		  var i;
		  while ((tagger = currentEventNode.j)) {
			if (typeof tagger == "function") {
			  message = tagger(message);
			} else {
			  for (var i = tagger.length; i--; ) {
				message = tagger[i](message);
			  }
			}
			currentEventNode = currentEventNode.p;
		  }
		  currentEventNode(message, stopPropagation); // stopPropagation implies isSync
		}

		callback.q = initialHandler;

		return callback;
	  }

	  function _VirtualDom_equalEvents(x, y) {
		return x.$ == y.$ && _Json_equality(x.a, y.a);
	  }

	  // DIFF

	  // TODO: Should we do patches like in iOS?
	  //
	  // type Patch
	  //   = At Int Patch
	  //   | Batch (List Patch)
	  //   | Change ...
	  //
	  // How could it not be better?
	  //
	  function _VirtualDom_diff(x, y) {
		var patches = [];
		_VirtualDom_diffHelp(x, y, patches, 0);
		return patches;
	  }

	  function _VirtualDom_pushPatch(patches, type, index, data) {
		var patch = {
		  $: type,
		  r: index,
		  s: data,
		  t: undefined,
		  u: undefined
		};
		patches.push(patch);
		return patch;
	  }

	  function _VirtualDom_diffHelp(x, y, patches, index) {
		if (x === y) {
		  return;
		}

		var xType = x.$;
		var yType = y.$;

		// Bail if you run into different types of nodes. Implies that the
		// structure has changed significantly and it's not worth a diff.
		if (xType !== yType) {
		  if (xType === 1 && yType === 2) {
			y = _VirtualDom_dekey(y);
			yType = 1;
		  } else {
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		  }
		}

		// Now we know that both nodes are the same $.
		switch (yType) {
		  case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--) {
			  same = xRefs[i] === yRefs[i];
			}
			if (same) {
			  y.k = x.k;
			  return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 &&
			  _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		  case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4) {
			  nesting = true;

			  typeof xTaggers !== "object"
				? (xTaggers = [xTaggers, xSubNode.j])
				: xTaggers.push(xSubNode.j);

			  xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4) {
			  nesting = true;

			  typeof yTaggers !== "object"
				? (yTaggers = [yTaggers, ySubNode.j])
				: yTaggers.push(ySubNode.j);

			  ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length) {
			  _VirtualDom_pushPatch(patches, 0, index, y);
			  return;
			}

			// check if taggers are "the same"
			if (
			  nesting
				? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers)
				: xTaggers !== yTaggers
			) {
			  _VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		  case 0:
			if (x.a !== y.a) {
			  _VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		  case 1:
			_VirtualDom_diffNodes(
			  x,
			  y,
			  patches,
			  index,
			  _VirtualDom_diffKids
			);
			return;

		  case 2:
			_VirtualDom_diffNodes(
			  x,
			  y,
			  patches,
			  index,
			  _VirtualDom_diffKeyedKids
			);
			return;

		  case 3:
			if (x.h !== y.h) {
			  _VirtualDom_pushPatch(patches, 0, index, y);
			  return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff &&
			  _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
		}
	  }

	  // assumes the incoming arrays are the same length
	  function _VirtualDom_pairwiseRefEqual(as, bs) {
		for (var i = 0; i < as.length; i++) {
		  if (as[i] !== bs[i]) {
			return false;
		  }
		}

		return true;
	  }

	  function _VirtualDom_diffNodes(x, y, patches, index, diffKids) {
		// Bail if obvious indicators have changed. Implies more serious
		// structural changes such that it's not worth it to diff.
		if (x.c !== y.c || x.f !== y.f) {
		  _VirtualDom_pushPatch(patches, 0, index, y);
		  return;
		}

		var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
		factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

		diffKids(x, y, patches, index);
	  }

	  // DIFF FACTS

	  // TODO Instead of creating a new diff object, it's possible to just test if
	  // there *is* a diff. During the actual patch, do the diff again and make the
	  // modifications directly. This way, there's no new allocations. Worth it?
	  function _VirtualDom_diffFacts(x, y, category) {
		var diff;

		// look for changes and removals
		for (var xKey in x) {
		  if (
			xKey === "a1" ||
			xKey === "a0" ||
			xKey === "a3" ||
			xKey === "a4"
		  ) {
			var subDiff = _VirtualDom_diffFacts(
			  x[xKey],
			  y[xKey] || {},
			  xKey
			);
			if (subDiff) {
			  diff = diff || {};
			  diff[xKey] = subDiff;
			}
			continue;
		  }

		  // remove if not in the new facts
		  if (!(xKey in y)) {
			diff = diff || {};
			diff[xKey] = !category
			  ? typeof x[xKey] === "string"
				? ""
				: null
			  : category === "a1"
			  ? ""
			  : category === "a0" || category === "a3"
			  ? undefined
			  : { f: x[xKey].f, o: undefined };

			continue;
		  }

		  var xValue = x[xKey];
		  var yValue = y[xKey];

		  // reference equal, so don't worry about it
		  if (
			(xValue === yValue && xKey !== "value" && xKey !== "checked") ||
			(category === "a0" && _VirtualDom_equalEvents(xValue, yValue))
		  ) {
			continue;
		  }

		  diff = diff || {};
		  diff[xKey] = yValue;
		}

		// add new stuff
		for (var yKey in y) {
		  if (!(yKey in x)) {
			diff = diff || {};
			diff[yKey] = y[yKey];
		  }
		}

		return diff;
	  }

	  // DIFF KIDS

	  function _VirtualDom_diffKids(xParent, yParent, patches, index) {
		var xKids = xParent.e;
		var yKids = yParent.e;

		var xLen = xKids.length;
		var yLen = yKids.length;

		// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

		if (xLen > yLen) {
		  _VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		  });
		} else if (xLen < yLen) {
		  _VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		  });
		}

		// PAIRWISE DIFF EVERYTHING ELSE

		for (
		  var minLen = xLen < yLen ? xLen : yLen, i = 0;
		  i < minLen;
		  i++
		) {
		  var xKid = xKids[i];
		  _VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		  index += xKid.b || 0;
		}
	  }

	  // KEYED DIFF

	  function _VirtualDom_diffKeyedKids(
		xParent,
		yParent,
		patches,
		rootIndex
	  ) {
		var localPatches = [];

		var changes = {}; // Dict String Entry
		var inserts = []; // Array { index : Int, entry : Entry }
		// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

		var xKids = xParent.e;
		var yKids = yParent.e;
		var xLen = xKids.length;
		var yLen = yKids.length;
		var xIndex = 0;
		var yIndex = 0;

		var index = rootIndex;

		while (xIndex < xLen && yIndex < yLen) {
		  var x = xKids[xIndex];
		  var y = yKids[yIndex];

		  var xKey = x.a;
		  var yKey = y.a;
		  var xNode = x.b;
		  var yNode = y.b;

		  var newMatch = undefined;
		  var oldMatch = undefined;

		  // check if keys match

		  if (xKey === yKey) {
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		  }

		  // look ahead 1 to detect insertions and removals.

		  var xNext = xKids[xIndex + 1];
		  var yNext = yKids[yIndex + 1];

		  if (xNext) {
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		  }

		  if (yNext) {
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		  }

		  // swap x and y
		  if (newMatch && oldMatch) {
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(
			  changes,
			  localPatches,
			  xKey,
			  yNode,
			  yIndex,
			  inserts
			);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(
			  changes,
			  localPatches,
			  xKey,
			  xNextNode,
			  index
			);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		  }

		  // insert y
		  if (newMatch) {
			index++;
			_VirtualDom_insertNode(
			  changes,
			  localPatches,
			  yKey,
			  yNode,
			  yIndex,
			  inserts
			);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		  }

		  // remove x
		  if (oldMatch) {
			index++;
			_VirtualDom_removeNode(
			  changes,
			  localPatches,
			  xKey,
			  xNode,
			  index
			);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		  }

		  // remove x, insert y
		  if (xNext && xNextKey === yNextKey) {
			index++;
			_VirtualDom_removeNode(
			  changes,
			  localPatches,
			  xKey,
			  xNode,
			  index
			);
			_VirtualDom_insertNode(
			  changes,
			  localPatches,
			  yKey,
			  yNode,
			  yIndex,
			  inserts
			);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		  }

		  break;
		}

		// eat up any remaining nodes with removeNode and insertNode

		while (xIndex < xLen) {
		  index++;
		  var x = xKids[xIndex];
		  var xNode = x.b;
		  _VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		  index += xNode.b || 0;
		  xIndex++;
		}

		while (yIndex < yLen) {
		  var endInserts = endInserts || [];
		  var y = yKids[yIndex];
		  _VirtualDom_insertNode(
			changes,
			localPatches,
			y.a,
			y.b,
			undefined,
			endInserts
		  );
		  yIndex++;
		}

		if (localPatches.length > 0 || inserts.length > 0 || endInserts) {
		  _VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		  });
		}
	  }

	  // CHANGES FROM KEYED DIFF

	  var _VirtualDom_POSTFIX = "_elmW6BL";

	  function _VirtualDom_insertNode(
		changes,
		localPatches,
		key,
		vnode,
		yIndex,
		inserts
	  ) {
		var entry = changes[key];

		// never seen this key before
		if (!entry) {
		  entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		  };

		  inserts.push({ r: yIndex, A: entry });
		  changes[key] = entry;

		  return;
		}

		// this key was removed earlier, a match!
		if (entry.c === 1) {
		  inserts.push({ r: yIndex, A: entry });

		  entry.c = 2;
		  var subPatches = [];
		  _VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		  entry.r = yIndex;
		  entry.s.s = {
			w: subPatches,
			A: entry
		  };

		  return;
		}

		// this key has already been inserted or moved, a duplicate!
		_VirtualDom_insertNode(
		  changes,
		  localPatches,
		  key + _VirtualDom_POSTFIX,
		  vnode,
		  yIndex,
		  inserts
		);
	  }

	  function _VirtualDom_removeNode(
		changes,
		localPatches,
		key,
		vnode,
		index
	  ) {
		var entry = changes[key];

		// never seen this key before
		if (!entry) {
		  var patch = _VirtualDom_pushPatch(
			localPatches,
			9,
			index,
			undefined
		  );

		  changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		  };

		  return;
		}

		// this key was inserted earlier, a match!
		if (entry.c === 0) {
		  entry.c = 2;
		  var subPatches = [];
		  _VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		  _VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		  });

		  return;
		}

		// this key has already been removed or moved, a duplicate!
		_VirtualDom_removeNode(
		  changes,
		  localPatches,
		  key + _VirtualDom_POSTFIX,
		  vnode,
		  index
		);
	  }

	  // ADD DOM NODES
	  //
	  // Each DOM node has an "index" assigned in order of traversal. It is important
	  // to minimize our crawl over the actual DOM, so these indexes (along with the
	  // descendantsCount of virtual nodes) let us skip touching entire subtrees of
	  // the DOM if we know there are no patches there.

	  function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode) {
		_VirtualDom_addDomNodesHelp(
		  domNode,
		  vNode,
		  patches,
		  0,
		  0,
		  vNode.b,
		  eventNode
		);
	  }

	  // assumes `patches` is non-empty and indexes increase monotonically.
	  function _VirtualDom_addDomNodesHelp(
		domNode,
		vNode,
		patches,
		i,
		low,
		high,
		eventNode
	  ) {
		var patch = patches[i];
		var index = patch.r;

		while (index === low) {
		  var patchType = patch.$;

		  if (patchType === 1) {
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		  } else if (patchType === 8) {
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0) {
			  _VirtualDom_addDomNodesHelp(
				domNode,
				vNode,
				subPatches,
				0,
				low,
				high,
				eventNode
			  );
			}
		  } else if (patchType === 9) {
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data) {
			  data.A.s = domNode;
			  var subPatches = data.w;
			  if (subPatches.length > 0) {
				_VirtualDom_addDomNodesHelp(
				  domNode,
				  vNode,
				  subPatches,
				  0,
				  low,
				  high,
				  eventNode
				);
			  }
			}
		  } else {
			patch.t = domNode;
			patch.u = eventNode;
		  }

		  i++;

		  if (!(patch = patches[i]) || (index = patch.r) > high) {
			return i;
		  }
		}

		var tag = vNode.$;

		if (tag === 4) {
		  var subNode = vNode.k;

		  while (subNode.$ === 4) {
			subNode = subNode.k;
		  }

		  return _VirtualDom_addDomNodesHelp(
			domNode,
			subNode,
			patches,
			i,
			low + 1,
			high,
			domNode.elm_event_node_ref
		  );
		}

		// tag must be 1 or 2 at this point

		var vKids = vNode.e;
		var childNodes = domNode.childNodes;
		for (var j = 0; j < vKids.length; j++) {
		  low++;
		  var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		  var nextLow = low + (vKid.b || 0);
		  if (low <= index && index <= nextLow) {
			i = _VirtualDom_addDomNodesHelp(
			  childNodes[j],
			  vKid,
			  patches,
			  i,
			  low,
			  nextLow,
			  eventNode
			);
			if (!(patch = patches[i]) || (index = patch.r) > high) {
			  return i;
			}
		  }
		  low = nextLow;
		}
		return i;
	  }

	  // APPLY PATCHES

	  function _VirtualDom_applyPatches(
		rootDomNode,
		oldVirtualNode,
		patches,
		eventNode
	  ) {
		if (patches.length === 0) {
		  return rootDomNode;
		}

		_VirtualDom_addDomNodes(
		  rootDomNode,
		  oldVirtualNode,
		  patches,
		  eventNode
		);
		return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
	  }

	  function _VirtualDom_applyPatchesHelp(rootDomNode, patches) {
		for (var i = 0; i < patches.length; i++) {
		  var patch = patches[i];
		  var localDomNode = patch.t;
		  var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		  if (localDomNode === rootDomNode) {
			rootDomNode = newNode;
		  }
		}
		return rootDomNode;
	  }

	  function _VirtualDom_applyPatch(domNode, patch) {
		switch (patch.$) {
		  case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		  case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		  case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		  case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		  case 2:
			if (domNode.elm_event_node_ref) {
			  domNode.elm_event_node_ref.j = patch.s;
			} else {
			  domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		  case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++) {
			  domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		  case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++) {
			  domNode.insertBefore(
				_VirtualDom_render(kids[i], patch.u),
				theEnd
			  );
			}
			return domNode;

		  case 9:
			var data = patch.s;
			if (!data) {
			  domNode.parentNode.removeChild(domNode);
			  return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== "undefined") {
			  domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		  case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		  case 5:
			return patch.s(domNode);

		  default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
		}
	  }

	  function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode) {
		var parentNode = domNode.parentNode;
		var newNode = _VirtualDom_render(vNode, eventNode);

		if (!newNode.elm_event_node_ref) {
		  newNode.elm_event_node_ref = domNode.elm_event_node_ref;
		}

		if (parentNode && newNode !== domNode) {
		  parentNode.replaceChild(newNode, domNode);
		}
		return newNode;
	  }

	  function _VirtualDom_applyPatchReorder(domNode, patch) {
		var data = patch.s;

		// remove end inserts
		var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(
		  data.y,
		  patch
		);

		// removals
		domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

		// inserts
		var inserts = data.x;
		for (var i = 0; i < inserts.length; i++) {
		  var insert = inserts[i];
		  var entry = insert.A;
		  var node =
			entry.c === 2 ? entry.s : _VirtualDom_render(entry.z, patch.u);
		  domNode.insertBefore(node, domNode.childNodes[insert.r]);
		}

		// add end inserts
		if (frag) {
		  _VirtualDom_appendChild(domNode, frag);
		}

		return domNode;
	  }

	  function _VirtualDom_applyPatchReorderEndInsertsHelp(
		endInserts,
		patch
	  ) {
		if (!endInserts) {
		  return;
		}

		var frag = _VirtualDom_doc.createDocumentFragment();
		for (var i = 0; i < endInserts.length; i++) {
		  var insert = endInserts[i];
		  var entry = insert.A;
		  _VirtualDom_appendChild(
			frag,
			entry.c === 2 ? entry.s : _VirtualDom_render(entry.z, patch.u)
		  );
		}
		return frag;
	  }

	  function _VirtualDom_virtualize(node) {
		// TEXT NODES

		if (node.nodeType === 3) {
		  return _VirtualDom_text(node.textContent);
		}

		// WEIRD NODES

		if (node.nodeType !== 1) {
		  return _VirtualDom_text("");
		}

		// ELEMENT NODES

		var attrList = _List_Nil;
		var attrs = node.attributes;
		for (var i = attrs.length; i--; ) {
		  var attr = attrs[i];
		  var name = attr.name;
		  var value = attr.value;
		  attrList = _List_Cons(
			A2(_VirtualDom_attribute, name, value),
			attrList
		  );
		}

		var tag = node.tagName.toLowerCase();
		var kidList = _List_Nil;
		var kids = node.childNodes;

		for (var i = kids.length; i--; ) {
		  kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
		}
		return A3(_VirtualDom_node, tag, attrList, kidList);
	  }

	  function _VirtualDom_dekey(keyedNode) {
		var keyedKids = keyedNode.e;
		var len = keyedKids.length;
		var kids = new Array(len);
		for (var i = 0; i < len; i++) {
		  kids[i] = keyedKids[i].b;
		}

		return {
		  $: 1,
		  c: keyedNode.c,
		  d: keyedNode.d,
		  e: kids,
		  f: keyedNode.f,
		  b: keyedNode.b
		};
	  }

	  // ELEMENT

	  var _Debugger_element;

	  var _Browser_element =
		_Debugger_element ||
		F4(function(impl, flagDecoder, debugMetadata, args) {
		  return _Platform_initialize(
			flagDecoder,
			args,
			impl.init,
			impl.update,
			impl.subscriptions,
			function(sendToApp, initialModel) {
			  var view = impl.view;
			  /**_UNUSED/
		var domNode = args['node'];
		//*/
			  /**/
			  var domNode =
				args && args["node"] ? args["node"] : _Debug_crash(0);
			  //*/
			  var currNode = _VirtualDom_virtualize(domNode);

			  return _Browser_makeAnimator(initialModel, function(model) {
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(
				  domNode,
				  currNode,
				  patches,
				  sendToApp
				);
				currNode = nextNode;
			  });
			}
		  );
		});

	  // DOCUMENT

	  var _Debugger_document;

	  var _Browser_document =
		_Debugger_document ||
		F4(function(impl, flagDecoder, debugMetadata, args) {
		  return _Platform_initialize(
			flagDecoder,
			args,
			impl.init,
			impl.update,
			impl.subscriptions,
			function(sendToApp, initialModel) {
			  var divertHrefToApp = impl.setup && impl.setup(sendToApp);
			  var view = impl.view;
			  var title = _VirtualDom_doc.title;
			  var bodyNode = _VirtualDom_doc.body;
			  var currNode = _VirtualDom_virtualize(bodyNode);
			  return _Browser_makeAnimator(initialModel, function(model) {
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node("body")(_List_Nil)(
				  doc.body
				);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(
				  bodyNode,
				  currNode,
				  patches,
				  sendToApp
				);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				title !== doc.title &&
				  (_VirtualDom_doc.title = title = doc.title);
			  });
			}
		  );
		});

	  // ANIMATION

	  var _Browser_cancelAnimationFrame =
		typeof cancelAnimationFrame !== "undefined"
		  ? cancelAnimationFrame
		  : function(id) {
			  clearTimeout(id);
			};

	  var _Browser_requestAnimationFrame =
		typeof requestAnimationFrame !== "undefined"
		  ? requestAnimationFrame
		  : function(callback) {
			  return setTimeout(callback, 1000 / 60);
			};

	  function _Browser_makeAnimator(model, draw) {
		draw(model);

		var state = 0;

		function updateIfNeeded() {
		  state =
			state === 1
			  ? 0
			  : (_Browser_requestAnimationFrame(updateIfNeeded),
				draw(model),
				1);
		}

		return function(nextModel, isSync) {
		  model = nextModel;

		  isSync
			? (draw(model), state === 2 && (state = 1))
			: (state === 0 &&
				_Browser_requestAnimationFrame(updateIfNeeded),
			  (state = 2));
		};
	  }

	  // APPLICATION

	  function _Browser_application(impl) {
		var onUrlChange = impl.onUrlChange;
		var onUrlRequest = impl.onUrlRequest;
		var key = function() {
		  key.a(onUrlChange(_Browser_getUrl()));
		};

		return _Browser_document({
		  setup: function(sendToApp) {
			key.a = sendToApp;
			_Browser_window.addEventListener("popstate", key);
			_Browser_window.navigator.userAgent.indexOf("Trident") < 0 ||
			  _Browser_window.addEventListener("hashchange", key);

			return F2(function(domNode, event) {
			  if (
				!event.ctrlKey &&
				!event.metaKey &&
				!event.shiftKey &&
				event.button < 1 &&
				!domNode.target &&
				!domNode.hasAttribute("download")
			  ) {
				event.preventDefault();
				var href = domNode.href;
				var curr = _Browser_getUrl();
				var next = $elm$url$Url$fromString(href).a;
				sendToApp(
				  onUrlRequest(
					next &&
					  curr.protocol === next.protocol &&
					  curr.host === next.host &&
					  curr.port_.a === next.port_.a
					  ? $elm$browser$Browser$Internal(next)
					  : $elm$browser$Browser$External(href)
				  )
				);
			  }
			});
		  },
		  init: function(flags) {
			return A3(impl.init, flags, _Browser_getUrl(), key);
		  },
		  view: impl.view,
		  update: impl.update,
		  subscriptions: impl.subscriptions
		});
	  }

	  function _Browser_getUrl() {
		return (
		  $elm$url$Url$fromString(_VirtualDom_doc.location.href).a ||
		  _Debug_crash(1)
		);
	  }

	  var _Browser_go = F2(function(key, n) {
		return A2(
		  $elm$core$Task$perform,
		  $elm$core$Basics$never,
		  _Scheduler_binding(function() {
			n && history.go(n);
			key();
		  })
		);
	  });

	  var _Browser_pushUrl = F2(function(key, url) {
		return A2(
		  $elm$core$Task$perform,
		  $elm$core$Basics$never,
		  _Scheduler_binding(function() {
			history.pushState({}, "", url);
			key();
		  })
		);
	  });

	  var _Browser_replaceUrl = F2(function(key, url) {
		return A2(
		  $elm$core$Task$perform,
		  $elm$core$Basics$never,
		  _Scheduler_binding(function() {
			history.replaceState({}, "", url);
			key();
		  })
		);
	  });

	  // GLOBAL EVENTS

	  var _Browser_fakeNode = {
		addEventListener: function() {},
		removeEventListener: function() {}
	  };
	  var _Browser_doc =
		typeof document !== "undefined" ? document : _Browser_fakeNode;
	  var _Browser_window =
		typeof window !== "undefined" ? window : _Browser_fakeNode;

	  var _Browser_on = F3(function(node, eventName, sendToSelf) {
		return _Scheduler_spawn(
		  _Scheduler_binding(function(callback) {
			function handler(event) {
			  _Scheduler_rawSpawn(sendToSelf(event));
			}
			node.addEventListener(
			  eventName,
			  handler,
			  _VirtualDom_passiveSupported && { passive: true }
			);
			return function() {
			  node.removeEventListener(eventName, handler);
			};
		  })
		);
	  });

	  var _Browser_decodeEvent = F2(function(decoder, event) {
		var result = _Json_runHelp(decoder, event);
		return $elm$core$Result$isOk(result)
		  ? $elm$core$Maybe$Just(result.a)
		  : $elm$core$Maybe$Nothing;
	  });

	  // PAGE VISIBILITY

	  function _Browser_visibilityInfo() {
		return typeof _VirtualDom_doc.hidden !== "undefined"
		  ? { hidden: "hidden", change: "visibilitychange" }
		  : typeof _VirtualDom_doc.mozHidden !== "undefined"
		  ? { hidden: "mozHidden", change: "mozvisibilitychange" }
		  : typeof _VirtualDom_doc.msHidden !== "undefined"
		  ? { hidden: "msHidden", change: "msvisibilitychange" }
		  : typeof _VirtualDom_doc.webkitHidden !== "undefined"
		  ? { hidden: "webkitHidden", change: "webkitvisibilitychange" }
		  : { hidden: "hidden", change: "visibilitychange" };
	  }

	  // ANIMATION FRAMES

	  function _Browser_rAF() {
		return _Scheduler_binding(function(callback) {
		  var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		  });

		  return function() {
			_Browser_cancelAnimationFrame(id);
		  };
		});
	  }

	  function _Browser_now() {
		return _Scheduler_binding(function(callback) {
		  callback(_Scheduler_succeed(Date.now()));
		});
	  }

	  // DOM STUFF

	  function _Browser_withNode(id, doStuff) {
		return _Scheduler_binding(function(callback) {
		  _Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(
			  node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		  });
		});
	  }

	  function _Browser_withWindow(doStuff) {
		return _Scheduler_binding(function(callback) {
		  _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		  });
		});
	  }

	  // FOCUS and BLUR

	  var _Browser_call = F2(function(functionName, id) {
		return _Browser_withNode(id, function(node) {
		  node[functionName]();
		  return _Utils_Tuple0;
		});
	  });

	  // WINDOW VIEWPORT

	  function _Browser_getViewport() {
		return {
		  scene: _Browser_getScene(),
		  viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		  }
		};
	  }

	  function _Browser_getScene() {
		var body = _Browser_doc.body;
		var elem = _Browser_doc.documentElement;
		return {
		  width: Math.max(
			body.scrollWidth,
			body.offsetWidth,
			elem.scrollWidth,
			elem.offsetWidth,
			elem.clientWidth
		  ),
		  height: Math.max(
			body.scrollHeight,
			body.offsetHeight,
			elem.scrollHeight,
			elem.offsetHeight,
			elem.clientHeight
		  )
		};
	  }

	  var _Browser_setViewport = F2(function(x, y) {
		return _Browser_withWindow(function() {
		  _Browser_window.scroll(x, y);
		  return _Utils_Tuple0;
		});
	  });

	  // ELEMENT VIEWPORT

	  function _Browser_getViewportOf(id) {
		return _Browser_withNode(id, function(node) {
		  return {
			scene: {
			  width: node.scrollWidth,
			  height: node.scrollHeight
			},
			viewport: {
			  x: node.scrollLeft,
			  y: node.scrollTop,
			  width: node.clientWidth,
			  height: node.clientHeight
			}
		  };
		});
	  }

	  var _Browser_setViewportOf = F3(function(id, x, y) {
		return _Browser_withNode(id, function(node) {
		  node.scrollLeft = x;
		  node.scrollTop = y;
		  return _Utils_Tuple0;
		});
	  });

	  // ELEMENT

	  function _Browser_getElement(id) {
		return _Browser_withNode(id, function(node) {
		  var rect = node.getBoundingClientRect();
		  var x = _Browser_window.pageXOffset;
		  var y = _Browser_window.pageYOffset;
		  return {
			scene: _Browser_getScene(),
			viewport: {
			  x: x,
			  y: y,
			  width: _Browser_doc.documentElement.clientWidth,
			  height: _Browser_doc.documentElement.clientHeight
			},
			element: {
			  x: x + rect.left,
			  y: y + rect.top,
			  width: rect.width,
			  height: rect.height
			}
		  };
		});
	  }

	  // LOAD and RELOAD

	  function _Browser_reload(skipCache) {
		return A2(
		  $elm$core$Task$perform,
		  $elm$core$Basics$never,
		  _Scheduler_binding(function(callback) {
			_VirtualDom_doc.location.reload(skipCache);
		  })
		);
	  }

	  function _Browser_load(url) {
		return A2(
		  $elm$core$Task$perform,
		  $elm$core$Basics$never,
		  _Scheduler_binding(function(callback) {
			try {
			  _Browser_window.location = url;
			} catch (err) {
			  // Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			  // Other browsers reload the page, so let's be consistent about that.
			  _VirtualDom_doc.location.reload(false);
			}
		  })
		);
	  }

	  // CREATE

	  var _Regex_never = /.^/;

	  var _Regex_fromStringWith = F2(function(options, string) {
		var flags = "g";
		if (options.multiline) {
		  flags += "m";
		}
		if (options.caseInsensitive) {
		  flags += "i";
		}

		try {
		  return $elm$core$Maybe$Just(new RegExp(string, flags));
		} catch (error) {
		  return $elm$core$Maybe$Nothing;
		}
	  });

	  // USE

	  var _Regex_contains = F2(function(re, string) {
		return string.match(re) !== null;
	  });

	  var _Regex_findAtMost = F3(function(n, re, str) {
		var out = [];
		var number = 0;
		var string = str;
		var lastIndex = re.lastIndex;
		var prevLastIndex = -1;
		var result;
		while (number++ < n && (result = re.exec(string))) {
		  if (prevLastIndex == re.lastIndex) break;
		  var i = result.length - 1;
		  var subs = new Array(i);
		  while (i > 0) {
			var submatch = result[i];
			subs[--i] = submatch
			  ? $elm$core$Maybe$Just(submatch)
			  : $elm$core$Maybe$Nothing;
		  }
		  out.push(
			A4(
			  $elm$regex$Regex$Match,
			  result[0],
			  result.index,
			  number,
			  _List_fromArray(subs)
			)
		  );
		  prevLastIndex = re.lastIndex;
		}
		re.lastIndex = lastIndex;
		return _List_fromArray(out);
	  });

	  var _Regex_replaceAtMost = F4(function(n, re, replacer, string) {
		var count = 0;
		function jsReplacer(match) {
		  if (count++ >= n) {
			return match;
		  }
		  var i = arguments.length - 3;
		  var submatches = new Array(i);
		  while (i > 0) {
			var submatch = arguments[i];
			submatches[--i] = submatch
			  ? $elm$core$Maybe$Just(submatch)
			  : $elm$core$Maybe$Nothing;
		  }
		  return replacer(
			A4(
			  $elm$regex$Regex$Match,
			  match,
			  arguments[arguments.length - 2],
			  count,
			  _List_fromArray(submatches)
			)
		  );
		}
		return string.replace(re, jsReplacer);
	  });

	  var _Regex_splitAtMost = F3(function(n, re, str) {
		var string = str;
		var out = [];
		var start = re.lastIndex;
		var restoreLastIndex = re.lastIndex;
		while (n--) {
		  var result = re.exec(string);
		  if (!result) break;
		  out.push(string.slice(start, result.index));
		  start = re.lastIndex;
		}
		out.push(string.slice(start));
		re.lastIndex = restoreLastIndex;
		return _List_fromArray(out);
	  });

	  var _Regex_infinity = Infinity;
	  var $author$project$Main$init =
		"\nmodule Main exposing (..)\n\nf : Int -> Int\nf x = x + 1\n\ng : Int -> Int\ng x = x * 2\n\nh = f << g\n";
	  var $elm$core$Basics$EQ = { $: "EQ" };
	  var $elm$core$Basics$GT = { $: "GT" };
	  var $elm$core$Basics$LT = { $: "LT" };
	  var $elm$core$List$cons = _List_cons;
	  var $elm$core$Dict$foldr = F3(function(func, acc, t) {
		foldr: while (true) {
		  if (t.$ === "RBEmpty_elm_builtin") {
			return acc;
		  } else {
			var key = t.b;
			var value = t.c;
			var left = t.d;
			var right = t.e;
			var $temp$func = func,
			  $temp$acc = A3(
				func,
				key,
				value,
				A3($elm$core$Dict$foldr, func, acc, right)
			  ),
			  $temp$t = left;
			func = $temp$func;
			acc = $temp$acc;
			t = $temp$t;
			continue foldr;
		  }
		}
	  });
	  var $elm$core$Dict$toList = function(dict) {
		return A3(
		  $elm$core$Dict$foldr,
		  F3(function(key, value, list) {
			return A2($elm$core$List$cons, _Utils_Tuple2(key, value), list);
		  }),
		  _List_Nil,
		  dict
		);
	  };
	  var $elm$core$Dict$keys = function(dict) {
		return A3(
		  $elm$core$Dict$foldr,
		  F3(function(key, value, keyList) {
			return A2($elm$core$List$cons, key, keyList);
		  }),
		  _List_Nil,
		  dict
		);
	  };
	  var $elm$core$Set$toList = function(_v0) {
		var dict = _v0.a;
		return $elm$core$Dict$keys(dict);
	  };
	  var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
	  var $elm$core$Array$foldr = F3(function(func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(function(node, acc) {
		  if (node.$ === "SubTree") {
			var subTree = node.a;
			return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
		  } else {
			var values = node.a;
			return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
		  }
		});
		return A3(
		  $elm$core$Elm$JsArray$foldr,
		  helper,
		  A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
		  tree
		);
	  });
	  var $elm$core$Array$toList = function(array) {
		return A3(
		  $elm$core$Array$foldr,
		  $elm$core$List$cons,
		  _List_Nil,
		  array
		);
	  };
	  var $elm$core$Result$Err = function(a) {
		return { $: "Err", a: a };
	  };
	  var $elm$json$Json$Decode$Failure = F2(function(a, b) {
		return { $: "Failure", a: a, b: b };
	  });
	  var $elm$json$Json$Decode$Field = F2(function(a, b) {
		return { $: "Field", a: a, b: b };
	  });
	  var $elm$json$Json$Decode$Index = F2(function(a, b) {
		return { $: "Index", a: a, b: b };
	  });
	  var $elm$core$Result$Ok = function(a) {
		return { $: "Ok", a: a };
	  };
	  var $elm$json$Json$Decode$OneOf = function(a) {
		return { $: "OneOf", a: a };
	  };
	  var $elm$core$Basics$False = { $: "False" };
	  var $elm$core$Basics$add = _Basics_add;
	  var $elm$core$Maybe$Just = function(a) {
		return { $: "Just", a: a };
	  };
	  var $elm$core$Maybe$Nothing = { $: "Nothing" };
	  var $elm$core$String$all = _String_all;
	  var $elm$core$Basics$and = _Basics_and;
	  var $elm$core$Basics$append = _Utils_append;
	  var $elm$json$Json$Encode$encode = _Json_encode;
	  var $elm$core$String$fromInt = _String_fromNumber;
	  var $elm$core$String$join = F2(function(sep, chunks) {
		return A2(_String_join, sep, _List_toArray(chunks));
	  });
	  var $elm$core$String$split = F2(function(sep, string) {
		return _List_fromArray(A2(_String_split, sep, string));
	  });
	  var $elm$json$Json$Decode$indent = function(str) {
		return A2(
		  $elm$core$String$join,
		  "\n    ",
		  A2($elm$core$String$split, "\n", str)
		);
	  };
	  var $elm$core$List$foldl = F3(function(func, acc, list) {
		foldl: while (true) {
		  if (!list.b) {
			return acc;
		  } else {
			var x = list.a;
			var xs = list.b;
			var $temp$func = func,
			  $temp$acc = A2(func, x, acc),
			  $temp$list = xs;
			func = $temp$func;
			acc = $temp$acc;
			list = $temp$list;
			continue foldl;
		  }
		}
	  });
	  var $elm$core$List$length = function(xs) {
		return A3(
		  $elm$core$List$foldl,
		  F2(function(_v0, i) {
			return i + 1;
		  }),
		  0,
		  xs
		);
	  };
	  var $elm$core$List$map2 = _List_map2;
	  var $elm$core$Basics$le = _Utils_le;
	  var $elm$core$Basics$sub = _Basics_sub;
	  var $elm$core$List$rangeHelp = F3(function(lo, hi, list) {
		rangeHelp: while (true) {
		  if (_Utils_cmp(lo, hi) < 1) {
			var $temp$lo = lo,
			  $temp$hi = hi - 1,
			  $temp$list = A2($elm$core$List$cons, hi, list);
			lo = $temp$lo;
			hi = $temp$hi;
			list = $temp$list;
			continue rangeHelp;
		  } else {
			return list;
		  }
		}
	  });
	  var $elm$core$List$range = F2(function(lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	  });
	  var $elm$core$List$indexedMap = F2(function(f, xs) {
		return A3(
		  $elm$core$List$map2,
		  f,
		  A2($elm$core$List$range, 0, $elm$core$List$length(xs) - 1),
		  xs
		);
	  });
	  var $elm$core$Char$toCode = _Char_toCode;
	  var $elm$core$Char$isLower = function(_char) {
		var code = $elm$core$Char$toCode(_char);
		return 97 <= code && code <= 122;
	  };
	  var $elm$core$Char$isUpper = function(_char) {
		var code = $elm$core$Char$toCode(_char);
		return code <= 90 && 65 <= code;
	  };
	  var $elm$core$Basics$or = _Basics_or;
	  var $elm$core$Char$isAlpha = function(_char) {
		return (
		  $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char)
		);
	  };
	  var $elm$core$Char$isDigit = function(_char) {
		var code = $elm$core$Char$toCode(_char);
		return code <= 57 && 48 <= code;
	  };
	  var $elm$core$Char$isAlphaNum = function(_char) {
		return (
		  $elm$core$Char$isLower(_char) ||
		  $elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char)
		);
	  };
	  var $elm$core$List$reverse = function(list) {
		return A3(
		  $elm$core$List$foldl,
		  $elm$core$List$cons,
		  _List_Nil,
		  list
		);
	  };
	  var $elm$core$String$uncons = _String_uncons;
	  var $elm$json$Json$Decode$errorOneOf = F2(function(i, error) {
		return (
		  "\n\n(" +
		  ($elm$core$String$fromInt(i + 1) +
			(") " +
			  $elm$json$Json$Decode$indent(
				$elm$json$Json$Decode$errorToString(error)
			  )))
		);
	  });
	  var $elm$json$Json$Decode$errorToString = function(error) {
		return A2(
		  $elm$json$Json$Decode$errorToStringHelp,
		  error,
		  _List_Nil
		);
	  };
	  var $elm$json$Json$Decode$errorToStringHelp = F2(function(
		error,
		context
	  ) {
		errorToStringHelp: while (true) {
		  switch (error.$) {
			case "Field":
			  var f = error.a;
			  var err = error.b;
			  var isSimple = (function() {
				var _v1 = $elm$core$String$uncons(f);
				if (_v1.$ === "Nothing") {
				  return false;
				} else {
				  var _v2 = _v1.a;
				  var _char = _v2.a;
				  var rest = _v2.b;
				  return (
					$elm$core$Char$isAlpha(_char) &&
					A2(
					  $elm$core$String$all,
					  $elm$core$Char$isAlphaNum,
					  rest
					)
				  );
				}
			  })();
			  var fieldName = isSimple ? "." + f : "['" + (f + "']");
			  var $temp$error = err,
				$temp$context = A2($elm$core$List$cons, fieldName, context);
			  error = $temp$error;
			  context = $temp$context;
			  continue errorToStringHelp;
			case "Index":
			  var i = error.a;
			  var err = error.b;
			  var indexName = "[" + ($elm$core$String$fromInt(i) + "]");
			  var $temp$error = err,
				$temp$context = A2($elm$core$List$cons, indexName, context);
			  error = $temp$error;
			  context = $temp$context;
			  continue errorToStringHelp;
			case "OneOf":
			  var errors = error.a;
			  if (!errors.b) {
				return (
				  "Ran into a Json.Decode.oneOf with no possibilities" +
				  (function() {
					if (!context.b) {
					  return "!";
					} else {
					  return (
						" at json" +
						A2(
						  $elm$core$String$join,
						  "",
						  $elm$core$List$reverse(context)
						)
					  );
					}
				  })()
				);
			  } else {
				if (!errors.b.b) {
				  var err = errors.a;
				  var $temp$error = err,
					$temp$context = context;
				  error = $temp$error;
				  context = $temp$context;
				  continue errorToStringHelp;
				} else {
				  var starter = (function() {
					if (!context.b) {
					  return "Json.Decode.oneOf";
					} else {
					  return (
						"The Json.Decode.oneOf at json" +
						A2(
						  $elm$core$String$join,
						  "",
						  $elm$core$List$reverse(context)
						)
					  );
					}
				  })();
				  var introduction =
					starter +
					(" failed in the following " +
					  ($elm$core$String$fromInt(
						$elm$core$List$length(errors)
					  ) +
						" ways:"));
				  return A2(
					$elm$core$String$join,
					"\n\n",
					A2(
					  $elm$core$List$cons,
					  introduction,
					  A2(
						$elm$core$List$indexedMap,
						$elm$json$Json$Decode$errorOneOf,
						errors
					  )
					)
				  );
				}
			  }
			default:
			  var msg = error.a;
			  var json = error.b;
			  var introduction = (function() {
				if (!context.b) {
				  return "Problem with the given value:\n\n";
				} else {
				  return (
					"Problem with the value at json" +
					(A2(
					  $elm$core$String$join,
					  "",
					  $elm$core$List$reverse(context)
					) +
					  ":\n\n    ")
				  );
				}
			  })();
			  return (
				introduction +
				($elm$json$Json$Decode$indent(
				  A2($elm$json$Json$Encode$encode, 4, json)
				) +
				  ("\n\n" + msg))
			  );
		  }
		}
	  });
	  var $elm$core$Array$branchFactor = 32;
	  var $elm$core$Array$Array_elm_builtin = F4(function(a, b, c, d) {
		return { $: "Array_elm_builtin", a: a, b: b, c: c, d: d };
	  });
	  var $elm$core$Elm$JsArray$empty = _JsArray_empty;
	  var $elm$core$Basics$ceiling = _Basics_ceiling;
	  var $elm$core$Basics$fdiv = _Basics_fdiv;
	  var $elm$core$Basics$logBase = F2(function(base, number) {
		return _Basics_log(number) / _Basics_log(base);
	  });
	  var $elm$core$Basics$toFloat = _Basics_toFloat;
	  var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
		A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor)
	  );
	  var $elm$core$Array$empty = A4(
		$elm$core$Array$Array_elm_builtin,
		0,
		$elm$core$Array$shiftStep,
		$elm$core$Elm$JsArray$empty,
		$elm$core$Elm$JsArray$empty
	  );
	  var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
	  var $elm$core$Array$Leaf = function(a) {
		return { $: "Leaf", a: a };
	  };
	  var $elm$core$Basics$apL = F2(function(f, x) {
		return f(x);
	  });
	  var $elm$core$Basics$apR = F2(function(x, f) {
		return f(x);
	  });
	  var $elm$core$Basics$eq = _Utils_equal;
	  var $elm$core$Basics$floor = _Basics_floor;
	  var $elm$core$Elm$JsArray$length = _JsArray_length;
	  var $elm$core$Basics$gt = _Utils_gt;
	  var $elm$core$Basics$max = F2(function(x, y) {
		return _Utils_cmp(x, y) > 0 ? x : y;
	  });
	  var $elm$core$Basics$mul = _Basics_mul;
	  var $elm$core$Array$SubTree = function(a) {
		return { $: "SubTree", a: a };
	  };
	  var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
	  var $elm$core$Array$compressNodes = F2(function(nodes, acc) {
		compressNodes: while (true) {
		  var _v0 = A2(
			$elm$core$Elm$JsArray$initializeFromList,
			$elm$core$Array$branchFactor,
			nodes
		  );
		  var node = _v0.a;
		  var remainingNodes = _v0.b;
		  var newAcc = A2(
			$elm$core$List$cons,
			$elm$core$Array$SubTree(node),
			acc
		  );
		  if (!remainingNodes.b) {
			return $elm$core$List$reverse(newAcc);
		  } else {
			var $temp$nodes = remainingNodes,
			  $temp$acc = newAcc;
			nodes = $temp$nodes;
			acc = $temp$acc;
			continue compressNodes;
		  }
		}
	  });
	  var $elm$core$Tuple$first = function(_v0) {
		var x = _v0.a;
		return x;
	  };
	  var $elm$core$Array$treeFromBuilder = F2(function(
		nodeList,
		nodeListSize
	  ) {
		treeFromBuilder: while (true) {
		  var newNodeSize = $elm$core$Basics$ceiling(
			nodeListSize / $elm$core$Array$branchFactor
		  );
		  if (newNodeSize === 1) {
			return A2(
			  $elm$core$Elm$JsArray$initializeFromList,
			  $elm$core$Array$branchFactor,
			  nodeList
			).a;
		  } else {
			var $temp$nodeList = A2(
				$elm$core$Array$compressNodes,
				nodeList,
				_List_Nil
			  ),
			  $temp$nodeListSize = newNodeSize;
			nodeList = $temp$nodeList;
			nodeListSize = $temp$nodeListSize;
			continue treeFromBuilder;
		  }
		}
	  });
	  var $elm$core$Array$builderToArray = F2(function(
		reverseNodeList,
		builder
	  ) {
		if (!builder.nodeListSize) {
		  return A4(
			$elm$core$Array$Array_elm_builtin,
			$elm$core$Elm$JsArray$length(builder.tail),
			$elm$core$Array$shiftStep,
			$elm$core$Elm$JsArray$empty,
			builder.tail
		  );
		} else {
		  var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
		  var depth = $elm$core$Basics$floor(
			A2(
			  $elm$core$Basics$logBase,
			  $elm$core$Array$branchFactor,
			  treeLen - 1
			)
		  );
		  var correctNodeList = reverseNodeList
			? $elm$core$List$reverse(builder.nodeList)
			: builder.nodeList;
		  var tree = A2(
			$elm$core$Array$treeFromBuilder,
			correctNodeList,
			builder.nodeListSize
		  );
		  return A4(
			$elm$core$Array$Array_elm_builtin,
			$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
			A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
			tree,
			builder.tail
		  );
		}
	  });
	  var $elm$core$Basics$idiv = _Basics_idiv;
	  var $elm$core$Basics$lt = _Utils_lt;
	  var $elm$core$Array$initializeHelp = F5(function(
		fn,
		fromIndex,
		len,
		nodeList,
		tail
	  ) {
		initializeHelp: while (true) {
		  if (fromIndex < 0) {
			return A2($elm$core$Array$builderToArray, false, {
			  nodeList: nodeList,
			  nodeListSize: (len / $elm$core$Array$branchFactor) | 0,
			  tail: tail
			});
		  } else {
			var leaf = $elm$core$Array$Leaf(
			  A3(
				$elm$core$Elm$JsArray$initialize,
				$elm$core$Array$branchFactor,
				fromIndex,
				fn
			  )
			);
			var $temp$fn = fn,
			  $temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
			  $temp$len = len,
			  $temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
			  $temp$tail = tail;
			fn = $temp$fn;
			fromIndex = $temp$fromIndex;
			len = $temp$len;
			nodeList = $temp$nodeList;
			tail = $temp$tail;
			continue initializeHelp;
		  }
		}
	  });
	  var $elm$core$Basics$remainderBy = _Basics_remainderBy;
	  var $elm$core$Array$initialize = F2(function(len, fn) {
		if (len <= 0) {
		  return $elm$core$Array$empty;
		} else {
		  var tailLen = len % $elm$core$Array$branchFactor;
		  var tail = A3(
			$elm$core$Elm$JsArray$initialize,
			tailLen,
			len - tailLen,
			fn
		  );
		  var initialFromIndex =
			len - tailLen - $elm$core$Array$branchFactor;
		  return A5(
			$elm$core$Array$initializeHelp,
			fn,
			initialFromIndex,
			len,
			_List_Nil,
			tail
		  );
		}
	  });
	  var $elm$core$Basics$True = { $: "True" };
	  var $elm$core$Result$isOk = function(result) {
		if (result.$ === "Ok") {
		  return true;
		} else {
		  return false;
		}
	  };
	  var $elm$json$Json$Decode$map = _Json_map1;
	  var $elm$json$Json$Decode$map2 = _Json_map2;
	  var $elm$json$Json$Decode$succeed = _Json_succeed;
	  var $elm$virtual_dom$VirtualDom$toHandlerInt = function(handler) {
		switch (handler.$) {
		  case "Normal":
			return 0;
		  case "MayStopPropagation":
			return 1;
		  case "MayPreventDefault":
			return 2;
		  default:
			return 3;
		}
	  };
	  var $elm$browser$Browser$External = function(a) {
		return { $: "External", a: a };
	  };
	  var $elm$browser$Browser$Internal = function(a) {
		return { $: "Internal", a: a };
	  };
	  var $elm$core$Basics$identity = function(x) {
		return x;
	  };
	  var $elm$browser$Browser$Dom$NotFound = function(a) {
		return { $: "NotFound", a: a };
	  };
	  var $elm$url$Url$Http = { $: "Http" };
	  var $elm$url$Url$Https = { $: "Https" };
	  var $elm$url$Url$Url = F6(function(
		protocol,
		host,
		port_,
		path,
		query,
		fragment
	  ) {
		return {
		  fragment: fragment,
		  host: host,
		  path: path,
		  port_: port_,
		  protocol: protocol,
		  query: query
		};
	  });
	  var $elm$core$String$contains = _String_contains;
	  var $elm$core$String$length = _String_length;
	  var $elm$core$String$slice = _String_slice;
	  var $elm$core$String$dropLeft = F2(function(n, string) {
		return n < 1
		  ? string
		  : A3(
			  $elm$core$String$slice,
			  n,
			  $elm$core$String$length(string),
			  string
			);
	  });
	  var $elm$core$String$indexes = _String_indexes;
	  var $elm$core$String$isEmpty = function(string) {
		return string === "";
	  };
	  var $elm$core$String$left = F2(function(n, string) {
		return n < 1 ? "" : A3($elm$core$String$slice, 0, n, string);
	  });
	  var $elm$core$String$toInt = _String_toInt;
	  var $elm$url$Url$chompBeforePath = F5(function(
		protocol,
		path,
		params,
		frag,
		str
	  ) {
		if (
		  $elm$core$String$isEmpty(str) ||
		  A2($elm$core$String$contains, "@", str)
		) {
		  return $elm$core$Maybe$Nothing;
		} else {
		  var _v0 = A2($elm$core$String$indexes, ":", str);
		  if (!_v0.b) {
			return $elm$core$Maybe$Just(
			  A6(
				$elm$url$Url$Url,
				protocol,
				str,
				$elm$core$Maybe$Nothing,
				path,
				params,
				frag
			  )
			);
		  } else {
			if (!_v0.b.b) {
			  var i = _v0.a;
			  var _v1 = $elm$core$String$toInt(
				A2($elm$core$String$dropLeft, i + 1, str)
			  );
			  if (_v1.$ === "Nothing") {
				return $elm$core$Maybe$Nothing;
			  } else {
				var port_ = _v1;
				return $elm$core$Maybe$Just(
				  A6(
					$elm$url$Url$Url,
					protocol,
					A2($elm$core$String$left, i, str),
					port_,
					path,
					params,
					frag
				  )
				);
			  }
			} else {
			  return $elm$core$Maybe$Nothing;
			}
		  }
		}
	  });
	  var $elm$url$Url$chompBeforeQuery = F4(function(
		protocol,
		params,
		frag,
		str
	  ) {
		if ($elm$core$String$isEmpty(str)) {
		  return $elm$core$Maybe$Nothing;
		} else {
		  var _v0 = A2($elm$core$String$indexes, "/", str);
		  if (!_v0.b) {
			return A5(
			  $elm$url$Url$chompBeforePath,
			  protocol,
			  "/",
			  params,
			  frag,
			  str
			);
		  } else {
			var i = _v0.a;
			return A5(
			  $elm$url$Url$chompBeforePath,
			  protocol,
			  A2($elm$core$String$dropLeft, i, str),
			  params,
			  frag,
			  A2($elm$core$String$left, i, str)
			);
		  }
		}
	  });
	  var $elm$url$Url$chompBeforeFragment = F3(function(
		protocol,
		frag,
		str
	  ) {
		if ($elm$core$String$isEmpty(str)) {
		  return $elm$core$Maybe$Nothing;
		} else {
		  var _v0 = A2($elm$core$String$indexes, "?", str);
		  if (!_v0.b) {
			return A4(
			  $elm$url$Url$chompBeforeQuery,
			  protocol,
			  $elm$core$Maybe$Nothing,
			  frag,
			  str
			);
		  } else {
			var i = _v0.a;
			return A4(
			  $elm$url$Url$chompBeforeQuery,
			  protocol,
			  $elm$core$Maybe$Just(
				A2($elm$core$String$dropLeft, i + 1, str)
			  ),
			  frag,
			  A2($elm$core$String$left, i, str)
			);
		  }
		}
	  });
	  var $elm$url$Url$chompAfterProtocol = F2(function(protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
		  return $elm$core$Maybe$Nothing;
		} else {
		  var _v0 = A2($elm$core$String$indexes, "#", str);
		  if (!_v0.b) {
			return A3(
			  $elm$url$Url$chompBeforeFragment,
			  protocol,
			  $elm$core$Maybe$Nothing,
			  str
			);
		  } else {
			var i = _v0.a;
			return A3(
			  $elm$url$Url$chompBeforeFragment,
			  protocol,
			  $elm$core$Maybe$Just(
				A2($elm$core$String$dropLeft, i + 1, str)
			  ),
			  A2($elm$core$String$left, i, str)
			);
		  }
		}
	  });
	  var $elm$core$String$startsWith = _String_startsWith;
	  var $elm$url$Url$fromString = function(str) {
		return A2($elm$core$String$startsWith, "http://", str)
		  ? A2(
			  $elm$url$Url$chompAfterProtocol,
			  $elm$url$Url$Http,
			  A2($elm$core$String$dropLeft, 7, str)
			)
		  : A2($elm$core$String$startsWith, "https://", str)
		  ? A2(
			  $elm$url$Url$chompAfterProtocol,
			  $elm$url$Url$Https,
			  A2($elm$core$String$dropLeft, 8, str)
			)
		  : $elm$core$Maybe$Nothing;
	  };
	  var $elm$core$Basics$never = function(_v0) {
		never: while (true) {
		  var nvr = _v0.a;
		  var $temp$_v0 = nvr;
		  _v0 = $temp$_v0;
		  continue never;
		}
	  };
	  var $elm$core$Task$Perform = function(a) {
		return { $: "Perform", a: a };
	  };
	  var $elm$core$Task$succeed = _Scheduler_succeed;
	  var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
	  var $elm$core$List$foldrHelper = F4(function(fn, acc, ctr, ls) {
		if (!ls.b) {
		  return acc;
		} else {
		  var a = ls.a;
		  var r1 = ls.b;
		  if (!r1.b) {
			return A2(fn, a, acc);
		  } else {
			var b = r1.a;
			var r2 = r1.b;
			if (!r2.b) {
			  return A2(fn, a, A2(fn, b, acc));
			} else {
			  var c = r2.a;
			  var r3 = r2.b;
			  if (!r3.b) {
				return A2(fn, a, A2(fn, b, A2(fn, c, acc)));
			  } else {
				var d = r3.a;
				var r4 = r3.b;
				var res =
				  ctr > 500
					? A3(
						$elm$core$List$foldl,
						fn,
						acc,
						$elm$core$List$reverse(r4)
					  )
					: A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
				return A2(fn, a, A2(fn, b, A2(fn, c, A2(fn, d, res))));
			  }
			}
		  }
		}
	  });
	  var $elm$core$List$foldr = F3(function(fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	  });
	  var $elm$core$List$map = F2(function(f, xs) {
		return A3(
		  $elm$core$List$foldr,
		  F2(function(x, acc) {
			return A2($elm$core$List$cons, f(x), acc);
		  }),
		  _List_Nil,
		  xs
		);
	  });
	  var $elm$core$Task$andThen = _Scheduler_andThen;
	  var $elm$core$Task$map = F2(function(func, taskA) {
		return A2(
		  $elm$core$Task$andThen,
		  function(a) {
			return $elm$core$Task$succeed(func(a));
		  },
		  taskA
		);
	  });
	  var $elm$core$Task$map2 = F3(function(func, taskA, taskB) {
		return A2(
		  $elm$core$Task$andThen,
		  function(a) {
			return A2(
			  $elm$core$Task$andThen,
			  function(b) {
				return $elm$core$Task$succeed(A2(func, a, b));
			  },
			  taskB
			);
		  },
		  taskA
		);
	  });
	  var $elm$core$Task$sequence = function(tasks) {
		return A3(
		  $elm$core$List$foldr,
		  $elm$core$Task$map2($elm$core$List$cons),
		  $elm$core$Task$succeed(_List_Nil),
		  tasks
		);
	  };
	  var $elm$core$Platform$sendToApp = _Platform_sendToApp;
	  var $elm$core$Task$spawnCmd = F2(function(router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
		  A2(
			$elm$core$Task$andThen,
			$elm$core$Platform$sendToApp(router),
			task
		  )
		);
	  });
	  var $elm$core$Task$onEffects = F3(function(router, commands, state) {
		return A2(
		  $elm$core$Task$map,
		  function(_v0) {
			return _Utils_Tuple0;
		  },
		  $elm$core$Task$sequence(
			A2(
			  $elm$core$List$map,
			  $elm$core$Task$spawnCmd(router),
			  commands
			)
		  )
		);
	  });
	  var $elm$core$Task$onSelfMsg = F3(function(_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	  });
	  var $elm$core$Task$cmdMap = F2(function(tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(A2($elm$core$Task$map, tagger, task));
	  });
	  _Platform_effectManagers["Task"] = _Platform_createManager(
		$elm$core$Task$init,
		$elm$core$Task$onEffects,
		$elm$core$Task$onSelfMsg,
		$elm$core$Task$cmdMap
	  );
	  var $elm$core$Task$command = _Platform_leaf("Task");
	  var $elm$core$Task$perform = F2(function(toMessage, task) {
		return $elm$core$Task$command(
		  $elm$core$Task$Perform(A2($elm$core$Task$map, toMessage, task))
		);
	  });
	  var $elm$core$Platform$Cmd$batch = _Platform_batch;
	  var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(
		_List_Nil
	  );
	  var $elm$core$Platform$Sub$batch = _Platform_batch;
	  var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(
		_List_Nil
	  );
	  var $elm$browser$Browser$sandbox = function(impl) {
		return _Browser_element({
		  init: function(_v0) {
			return _Utils_Tuple2(impl.init, $elm$core$Platform$Cmd$none);
		  },
		  subscriptions: function(_v1) {
			return $elm$core$Platform$Sub$none;
		  },
		  update: F2(function(msg, model) {
			return _Utils_Tuple2(
			  A2(impl.update, msg, model),
			  $elm$core$Platform$Cmd$none
			);
		  }),
		  view: impl.view
		});
	  };
	  var $author$project$Main$update = F2(function(action, model) {
		var m = action.a;
		return m;
	  });
	  var $author$project$Main$Replace = function(a) {
		return { $: "Replace", a: a };
	  };
	  var $elm$html$Html$div = _VirtualDom_node("div");
	  var $elm$virtual_dom$VirtualDom$Normal = function(a) {
		return { $: "Normal", a: a };
	  };
	  var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
	  var $elm$html$Html$Events$on = F2(function(event, decoder) {
		return A2(
		  $elm$virtual_dom$VirtualDom$on,
		  event,
		  $elm$virtual_dom$VirtualDom$Normal(decoder)
		);
	  });
	  var $elm$json$Json$Decode$field = _Json_decodeField;
	  var $elm$json$Json$Decode$at = F2(function(fields, decoder) {
		return A3(
		  $elm$core$List$foldr,
		  $elm$json$Json$Decode$field,
		  decoder,
		  fields
		);
	  });
	  var $elm$json$Json$Decode$string = _Json_decodeString;
	  var $elm$html$Html$Events$targetValue = A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(["target", "value"]),
		$elm$json$Json$Decode$string
	  );
	  var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
	  var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
	  var $elm$html$Html$textarea = _VirtualDom_node("textarea");
	  var $author$project$Ast$BinOp$L = { $: "L" };
	  var $author$project$Ast$BinOp$R = { $: "R" };
	  var $elm$core$Dict$RBEmpty_elm_builtin = { $: "RBEmpty_elm_builtin" };
	  var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
	  var $elm$core$Dict$Black = { $: "Black" };
	  var $elm$core$Dict$RBNode_elm_builtin = F5(function(a, b, c, d, e) {
		return { $: "RBNode_elm_builtin", a: a, b: b, c: c, d: d, e: e };
	  });
	  var $elm$core$Dict$Red = { $: "Red" };
	  var $elm$core$Dict$balance = F5(function(
		color,
		key,
		value,
		left,
		right
	  ) {
		if (right.$ === "RBNode_elm_builtin" && right.a.$ === "Red") {
		  var _v1 = right.a;
		  var rK = right.b;
		  var rV = right.c;
		  var rLeft = right.d;
		  var rRight = right.e;
		  if (left.$ === "RBNode_elm_builtin" && left.a.$ === "Red") {
			var _v3 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
			  $elm$core$Dict$RBNode_elm_builtin,
			  $elm$core$Dict$Red,
			  key,
			  value,
			  A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Black,
				lK,
				lV,
				lLeft,
				lRight
			  ),
			  A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Black,
				rK,
				rV,
				rLeft,
				rRight
			  )
			);
		  } else {
			return A5(
			  $elm$core$Dict$RBNode_elm_builtin,
			  color,
			  rK,
			  rV,
			  A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				key,
				value,
				left,
				rLeft
			  ),
			  rRight
			);
		  }
		} else {
		  if (
			left.$ === "RBNode_elm_builtin" &&
			left.a.$ === "Red" &&
			left.d.$ === "RBNode_elm_builtin" &&
			left.d.a.$ === "Red"
		  ) {
			var _v5 = left.a;
			var lK = left.b;
			var lV = left.c;
			var _v6 = left.d;
			var _v7 = _v6.a;
			var llK = _v6.b;
			var llV = _v6.c;
			var llLeft = _v6.d;
			var llRight = _v6.e;
			var lRight = left.e;
			return A5(
			  $elm$core$Dict$RBNode_elm_builtin,
			  $elm$core$Dict$Red,
			  lK,
			  lV,
			  A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Black,
				llK,
				llV,
				llLeft,
				llRight
			  ),
			  A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Black,
				key,
				value,
				lRight,
				right
			  )
			);
		  } else {
			return A5(
			  $elm$core$Dict$RBNode_elm_builtin,
			  color,
			  key,
			  value,
			  left,
			  right
			);
		  }
		}
	  });
	  var $elm$core$Basics$compare = _Utils_compare;
	  var $elm$core$Dict$insertHelp = F3(function(key, value, dict) {
		if (dict.$ === "RBEmpty_elm_builtin") {
		  return A5(
			$elm$core$Dict$RBNode_elm_builtin,
			$elm$core$Dict$Red,
			key,
			value,
			$elm$core$Dict$RBEmpty_elm_builtin,
			$elm$core$Dict$RBEmpty_elm_builtin
		  );
		} else {
		  var nColor = dict.a;
		  var nKey = dict.b;
		  var nValue = dict.c;
		  var nLeft = dict.d;
		  var nRight = dict.e;
		  var _v1 = A2($elm$core$Basics$compare, key, nKey);
		  switch (_v1.$) {
			case "LT":
			  return A5(
				$elm$core$Dict$balance,
				nColor,
				nKey,
				nValue,
				A3($elm$core$Dict$insertHelp, key, value, nLeft),
				nRight
			  );
			case "EQ":
			  return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				nColor,
				nKey,
				value,
				nLeft,
				nRight
			  );
			default:
			  return A5(
				$elm$core$Dict$balance,
				nColor,
				nKey,
				nValue,
				nLeft,
				A3($elm$core$Dict$insertHelp, key, value, nRight)
			  );
		  }
		}
	  });
	  var $elm$core$Dict$insert = F3(function(key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if (_v0.$ === "RBNode_elm_builtin" && _v0.a.$ === "Red") {
		  var _v1 = _v0.a;
		  var k = _v0.b;
		  var v = _v0.c;
		  var l = _v0.d;
		  var r = _v0.e;
		  return A5(
			$elm$core$Dict$RBNode_elm_builtin,
			$elm$core$Dict$Black,
			k,
			v,
			l,
			r
		  );
		} else {
		  var x = _v0;
		  return x;
		}
	  });
	  var $author$project$Ast$BinOp$operators = A3(
		$elm$core$Dict$insert,
		"=",
		_Utils_Tuple2($author$project$Ast$BinOp$R, 0),
		A3(
		  $elm$core$Dict$insert,
		  "|>",
		  _Utils_Tuple2($author$project$Ast$BinOp$R, 1),
		  A3(
			$elm$core$Dict$insert,
			"<|",
			_Utils_Tuple2($author$project$Ast$BinOp$R, 1),
			A3(
			  $elm$core$Dict$insert,
			  ">>",
			  _Utils_Tuple2($author$project$Ast$BinOp$L, 9),
			  A3(
				$elm$core$Dict$insert,
				"<<",
				_Utils_Tuple2($author$project$Ast$BinOp$L, 9),
				A3(
				  $elm$core$Dict$insert,
				  "^",
				  _Utils_Tuple2($author$project$Ast$BinOp$L, 8),
				  A3(
					$elm$core$Dict$insert,
					"rem",
					_Utils_Tuple2($author$project$Ast$BinOp$L, 7),
					A3(
					  $elm$core$Dict$insert,
					  "//",
					  _Utils_Tuple2($author$project$Ast$BinOp$L, 7),
					  A3(
						$elm$core$Dict$insert,
						"%",
						_Utils_Tuple2($author$project$Ast$BinOp$L, 7),
						A3(
						  $elm$core$Dict$insert,
						  "/",
						  _Utils_Tuple2($author$project$Ast$BinOp$L, 7),
						  A3(
							$elm$core$Dict$insert,
							"*",
							_Utils_Tuple2($author$project$Ast$BinOp$L, 7),
							A3(
							  $elm$core$Dict$insert,
							  "-",
							  _Utils_Tuple2($author$project$Ast$BinOp$L, 6),
							  A3(
								$elm$core$Dict$insert,
								"+",
								_Utils_Tuple2(
								  $author$project$Ast$BinOp$L,
								  6
								),
								A3(
								  $elm$core$Dict$insert,
								  "::",
								  _Utils_Tuple2(
									$author$project$Ast$BinOp$R,
									5
								  ),
								  A3(
									$elm$core$Dict$insert,
									"++",
									_Utils_Tuple2(
									  $author$project$Ast$BinOp$R,
									  5
									),
									A3(
									  $elm$core$Dict$insert,
									  "<=",
									  _Utils_Tuple2(
										$author$project$Ast$BinOp$L,
										4
									  ),
									  A3(
										$elm$core$Dict$insert,
										">=",
										_Utils_Tuple2(
										  $author$project$Ast$BinOp$L,
										  4
										),
										A3(
										  $elm$core$Dict$insert,
										  ">",
										  _Utils_Tuple2(
											$author$project$Ast$BinOp$L,
											4
										  ),
										  A3(
											$elm$core$Dict$insert,
											"<",
											_Utils_Tuple2(
											  $author$project$Ast$BinOp$L,
											  4
											),
											A3(
											  $elm$core$Dict$insert,
											  "/=",
											  _Utils_Tuple2(
												$author$project$Ast$BinOp$L,
												4
											  ),
											  A3(
												$elm$core$Dict$insert,
												"==",
												_Utils_Tuple2(
												  $author$project$Ast$BinOp$L,
												  4
												),
												A3(
												  $elm$core$Dict$insert,
												  "&&",
												  _Utils_Tuple2(
													$author$project$Ast$BinOp$L,
													3
												  ),
												  A3(
													$elm$core$Dict$insert,
													"||",
													_Utils_Tuple2(
													  $author$project$Ast$BinOp$L,
													  2
													),
													$elm$core$Dict$empty
												  )
												)
											  )
											)
										  )
										)
									  )
									)
								  )
								)
							  )
							)
						  )
						)
					  )
					)
				  )
				)
			  )
			)
		  )
		)
	  );
	  var $andre_dietrich$parser_combinators$Combine$app = function(_v0) {
		var inner = _v0.a;
		return inner;
	  };
	  var $andre_dietrich$parser_combinators$Combine$InputStream = F3(
		function(data, input, position) {
		  return { data: data, input: input, position: position };
		}
	  );
	  var $andre_dietrich$parser_combinators$Combine$initStream = function(
		s
	  ) {
		return A3(
		  $andre_dietrich$parser_combinators$Combine$InputStream,
		  s,
		  s,
		  0
		);
	  };
	  var $andre_dietrich$parser_combinators$Combine$runParser = F3(
		function(p, st, s) {
		  var _v0 = A3(
			$andre_dietrich$parser_combinators$Combine$app,
			p,
			st,
			$andre_dietrich$parser_combinators$Combine$initStream(s)
		  );
		  if (_v0.c.$ === "Ok") {
			var state = _v0.a;
			var stream = _v0.b;
			var res = _v0.c.a;
			return $elm$core$Result$Ok(_Utils_Tuple3(state, stream, res));
		  } else {
			var state = _v0.a;
			var stream = _v0.b;
			var ms = _v0.c.a;
			return $elm$core$Result$Err(_Utils_Tuple3(state, stream, ms));
		  }
		}
	  );
	  var $andre_dietrich$parser_combinators$Combine$parse = function(p) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$runParser,
		  p,
		  _Utils_Tuple0
		);
	  };
	  var $andre_dietrich$parser_combinators$Combine$Parser = function(a) {
		return { $: "Parser", a: a };
	  };
	  var $andre_dietrich$parser_combinators$Combine$end = $andre_dietrich$parser_combinators$Combine$Parser(
		F2(function(state, stream) {
		  return stream.input === ""
			? _Utils_Tuple3(
				state,
				stream,
				$elm$core$Result$Ok(_Utils_Tuple0)
			  )
			: _Utils_Tuple3(
				state,
				stream,
				$elm$core$Result$Err(
				  _List_fromArray(["expected end of input"])
				)
			  );
		})
	  );
	  var $elm$core$Basics$always = F2(function(a, _v0) {
		return a;
	  });
	  var $andre_dietrich$parser_combinators$Combine$andThen = F2(function(
		f,
		p
	  ) {
		return $andre_dietrich$parser_combinators$Combine$Parser(
		  F2(function(state, stream) {
			var _v0 = A3(
			  $andre_dietrich$parser_combinators$Combine$app,
			  p,
			  state,
			  stream
			);
			if (_v0.c.$ === "Ok") {
			  var rstate = _v0.a;
			  var rstream = _v0.b;
			  var res = _v0.c.a;
			  return A3(
				$andre_dietrich$parser_combinators$Combine$app,
				f(res),
				rstate,
				rstream
			  );
			} else {
			  var estate = _v0.a;
			  var estream = _v0.b;
			  var ms = _v0.c.a;
			  return _Utils_Tuple3(
				estate,
				estream,
				$elm$core$Result$Err(ms)
			  );
			}
		  })
		);
	  });
	  var $pilatch$flip$Flip$flip = F3(function(_function, argB, argA) {
		return A2(_function, argA, argB);
	  });
	  var $andre_dietrich$parser_combinators$Combine$bimap = F3(function(
		fok,
		ferr,
		p
	  ) {
		return $andre_dietrich$parser_combinators$Combine$Parser(
		  F2(function(state, stream) {
			var _v0 = A3(
			  $andre_dietrich$parser_combinators$Combine$app,
			  p,
			  state,
			  stream
			);
			if (_v0.c.$ === "Ok") {
			  var rstate = _v0.a;
			  var rstream = _v0.b;
			  var res = _v0.c.a;
			  return _Utils_Tuple3(
				rstate,
				rstream,
				$elm$core$Result$Ok(fok(res))
			  );
			} else {
			  var estate = _v0.a;
			  var estream = _v0.b;
			  var ms = _v0.c.a;
			  return _Utils_Tuple3(
				estate,
				estream,
				$elm$core$Result$Err(ferr(ms))
			  );
			}
		  })
		);
	  });
	  var $andre_dietrich$parser_combinators$Combine$map = F2(function(
		f,
		p
	  ) {
		return A3(
		  $andre_dietrich$parser_combinators$Combine$bimap,
		  f,
		  $elm$core$Basics$identity,
		  p
		);
	  });
	  var $andre_dietrich$parser_combinators$Combine$andMap = F2(function(
		rp,
		lp
	  ) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$andThen,
		  A2(
			$pilatch$flip$Flip$flip,
			$andre_dietrich$parser_combinators$Combine$map,
			rp
		  ),
		  lp
		);
	  });
	  var $andre_dietrich$parser_combinators$Combine$ignore = F2(function(
		p1,
		p2
	  ) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  p1,
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$elm$core$Basics$always,
			p2
		  )
		);
	  });
	  var $andre_dietrich$parser_combinators$Combine$keep = F2(function(
		p1,
		p2
	  ) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  p1,
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$pilatch$flip$Flip$flip($elm$core$Basics$always),
			p2
		  )
		);
	  });
	  var $andre_dietrich$parser_combinators$Combine$manyTill = F2(function(
		p,
		end_
	  ) {
		var accumulate = F3(function(acc, state, stream) {
		  accumulate: while (true) {
			var _v0 = A3(
			  $andre_dietrich$parser_combinators$Combine$app,
			  end_,
			  state,
			  stream
			);
			if (_v0.c.$ === "Ok") {
			  var rstate = _v0.a;
			  var rstream = _v0.b;
			  return _Utils_Tuple3(
				rstate,
				rstream,
				$elm$core$Result$Ok($elm$core$List$reverse(acc))
			  );
			} else {
			  var estate = _v0.a;
			  var estream = _v0.b;
			  var ms = _v0.c.a;
			  var _v1 = A3(
				$andre_dietrich$parser_combinators$Combine$app,
				p,
				state,
				stream
			  );
			  if (_v1.c.$ === "Ok") {
				var rstate = _v1.a;
				var rstream = _v1.b;
				var res = _v1.c.a;
				var $temp$acc = A2($elm$core$List$cons, res, acc),
				  $temp$state = rstate,
				  $temp$stream = rstream;
				acc = $temp$acc;
				state = $temp$state;
				stream = $temp$stream;
				continue accumulate;
			  } else {
				return _Utils_Tuple3(
				  estate,
				  estream,
				  $elm$core$Result$Err(ms)
				);
			  }
			}
		  }
		});
		return $andre_dietrich$parser_combinators$Combine$Parser(
		  accumulate(_List_Nil)
		);
	  });
	  var $andre_dietrich$parser_combinators$Combine$or = F2(function(
		lp,
		rp
	  ) {
		return $andre_dietrich$parser_combinators$Combine$Parser(
		  F2(function(state, stream) {
			var _v0 = A3(
			  $andre_dietrich$parser_combinators$Combine$app,
			  lp,
			  state,
			  stream
			);
			if (_v0.c.$ === "Ok") {
			  var res = _v0;
			  return res;
			} else {
			  var lms = _v0.c.a;
			  var _v1 = A3(
				$andre_dietrich$parser_combinators$Combine$app,
				rp,
				state,
				stream
			  );
			  if (_v1.c.$ === "Ok") {
				var res = _v1;
				return res;
			  } else {
				var rms = _v1.c.a;
				return _Utils_Tuple3(
				  state,
				  stream,
				  $elm$core$Result$Err(_Utils_ap(lms, rms))
				);
			  }
			}
		  })
		);
	  });
	  var $elm$core$Basics$composeR = F3(function(f, g, x) {
		return g(f(x));
	  });
	  var $elm$regex$Regex$Match = F4(function(
		match,
		index,
		number,
		submatches
	  ) {
		return {
		  index: index,
		  match: match,
		  number: number,
		  submatches: submatches
		};
	  });
	  var $elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
	  var $elm$regex$Regex$fromString = function(string) {
		return A2(
		  $elm$regex$Regex$fromStringWith,
		  { caseInsensitive: false, multiline: false },
		  string
		);
	  };
	  var $elm$regex$Regex$findAtMost = _Regex_findAtMost;
	  var $elm$regex$Regex$never = _Regex_never;
	  var $elm$core$Maybe$withDefault = F2(function(_default, maybe) {
		if (maybe.$ === "Just") {
		  var value = maybe.a;
		  return value;
		} else {
		  return _default;
		}
	  });
	  var $andre_dietrich$parser_combinators$Combine$regexer = F5(function(
		input,
		output,
		pat,
		state,
		stream
	  ) {
		var pattern = A2($elm$core$String$startsWith, "^", pat)
		  ? pat
		  : "^" + pat;
		var _v0 = A3(
		  $elm$regex$Regex$findAtMost,
		  1,
		  A2(
			$elm$core$Maybe$withDefault,
			$elm$regex$Regex$never,
			input(pattern)
		  ),
		  stream.input
		);
		if (_v0.b && !_v0.b.b) {
		  var match = _v0.a;
		  var len = $elm$core$String$length(match.match);
		  var pos = stream.position + len;
		  var rem = A2($elm$core$String$dropLeft, len, stream.input);
		  return _Utils_Tuple3(
			state,
			_Utils_update(stream, { input: rem, position: pos }),
			$elm$core$Result$Ok(output(match))
		  );
		} else {
		  return _Utils_Tuple3(
			state,
			stream,
			$elm$core$Result$Err(
			  _List_fromArray([
				"expected input matching Regexp /" + (pattern + "/")
			  ])
			)
		  );
		}
	  });
	  var $andre_dietrich$parser_combinators$Combine$regex = A2(
		$elm$core$Basics$composeR,
		A2(
		  $andre_dietrich$parser_combinators$Combine$regexer,
		  $elm$regex$Regex$fromString,
		  function($) {
			return $.match;
		  }
		),
		$andre_dietrich$parser_combinators$Combine$Parser
	  );
	  var $author$project$Ast$Helpers$spaces = $andre_dietrich$parser_combinators$Combine$regex(
		"[ \\t]*"
	  );
	  var $andre_dietrich$parser_combinators$Combine$emptyErr = $andre_dietrich$parser_combinators$Combine$Parser(
		F2(function(state, stream) {
		  return _Utils_Tuple3(
			state,
			stream,
			$elm$core$Result$Err(_List_Nil)
		  );
		})
	  );
	  var $andre_dietrich$parser_combinators$Combine$choice = function(xs) {
		return A3(
		  $elm$core$List$foldr,
		  $andre_dietrich$parser_combinators$Combine$or,
		  $andre_dietrich$parser_combinators$Combine$emptyErr,
		  xs
		);
	  };
	  var $author$project$Ast$Statement$Comment = function(a) {
		return { $: "Comment", a: a };
	  };
	  var $andre_dietrich$parser_combinators$Combine$mapError = $andre_dietrich$parser_combinators$Combine$bimap(
		$elm$core$Basics$identity
	  );
	  var $andre_dietrich$parser_combinators$Combine$onerror = F2(function(
		m,
		p
	  ) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$mapError,
		  $elm$core$Basics$always(_List_fromArray([m])),
		  p
		);
	  });
	  var $andre_dietrich$parser_combinators$Combine$primitive = $andre_dietrich$parser_combinators$Combine$Parser;
	  var $andre_dietrich$parser_combinators$Combine$Char$satisfy = function(
		pred
	  ) {
		return $andre_dietrich$parser_combinators$Combine$primitive(
		  F2(function(state, stream) {
			var message = "could not satisfy predicate";
			var _v0 = $elm$core$String$uncons(stream.input);
			if (_v0.$ === "Just") {
			  var _v1 = _v0.a;
			  var h = _v1.a;
			  var rest = _v1.b;
			  return pred(h)
				? _Utils_Tuple3(
					state,
					_Utils_update(stream, {
					  input: rest,
					  position: stream.position + 1
					}),
					$elm$core$Result$Ok(h)
				  )
				: _Utils_Tuple3(
					state,
					stream,
					$elm$core$Result$Err(_List_fromArray([message]))
				  );
			} else {
			  return _Utils_Tuple3(
				state,
				stream,
				$elm$core$Result$Err(_List_fromArray([message]))
			  );
			}
		  })
		);
	  };
	  var $andre_dietrich$parser_combinators$Combine$Char$anyChar = A2(
		$andre_dietrich$parser_combinators$Combine$onerror,
		"expected any character",
		$andre_dietrich$parser_combinators$Combine$Char$satisfy(
		  $elm$core$Basics$always(true)
		)
	  );
	  var $elm$core$Basics$composeL = F3(function(g, f, x) {
		return g(f(x));
	  });
	  var $elm$core$String$fromList = _String_fromList;
	  var $andre_dietrich$parser_combinators$Combine$string = function(s) {
		return $andre_dietrich$parser_combinators$Combine$Parser(
		  F2(function(state, stream) {
			if (A2($elm$core$String$startsWith, s, stream.input)) {
			  var len = $elm$core$String$length(s);
			  var pos = stream.position + len;
			  var rem = A2($elm$core$String$dropLeft, len, stream.input);
			  return _Utils_Tuple3(
				state,
				_Utils_update(stream, { input: rem, position: pos }),
				$elm$core$Result$Ok(s)
			  );
			} else {
			  return _Utils_Tuple3(
				state,
				stream,
				$elm$core$Result$Err(
				  _List_fromArray(['expected "' + (s + '"')])
				)
			  );
			}
		  })
		);
	  };
	  var $author$project$Ast$Common$addMeta = F3(function(l, c, e) {
		return _Utils_Tuple2(e, { column: c, line: l });
	  });
	  var $andre_dietrich$parser_combinators$Combine$ParseLocation = F3(
		function(source, line, column) {
		  return { column: column, line: line, source: source };
		}
	  );
	  var $andre_dietrich$parser_combinators$Combine$currentLocation = function(
		stream
	  ) {
		var find = F3(function(position, currentLine_, lines) {
		  find: while (true) {
			if (!lines.b) {
			  return A3(
				$andre_dietrich$parser_combinators$Combine$ParseLocation,
				"",
				currentLine_,
				position
			  );
			} else {
			  var line = lines.a;
			  var rest = lines.b;
			  var length = $elm$core$String$length(line);
			  var lengthPlusNL = length + 1;
			  if (_Utils_eq(position, length)) {
				return A3(
				  $andre_dietrich$parser_combinators$Combine$ParseLocation,
				  line,
				  currentLine_,
				  position
				);
			  } else {
				if (_Utils_cmp(position, length) > 0) {
				  var $temp$position = position - lengthPlusNL,
					$temp$currentLine_ = currentLine_ + 1,
					$temp$lines = rest;
				  position = $temp$position;
				  currentLine_ = $temp$currentLine_;
				  lines = $temp$lines;
				  continue find;
				} else {
				  return A3(
					$andre_dietrich$parser_combinators$Combine$ParseLocation,
					line,
					currentLine_,
					position
				  );
				}
			  }
			}
		  }
		});
		return A3(
		  find,
		  stream.position,
		  0,
		  A2($elm$core$String$split, "\n", stream.data)
		);
	  };
	  var $andre_dietrich$parser_combinators$Combine$withLocation = function(
		f
	  ) {
		return $andre_dietrich$parser_combinators$Combine$Parser(
		  F2(function(state, stream) {
			return A3(
			  $andre_dietrich$parser_combinators$Combine$app,
			  f(
				$andre_dietrich$parser_combinators$Combine$currentLocation(
				  stream
				)
			  ),
			  state,
			  stream
			);
		  })
		);
	  };
	  var $author$project$Ast$Common$withMeta = function(p) {
		return $andre_dietrich$parser_combinators$Combine$withLocation(
		  function(a) {
			return A2(
			  $andre_dietrich$parser_combinators$Combine$map,
			  function(x) {
				return A3(
				  $author$project$Ast$Common$addMeta,
				  a.line,
				  a.column,
				  x
				);
			  },
			  p
			);
		  }
		);
	  };
	  var $author$project$Ast$Statement$multiLineComment = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$map,
		  A2(
			$elm$core$Basics$composeL,
			$author$project$Ast$Statement$Comment,
			$elm$core$String$fromList
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$keep,
			A2(
			  $andre_dietrich$parser_combinators$Combine$manyTill,
			  $andre_dietrich$parser_combinators$Combine$Char$anyChar,
			  $andre_dietrich$parser_combinators$Combine$string("-}")
			),
			$andre_dietrich$parser_combinators$Combine$string("{-")
		  )
		)
	  );
	  var $andre_dietrich$parser_combinators$Combine$whitespace = A2(
		$andre_dietrich$parser_combinators$Combine$onerror,
		"optional whitespace",
		$andre_dietrich$parser_combinators$Combine$regex("\\s*")
	  );
	  var $author$project$Ast$Statement$singleLineComment = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$map,
		  $author$project$Ast$Statement$Comment,
		  A2(
			$andre_dietrich$parser_combinators$Combine$ignore,
			$andre_dietrich$parser_combinators$Combine$whitespace,
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $andre_dietrich$parser_combinators$Combine$regex(".*"),
			  $andre_dietrich$parser_combinators$Combine$string("--")
			)
		  )
		)
	  );
	  var $author$project$Ast$Statement$comment = A2(
		$andre_dietrich$parser_combinators$Combine$or,
		$author$project$Ast$Statement$singleLineComment,
		$author$project$Ast$Statement$multiLineComment
	  );
	  var $author$project$Ast$Statement$EffectModuleDeclaration = F3(
		function(a, b, c) {
		  return { $: "EffectModuleDeclaration", a: a, b: b, c: c };
		}
	  );
	  var $andre_dietrich$parser_combinators$Combine$between = F3(function(
		lp,
		rp,
		p
	  ) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$ignore,
		  rp,
		  A2($andre_dietrich$parser_combinators$Combine$keep, p, lp)
		);
	  });
	  var $andre_dietrich$parser_combinators$Combine$braces = A2(
		$andre_dietrich$parser_combinators$Combine$between,
		$andre_dietrich$parser_combinators$Combine$string("{"),
		$andre_dietrich$parser_combinators$Combine$string("}")
	  );
	  var $author$project$Ast$Helpers$between_ = function(p) {
		return A2($andre_dietrich$parser_combinators$Combine$between, p, p);
	  };
	  var $andre_dietrich$parser_combinators$Combine$many = function(p) {
		var accumulate = F3(function(acc, state, stream) {
		  accumulate: while (true) {
			var _v0 = A3(
			  $andre_dietrich$parser_combinators$Combine$app,
			  p,
			  state,
			  stream
			);
			if (_v0.c.$ === "Ok") {
			  var rstate = _v0.a;
			  var rstream = _v0.b;
			  var res = _v0.c.a;
			  if (_Utils_eq(stream, rstream)) {
				return _Utils_Tuple3(
				  rstate,
				  rstream,
				  $elm$core$List$reverse(acc)
				);
			  } else {
				var $temp$acc = A2($elm$core$List$cons, res, acc),
				  $temp$state = rstate,
				  $temp$stream = rstream;
				acc = $temp$acc;
				state = $temp$state;
				stream = $temp$stream;
				continue accumulate;
			  }
			} else {
			  return _Utils_Tuple3(
				state,
				stream,
				$elm$core$List$reverse(acc)
			  );
			}
		  }
		});
		return $andre_dietrich$parser_combinators$Combine$Parser(
		  F2(function(state, stream) {
			var _v1 = A3(accumulate, _List_Nil, state, stream);
			var rstate = _v1.a;
			var rstream = _v1.b;
			var res = _v1.c;
			return _Utils_Tuple3(rstate, rstream, $elm$core$Result$Ok(res));
		  })
		);
	  };
	  var $andre_dietrich$parser_combinators$Combine$sepBy1 = F2(function(
		sep,
		p
	  ) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  $andre_dietrich$parser_combinators$Combine$many(
			A2($andre_dietrich$parser_combinators$Combine$keep, p, sep)
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$elm$core$List$cons,
			p
		  )
		);
	  });
	  var $author$project$Ast$Helpers$commaSeparated = function(p) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$sepBy1,
		  $andre_dietrich$parser_combinators$Combine$string(","),
		  A2(
			$author$project$Ast$Helpers$between_,
			$andre_dietrich$parser_combinators$Combine$whitespace,
			p
		  )
		);
	  };
	  var $author$project$Ast$Statement$AllExport = { $: "AllExport" };
	  var $andre_dietrich$parser_combinators$Combine$onsuccess = function(
		res
	  ) {
		return $andre_dietrich$parser_combinators$Combine$map(
		  $elm$core$Basics$always(res)
		);
	  };
	  var $author$project$Ast$Helpers$symbol = function(k) {
		return A2(
		  $author$project$Ast$Helpers$between_,
		  $andre_dietrich$parser_combinators$Combine$whitespace,
		  $andre_dietrich$parser_combinators$Combine$string(k)
		);
	  };
	  var $author$project$Ast$Statement$allExport = A2(
		$andre_dietrich$parser_combinators$Combine$onsuccess,
		$author$project$Ast$Statement$AllExport,
		$author$project$Ast$Helpers$symbol("..")
	  );
	  var $andre_dietrich$parser_combinators$Combine$parens = A2(
		$andre_dietrich$parser_combinators$Combine$between,
		$andre_dietrich$parser_combinators$Combine$string("("),
		$andre_dietrich$parser_combinators$Combine$string(")")
	  );
	  var $author$project$Ast$Statement$SubsetExport = function(a) {
		return { $: "SubsetExport", a: a };
	  };
	  var $author$project$Ast$Statement$FunctionExport = function(a) {
		return { $: "FunctionExport", a: a };
	  };
	  var $andre_dietrich$parser_combinators$Combine$fail = function(m) {
		return $andre_dietrich$parser_combinators$Combine$Parser(
		  F2(function(state, stream) {
			return _Utils_Tuple3(
			  state,
			  stream,
			  $elm$core$Result$Err(_List_fromArray([m]))
			);
		  })
		);
	  };
	  var $andre_dietrich$parser_combinators$Combine$Char$lower = A2(
		$andre_dietrich$parser_combinators$Combine$onerror,
		"expected a lowercase character",
		$andre_dietrich$parser_combinators$Combine$Char$satisfy(
		  $elm$core$Char$isLower
		)
	  );
	  var $elm$core$List$any = F2(function(isOkay, list) {
		any: while (true) {
		  if (!list.b) {
			return false;
		  } else {
			var x = list.a;
			var xs = list.b;
			if (isOkay(x)) {
			  return true;
			} else {
			  var $temp$isOkay = isOkay,
				$temp$list = xs;
			  isOkay = $temp$isOkay;
			  list = $temp$list;
			  continue any;
			}
		  }
		}
	  });
	  var $elm$core$List$member = F2(function(x, xs) {
		return A2(
		  $elm$core$List$any,
		  function(a) {
			return _Utils_eq(a, x);
		  },
		  xs
		);
	  });
	  var $elm$core$String$cons = _String_cons;
	  var $author$project$Ast$Helpers$name = function(p) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  $andre_dietrich$parser_combinators$Combine$regex(
			"[a-zA-Z0-9-_]*"
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$elm$core$String$cons,
			p
		  )
		);
	  };
	  var $author$project$Ast$Helpers$reserved = _List_fromArray([
		"module",
		"where",
		"import",
		"as",
		"exposing",
		"type",
		"alias",
		"port",
		"if",
		"then",
		"else",
		"let",
		"in",
		"case",
		"of"
	  ]);
	  var $andre_dietrich$parser_combinators$Combine$succeed = function(
		res
	  ) {
		return $andre_dietrich$parser_combinators$Combine$Parser(
		  F2(function(state, stream) {
			return _Utils_Tuple3(state, stream, $elm$core$Result$Ok(res));
		  })
		);
	  };
	  var $author$project$Ast$Helpers$varName = A2(
		$andre_dietrich$parser_combinators$Combine$andThen,
		function(n) {
		  return A2(
			$elm$core$List$member,
			n,
			$author$project$Ast$Helpers$reserved
		  )
			? $andre_dietrich$parser_combinators$Combine$fail(
				"name '" + (n + "' is reserved")
			  )
			: $andre_dietrich$parser_combinators$Combine$succeed(n);
		},
		$author$project$Ast$Helpers$name(
		  $andre_dietrich$parser_combinators$Combine$Char$lower
		)
	  );
	  var $author$project$Ast$Helpers$wild = $andre_dietrich$parser_combinators$Combine$string(
		"_"
	  );
	  var $author$project$Ast$Helpers$loName = A2(
		$andre_dietrich$parser_combinators$Combine$or,
		$author$project$Ast$Helpers$wild,
		$author$project$Ast$Helpers$varName
	  );
	  var $author$project$Ast$Helpers$functionName = $author$project$Ast$Helpers$loName;
	  var $andre_dietrich$parser_combinators$Combine$lazy = function(t) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$andThen,
		  t,
		  $andre_dietrich$parser_combinators$Combine$succeed(_Utils_Tuple0)
		);
	  };
	  var $author$project$Ast$Helpers$reservedOperators = _List_fromArray([
		"=",
		".",
		"..",
		"->",
		"--",
		"|",
		":"
	  ]);
	  var $author$project$Ast$Helpers$operator = $andre_dietrich$parser_combinators$Combine$lazy(
		function(_v0) {
		  return A2(
			$andre_dietrich$parser_combinators$Combine$andThen,
			function(n) {
			  return A2(
				$elm$core$List$member,
				n,
				$author$project$Ast$Helpers$reservedOperators
			  )
				? $andre_dietrich$parser_combinators$Combine$fail(
					"operator '" + (n + "' is reserved")
				  )
				: $andre_dietrich$parser_combinators$Combine$succeed(n);
			},
			$andre_dietrich$parser_combinators$Combine$regex(
			  "[+\\-\\/*=.$<>:&|^?%#@~!]+|\u008As\u0008"
			)
		  );
		}
	  );
	  var $author$project$Ast$Statement$functionExport = A2(
		$andre_dietrich$parser_combinators$Combine$map,
		$author$project$Ast$Statement$FunctionExport,
		$andre_dietrich$parser_combinators$Combine$choice(
		  _List_fromArray([
			$author$project$Ast$Helpers$functionName,
			$andre_dietrich$parser_combinators$Combine$parens(
			  $author$project$Ast$Helpers$operator
			)
		  ])
		)
	  );
	  var $author$project$Ast$Statement$TypeExport = F2(function(a, b) {
		return { $: "TypeExport", a: a, b: b };
	  });
	  var $andre_dietrich$parser_combinators$Combine$Char$upper = A2(
		$andre_dietrich$parser_combinators$Combine$onerror,
		"expected an uppercase character",
		$andre_dietrich$parser_combinators$Combine$Char$satisfy(
		  $elm$core$Char$isUpper
		)
	  );
	  var $author$project$Ast$Helpers$upName = $author$project$Ast$Helpers$name(
		$andre_dietrich$parser_combinators$Combine$Char$upper
	  );
	  var $author$project$Ast$Statement$constructorSubsetExports = A2(
		$andre_dietrich$parser_combinators$Combine$map,
		$author$project$Ast$Statement$SubsetExport,
		$author$project$Ast$Helpers$commaSeparated(
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$author$project$Ast$Statement$FunctionExport,
			$author$project$Ast$Helpers$upName
		  )
		)
	  );
	  var $andre_dietrich$parser_combinators$Combine$maybe = function(p) {
		return $andre_dietrich$parser_combinators$Combine$Parser(
		  F2(function(state, stream) {
			var _v0 = A3(
			  $andre_dietrich$parser_combinators$Combine$app,
			  p,
			  state,
			  stream
			);
			if (_v0.c.$ === "Ok") {
			  var rstate = _v0.a;
			  var rstream = _v0.b;
			  var res = _v0.c.a;
			  return _Utils_Tuple3(
				rstate,
				rstream,
				$elm$core$Result$Ok($elm$core$Maybe$Just(res))
			  );
			} else {
			  return _Utils_Tuple3(
				state,
				stream,
				$elm$core$Result$Ok($elm$core$Maybe$Nothing)
			  );
			}
		  })
		);
	  };
	  var $author$project$Ast$Statement$constructorExports = $andre_dietrich$parser_combinators$Combine$maybe(
		$andre_dietrich$parser_combinators$Combine$parens(
		  $andre_dietrich$parser_combinators$Combine$choice(
			_List_fromArray([
			  $author$project$Ast$Statement$allExport,
			  $author$project$Ast$Statement$constructorSubsetExports
			])
		  )
		)
	  );
	  var $author$project$Ast$Statement$typeExport = A2(
		$andre_dietrich$parser_combinators$Combine$andMap,
		$author$project$Ast$Statement$constructorExports,
		A2(
		  $andre_dietrich$parser_combinators$Combine$map,
		  $author$project$Ast$Statement$TypeExport,
		  A2(
			$andre_dietrich$parser_combinators$Combine$ignore,
			$author$project$Ast$Helpers$spaces,
			$author$project$Ast$Helpers$upName
		  )
		)
	  );
	  var $author$project$Ast$Statement$subsetExport = A2(
		$andre_dietrich$parser_combinators$Combine$map,
		$author$project$Ast$Statement$SubsetExport,
		$author$project$Ast$Helpers$commaSeparated(
		  A2(
			$andre_dietrich$parser_combinators$Combine$or,
			$author$project$Ast$Statement$typeExport,
			$author$project$Ast$Statement$functionExport
		  )
		)
	  );
	  var $author$project$Ast$Statement$exports = $andre_dietrich$parser_combinators$Combine$parens(
		$andre_dietrich$parser_combinators$Combine$choice(
		  _List_fromArray([
			$author$project$Ast$Statement$allExport,
			$author$project$Ast$Statement$subsetExport
		  ])
		)
	  );
	  var $author$project$Ast$Helpers$spaces_ = $andre_dietrich$parser_combinators$Combine$regex(
		"[ \\t]+"
	  );
	  var $author$project$Ast$Helpers$initialSymbol = function(k) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$ignore,
		  $author$project$Ast$Helpers$spaces_,
		  $andre_dietrich$parser_combinators$Combine$string(k)
		);
	  };
	  var $author$project$Ast$Helpers$moduleName = A2(
		$author$project$Ast$Helpers$between_,
		$author$project$Ast$Helpers$spaces,
		A2(
		  $andre_dietrich$parser_combinators$Combine$sepBy1,
		  $andre_dietrich$parser_combinators$Combine$string("."),
		  $author$project$Ast$Helpers$upName
		)
	  );
	  var $elm$core$Tuple$pair = F2(function(a, b) {
		return _Utils_Tuple2(a, b);
	  });
	  var $author$project$Ast$Statement$effectModuleDeclaration = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  A2(
			$andre_dietrich$parser_combinators$Combine$keep,
			$author$project$Ast$Statement$exports,
			$author$project$Ast$Helpers$symbol("exposing")
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$andMap,
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $andre_dietrich$parser_combinators$Combine$braces(
				$author$project$Ast$Helpers$commaSeparated(
				  A2(
					$andre_dietrich$parser_combinators$Combine$andMap,
					A2(
					  $andre_dietrich$parser_combinators$Combine$keep,
					  $author$project$Ast$Helpers$upName,
					  $author$project$Ast$Helpers$symbol("=")
					),
					A2(
					  $andre_dietrich$parser_combinators$Combine$map,
					  $elm$core$Tuple$pair,
					  $author$project$Ast$Helpers$loName
					)
				  )
				)
			  ),
			  $author$project$Ast$Helpers$symbol("where")
			),
			A2(
			  $andre_dietrich$parser_combinators$Combine$map,
			  $author$project$Ast$Statement$EffectModuleDeclaration,
			  A2(
				$andre_dietrich$parser_combinators$Combine$keep,
				$author$project$Ast$Helpers$moduleName,
				A2(
				  $andre_dietrich$parser_combinators$Combine$keep,
				  $author$project$Ast$Helpers$symbol("module"),
				  $author$project$Ast$Helpers$initialSymbol("effect")
				)
			  )
			)
		  )
		)
	  );
	  var $author$project$Ast$Statement$FunctionDeclaration = F2(function(
		a,
		b
	  ) {
		return { $: "FunctionDeclaration", a: a, b: b };
	  });
	  var $author$project$Ast$Expression$Application = F2(function(a, b) {
		return { $: "Application", a: a, b: b };
	  });
	  var $author$project$Ast$Expression$Case = F2(function(a, b) {
		return { $: "Case", a: a, b: b };
	  });
	  var $author$project$Ast$Expression$Cont = function(a) {
		return { $: "Cont", a: a };
	  };
	  var $author$project$Ast$Expression$If = F3(function(a, b, c) {
		return { $: "If", a: a, b: b, c: c };
	  });
	  var $author$project$Ast$Expression$Lambda = F2(function(a, b) {
		return { $: "Lambda", a: a, b: b };
	  });
	  var $author$project$Ast$Expression$Let = F2(function(a, b) {
		return { $: "Let", a: a, b: b };
	  });
	  var $author$project$Ast$Expression$List = function(a) {
		return { $: "List", a: a };
	  };
	  var $author$project$Ast$Expression$Record = function(a) {
		return { $: "Record", a: a };
	  };
	  var $author$project$Ast$Expression$RecordUpdate = F2(function(a, b) {
		return { $: "RecordUpdate", a: a, b: b };
	  });
	  var $author$project$Ast$Expression$Stop = function(a) {
		return { $: "Stop", a: a };
	  };
	  var $author$project$Ast$Expression$Tuple = function(a) {
		return { $: "Tuple", a: a };
	  };
	  var $author$project$Ast$Expression$Access = F2(function(a, b) {
		return { $: "Access", a: a, b: b };
	  });
	  var $andre_dietrich$parser_combinators$Combine$many1 = function(p) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  $andre_dietrich$parser_combinators$Combine$many(p),
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$elm$core$List$cons,
			p
		  )
		);
	  };
	  var $author$project$Ast$Expression$Variable = function(a) {
		return { $: "Variable", a: a };
	  };
	  var $author$project$Ast$Helpers$emptyTuple = $andre_dietrich$parser_combinators$Combine$string(
		"()"
	  );
	  var $author$project$Ast$Expression$variable = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$map,
		  $author$project$Ast$Expression$Variable,
		  $andre_dietrich$parser_combinators$Combine$choice(
			_List_fromArray([
			  $author$project$Ast$Helpers$loName,
			  $andre_dietrich$parser_combinators$Combine$parens(
				$author$project$Ast$Helpers$operator
			  ),
			  $andre_dietrich$parser_combinators$Combine$parens(
				$andre_dietrich$parser_combinators$Combine$regex(",+")
			  ),
			  $author$project$Ast$Helpers$emptyTuple
			])
		  )
		)
	  );
	  var $author$project$Ast$Expression$access = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  $andre_dietrich$parser_combinators$Combine$many1(
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $author$project$Ast$Common$withMeta(
				$author$project$Ast$Helpers$varName
			  ),
			  $andre_dietrich$parser_combinators$Combine$string(".")
			)
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$author$project$Ast$Expression$Access,
			$author$project$Ast$Expression$variable
		  )
		)
	  );
	  var $author$project$Ast$Expression$AccessFunction = function(a) {
		return { $: "AccessFunction", a: a };
	  };
	  var $author$project$Ast$Expression$accessFunction = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$map,
		  $author$project$Ast$Expression$AccessFunction,
		  A2(
			$andre_dietrich$parser_combinators$Combine$keep,
			$author$project$Ast$Helpers$loName,
			$andre_dietrich$parser_combinators$Combine$string(".")
		  )
		)
	  );
	  var $author$project$Ast$Pattern$applicationToList = function(
		application
	  ) {
		if (application.a.$ === "PApplication") {
		  var _v1 = application.a;
		  var left = _v1.a;
		  var right = _v1.b;
		  return _Utils_ap(
			$author$project$Ast$Pattern$applicationToList(left),
			_List_fromArray([right])
		  );
		} else {
		  var other = application;
		  return _List_fromArray([other]);
		}
	  };
	  var $andre_dietrich$parser_combinators$Combine$chainl = F2(function(
		op,
		p
	  ) {
		var accumulate = function(x) {
		  return A2(
			$andre_dietrich$parser_combinators$Combine$or,
			A2(
			  $andre_dietrich$parser_combinators$Combine$andThen,
			  function(f) {
				return A2(
				  $andre_dietrich$parser_combinators$Combine$andThen,
				  function(y) {
					return accumulate(A2(f, x, y));
				  },
				  p
				);
			  },
			  op
			),
			$andre_dietrich$parser_combinators$Combine$succeed(x)
		  );
		};
		return A2(
		  $andre_dietrich$parser_combinators$Combine$andThen,
		  accumulate,
		  p
		);
	  });
	  var $author$project$Ast$Common$Character = function(a) {
		return { $: "Character", a: a };
	  };
	  var $author$project$Ast$Expression$Literal = function(a) {
		return { $: "Literal", a: a };
	  };
	  var $elm$core$String$fromChar = function(_char) {
		return A2($elm$core$String$cons, _char, "");
	  };
	  var $elm$core$Char$fromCode = _Char_fromCode;
	  var $elm$core$Basics$pow = _Basics_pow;
	  var $rtfeldman$elm_hex$Hex$fromStringHelp = F3(function(
		position,
		chars,
		accumulated
	  ) {
		fromStringHelp: while (true) {
		  if (!chars.b) {
			return $elm$core$Result$Ok(accumulated);
		  } else {
			var _char = chars.a;
			var rest = chars.b;
			switch (_char.valueOf()) {
			  case "0":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated = accumulated;
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  case "1":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated =
					accumulated + A2($elm$core$Basics$pow, 16, position);
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  case "2":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated =
					accumulated +
					2 * A2($elm$core$Basics$pow, 16, position);
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  case "3":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated =
					accumulated +
					3 * A2($elm$core$Basics$pow, 16, position);
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  case "4":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated =
					accumulated +
					4 * A2($elm$core$Basics$pow, 16, position);
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  case "5":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated =
					accumulated +
					5 * A2($elm$core$Basics$pow, 16, position);
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  case "6":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated =
					accumulated +
					6 * A2($elm$core$Basics$pow, 16, position);
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  case "7":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated =
					accumulated +
					7 * A2($elm$core$Basics$pow, 16, position);
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  case "8":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated =
					accumulated +
					8 * A2($elm$core$Basics$pow, 16, position);
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  case "9":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated =
					accumulated +
					9 * A2($elm$core$Basics$pow, 16, position);
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  case "a":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated =
					accumulated +
					10 * A2($elm$core$Basics$pow, 16, position);
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  case "b":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated =
					accumulated +
					11 * A2($elm$core$Basics$pow, 16, position);
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  case "c":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated =
					accumulated +
					12 * A2($elm$core$Basics$pow, 16, position);
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  case "d":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated =
					accumulated +
					13 * A2($elm$core$Basics$pow, 16, position);
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  case "e":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated =
					accumulated +
					14 * A2($elm$core$Basics$pow, 16, position);
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  case "f":
				var $temp$position = position - 1,
				  $temp$chars = rest,
				  $temp$accumulated =
					accumulated +
					15 * A2($elm$core$Basics$pow, 16, position);
				position = $temp$position;
				chars = $temp$chars;
				accumulated = $temp$accumulated;
				continue fromStringHelp;
			  default:
				var nonHex = _char;
				return $elm$core$Result$Err(
				  $elm$core$String$fromChar(nonHex) +
					" is not a valid hexadecimal character."
				);
			}
		  }
		}
	  });
	  var $elm$core$Result$map = F2(function(func, ra) {
		if (ra.$ === "Ok") {
		  var a = ra.a;
		  return $elm$core$Result$Ok(func(a));
		} else {
		  var e = ra.a;
		  return $elm$core$Result$Err(e);
		}
	  });
	  var $elm$core$Result$mapError = F2(function(f, result) {
		if (result.$ === "Ok") {
		  var v = result.a;
		  return $elm$core$Result$Ok(v);
		} else {
		  var e = result.a;
		  return $elm$core$Result$Err(f(e));
		}
	  });
	  var $elm$core$Basics$negate = function(n) {
		return -n;
	  };
	  var $elm$core$List$tail = function(list) {
		if (list.b) {
		  var x = list.a;
		  var xs = list.b;
		  return $elm$core$Maybe$Just(xs);
		} else {
		  return $elm$core$Maybe$Nothing;
		}
	  };
	  var $elm$core$String$foldr = _String_foldr;
	  var $elm$core$String$toList = function(string) {
		return A3(
		  $elm$core$String$foldr,
		  $elm$core$List$cons,
		  _List_Nil,
		  string
		);
	  };
	  var $rtfeldman$elm_hex$Hex$fromString = function(str) {
		if ($elm$core$String$isEmpty(str)) {
		  return $elm$core$Result$Err(
			"Empty strings are not valid hexadecimal strings."
		  );
		} else {
		  var result = (function() {
			if (A2($elm$core$String$startsWith, "-", str)) {
			  var list = A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				$elm$core$List$tail($elm$core$String$toList(str))
			  );
			  return A2(
				$elm$core$Result$map,
				$elm$core$Basics$negate,
				A3(
				  $rtfeldman$elm_hex$Hex$fromStringHelp,
				  $elm$core$List$length(list) - 1,
				  list,
				  0
				)
			  );
			} else {
			  return A3(
				$rtfeldman$elm_hex$Hex$fromStringHelp,
				$elm$core$String$length(str) - 1,
				$elm$core$String$toList(str),
				0
			  );
			}
		  })();
		  var formatError = function(err) {
			return A2(
			  $elm$core$String$join,
			  " ",
			  _List_fromArray([
				'"' + (str + '"'),
				"is not a valid hexadecimal string because",
				err
			  ])
			);
		  };
		  return A2($elm$core$Result$mapError, formatError, result);
		}
	  };
	  var $elm$core$String$toLower = _String_toLower;
	  var $elm$core$Result$withDefault = F2(function(def, result) {
		if (result.$ === "Ok") {
		  var a = result.a;
		  return a;
		} else {
		  return def;
		}
	  });
	  var $author$project$Ast$Literal$characterParser = A2(
		$author$project$Ast$Helpers$between_,
		$andre_dietrich$parser_combinators$Combine$string("'"),
		A2(
		  $andre_dietrich$parser_combinators$Combine$or,
		  A2(
			$andre_dietrich$parser_combinators$Combine$andThen,
			function(a) {
			  var _v0 = $elm$core$String$uncons(a);
			  _v0$6: while (true) {
				if (_v0.$ === "Just") {
				  switch (_v0.a.a.valueOf()) {
					case "n":
					  if (_v0.a.b === "") {
						var _v1 = _v0.a;
						return $andre_dietrich$parser_combinators$Combine$succeed(
						  _Utils_chr("\n")
						);
					  } else {
						break _v0$6;
					  }
					case "t":
					  if (_v0.a.b === "") {
						var _v2 = _v0.a;
						return $andre_dietrich$parser_combinators$Combine$succeed(
						  _Utils_chr("\t")
						);
					  } else {
						break _v0$6;
					  }
					case "r":
					  if (_v0.a.b === "") {
						var _v3 = _v0.a;
						return $andre_dietrich$parser_combinators$Combine$succeed(
						  _Utils_chr("\u000D")
						);
					  } else {
						break _v0$6;
					  }
					case "\\":
					  if (_v0.a.b === "") {
						var _v4 = _v0.a;
						return $andre_dietrich$parser_combinators$Combine$succeed(
						  _Utils_chr("\\")
						);
					  } else {
						break _v0$6;
					  }
					case "0":
					  if (_v0.a.b === "") {
						var _v5 = _v0.a;
						return $andre_dietrich$parser_combinators$Combine$succeed(
						  _Utils_chr("\u0000")
						);
					  } else {
						break _v0$6;
					  }
					case "x":
					  var _v6 = _v0.a;
					  var hex = _v6.b;
					  return A2(
						$elm$core$Result$withDefault,
						$andre_dietrich$parser_combinators$Combine$fail(
						  "Invalid charcode"
						),
						A2(
						  $elm$core$Result$map,
						  $andre_dietrich$parser_combinators$Combine$succeed,
						  A2(
							$elm$core$Result$map,
							$elm$core$Char$fromCode,
							$rtfeldman$elm_hex$Hex$fromString(
							  $elm$core$String$toLower(hex)
							)
						  )
						)
					  );
					default:
					  break _v0$6;
				  }
				} else {
				  return $andre_dietrich$parser_combinators$Combine$fail(
					"No character"
				  );
				}
			  }
			  var other = _v0.a;
			  return $andre_dietrich$parser_combinators$Combine$fail(
				"No such character as \\" +
				  $elm$core$String$fromChar(other.a)
			  );
			},
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $andre_dietrich$parser_combinators$Combine$regex(
				"(n|t|r|\\\\|x..)"
			  ),
			  $andre_dietrich$parser_combinators$Combine$string("\\")
			)
		  ),
		  $andre_dietrich$parser_combinators$Combine$Char$anyChar
		)
	  );
	  var $author$project$Ast$Expression$character = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$map,
		  A2(
			$elm$core$Basics$composeL,
			$author$project$Ast$Expression$Literal,
			$author$project$Ast$Common$Character
		  ),
		  $author$project$Ast$Literal$characterParser
		)
	  );
	  var $author$project$Ast$Expression$Constructor = function(a) {
		return { $: "Constructor", a: a };
	  };
	  var $author$project$Ast$Expression$constructor = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$map,
		  $author$project$Ast$Expression$Constructor,
		  $author$project$Ast$Helpers$upName
		)
	  );
	  var $elm$core$String$filter = _String_filter;
	  var $andre_dietrich$parser_combinators$Combine$Char$newline = A2(
		$andre_dietrich$parser_combinators$Combine$onerror,
		"expected a newline",
		$andre_dietrich$parser_combinators$Combine$Char$satisfy(
		  $elm$core$Basics$eq(_Utils_chr("\n"))
		)
	  );
	  var $author$project$Ast$Helpers$countIndent = A2(
		$andre_dietrich$parser_combinators$Combine$andThen,
		A2(
		  $elm$core$Basics$composeR,
		  $elm$core$String$filter(function(_char) {
			return _Utils_eq(_char, _Utils_chr(" "));
		  }),
		  A2(
			$elm$core$Basics$composeR,
			$elm$core$String$length,
			$andre_dietrich$parser_combinators$Combine$succeed
		  )
		),
		A2(
		  $andre_dietrich$parser_combinators$Combine$keep,
		  $author$project$Ast$Helpers$spaces,
		  $andre_dietrich$parser_combinators$Combine$Char$newline
		)
	  );
	  var $author$project$Ast$Helpers$exactIndentation = function(_int) {
		return $andre_dietrich$parser_combinators$Combine$regex(
		  "\n*[ \\t]{" + ($elm$core$String$fromInt(_int) + "}\n*")
		);
	  };
	  var $author$project$Ast$Expression$External = F2(function(a, b) {
		return { $: "External", a: a, b: b };
	  });
	  var $author$project$Ast$Expression$external = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  A2(
			$andre_dietrich$parser_combinators$Combine$or,
			$author$project$Ast$Expression$variable,
			$author$project$Ast$Expression$constructor
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$author$project$Ast$Expression$External,
			$andre_dietrich$parser_combinators$Combine$many1(
			  A2(
				$andre_dietrich$parser_combinators$Combine$ignore,
				$andre_dietrich$parser_combinators$Combine$string("."),
				$author$project$Ast$Helpers$upName
			  )
			)
		  )
		)
	  );
	  var $author$project$Ast$Common$Float = function(a) {
		return { $: "Float", a: a };
	  };
	  var $elm$core$String$toFloat = _String_toFloat;
	  var $andre_dietrich$parser_combinators$Combine$Num$unwrap = function(
		value
	  ) {
		if (value.$ === "Just") {
		  var v = value.a;
		  return $andre_dietrich$parser_combinators$Combine$succeed(v);
		} else {
		  return $andre_dietrich$parser_combinators$Combine$fail(
			"impossible state in Combine.Num.unwrap"
		  );
		}
	  };
	  var $andre_dietrich$parser_combinators$Combine$Num$float = A2(
		$andre_dietrich$parser_combinators$Combine$onerror,
		"expected a float",
		A2(
		  $andre_dietrich$parser_combinators$Combine$andThen,
		  $andre_dietrich$parser_combinators$Combine$Num$unwrap,
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$elm$core$String$toFloat,
			$andre_dietrich$parser_combinators$Combine$regex(
			  "-?(?:0|[1-9]\\d*)\\.\\d+"
			)
		  )
		)
	  );
	  var $author$project$Ast$Literal$floatParser = $andre_dietrich$parser_combinators$Combine$Num$float;
	  var $author$project$Ast$Expression$float = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$map,
		  A2(
			$elm$core$Basics$composeL,
			$author$project$Ast$Expression$Literal,
			$author$project$Ast$Common$Float
		  ),
		  $author$project$Ast$Literal$floatParser
		)
	  );
	  var $author$project$Ast$Common$Integer = function(a) {
		return { $: "Integer", a: a };
	  };
	  var $andre_dietrich$parser_combinators$Combine$Num$int = A2(
		$andre_dietrich$parser_combinators$Combine$onerror,
		"expected an int",
		A2(
		  $andre_dietrich$parser_combinators$Combine$andThen,
		  $andre_dietrich$parser_combinators$Combine$Num$unwrap,
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$elm$core$String$toInt,
			$andre_dietrich$parser_combinators$Combine$regex(
			  "-?(?:0|[1-9]\\d*)"
			)
		  )
		)
	  );
	  var $author$project$Ast$Literal$intParser = $andre_dietrich$parser_combinators$Combine$Num$int;
	  var $author$project$Ast$Expression$integer = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$map,
		  A2(
			$elm$core$Basics$composeL,
			$author$project$Ast$Expression$Literal,
			$author$project$Ast$Common$Integer
		  ),
		  $author$project$Ast$Literal$intParser
		)
	  );
	  var $andre_dietrich$parser_combinators$Combine$brackets = A2(
		$andre_dietrich$parser_combinators$Combine$between,
		$andre_dietrich$parser_combinators$Combine$string("["),
		$andre_dietrich$parser_combinators$Combine$string("]")
	  );
	  var $andre_dietrich$parser_combinators$Combine$sepBy = F2(function(
		sep,
		p
	  ) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$or,
		  A2($andre_dietrich$parser_combinators$Combine$sepBy1, sep, p),
		  $andre_dietrich$parser_combinators$Combine$succeed(_List_Nil)
		);
	  });
	  var $author$project$Ast$Helpers$commaSeparated_ = function(p) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$sepBy,
		  $andre_dietrich$parser_combinators$Combine$string(","),
		  A2(
			$author$project$Ast$Helpers$between_,
			$andre_dietrich$parser_combinators$Combine$whitespace,
			p
		  )
		);
	  };
	  var $author$project$Ast$Helpers$listParser = function(el) {
		return $andre_dietrich$parser_combinators$Combine$brackets(
		  $author$project$Ast$Helpers$commaSeparated_(el)
		);
	  };
	  var $andre_dietrich$parser_combinators$Combine$lookAhead = function(
		p
	  ) {
		return $andre_dietrich$parser_combinators$Combine$Parser(
		  F2(function(state, stream) {
			var _v0 = A3(
			  $andre_dietrich$parser_combinators$Combine$app,
			  p,
			  state,
			  stream
			);
			if (_v0.c.$ === "Ok") {
			  var rstate = _v0.a;
			  var res = _v0.c.a;
			  return _Utils_Tuple3(
				rstate,
				stream,
				$elm$core$Result$Ok(res)
			  );
			} else {
			  var err = _v0;
			  return err;
			}
		  })
		);
	  };
	  var $author$project$Ast$Common$PApplication = F2(function(a, b) {
		return { $: "PApplication", a: a, b: b };
	  });
	  var $author$project$Ast$Helpers$spacesOrIndentedNewline = function(
		indentation
	  ) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v0
		) {
		  return A2(
			$andre_dietrich$parser_combinators$Combine$or,
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $andre_dietrich$parser_combinators$Combine$succeed(
				_Utils_Tuple0
			  ),
			  $author$project$Ast$Helpers$spaces_
			),
			A2(
			  $andre_dietrich$parser_combinators$Combine$andThen,
			  function(column) {
				return _Utils_cmp(column, indentation) < 0
				  ? $andre_dietrich$parser_combinators$Combine$fail(
					  "Arguments have to be at least the same indentation as the function"
					)
				  : $andre_dietrich$parser_combinators$Combine$succeed(
					  _Utils_Tuple0
					);
			  },
			  $author$project$Ast$Helpers$countIndent
			)
		  );
		});
	  };
	  var $author$project$Ast$Pattern$appParser = function(next) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v0
		) {
		  return A2(
			$andre_dietrich$parser_combinators$Combine$or,
			$andre_dietrich$parser_combinators$Combine$withLocation(
			  function(l) {
				return A2(
				  $andre_dietrich$parser_combinators$Combine$chainl,
				  A2(
					$andre_dietrich$parser_combinators$Combine$onsuccess,
					F2(function(a, b) {
					  return A3(
						$author$project$Ast$Common$addMeta,
						l.line,
						l.column,
						A2($author$project$Ast$Common$PApplication, a, b)
					  );
					}),
					$author$project$Ast$Helpers$spacesOrIndentedNewline(
					  l.column + 1
					)
				  ),
				  next
				);
			  }
			),
			next
		  );
		});
	  };
	  var $author$project$Ast$Common$PAs = F2(function(a, b) {
		return { $: "PAs", a: a, b: b };
	  });
	  var $author$project$Ast$Pattern$asParser_ = function(a) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v0
		) {
		  return A2(
			$andre_dietrich$parser_combinators$Combine$or,
			A2(
			  $andre_dietrich$parser_combinators$Combine$andThen,
			  $author$project$Ast$Pattern$asParser_,
			  $author$project$Ast$Common$withMeta(
				A2(
				  $andre_dietrich$parser_combinators$Combine$map,
				  $author$project$Ast$Common$PAs(a),
				  A2(
					$andre_dietrich$parser_combinators$Combine$keep,
					$author$project$Ast$Helpers$varName,
					$author$project$Ast$Helpers$symbol("as")
				  )
				)
			  )
			),
			$andre_dietrich$parser_combinators$Combine$succeed(a)
		  );
		});
	  };
	  var $author$project$Ast$Pattern$asParser = function(next) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v0
		) {
		  return A2(
			$andre_dietrich$parser_combinators$Combine$andThen,
			$author$project$Ast$Pattern$asParser_,
			next
		  );
		});
	  };
	  var $author$project$Ast$Common$PCons = F2(function(a, b) {
		return { $: "PCons", a: a, b: b };
	  });
	  var $author$project$Ast$Pattern$consParser = function(next) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v0
		) {
		  return A2(
			$andre_dietrich$parser_combinators$Combine$or,
			$author$project$Ast$Common$withMeta(
			  A2(
				$andre_dietrich$parser_combinators$Combine$andMap,
				$author$project$Ast$Pattern$consParser(next),
				A2(
				  $andre_dietrich$parser_combinators$Combine$ignore,
				  $author$project$Ast$Common$withMeta(
					$author$project$Ast$Helpers$symbol("::")
				  ),
				  A2(
					$andre_dietrich$parser_combinators$Combine$map,
					$author$project$Ast$Common$PCons,
					next
				  )
				)
			  )
			),
			next
		  );
		});
	  };
	  var $author$project$Ast$Common$PList = function(a) {
		return { $: "PList", a: a };
	  };
	  var $author$project$Ast$Common$PLiteral = function(a) {
		return { $: "PLiteral", a: a };
	  };
	  var $author$project$Ast$Common$PRecord = function(a) {
		return { $: "PRecord", a: a };
	  };
	  var $author$project$Ast$Common$PTuple = function(a) {
		return { $: "PTuple", a: a };
	  };
	  var $author$project$Ast$Common$PConstructor = function(a) {
		return { $: "PConstructor", a: a };
	  };
	  var $author$project$Ast$Pattern$constructorParser = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$map,
		  $author$project$Ast$Common$PConstructor,
		  $author$project$Ast$Helpers$upName
		)
	  );
	  var $author$project$Ast$Common$String = function(a) {
		return { $: "String", a: a };
	  };
	  var $elm$core$String$concat = function(strings) {
		return A2($elm$core$String$join, "", strings);
	  };
	  var $author$project$Ast$Literal$stringParser = (function() {
		var singleString = A2(
		  $andre_dietrich$parser_combinators$Combine$ignore,
		  $andre_dietrich$parser_combinators$Combine$string('"'),
		  A2(
			$andre_dietrich$parser_combinators$Combine$keep,
			$andre_dietrich$parser_combinators$Combine$regex(
			  '(\\\\\\\\|\\\\"|[^"\n])*'
			),
			$andre_dietrich$parser_combinators$Combine$string('"')
		  )
		);
		var multiString = A2(
		  $andre_dietrich$parser_combinators$Combine$map,
		  $elm$core$String$concat,
		  A2(
			$andre_dietrich$parser_combinators$Combine$ignore,
			$andre_dietrich$parser_combinators$Combine$string('"""'),
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $andre_dietrich$parser_combinators$Combine$many(
				$andre_dietrich$parser_combinators$Combine$regex('[^"]*')
			  ),
			  $andre_dietrich$parser_combinators$Combine$string('"""')
			)
		  )
		);
		return A2(
		  $andre_dietrich$parser_combinators$Combine$or,
		  multiString,
		  singleString
		);
	  })();
	  var $author$project$Ast$Literal$literalParser = $andre_dietrich$parser_combinators$Combine$choice(
		_List_fromArray([
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$author$project$Ast$Common$Float,
			$author$project$Ast$Literal$floatParser
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$author$project$Ast$Common$Integer,
			$author$project$Ast$Literal$intParser
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$author$project$Ast$Common$Character,
			$author$project$Ast$Literal$characterParser
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$author$project$Ast$Common$String,
			$author$project$Ast$Literal$stringParser
		  )
		])
	  );
	  var $author$project$Ast$Helpers$tupleParser = function(el) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$andThen,
		  function(a) {
			if (a.b && !a.b.b) {
			  return $andre_dietrich$parser_combinators$Combine$fail(
				"No single tuples"
			  );
			} else {
			  var anyOther = a;
			  return $andre_dietrich$parser_combinators$Combine$succeed(
				anyOther
			  );
			}
		  },
		  $andre_dietrich$parser_combinators$Combine$parens(
			$author$project$Ast$Helpers$commaSeparated_(el)
		  )
		);
	  };
	  var $author$project$Ast$Common$PVariable = function(a) {
		return { $: "PVariable", a: a };
	  };
	  var $author$project$Ast$Helpers$funName = $andre_dietrich$parser_combinators$Combine$choice(
		_List_fromArray([
		  $author$project$Ast$Helpers$varName,
		  $andre_dietrich$parser_combinators$Combine$parens(
			$author$project$Ast$Helpers$operator
		  )
		])
	  );
	  var $author$project$Ast$Pattern$varParser = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$map,
		  $author$project$Ast$Common$PVariable,
		  $author$project$Ast$Helpers$funName
		)
	  );
	  var $author$project$Ast$Common$PWild = { $: "PWild" };
	  var $author$project$Ast$Pattern$wildParser = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$map,
		  $elm$core$Basics$always($author$project$Ast$Common$PWild),
		  $author$project$Ast$Helpers$wild
		)
	  );
	  var $author$project$Ast$Pattern$terminalParser = function(next) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v0
		) {
		  return $andre_dietrich$parser_combinators$Combine$choice(
			_List_fromArray([
			  $author$project$Ast$Pattern$wildParser,
			  $author$project$Ast$Pattern$varParser,
			  $author$project$Ast$Pattern$constructorParser,
			  $author$project$Ast$Common$withMeta(
				A2(
				  $andre_dietrich$parser_combinators$Combine$map,
				  $author$project$Ast$Common$PLiteral,
				  $author$project$Ast$Literal$literalParser
				)
			  ),
			  $author$project$Ast$Common$withMeta(
				A2(
				  $andre_dietrich$parser_combinators$Combine$map,
				  $author$project$Ast$Common$PRecord,
				  $andre_dietrich$parser_combinators$Combine$braces(
					$author$project$Ast$Helpers$commaSeparated_(
					  $author$project$Ast$Common$withMeta(
						$author$project$Ast$Helpers$loName
					  )
					)
				  )
				)
			  ),
			  $author$project$Ast$Common$withMeta(
				A2(
				  $andre_dietrich$parser_combinators$Combine$map,
				  $author$project$Ast$Common$PTuple,
				  $author$project$Ast$Helpers$tupleParser(next)
				)
			  ),
			  $author$project$Ast$Common$withMeta(
				A2(
				  $andre_dietrich$parser_combinators$Combine$map,
				  $author$project$Ast$Common$PList,
				  $author$project$Ast$Helpers$listParser(next)
				)
			  ),
			  $andre_dietrich$parser_combinators$Combine$parens(next)
			])
		  );
		});
	  };
	  var $author$project$Ast$Pattern$precedence = _List_fromArray([
		$author$project$Ast$Pattern$asParser,
		$author$project$Ast$Pattern$consParser,
		$author$project$Ast$Pattern$appParser,
		$author$project$Ast$Pattern$terminalParser
	  ]);
	  function $author$project$Ast$Pattern$cyclic$pattern() {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v0
		) {
		  return A3(
			$elm$core$List$foldr,
			$elm$core$Basics$identity,
			$author$project$Ast$Pattern$cyclic$pattern(),
			$author$project$Ast$Pattern$precedence
		  );
		});
	  }
	  try {
		var $author$project$Ast$Pattern$pattern = $author$project$Ast$Pattern$cyclic$pattern();
		$author$project$Ast$Pattern$cyclic$pattern = function() {
		  return $author$project$Ast$Pattern$pattern;
		};
	  } catch ($) {
		throw "Some top-level definitions from `Ast.Pattern` are causing infinite recursion:\n\n  ┌─────┐\n  │    pattern\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!";
	  }
	  var $author$project$Ast$Expression$simplifiedRecord = (function() {
		var branch = function(_v1) {
		  var line = _v1.line;
		  var column = _v1.column;
		  return A2(
			$andre_dietrich$parser_combinators$Combine$map,
			function(a) {
			  return _Utils_Tuple2(
				A3($author$project$Ast$Common$addMeta, line, column, a),
				A3(
				  $author$project$Ast$Common$addMeta,
				  line,
				  column,
				  $author$project$Ast$Expression$Variable(a)
				)
			  );
			},
			$author$project$Ast$Helpers$loName
		  );
		};
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v0
		) {
		  return $author$project$Ast$Common$withMeta(
			A2(
			  $andre_dietrich$parser_combinators$Combine$map,
			  $author$project$Ast$Expression$Record,
			  $andre_dietrich$parser_combinators$Combine$braces(
				$author$project$Ast$Helpers$commaSeparated(
				  $andre_dietrich$parser_combinators$Combine$withLocation(
					branch
				  )
				)
			  )
			)
		  );
		});
	  })();
	  var $elm$core$Basics$not = _Basics_not;
	  var $elm_community$list_extra$List$Extra$dropWhile = F2(function(
		predicate,
		list
	  ) {
		dropWhile: while (true) {
		  if (!list.b) {
			return _List_Nil;
		  } else {
			var x = list.a;
			var xs = list.b;
			if (predicate(x)) {
			  var $temp$predicate = predicate,
				$temp$list = xs;
			  predicate = $temp$predicate;
			  list = $temp$list;
			  continue dropWhile;
			} else {
			  return list;
			}
		  }
		}
	  });
	  var $elm_community$list_extra$List$Extra$takeWhile = function(
		predicate
	  ) {
		var takeWhileMemo = F2(function(memo, list) {
		  takeWhileMemo: while (true) {
			if (!list.b) {
			  return $elm$core$List$reverse(memo);
			} else {
			  var x = list.a;
			  var xs = list.b;
			  if (predicate(x)) {
				var $temp$memo = A2($elm$core$List$cons, x, memo),
				  $temp$list = xs;
				memo = $temp$memo;
				list = $temp$list;
				continue takeWhileMemo;
			  } else {
				return $elm$core$List$reverse(memo);
			  }
			}
		  }
		});
		return takeWhileMemo(_List_Nil);
	  };
	  var $elm_community$list_extra$List$Extra$span = F2(function(p, xs) {
		return _Utils_Tuple2(
		  A2($elm_community$list_extra$List$Extra$takeWhile, p, xs),
		  A2($elm_community$list_extra$List$Extra$dropWhile, p, xs)
		);
	  });
	  var $elm_community$list_extra$List$Extra$break = function(p) {
		return $elm_community$list_extra$List$Extra$span(
		  A2($elm$core$Basics$composeL, $elm$core$Basics$not, p)
		);
	  };
	  var $elm$core$List$maybeCons = F3(function(f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === "Just") {
		  var x = _v0.a;
		  return A2($elm$core$List$cons, x, xs);
		} else {
		  return xs;
		}
	  });
	  var $elm$core$List$filterMap = F2(function(f, xs) {
		return A3(
		  $elm$core$List$foldr,
		  $elm$core$List$maybeCons(f),
		  _List_Nil,
		  xs
		);
	  });
	  var $author$project$Ast$BinOp$N = { $: "N" };
	  var $elm$core$List$all = F2(function(isOkay, list) {
		return !A2(
		  $elm$core$List$any,
		  A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
		  list
		);
	  });
	  var $elm$core$Dict$get = F2(function(targetKey, dict) {
		get: while (true) {
		  if (dict.$ === "RBEmpty_elm_builtin") {
			return $elm$core$Maybe$Nothing;
		  } else {
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			var _v1 = A2($elm$core$Basics$compare, targetKey, key);
			switch (_v1.$) {
			  case "LT":
				var $temp$targetKey = targetKey,
				  $temp$dict = left;
				targetKey = $temp$targetKey;
				dict = $temp$dict;
				continue get;
			  case "EQ":
				return $elm$core$Maybe$Just(value);
			  default:
				var $temp$targetKey = targetKey,
				  $temp$dict = right;
				targetKey = $temp$targetKey;
				dict = $temp$dict;
				continue get;
			}
		  }
		}
	  });
	  var $author$project$Ast$Expression$op = F2(function(ops, n) {
		return A2(
		  $elm$core$Maybe$withDefault,
		  _Utils_Tuple2($author$project$Ast$BinOp$L, 9),
		  A2($elm$core$Dict$get, n, ops)
		);
	  });
	  var $author$project$Ast$Expression$assoc = F2(function(ops, n) {
		return A2($author$project$Ast$Expression$op, ops, n).a;
	  });
	  var $author$project$Ast$Common$dropMeta = function(_v0) {
		var e = _v0.a;
		return e;
	  };
	  var $elm$core$List$filter = F2(function(isGood, list) {
		return A3(
		  $elm$core$List$foldr,
		  F2(function(x, xs) {
			return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
		  }),
		  _List_Nil,
		  list
		);
	  });
	  var $elm$core$Tuple$second = function(_v0) {
		var y = _v0.b;
		return y;
	  };
	  var $author$project$Ast$Expression$level = F2(function(ops, n) {
		return A2($author$project$Ast$Expression$op, ops, n).b;
	  });
	  var $author$project$Ast$Expression$hasLevel = F3(function(
		ops,
		l,
		_v0
	  ) {
		var n = _v0.a;
		return _Utils_eq(
		  A2(
			$author$project$Ast$Expression$level,
			ops,
			$author$project$Ast$Common$dropMeta(n)
		  ),
		  l
		);
	  });
	  var $author$project$Ast$Expression$findAssoc = F3(function(
		ops,
		l,
		eops
	  ) {
		var lops = A2(
		  $elm$core$List$filter,
		  A2($author$project$Ast$Expression$hasLevel, ops, l),
		  eops
		);
		var error = function(issue) {
		  var operators = A2(
			$elm$core$String$join,
			" and ",
			A2(
			  $elm$core$List$map,
			  A2(
				$elm$core$Basics$composeR,
				$elm$core$Tuple$first,
				$author$project$Ast$Common$dropMeta
			  ),
			  lops
			)
		  );
		  return "conflicting " + (issue + (" for operators " + operators));
		};
		var assocs = A2(
		  $elm$core$List$map,
		  A2(
			$elm$core$Basics$composeL,
			A2(
			  $elm$core$Basics$composeL,
			  $author$project$Ast$Expression$assoc(ops),
			  $author$project$Ast$Common$dropMeta
			),
			$elm$core$Tuple$first
		  ),
		  lops
		);
		if (
		  A2(
			$elm$core$List$all,
			$elm$core$Basics$eq($author$project$Ast$BinOp$L),
			assocs
		  )
		) {
		  return $andre_dietrich$parser_combinators$Combine$succeed(
			$author$project$Ast$BinOp$L
		  );
		} else {
		  if (
			A2(
			  $elm$core$List$all,
			  $elm$core$Basics$eq($author$project$Ast$BinOp$R),
			  assocs
			)
		  ) {
			return $andre_dietrich$parser_combinators$Combine$succeed(
			  $author$project$Ast$BinOp$R
			);
		  } else {
			if (
			  A2(
				$elm$core$List$all,
				$elm$core$Basics$eq($author$project$Ast$BinOp$N),
				assocs
			  )
			) {
			  if (assocs.b && !assocs.b.b) {
				return $andre_dietrich$parser_combinators$Combine$succeed(
				  $author$project$Ast$BinOp$N
				);
			  } else {
				return $andre_dietrich$parser_combinators$Combine$fail(
				  error("precedence")
				);
			  }
			} else {
			  return $andre_dietrich$parser_combinators$Combine$fail(
				error("associativity")
			  );
			}
		  }
		}
	  });
	  var $author$project$Ast$Expression$BinOp = F3(function(a, b, c) {
		return { $: "BinOp", a: a, b: b, c: c };
	  });
	  var $author$project$Ast$Expression$joinL = F2(function(es, ops) {
		joinL: while (true) {
		  var _v0 = _Utils_Tuple2(es, ops);
		  _v0$2: while (true) {
			if (_v0.a.b) {
			  if (!_v0.a.b.b) {
				if (!_v0.b.b) {
				  var _v1 = _v0.a;
				  var e = _v1.a;
				  return $andre_dietrich$parser_combinators$Combine$succeed(
					e
				  );
				} else {
				  break _v0$2;
				}
			  } else {
				if (_v0.b.b) {
				  var _v2 = _v0.a;
				  var a = _v2.a;
				  var _v3 = _v2.b;
				  var b = _v3.a;
				  var remE = _v3.b;
				  var _v4 = _v0.b;
				  var _v5 = _v4.a;
				  var e = _v5.a;
				  var line = _v5.b.line;
				  var column = _v5.b.column;
				  var remO = _v4.b;
				  var $temp$es = A2(
					  $elm$core$List$cons,
					  A3(
						$author$project$Ast$Common$addMeta,
						line,
						column,
						A3(
						  $author$project$Ast$Expression$BinOp,
						  A3(
							$author$project$Ast$Common$addMeta,
							line,
							column,
							$author$project$Ast$Expression$Variable(e)
						  ),
						  a,
						  b
						)
					  ),
					  remE
					),
					$temp$ops = remO;
				  es = $temp$es;
				  ops = $temp$ops;
				  continue joinL;
				} else {
				  break _v0$2;
				}
			  }
			} else {
			  break _v0$2;
			}
		  }
		  return $andre_dietrich$parser_combinators$Combine$fail("");
		}
	  });
	  var $author$project$Ast$Expression$joinR = F2(function(es, ops) {
		var _v0 = _Utils_Tuple2(es, ops);
		_v0$2: while (true) {
		  if (_v0.a.b) {
			if (!_v0.a.b.b) {
			  if (!_v0.b.b) {
				var _v1 = _v0.a;
				var e = _v1.a;
				return $andre_dietrich$parser_combinators$Combine$succeed(
				  e
				);
			  } else {
				break _v0$2;
			  }
			} else {
			  if (_v0.b.b) {
				var _v2 = _v0.a;
				var a = _v2.a;
				var _v3 = _v2.b;
				var b = _v3.a;
				var remE = _v3.b;
				var _v4 = _v0.b;
				var _v5 = _v4.a;
				var e = _v5.a;
				var line = _v5.b.line;
				var column = _v5.b.column;
				var remO = _v4.b;
				return A2(
				  $andre_dietrich$parser_combinators$Combine$andThen,
				  function(exp) {
					return $andre_dietrich$parser_combinators$Combine$succeed(
					  A3(
						$author$project$Ast$Common$addMeta,
						line,
						column,
						A3(
						  $author$project$Ast$Expression$BinOp,
						  A3(
							$author$project$Ast$Common$addMeta,
							line,
							column,
							$author$project$Ast$Expression$Variable(e)
						  ),
						  a,
						  exp
						)
					  )
					);
				  },
				  A2(
					$author$project$Ast$Expression$joinR,
					A2($elm$core$List$cons, b, remE),
					remO
				  )
				);
			  } else {
				break _v0$2;
			  }
			}
		  } else {
			break _v0$2;
		  }
		}
		return $andre_dietrich$parser_combinators$Combine$fail("");
	  });
	  var $andre_dietrich$parser_combinators$Combine$sequence = function(
		parsers
	  ) {
		var accumulate = F4(function(acc, ps, state, stream) {
		  accumulate: while (true) {
			if (!ps.b) {
			  return _Utils_Tuple3(
				state,
				stream,
				$elm$core$Result$Ok($elm$core$List$reverse(acc))
			  );
			} else {
			  var x = ps.a;
			  var xs = ps.b;
			  var _v1 = A3(
				$andre_dietrich$parser_combinators$Combine$app,
				x,
				state,
				stream
			  );
			  if (_v1.c.$ === "Ok") {
				var rstate = _v1.a;
				var rstream = _v1.b;
				var res = _v1.c.a;
				var $temp$acc = A2($elm$core$List$cons, res, acc),
				  $temp$ps = xs,
				  $temp$state = rstate,
				  $temp$stream = rstream;
				acc = $temp$acc;
				ps = $temp$ps;
				state = $temp$state;
				stream = $temp$stream;
				continue accumulate;
			  } else {
				var estate = _v1.a;
				var estream = _v1.b;
				var ms = _v1.c.a;
				return _Utils_Tuple3(
				  estate,
				  estream,
				  $elm$core$Result$Err(ms)
				);
			  }
			}
		  }
		});
		return $andre_dietrich$parser_combinators$Combine$Parser(
		  F2(function(state, stream) {
			return A4(accumulate, _List_Nil, parsers, state, stream);
		  })
		);
	  };
	  var $author$project$Ast$Expression$split = F4(function(
		ops,
		l,
		e,
		eops
	  ) {
		if (!eops.b) {
		  return $andre_dietrich$parser_combinators$Combine$succeed(e);
		} else {
		  return A2(
			$andre_dietrich$parser_combinators$Combine$andThen,
			function(association) {
			  return A2(
				$andre_dietrich$parser_combinators$Combine$andThen,
				function(es) {
				  var ops_ = A2(
					$elm$core$List$filterMap,
					function(x) {
					  return A3(
						$author$project$Ast$Expression$hasLevel,
						ops,
						l,
						x
					  )
						? $elm$core$Maybe$Just(x.a)
						: $elm$core$Maybe$Nothing;
					},
					eops
				  );
				  if (association.$ === "R") {
					return A2(
					  $author$project$Ast$Expression$joinR,
					  es,
					  ops_
					);
				  } else {
					return A2(
					  $author$project$Ast$Expression$joinL,
					  es,
					  ops_
					);
				  }
				},
				$andre_dietrich$parser_combinators$Combine$sequence(
				  A4(
					$author$project$Ast$Expression$splitLevel,
					ops,
					l,
					e,
					eops
				  )
				)
			  );
			},
			A3($author$project$Ast$Expression$findAssoc, ops, l, eops)
		  );
		}
	  });
	  var $author$project$Ast$Expression$splitLevel = F4(function(
		ops,
		l,
		e,
		eops
	  ) {
		var _v0 = A2(
		  $elm_community$list_extra$List$Extra$break,
		  A2($author$project$Ast$Expression$hasLevel, ops, l),
		  eops
		);
		if (_v0.b.b) {
		  var lops = _v0.a;
		  var _v1 = _v0.b;
		  var _v2 = _v1.a;
		  var e_ = _v2.b;
		  var rops = _v1.b;
		  return A2(
			$elm$core$List$cons,
			A4($author$project$Ast$Expression$split, ops, l + 1, e, lops),
			A4($author$project$Ast$Expression$splitLevel, ops, l, e_, rops)
		  );
		} else {
		  var lops = _v0.a;
		  return _List_fromArray([
			A4($author$project$Ast$Expression$split, ops, l + 1, e, lops)
		  ]);
		}
	  });
	  var $author$project$Ast$Expression$string = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$map,
		  A2(
			$elm$core$Basics$composeL,
			$author$project$Ast$Expression$Literal,
			$author$project$Ast$Common$String
		  ),
		  $author$project$Ast$Literal$stringParser
		)
	  );
	  var $author$project$Ast$Expression$successOrEmptyList = function(p) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v0
		) {
		  return $andre_dietrich$parser_combinators$Combine$choice(
			_List_fromArray([
			  p,
			  $andre_dietrich$parser_combinators$Combine$succeed(_List_Nil)
			])
		  );
		});
	  };
	  var $author$project$Ast$Helpers$symbol_ = function(k) {
		return A2(
		  $author$project$Ast$Helpers$between_,
		  $andre_dietrich$parser_combinators$Combine$whitespace,
		  A2(
			$andre_dietrich$parser_combinators$Combine$ignore,
			$andre_dietrich$parser_combinators$Combine$regex("( |\\n)+"),
			$andre_dietrich$parser_combinators$Combine$string(k)
		  )
		);
	  };
	  var $author$project$Ast$Expression$application = function(ops) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v13
		) {
		  return $andre_dietrich$parser_combinators$Combine$withLocation(
			function(l) {
			  return A2(
				$andre_dietrich$parser_combinators$Combine$chainl,
				A2(
				  $andre_dietrich$parser_combinators$Combine$onsuccess,
				  F2(function(a, b) {
					return A3(
					  $author$project$Ast$Common$addMeta,
					  l.line,
					  l.column,
					  A2($author$project$Ast$Expression$Application, a, b)
					);
				  }),
				  $author$project$Ast$Helpers$spacesOrIndentedNewline(
					l.column + 1
				  )
				),
				$author$project$Ast$Expression$term(ops)
			  );
			}
		  );
		});
	  };
	  var $author$project$Ast$Expression$binary = function(ops) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$andThen,
		  function(e) {
			return A2(
			  $andre_dietrich$parser_combinators$Combine$andThen,
			  function(eops) {
				return A4(
				  $author$project$Ast$Expression$split,
				  ops,
				  0,
				  e,
				  eops
				);
			  },
			  $author$project$Ast$Expression$successOrEmptyList(
				$author$project$Ast$Expression$next_(ops)
			  )
			);
		  },
		  $author$project$Ast$Expression$application(ops)
		);
	  };
	  var $author$project$Ast$Expression$caseExpression = function(ops) {
		var binding = function(indent) {
		  return $andre_dietrich$parser_combinators$Combine$lazy(function(
			_v12
		  ) {
			return A2(
			  $andre_dietrich$parser_combinators$Combine$andMap,
			  A2(
				$andre_dietrich$parser_combinators$Combine$keep,
				$author$project$Ast$Expression$expression(ops),
				$author$project$Ast$Helpers$symbol("->")
			  ),
			  A2(
				$andre_dietrich$parser_combinators$Combine$map,
				$elm$core$Tuple$pair,
				A2(
				  $andre_dietrich$parser_combinators$Combine$keep,
				  $author$project$Ast$Pattern$pattern,
				  $author$project$Ast$Helpers$exactIndentation(indent)
				)
			  )
			);
		  });
		};
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v11
		) {
		  return $author$project$Ast$Common$withMeta(
			A2(
			  $andre_dietrich$parser_combinators$Combine$andMap,
			  A2(
				$andre_dietrich$parser_combinators$Combine$andThen,
				function(indent) {
				  return $andre_dietrich$parser_combinators$Combine$many1(
					binding(indent)
				  );
				},
				A2(
				  $andre_dietrich$parser_combinators$Combine$keep,
				  $andre_dietrich$parser_combinators$Combine$lookAhead(
					$author$project$Ast$Helpers$countIndent
				  ),
				  A2(
					$andre_dietrich$parser_combinators$Combine$keep,
					$andre_dietrich$parser_combinators$Combine$string("of"),
					$andre_dietrich$parser_combinators$Combine$whitespace
				  )
				)
			  ),
			  A2(
				$andre_dietrich$parser_combinators$Combine$map,
				$author$project$Ast$Expression$Case,
				A2(
				  $andre_dietrich$parser_combinators$Combine$keep,
				  $author$project$Ast$Expression$expression(ops),
				  $author$project$Ast$Helpers$symbol("case")
				)
			  )
			)
		  );
		});
	  };
	  var $author$project$Ast$Expression$expression = function(ops) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v10
		) {
		  return $andre_dietrich$parser_combinators$Combine$choice(
			_List_fromArray([
			  $author$project$Ast$Expression$binary(ops),
			  $author$project$Ast$Expression$letExpression(ops),
			  $author$project$Ast$Expression$caseExpression(ops),
			  $author$project$Ast$Expression$ifExpression(ops),
			  $author$project$Ast$Expression$lambda(ops)
			])
		  );
		});
	  };
	  var $author$project$Ast$Expression$ifExpression = function(ops) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v9
		) {
		  return $author$project$Ast$Common$withMeta(
			A2(
			  $andre_dietrich$parser_combinators$Combine$andMap,
			  A2(
				$andre_dietrich$parser_combinators$Combine$keep,
				$author$project$Ast$Expression$expression(ops),
				$author$project$Ast$Helpers$symbol("else")
			  ),
			  A2(
				$andre_dietrich$parser_combinators$Combine$andMap,
				A2(
				  $andre_dietrich$parser_combinators$Combine$keep,
				  $author$project$Ast$Expression$expression(ops),
				  $author$project$Ast$Helpers$symbol("then")
				),
				A2(
				  $andre_dietrich$parser_combinators$Combine$map,
				  $author$project$Ast$Expression$If,
				  A2(
					$andre_dietrich$parser_combinators$Combine$keep,
					$author$project$Ast$Expression$expression(ops),
					$author$project$Ast$Helpers$symbol("if")
				  )
				)
			  )
			)
		  );
		});
	  };
	  var $author$project$Ast$Expression$lambda = function(ops) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v8
		) {
		  return $author$project$Ast$Common$withMeta(
			A2(
			  $andre_dietrich$parser_combinators$Combine$andMap,
			  A2(
				$andre_dietrich$parser_combinators$Combine$keep,
				$author$project$Ast$Expression$expression(ops),
				$author$project$Ast$Helpers$symbol("->")
			  ),
			  A2(
				$andre_dietrich$parser_combinators$Combine$map,
				$author$project$Ast$Expression$Lambda,
				A2(
				  $andre_dietrich$parser_combinators$Combine$keep,
				  A2(
					$andre_dietrich$parser_combinators$Combine$andThen,
					A2(
					  $elm$core$Basics$composeL,
					  $andre_dietrich$parser_combinators$Combine$succeed,
					  $author$project$Ast$Pattern$applicationToList
					),
					$author$project$Ast$Pattern$pattern
				  ),
				  $author$project$Ast$Helpers$symbol("\\")
				)
			  )
			)
		  );
		});
	  };
	  var $author$project$Ast$Expression$letBinding = function(ops) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v7
		) {
		  return A2(
			$andre_dietrich$parser_combinators$Combine$andMap,
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $author$project$Ast$Expression$expression(ops),
			  $author$project$Ast$Helpers$symbol("=")
			),
			A2(
			  $andre_dietrich$parser_combinators$Combine$map,
			  $elm$core$Tuple$pair,
			  A2(
				$author$project$Ast$Helpers$between_,
				$andre_dietrich$parser_combinators$Combine$whitespace,
				$author$project$Ast$Pattern$pattern
			  )
			)
		  );
		});
	  };
	  var $author$project$Ast$Expression$letExpression = function(ops) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v6
		) {
		  return $author$project$Ast$Common$withMeta(
			A2(
			  $andre_dietrich$parser_combinators$Combine$andMap,
			  A2(
				$andre_dietrich$parser_combinators$Combine$keep,
				$author$project$Ast$Expression$expression(ops),
				$author$project$Ast$Helpers$symbol("in")
			  ),
			  A2(
				$andre_dietrich$parser_combinators$Combine$map,
				$author$project$Ast$Expression$Let,
				A2(
				  $andre_dietrich$parser_combinators$Combine$keep,
				  $andre_dietrich$parser_combinators$Combine$many1(
					$author$project$Ast$Expression$letBinding(ops)
				  ),
				  $author$project$Ast$Helpers$symbol_("let")
				)
			  )
			)
		  );
		});
	  };
	  var $author$project$Ast$Expression$list = function(ops) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v5
		) {
		  return $author$project$Ast$Common$withMeta(
			A2(
			  $andre_dietrich$parser_combinators$Combine$map,
			  $author$project$Ast$Expression$List,
			  $author$project$Ast$Helpers$listParser(
				$author$project$Ast$Expression$expression(ops)
			  )
			)
		  );
		});
	  };
	  var $author$project$Ast$Expression$next_ = function(ops) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$andThen,
		  function(o) {
			return A2(
			  $andre_dietrich$parser_combinators$Combine$andThen,
			  function(e) {
				if (e.$ === "Cont") {
				  var t = e.a;
				  return A2(
					$andre_dietrich$parser_combinators$Combine$map,
					$elm$core$List$cons(_Utils_Tuple2(o, t)),
					$author$project$Ast$Expression$successOrEmptyList(
					  $author$project$Ast$Expression$next_(ops)
					)
				  );
				} else {
				  var ex = e.a;
				  return $andre_dietrich$parser_combinators$Combine$succeed(
					_List_fromArray([_Utils_Tuple2(o, ex)])
				  );
				}
			  },
			  A2(
				$andre_dietrich$parser_combinators$Combine$or,
				A2(
				  $andre_dietrich$parser_combinators$Combine$map,
				  $author$project$Ast$Expression$Cont,
				  $author$project$Ast$Expression$application(ops)
				),
				A2(
				  $andre_dietrich$parser_combinators$Combine$map,
				  $author$project$Ast$Expression$Stop,
				  $author$project$Ast$Expression$expression(ops)
				)
			  )
			);
		  },
		  $author$project$Ast$Common$withMeta(
			A2(
			  $author$project$Ast$Helpers$between_,
			  $andre_dietrich$parser_combinators$Combine$whitespace,
			  $author$project$Ast$Helpers$operator
			)
		  )
		);
	  };
	  var $author$project$Ast$Expression$record = function(ops) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v3
		) {
		  return $author$project$Ast$Common$withMeta(
			A2(
			  $andre_dietrich$parser_combinators$Combine$map,
			  $author$project$Ast$Expression$Record,
			  $andre_dietrich$parser_combinators$Combine$braces(
				$author$project$Ast$Helpers$commaSeparated(
				  A2(
					$andre_dietrich$parser_combinators$Combine$andMap,
					A2(
					  $andre_dietrich$parser_combinators$Combine$keep,
					  $author$project$Ast$Expression$expression(ops),
					  $author$project$Ast$Helpers$symbol("=")
					),
					A2(
					  $andre_dietrich$parser_combinators$Combine$map,
					  $elm$core$Tuple$pair,
					  $author$project$Ast$Common$withMeta(
						$author$project$Ast$Helpers$loName
					  )
					)
				  )
				)
			  )
			)
		  );
		});
	  };
	  var $author$project$Ast$Expression$recordUpdate = function(ops) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v2
		) {
		  return $author$project$Ast$Common$withMeta(
			A2(
			  $andre_dietrich$parser_combinators$Combine$andMap,
			  A2(
				$andre_dietrich$parser_combinators$Combine$ignore,
				$andre_dietrich$parser_combinators$Combine$string("}"),
				A2(
				  $andre_dietrich$parser_combinators$Combine$keep,
				  $author$project$Ast$Helpers$commaSeparated(
					A2(
					  $andre_dietrich$parser_combinators$Combine$andMap,
					  A2(
						$andre_dietrich$parser_combinators$Combine$keep,
						$author$project$Ast$Expression$expression(ops),
						$author$project$Ast$Helpers$symbol("=")
					  ),
					  A2(
						$andre_dietrich$parser_combinators$Combine$map,
						$elm$core$Tuple$pair,
						$author$project$Ast$Common$withMeta(
						  $author$project$Ast$Helpers$loName
						)
					  )
					)
				  ),
				  $author$project$Ast$Helpers$symbol("|")
				)
			  ),
			  A2(
				$andre_dietrich$parser_combinators$Combine$map,
				$author$project$Ast$Expression$RecordUpdate,
				A2(
				  $andre_dietrich$parser_combinators$Combine$keep,
				  $author$project$Ast$Common$withMeta(
					$author$project$Ast$Helpers$loName
				  ),
				  $author$project$Ast$Helpers$symbol("{")
				)
			  )
			)
		  );
		});
	  };
	  var $author$project$Ast$Expression$term = function(ops) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v1
		) {
		  return $andre_dietrich$parser_combinators$Combine$choice(
			_List_fromArray([
			  $author$project$Ast$Expression$external,
			  $author$project$Ast$Expression$access,
			  $author$project$Ast$Expression$variable,
			  $author$project$Ast$Expression$constructor,
			  $author$project$Ast$Expression$accessFunction,
			  $author$project$Ast$Expression$string,
			  $author$project$Ast$Expression$float,
			  $author$project$Ast$Expression$integer,
			  $author$project$Ast$Expression$character,
			  $andre_dietrich$parser_combinators$Combine$parens(
				A2(
				  $author$project$Ast$Helpers$between_,
				  $andre_dietrich$parser_combinators$Combine$whitespace,
				  $author$project$Ast$Expression$expression(ops)
				)
			  ),
			  $author$project$Ast$Expression$list(ops),
			  $author$project$Ast$Expression$tuple(ops),
			  $author$project$Ast$Expression$recordUpdate(ops),
			  $author$project$Ast$Expression$record(ops),
			  $author$project$Ast$Expression$simplifiedRecord
			])
		  );
		});
	  };
	  var $author$project$Ast$Expression$tuple = function(ops) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v0
		) {
		  return $author$project$Ast$Common$withMeta(
			A2(
			  $andre_dietrich$parser_combinators$Combine$map,
			  $author$project$Ast$Expression$Tuple,
			  $author$project$Ast$Helpers$tupleParser(
				$author$project$Ast$Expression$expression(ops)
			  )
			)
		  );
		});
	  };
	  var $author$project$Ast$Statement$functionDeclaration = function(
		ops
	  ) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$andThen,
		  function(full) {
			var decl = full.a;
			_v0$2: while (true) {
			  if (decl.$ === "FunctionDeclaration") {
				switch (decl.a.a.$) {
				  case "PVariable":
					var _v1 = decl.a;
					return $andre_dietrich$parser_combinators$Combine$succeed(
					  full
					);
				  case "PApplication":
					var _v2 = decl.a;
					var _v3 = _v2.a;
					return $andre_dietrich$parser_combinators$Combine$succeed(
					  full
					);
				  default:
					break _v0$2;
				}
			  } else {
				break _v0$2;
			  }
			}
			return $andre_dietrich$parser_combinators$Combine$fail(
			  "wrong pattern in function declaration"
			);
		  },
		  $author$project$Ast$Common$withMeta(
			A2(
			  $andre_dietrich$parser_combinators$Combine$andMap,
			  A2(
				$andre_dietrich$parser_combinators$Combine$keep,
				$author$project$Ast$Expression$expression(ops),
				A2(
				  $andre_dietrich$parser_combinators$Combine$keep,
				  $andre_dietrich$parser_combinators$Combine$whitespace,
				  $author$project$Ast$Helpers$symbol("=")
				)
			  ),
			  A2(
				$andre_dietrich$parser_combinators$Combine$map,
				$author$project$Ast$Statement$FunctionDeclaration,
				$author$project$Ast$Pattern$pattern
			  )
			)
		  )
		);
	  };
	  var $author$project$Ast$Statement$FunctionTypeDeclaration = F2(
		function(a, b) {
		  return { $: "FunctionTypeDeclaration", a: a, b: b };
		}
	  );
	  var $author$project$Ast$Statement$TypeConstructor = F2(function(
		a,
		b
	  ) {
		return { $: "TypeConstructor", a: a, b: b };
	  });
	  var $author$project$Ast$Statement$TypeRecord = function(a) {
		return { $: "TypeRecord", a: a };
	  };
	  var $author$project$Ast$Statement$TypeRecordConstructor = F2(function(
		a,
		b
	  ) {
		return { $: "TypeRecordConstructor", a: a, b: b };
	  });
	  var $author$project$Ast$Statement$TypeTuple = function(a) {
		return { $: "TypeTuple", a: a };
	  };
	  var $andre_dietrich$parser_combinators$Combine$chainr = F2(function(
		op,
		p
	  ) {
		var accumulate = function(x) {
		  return A2(
			$andre_dietrich$parser_combinators$Combine$or,
			A2(
			  $andre_dietrich$parser_combinators$Combine$andThen,
			  function(f) {
				return A2(
				  $andre_dietrich$parser_combinators$Combine$andThen,
				  function(y) {
					return $andre_dietrich$parser_combinators$Combine$succeed(
					  A2(f, x, y)
					);
				  },
				  A2(
					$andre_dietrich$parser_combinators$Combine$andThen,
					accumulate,
					p
				  )
				);
			  },
			  op
			),
			$andre_dietrich$parser_combinators$Combine$succeed(x)
		  );
		};
		return A2(
		  $andre_dietrich$parser_combinators$Combine$andThen,
		  accumulate,
		  p
		);
	  });
	  var $author$project$Ast$Statement$TypeApplication = F2(function(
		a,
		b
	  ) {
		return { $: "TypeApplication", a: a, b: b };
	  });
	  var $author$project$Ast$Statement$typeApplication = A2(
		$andre_dietrich$parser_combinators$Combine$onsuccess,
		$author$project$Ast$Statement$TypeApplication,
		$author$project$Ast$Helpers$symbol("->")
	  );
	  var $author$project$Ast$Statement$typeConstant = A2(
		$andre_dietrich$parser_combinators$Combine$andMap,
		$andre_dietrich$parser_combinators$Combine$succeed(_List_Nil),
		A2(
		  $andre_dietrich$parser_combinators$Combine$map,
		  $author$project$Ast$Statement$TypeConstructor,
		  A2(
			$andre_dietrich$parser_combinators$Combine$sepBy1,
			$andre_dietrich$parser_combinators$Combine$string("."),
			$author$project$Ast$Helpers$upName
		  )
		)
	  );
	  var $author$project$Ast$Statement$TypeVariable = function(a) {
		return { $: "TypeVariable", a: a };
	  };
	  var $author$project$Ast$Statement$typeVariable = A2(
		$andre_dietrich$parser_combinators$Combine$map,
		$author$project$Ast$Statement$TypeVariable,
		$andre_dietrich$parser_combinators$Combine$regex("[a-z]+(\\w|_)*")
	  );
	  function $author$project$Ast$Statement$cyclic$typeAnnotation() {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v8
		) {
		  return A2(
			$andre_dietrich$parser_combinators$Combine$chainr,
			$author$project$Ast$Statement$typeApplication,
			$author$project$Ast$Statement$cyclic$type_()
		  );
		});
	  }
	  function $author$project$Ast$Statement$cyclic$typeConstructor() {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v7
		) {
		  return A2(
			$andre_dietrich$parser_combinators$Combine$andMap,
			$andre_dietrich$parser_combinators$Combine$many(
			  $author$project$Ast$Statement$cyclic$typeParameter()
			),
			A2(
			  $andre_dietrich$parser_combinators$Combine$map,
			  $author$project$Ast$Statement$TypeConstructor,
			  A2(
				$andre_dietrich$parser_combinators$Combine$sepBy1,
				$andre_dietrich$parser_combinators$Combine$string("."),
				$author$project$Ast$Helpers$upName
			  )
			)
		  );
		});
	  }
	  function $author$project$Ast$Statement$cyclic$typeParameter() {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v6
		) {
		  return A2(
			$author$project$Ast$Helpers$between_,
			A2(
			  $andre_dietrich$parser_combinators$Combine$or,
			  A2(
				$andre_dietrich$parser_combinators$Combine$keep,
				$author$project$Ast$Helpers$spaces_,
				A2(
				  $andre_dietrich$parser_combinators$Combine$keep,
				  $andre_dietrich$parser_combinators$Combine$Char$newline,
				  $author$project$Ast$Helpers$spaces
				)
			  ),
			  $author$project$Ast$Helpers$spaces
			),
			$andre_dietrich$parser_combinators$Combine$choice(
			  _List_fromArray([
				$author$project$Ast$Statement$typeVariable,
				$author$project$Ast$Statement$typeConstant,
				$author$project$Ast$Statement$cyclic$typeRecordConstructor(),
				$author$project$Ast$Statement$cyclic$typeRecord(),
				$author$project$Ast$Statement$cyclic$typeTuple(),
				$andre_dietrich$parser_combinators$Combine$parens(
				  $author$project$Ast$Statement$cyclic$typeAnnotation()
				)
			  ])
			)
		  );
		});
	  }
	  function $author$project$Ast$Statement$cyclic$typeRecord() {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v5
		) {
		  return $andre_dietrich$parser_combinators$Combine$braces(
			A2(
			  $andre_dietrich$parser_combinators$Combine$map,
			  $author$project$Ast$Statement$TypeRecord,
			  $author$project$Ast$Statement$cyclic$typeRecordPairs()
			)
		  );
		});
	  }
	  function $author$project$Ast$Statement$cyclic$typeRecordConstructor() {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v4
		) {
		  return $andre_dietrich$parser_combinators$Combine$braces(
			A2(
			  $andre_dietrich$parser_combinators$Combine$andMap,
			  A2(
				$andre_dietrich$parser_combinators$Combine$ignore,
				$author$project$Ast$Helpers$symbol("|"),
				$author$project$Ast$Statement$cyclic$typeRecordPairs()
			  ),
			  A2(
				$andre_dietrich$parser_combinators$Combine$map,
				$author$project$Ast$Statement$TypeRecordConstructor,
				A2(
				  $author$project$Ast$Helpers$between_,
				  $author$project$Ast$Helpers$spaces,
				  $author$project$Ast$Statement$typeVariable
				)
			  )
			)
		  );
		});
	  }
	  function $author$project$Ast$Statement$cyclic$typeRecordPair() {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v3
		) {
		  return A2(
			$andre_dietrich$parser_combinators$Combine$andMap,
			$author$project$Ast$Statement$cyclic$typeAnnotation(),
			A2(
			  $andre_dietrich$parser_combinators$Combine$map,
			  $elm$core$Tuple$pair,
			  A2(
				$andre_dietrich$parser_combinators$Combine$ignore,
				$author$project$Ast$Helpers$symbol(":"),
				$author$project$Ast$Helpers$loName
			  )
			)
		  );
		});
	  }
	  function $author$project$Ast$Statement$cyclic$typeRecordPairs() {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v2
		) {
		  return $author$project$Ast$Helpers$commaSeparated_(
			$author$project$Ast$Statement$cyclic$typeRecordPair()
		  );
		});
	  }
	  function $author$project$Ast$Statement$cyclic$typeTuple() {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v1
		) {
		  return A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$author$project$Ast$Statement$TypeTuple,
			$andre_dietrich$parser_combinators$Combine$parens(
			  $author$project$Ast$Helpers$commaSeparated_(
				$author$project$Ast$Statement$cyclic$type_()
			  )
			)
		  );
		});
	  }
	  function $author$project$Ast$Statement$cyclic$type_() {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v0
		) {
		  return A2(
			$author$project$Ast$Helpers$between_,
			$author$project$Ast$Helpers$spaces,
			$andre_dietrich$parser_combinators$Combine$choice(
			  _List_fromArray([
				$author$project$Ast$Statement$cyclic$typeConstructor(),
				$author$project$Ast$Statement$typeVariable,
				$author$project$Ast$Statement$cyclic$typeRecordConstructor(),
				$author$project$Ast$Statement$cyclic$typeRecord(),
				$author$project$Ast$Statement$cyclic$typeTuple(),
				$andre_dietrich$parser_combinators$Combine$parens(
				  $author$project$Ast$Statement$cyclic$typeAnnotation()
				)
			  ])
			)
		  );
		});
	  }
	  try {
		var $author$project$Ast$Statement$typeAnnotation = $author$project$Ast$Statement$cyclic$typeAnnotation();
		$author$project$Ast$Statement$cyclic$typeAnnotation = function() {
		  return $author$project$Ast$Statement$typeAnnotation;
		};
		var $author$project$Ast$Statement$typeConstructor = $author$project$Ast$Statement$cyclic$typeConstructor();
		$author$project$Ast$Statement$cyclic$typeConstructor = function() {
		  return $author$project$Ast$Statement$typeConstructor;
		};
		var $author$project$Ast$Statement$typeParameter = $author$project$Ast$Statement$cyclic$typeParameter();
		$author$project$Ast$Statement$cyclic$typeParameter = function() {
		  return $author$project$Ast$Statement$typeParameter;
		};
		var $author$project$Ast$Statement$typeRecord = $author$project$Ast$Statement$cyclic$typeRecord();
		$author$project$Ast$Statement$cyclic$typeRecord = function() {
		  return $author$project$Ast$Statement$typeRecord;
		};
		var $author$project$Ast$Statement$typeRecordConstructor = $author$project$Ast$Statement$cyclic$typeRecordConstructor();
		$author$project$Ast$Statement$cyclic$typeRecordConstructor = function() {
		  return $author$project$Ast$Statement$typeRecordConstructor;
		};
		var $author$project$Ast$Statement$typeRecordPair = $author$project$Ast$Statement$cyclic$typeRecordPair();
		$author$project$Ast$Statement$cyclic$typeRecordPair = function() {
		  return $author$project$Ast$Statement$typeRecordPair;
		};
		var $author$project$Ast$Statement$typeRecordPairs = $author$project$Ast$Statement$cyclic$typeRecordPairs();
		$author$project$Ast$Statement$cyclic$typeRecordPairs = function() {
		  return $author$project$Ast$Statement$typeRecordPairs;
		};
		var $author$project$Ast$Statement$typeTuple = $author$project$Ast$Statement$cyclic$typeTuple();
		$author$project$Ast$Statement$cyclic$typeTuple = function() {
		  return $author$project$Ast$Statement$typeTuple;
		};
		var $author$project$Ast$Statement$type_ = $author$project$Ast$Statement$cyclic$type_();
		$author$project$Ast$Statement$cyclic$type_ = function() {
		  return $author$project$Ast$Statement$type_;
		};
	  } catch ($) {
		throw "Some top-level definitions from `Ast.Statement` are causing infinite recursion:\n\n  ┌─────┐\n  │    typeAnnotation\n  │     ↓\n  │    typeConstructor\n  │     ↓\n  │    typeParameter\n  │     ↓\n  │    typeRecord\n  │     ↓\n  │    typeRecordConstructor\n  │     ↓\n  │    typeRecordPair\n  │     ↓\n  │    typeRecordPairs\n  │     ↓\n  │    typeTuple\n  │     ↓\n  │    type_\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!";
	  }
	  var $author$project$Ast$Statement$functionTypeDeclaration = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  $author$project$Ast$Statement$typeAnnotation,
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$author$project$Ast$Statement$FunctionTypeDeclaration,
			A2(
			  $andre_dietrich$parser_combinators$Combine$ignore,
			  $author$project$Ast$Helpers$symbol(":"),
			  $author$project$Ast$Helpers$funName
			)
		  )
		)
	  );
	  var $author$project$Ast$Statement$ImportStatement = F3(function(
		a,
		b,
		c
	  ) {
		return { $: "ImportStatement", a: a, b: b, c: c };
	  });
	  var $author$project$Ast$Statement$importStatement = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  $andre_dietrich$parser_combinators$Combine$maybe(
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $author$project$Ast$Statement$exports,
			  $author$project$Ast$Helpers$symbol("exposing")
			)
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$andMap,
			$andre_dietrich$parser_combinators$Combine$maybe(
			  A2(
				$andre_dietrich$parser_combinators$Combine$keep,
				$author$project$Ast$Helpers$upName,
				$author$project$Ast$Helpers$symbol("as")
			  )
			),
			A2(
			  $andre_dietrich$parser_combinators$Combine$map,
			  $author$project$Ast$Statement$ImportStatement,
			  A2(
				$andre_dietrich$parser_combinators$Combine$keep,
				$author$project$Ast$Helpers$moduleName,
				$author$project$Ast$Helpers$initialSymbol("import")
			  )
			)
		  )
		)
	  );
	  var $author$project$Ast$Statement$InfixDeclaration = F3(function(
		a,
		b,
		c
	  ) {
		return { $: "InfixDeclaration", a: a, b: b, c: c };
	  });
	  var $author$project$Ast$Statement$infixDeclaration = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  A2(
			$andre_dietrich$parser_combinators$Combine$keep,
			A2(
			  $andre_dietrich$parser_combinators$Combine$or,
			  $author$project$Ast$Helpers$loName,
			  $author$project$Ast$Helpers$operator
			),
			$author$project$Ast$Helpers$spaces
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$andMap,
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $andre_dietrich$parser_combinators$Combine$Num$int,
			  $author$project$Ast$Helpers$spaces
			),
			A2(
			  $andre_dietrich$parser_combinators$Combine$map,
			  $author$project$Ast$Statement$InfixDeclaration,
			  $andre_dietrich$parser_combinators$Combine$choice(
				_List_fromArray([
				  A2(
					$andre_dietrich$parser_combinators$Combine$onsuccess,
					$author$project$Ast$BinOp$L,
					$author$project$Ast$Helpers$initialSymbol("infixl")
				  ),
				  A2(
					$andre_dietrich$parser_combinators$Combine$onsuccess,
					$author$project$Ast$BinOp$R,
					$author$project$Ast$Helpers$initialSymbol("infixr")
				  ),
				  A2(
					$andre_dietrich$parser_combinators$Combine$onsuccess,
					$author$project$Ast$BinOp$N,
					$author$project$Ast$Helpers$initialSymbol("infix")
				  )
				])
			  )
			)
		  )
		)
	  );
	  var $author$project$Ast$Statement$ModuleDeclaration = F2(function(
		a,
		b
	  ) {
		return { $: "ModuleDeclaration", a: a, b: b };
	  });
	  var $author$project$Ast$Statement$moduleDeclaration = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  A2(
			$andre_dietrich$parser_combinators$Combine$keep,
			$author$project$Ast$Statement$exports,
			$author$project$Ast$Helpers$symbol("exposing")
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$author$project$Ast$Statement$ModuleDeclaration,
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $author$project$Ast$Helpers$moduleName,
			  $author$project$Ast$Helpers$initialSymbol("module")
			)
		  )
		)
	  );
	  var $author$project$Ast$Statement$PortDeclaration = F3(function(
		a,
		b,
		c
	  ) {
		return { $: "PortDeclaration", a: a, b: b, c: c };
	  });
	  var $author$project$Ast$Statement$portDeclaration = function(ops) {
		return $author$project$Ast$Common$withMeta(
		  A2(
			$andre_dietrich$parser_combinators$Combine$andMap,
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $author$project$Ast$Expression$expression(ops),
			  $author$project$Ast$Helpers$symbol("=")
			),
			A2(
			  $andre_dietrich$parser_combinators$Combine$andMap,
			  $andre_dietrich$parser_combinators$Combine$many(
				A2(
				  $author$project$Ast$Helpers$between_,
				  $author$project$Ast$Helpers$spaces,
				  $author$project$Ast$Helpers$loName
				)
			  ),
			  A2(
				$andre_dietrich$parser_combinators$Combine$map,
				$author$project$Ast$Statement$PortDeclaration,
				A2(
				  $andre_dietrich$parser_combinators$Combine$keep,
				  $author$project$Ast$Helpers$loName,
				  $author$project$Ast$Helpers$initialSymbol("port")
				)
			  )
			)
		  )
		);
	  };
	  var $author$project$Ast$Statement$PortModuleDeclaration = F2(function(
		a,
		b
	  ) {
		return { $: "PortModuleDeclaration", a: a, b: b };
	  });
	  var $author$project$Ast$Statement$portModuleDeclaration = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  A2(
			$andre_dietrich$parser_combinators$Combine$keep,
			$author$project$Ast$Statement$exports,
			$author$project$Ast$Helpers$symbol("exposing")
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$author$project$Ast$Statement$PortModuleDeclaration,
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $author$project$Ast$Helpers$moduleName,
			  A2(
				$andre_dietrich$parser_combinators$Combine$keep,
				$author$project$Ast$Helpers$symbol("module"),
				$author$project$Ast$Helpers$initialSymbol("port")
			  )
			)
		  )
		)
	  );
	  var $author$project$Ast$Statement$PortTypeDeclaration = F2(function(
		a,
		b
	  ) {
		return { $: "PortTypeDeclaration", a: a, b: b };
	  });
	  var $author$project$Ast$Statement$portTypeDeclaration = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  A2(
			$andre_dietrich$parser_combinators$Combine$keep,
			$author$project$Ast$Statement$typeAnnotation,
			$author$project$Ast$Helpers$symbol(":")
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$author$project$Ast$Statement$PortTypeDeclaration,
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $author$project$Ast$Helpers$loName,
			  $author$project$Ast$Helpers$initialSymbol("port")
			)
		  )
		)
	  );
	  var $author$project$Ast$Statement$TypeAliasDeclaration = F2(function(
		a,
		b
	  ) {
		return { $: "TypeAliasDeclaration", a: a, b: b };
	  });
	  var $author$project$Ast$Statement$typeAliasDeclaration = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  A2(
			$andre_dietrich$parser_combinators$Combine$keep,
			$author$project$Ast$Statement$typeAnnotation,
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $author$project$Ast$Helpers$symbol("="),
			  $andre_dietrich$parser_combinators$Combine$whitespace
			)
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$author$project$Ast$Statement$TypeAliasDeclaration,
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $author$project$Ast$Statement$type_,
			  A2(
				$andre_dietrich$parser_combinators$Combine$keep,
				$author$project$Ast$Helpers$symbol("alias"),
				$author$project$Ast$Helpers$initialSymbol("type")
			  )
			)
		  )
		)
	  );
	  var $author$project$Ast$Statement$TypeDeclaration = F2(function(
		a,
		b
	  ) {
		return { $: "TypeDeclaration", a: a, b: b };
	  });
	  var $author$project$Ast$Statement$typeDeclaration = $author$project$Ast$Common$withMeta(
		A2(
		  $andre_dietrich$parser_combinators$Combine$andMap,
		  A2(
			$andre_dietrich$parser_combinators$Combine$keep,
			A2(
			  $andre_dietrich$parser_combinators$Combine$sepBy1,
			  $author$project$Ast$Helpers$symbol("|"),
			  A2(
				$author$project$Ast$Helpers$between_,
				$andre_dietrich$parser_combinators$Combine$whitespace,
				$author$project$Ast$Statement$typeConstructor
			  )
			),
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $author$project$Ast$Helpers$symbol("="),
			  $andre_dietrich$parser_combinators$Combine$whitespace
			)
		  ),
		  A2(
			$andre_dietrich$parser_combinators$Combine$map,
			$author$project$Ast$Statement$TypeDeclaration,
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $author$project$Ast$Statement$type_,
			  $author$project$Ast$Helpers$initialSymbol("type")
			)
		  )
		)
	  );
	  var $author$project$Ast$Statement$statement = function(ops) {
		return $andre_dietrich$parser_combinators$Combine$lazy(function(
		  _v0
		) {
		  return $andre_dietrich$parser_combinators$Combine$choice(
			_List_fromArray([
			  $author$project$Ast$Statement$portModuleDeclaration,
			  $author$project$Ast$Statement$effectModuleDeclaration,
			  $author$project$Ast$Statement$moduleDeclaration,
			  $author$project$Ast$Statement$importStatement,
			  $author$project$Ast$Statement$typeAliasDeclaration,
			  $author$project$Ast$Statement$typeDeclaration,
			  $author$project$Ast$Statement$portTypeDeclaration,
			  $author$project$Ast$Statement$portDeclaration(ops),
			  $author$project$Ast$Statement$functionTypeDeclaration,
			  $author$project$Ast$Statement$functionDeclaration(ops),
			  $author$project$Ast$Statement$infixDeclaration,
			  $author$project$Ast$Statement$comment
			])
		  );
		});
	  };
	  var $author$project$Ast$Statement$statements = function(ops) {
		return A2(
		  $andre_dietrich$parser_combinators$Combine$manyTill,
		  A2(
			$andre_dietrich$parser_combinators$Combine$ignore,
			A2(
			  $andre_dietrich$parser_combinators$Combine$or,
			  $andre_dietrich$parser_combinators$Combine$whitespace,
			  $author$project$Ast$Helpers$spaces
			),
			A2(
			  $andre_dietrich$parser_combinators$Combine$keep,
			  $author$project$Ast$Statement$statement(ops),
			  A2(
				$andre_dietrich$parser_combinators$Combine$or,
				$andre_dietrich$parser_combinators$Combine$whitespace,
				$author$project$Ast$Helpers$spaces
			  )
			)
		  ),
		  $andre_dietrich$parser_combinators$Combine$end
		);
	  };
	  var $author$project$Ast$parseModule = function(ops) {
		return $andre_dietrich$parser_combinators$Combine$parse(
		  $author$project$Ast$Statement$statements(ops)
		);
	  };
	  var $author$project$Ast$Statement$infixStatements = (function() {
		var statements_ = A2(
		  $andre_dietrich$parser_combinators$Combine$ignore,
		  $andre_dietrich$parser_combinators$Combine$end,
		  $andre_dietrich$parser_combinators$Combine$many(
			A2(
			  $andre_dietrich$parser_combinators$Combine$ignore,
			  $andre_dietrich$parser_combinators$Combine$whitespace,
			  $andre_dietrich$parser_combinators$Combine$choice(
				_List_fromArray([
				  A2(
					$andre_dietrich$parser_combinators$Combine$map,
					$elm$core$Maybe$Just,
					$author$project$Ast$Statement$infixDeclaration
				  ),
				  A2(
					$andre_dietrich$parser_combinators$Combine$onsuccess,
					$elm$core$Maybe$Nothing,
					$andre_dietrich$parser_combinators$Combine$regex(".*")
				  )
				])
			  )
			)
		  )
		);
		return A2(
		  $andre_dietrich$parser_combinators$Combine$andThen,
		  function(xs) {
			return $andre_dietrich$parser_combinators$Combine$succeed(
			  A2($elm$core$List$filterMap, $elm$core$Basics$identity, xs)
			);
		  },
		  statements_
		);
	  })();
	  var $elm$core$Debug$todo = _Debug_todo;
	  var $author$project$Ast$Statement$opTable = function(ops) {
		var collect = F2(function(_v1, d) {
		  var s = _v1.a;
		  if (s.$ === "InfixDeclaration") {
			var a = s.a;
			var l = s.b;
			var n = s.c;
			return A3($elm$core$Dict$insert, n, _Utils_Tuple2(a, l), d);
		  } else {
			return _Debug_todo("Ast.Statement", {
			  start: { line: 449, column: 21 },
			  end: { line: 449, column: 31 }
			})("impossible");
		  }
		});
		return A2(
		  $andre_dietrich$parser_combinators$Combine$andThen,
		  function(xs) {
			return $andre_dietrich$parser_combinators$Combine$succeed(
			  A3($elm$core$List$foldr, collect, ops, xs)
			);
		  },
		  $author$project$Ast$Statement$infixStatements
		);
	  };
	  var $author$project$Ast$parseOpTable = function(ops) {
		return $andre_dietrich$parser_combinators$Combine$parse(
		  $author$project$Ast$Statement$opTable(ops)
		);
	  };
	  var $author$project$Ast$parse = function(input) {
		var _v0 = A2(
		  $author$project$Ast$parseOpTable,
		  $author$project$Ast$BinOp$operators,
		  input
		);
		if (_v0.$ === "Ok") {
		  var _v1 = _v0.a;
		  var state = _v1.a;
		  var stream = _v1.b;
		  var ops = _v1.c;
		  return A2($author$project$Ast$parseModule, ops, input);
		} else {
		  var e = _v0.a;
		  return $elm$core$Result$Err(e);
		}
	  };
	  var $elm$html$Html$li = _VirtualDom_node("li");
	  var $elm$html$Html$pre = _VirtualDom_node("pre");
	  var $elm$core$Debug$toString = _Debug_toString;
	  var $elm$html$Html$ul = _VirtualDom_node("ul");
	  var $author$project$Main$withChild = F2(function(title, children) {
		return A2(
		  $elm$html$Html$li,
		  _List_Nil,
		  _List_fromArray([
			A2(
			  $elm$html$Html$pre,
			  _List_Nil,
			  _List_fromArray([
				$elm$html$Html$text($elm$core$Debug$toString(title))
			  ])
			),
			A2($elm$html$Html$ul, _List_Nil, children)
		  ])
		);
	  });
	  var $author$project$Main$expression = function(e) {
		var _v0 = e.a;
		switch (_v0.$) {
		  case "List":
			var es = _v0.a;
			return A2(
			  $author$project$Main$withChild,
			  e,
			  A2($elm$core$List$map, $author$project$Main$expression, es)
			);
		  case "Application":
			var e1 = _v0.a;
			var e2 = _v0.b;
			return A2(
			  $author$project$Main$withChild,
			  e,
			  _List_fromArray([
				$author$project$Main$expression(e1),
				$author$project$Main$expression(e2)
			  ])
			);
		  default:
			var e_ = _v0;
			return A2(
			  $elm$html$Html$li,
			  _List_Nil,
			  _List_fromArray([
				A2(
				  $elm$html$Html$pre,
				  _List_Nil,
				  _List_fromArray([
					$elm$html$Html$text($elm$core$Debug$toString(e))
				  ])
				)
			  ])
			);
		}
	  };
	  var $author$project$Main$statement = function(_v0) {
		var s = _v0.a;
		if (s.$ === "FunctionDeclaration") {
		  var e = s.b;
		  return A2(
			$author$project$Main$withChild,
			s,
			_List_fromArray([$author$project$Main$expression(e)])
		  );
		} else {
		  var s_ = s;
		  return A2(
			$elm$html$Html$li,
			_List_Nil,
			_List_fromArray([
			  A2(
				$elm$html$Html$pre,
				_List_Nil,
				_List_fromArray([
				  $elm$html$Html$text($elm$core$Debug$toString(s))
				])
			  )
			])
		  );
		}
	  };
	  var $author$project$Main$tree = function(m) {
		var _v0 = $author$project$Ast$parse(m);
		if (_v0.$ === "Ok") {
		  var _v1 = _v0.a;
		  var statements = _v1.c;
		  return A2(
			$elm$html$Html$ul,
			_List_Nil,
			A2(
			  $elm$core$List$map,
			  $author$project$Main$statement,
			  statements
			)
		  );
		} else {
		  var err = _v0;
		  return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray([
			  $elm$html$Html$text($elm$core$Debug$toString(err))
			])
		  );
		}
	  };
	  var $author$project$Main$view = function(model) {
		return A2(
		  $elm$html$Html$div,
		  _List_Nil,
		  _List_fromArray([
			A2(
			  $elm$html$Html$textarea,
			  _List_fromArray([
				A2(
				  $elm$html$Html$Events$on,
				  "input",
				  A2(
					$elm$json$Json$Decode$map,
					$author$project$Main$Replace,
					$elm$html$Html$Events$targetValue
				  )
				)
			  ]),
			  _List_fromArray([$elm$html$Html$text(model)])
			),
			$author$project$Main$tree(model)
		  ])
		);
	  };
	  var $author$project$Main$main = $elm$browser$Browser$sandbox({
		init: $author$project$Main$init,
		update: $author$project$Main$update,
		view: $author$project$Main$view
	  });
	  _Platform_export({
		Main: {
		  init: $author$project$Main$main(
			$elm$json$Json$Decode$succeed(_Utils_Tuple0)
		  )(0)
		}
	  });
	})(this);

	var app = Elm.Main.init({ node: document.getElementById("elm") });
  } catch (e) {
	// display initialization errors (e.g. bad flags, infinite recursion)
	var header = document.createElement("h1");
	header.style.fontFamily = "monospace";
	header.innerText = "Initialization Error";
	var pre = document.getElementById("elm");
	document.body.insertBefore(header, pre);
	pre.innerText = e;
	throw e;
  }
