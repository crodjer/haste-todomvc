// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = new F(f);
}

function F(f) {
    this.f = f;
}

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f instanceof F) {
            var f = t.f.f;
            t.f = 0;
            t.f = f();
        }
        return t.f;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f = 0;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
function imul(a, b) {
  // ignore high a * high a as the result will always be truncated
  var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
  var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
  var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
  return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['b']['i8'] = 'U'.charCodeAt(0);
    le['b']['i8'] = 'T'.charCodeAt(0);
    le['b']['i8'] = 'F'.charCodeAt(0);
    le['b']['i8'] = '-'.charCodeAt(0);
    le['b']['i8'] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;        
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsQuerySelector(elem, query) {
  if (!elem || typeof elem.querySelector !== 'function') {
    return [0];
  }

  var e = elem.querySelector(query);
  return e ? [1, [0, e]] : [0];
}

function jsQuerySelectorAll(elem, query) {
  var els = [0],
      len, nl, i;

  if (!elem || typeof elem.querySelectorAll !== 'function') {
    return els;
  }

  nl = elem.querySelectorAll(query);
  len = nl.length;

  for (i=len-1; i >= 0; --i) {
    els = [1, [0, nl[i]], els];
  }

  return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=[0],_1=function(_2,_3){var _4=E(_2);return _4[0]==0?E(_3):[1,_4[1],new T(function(){return B(_1(_4[2],_3));})];},_5=0,_6=function(_7,_){while(1){var _8=E(_7);if(!_8[0]){return _5;}else{var _9=_8[2],_a=E(_8[1]);switch(_a[0]){case 0:var _b=B(A(_a[1],[_])),_c=_b;_7=B(_1(_9,[1,_c,_0]));continue;case 1:_7=B(_1(_9,_a[1]));continue;default:_7=_9;continue;}}}},_d=function(_e,_f,_){var _g=E(_e);switch(_g[0]){case 0:var _h=B(A(_g[1],[_])),_i=_h;return new F(function(){return _6(B(_1(_f,[1,_i,_0])),_);});break;case 1:return new F(function(){return _6(B(_1(_f,_g[1])),_);});break;default:return new F(function(){return _6(_f,_);});}},_j=new T(function(){return B(unCStr(", "));}),_k=new T(function(){return B(unCStr("identifier = "));}),_l=new T(function(){return B(unCStr("Todo {"));}),_m=[0,125],_n=new T(function(){return B(unCStr("completed = "));}),_o=new T(function(){return B(unCStr("task = "));}),_p=[0,34],_q=function(_r){while(1){var _s=E(_r);if(!_s[0]){_r=[1,I_fromInt(_s[1])];continue;}else{return new F(function(){return I_toString(_s[1]);});}}},_t=function(_u,_v){return new F(function(){return _1(fromJSStr(B(_q(_u))),_v);});},_w=function(_x,_y){var _z=E(_x);if(!_z[0]){var _A=_z[1],_B=E(_y);return _B[0]==0?_A<_B[1]:I_compareInt(_B[1],_A)>0;}else{var _C=_z[1],_D=E(_y);return _D[0]==0?I_compareInt(_C,_D[1])<0:I_compare(_C,_D[1])<0;}},_E=[0,41],_F=[0,40],_G=[0,0],_H=function(_I,_J,_K){return _I<=6?B(_t(_J,_K)):!B(_w(_J,_G))?B(_t(_J,_K)):[1,_F,new T(function(){return B(_1(fromJSStr(B(_q(_J))),[1,_E,_K]));})];},_L=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_M=new T(function(){return B(err(_L));}),_N=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_O=new T(function(){return B(err(_N));}),_P=function(_Q,_R){while(1){var _S=E(_Q);if(!_S[0]){return E(_O);}else{var _T=E(_R);if(!_T){return E(_S[1]);}else{_Q=_S[2];_R=_T-1|0;continue;}}}},_U=new T(function(){return B(unCStr("ACK"));}),_V=new T(function(){return B(unCStr("BEL"));}),_W=new T(function(){return B(unCStr("BS"));}),_X=new T(function(){return B(unCStr("SP"));}),_Y=[1,_X,_0],_Z=new T(function(){return B(unCStr("US"));}),_10=[1,_Z,_Y],_11=new T(function(){return B(unCStr("RS"));}),_12=[1,_11,_10],_13=new T(function(){return B(unCStr("GS"));}),_14=[1,_13,_12],_15=new T(function(){return B(unCStr("FS"));}),_16=[1,_15,_14],_17=new T(function(){return B(unCStr("ESC"));}),_18=[1,_17,_16],_19=new T(function(){return B(unCStr("SUB"));}),_1a=[1,_19,_18],_1b=new T(function(){return B(unCStr("EM"));}),_1c=[1,_1b,_1a],_1d=new T(function(){return B(unCStr("CAN"));}),_1e=[1,_1d,_1c],_1f=new T(function(){return B(unCStr("ETB"));}),_1g=[1,_1f,_1e],_1h=new T(function(){return B(unCStr("SYN"));}),_1i=[1,_1h,_1g],_1j=new T(function(){return B(unCStr("NAK"));}),_1k=[1,_1j,_1i],_1l=new T(function(){return B(unCStr("DC4"));}),_1m=[1,_1l,_1k],_1n=new T(function(){return B(unCStr("DC3"));}),_1o=[1,_1n,_1m],_1p=new T(function(){return B(unCStr("DC2"));}),_1q=[1,_1p,_1o],_1r=new T(function(){return B(unCStr("DC1"));}),_1s=[1,_1r,_1q],_1t=new T(function(){return B(unCStr("DLE"));}),_1u=[1,_1t,_1s],_1v=new T(function(){return B(unCStr("SI"));}),_1w=[1,_1v,_1u],_1x=new T(function(){return B(unCStr("SO"));}),_1y=[1,_1x,_1w],_1z=new T(function(){return B(unCStr("CR"));}),_1A=[1,_1z,_1y],_1B=new T(function(){return B(unCStr("FF"));}),_1C=[1,_1B,_1A],_1D=new T(function(){return B(unCStr("VT"));}),_1E=[1,_1D,_1C],_1F=new T(function(){return B(unCStr("LF"));}),_1G=[1,_1F,_1E],_1H=new T(function(){return B(unCStr("HT"));}),_1I=[1,_1H,_1G],_1J=[1,_W,_1I],_1K=[1,_V,_1J],_1L=[1,_U,_1K],_1M=new T(function(){return B(unCStr("ENQ"));}),_1N=[1,_1M,_1L],_1O=new T(function(){return B(unCStr("EOT"));}),_1P=[1,_1O,_1N],_1Q=new T(function(){return B(unCStr("ETX"));}),_1R=[1,_1Q,_1P],_1S=new T(function(){return B(unCStr("STX"));}),_1T=[1,_1S,_1R],_1U=new T(function(){return B(unCStr("SOH"));}),_1V=[1,_1U,_1T],_1W=new T(function(){return B(unCStr("NUL"));}),_1X=[1,_1W,_1V],_1Y=[0,92],_1Z=new T(function(){return B(unCStr("\\DEL"));}),_20=new T(function(){return B(unCStr("\\a"));}),_21=new T(function(){return B(unCStr("\\\\"));}),_22=new T(function(){return B(unCStr("\\SO"));}),_23=new T(function(){return B(unCStr("\\r"));}),_24=new T(function(){return B(unCStr("\\f"));}),_25=new T(function(){return B(unCStr("\\v"));}),_26=new T(function(){return B(unCStr("\\n"));}),_27=new T(function(){return B(unCStr("\\t"));}),_28=new T(function(){return B(unCStr("\\b"));}),_29=function(_2a,_2b){if(_2a<=127){var _2c=E(_2a);switch(_2c){case 92:return new F(function(){return _1(_21,_2b);});break;case 127:return new F(function(){return _1(_1Z,_2b);});break;default:if(_2c<32){var _2d=E(_2c);switch(_2d){case 7:return new F(function(){return _1(_20,_2b);});break;case 8:return new F(function(){return _1(_28,_2b);});break;case 9:return new F(function(){return _1(_27,_2b);});break;case 10:return new F(function(){return _1(_26,_2b);});break;case 11:return new F(function(){return _1(_25,_2b);});break;case 12:return new F(function(){return _1(_24,_2b);});break;case 13:return new F(function(){return _1(_23,_2b);});break;case 14:return new F(function(){return _1(_22,new T(function(){var _2e=E(_2b);if(!_2e[0]){var _2f=[0];}else{var _2f=E(E(_2e[1])[1])==72?B(unAppCStr("\\&",_2e)):E(_2e);}return _2f;}));});break;default:return new F(function(){return _1([1,_1Y,new T(function(){var _2g=_2d;return _2g>=0?B(_P(_1X,_2g)):E(_M);})],_2b);});}}else{return [1,[0,_2c],_2b];}}}else{return [1,_1Y,new T(function(){var _2h=jsShowI(_2a),_2i=_2h;return B(_1(fromJSStr(_2i),new T(function(){var _2j=E(_2b);if(!_2j[0]){var _2k=[0];}else{var _2l=E(_2j[1])[1];if(_2l<48){var _2m=E(_2j);}else{var _2m=_2l>57?E(_2j):B(unAppCStr("\\&",_2j));}var _2n=_2m,_2o=_2n,_2k=_2o;}return _2k;})));})];}},_2p=new T(function(){return B(unCStr("\\\""));}),_2q=function(_2r,_2s){var _2t=E(_2r);if(!_2t[0]){return E(_2s);}else{var _2u=_2t[2],_2v=E(E(_2t[1])[1]);if(_2v==34){return new F(function(){return _1(_2p,new T(function(){return B(_2q(_2u,_2s));}));});}else{return new F(function(){return _29(_2v,new T(function(){return B(_2q(_2u,_2s));}));});}}},_2w=new T(function(){return B(unCStr("True"));}),_2x=new T(function(){return B(unCStr("False"));}),_2y=function(_2z,_2A,_2B,_2C,_2D){var _2E=function(_2F){return new F(function(){return _1(_l,new T(function(){return B(_1(_k,new T(function(){return B(_H(0,_2A,new T(function(){return B(_1(_j,new T(function(){return B(_1(_o,[1,_p,new T(function(){return B(_2q(_2B,[1,_p,new T(function(){return B(_1(_j,new T(function(){return B(_1(_n,new T(function(){return !E(_2C)?B(_1(_2x,[1,_m,_2F])):B(_1(_2w,[1,_m,_2F]));})));})));})]));})]));})));})));})));}));});};return _2z<11?B(_2E(_2D)):[1,_F,new T(function(){return B(_2E([1,_E,_2D]));})];},_2G=function(_2H){return new T(function(){var _2I=E(_2H);return [0,toJSStr(B(_2y(0,_2I[1],_2I[2],_2I[3],_0)))];});},_2J=function(_2K){return [1,B(_2G(_2K))];},_2L=function(_2M,_2N){var _2O=E(_2N);return _2O[0]==0?[0]:[1,new T(function(){return B(A(_2M,[_2O[1]]));}),new T(function(){return B(_2L(_2M,_2O[2]));})];},_2P=function(_2Q){return [3,new T(function(){return B(_2L(_2J,_2Q));})];},_2R=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_2S=new T(function(){return B(err(_2R));}),_2T=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_2U=new T(function(){return B(err(_2T));}),_2V=new T(function(){return B(unCStr("Control.Exception.Base"));}),_2W=new T(function(){return B(unCStr("base"));}),_2X=new T(function(){return B(unCStr("PatternMatchFail"));}),_2Y=new T(function(){var _2Z=hs_wordToWord64(18445595),_30=_2Z,_31=hs_wordToWord64(52003073),_32=_31;return [0,_30,_32,[0,_30,_32,_2W,_2V,_2X],_0];}),_33=function(_34){return E(_2Y);},_35=function(_36){return E(E(_36)[1]);},_37=function(_38,_39,_3a){var _3b=B(A(_38,[_])),_3c=B(A(_39,[_])),_3d=hs_eqWord64(_3b[1],_3c[1]),_3e=_3d;if(!E(_3e)){return [0];}else{var _3f=hs_eqWord64(_3b[2],_3c[2]),_3g=_3f;return E(_3g)==0?[0]:[1,_3a];}},_3h=function(_3i){var _3j=E(_3i);return new F(function(){return _37(B(_35(_3j[1])),_33,_3j[2]);});},_3k=function(_3l){return E(E(_3l)[1]);},_3m=function(_3n,_3o){return new F(function(){return _1(E(_3n)[1],_3o);});},_3p=[0,44],_3q=[0,93],_3r=[0,91],_3s=function(_3t,_3u,_3v){var _3w=E(_3u);return _3w[0]==0?B(unAppCStr("[]",_3v)):[1,_3r,new T(function(){return B(A(_3t,[_3w[1],new T(function(){var _3x=function(_3y){var _3z=E(_3y);return _3z[0]==0?E([1,_3q,_3v]):[1,_3p,new T(function(){return B(A(_3t,[_3z[1],new T(function(){return B(_3x(_3z[2]));})]));})];};return B(_3x(_3w[2]));})]));})];},_3A=function(_3B,_3C){return new F(function(){return _3s(_3m,_3B,_3C);});},_3D=function(_3E,_3F,_3G){return new F(function(){return _1(E(_3F)[1],_3G);});},_3H=[0,_3D,_3k,_3A],_3I=new T(function(){return [0,_33,_3H,_3J,_3h];}),_3J=function(_3K){return [0,_3I,_3K];},_3L=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_3M=function(_3N,_3O){return new F(function(){return die(new T(function(){return B(A(_3O,[_3N]));}));});},_3P=function(_3Q,_3R){var _3S=E(_3R);if(!_3S[0]){return [0,_0,_0];}else{var _3T=_3S[1];if(!B(A(_3Q,[_3T]))){return [0,_0,_3S];}else{var _3U=new T(function(){var _3V=B(_3P(_3Q,_3S[2]));return [0,_3V[1],_3V[2]];});return [0,[1,_3T,new T(function(){return E(E(_3U)[1]);})],new T(function(){return E(E(_3U)[2]);})];}}},_3W=[0,32],_3X=[0,10],_3Y=[1,_3X,_0],_3Z=function(_40){return E(E(_40)[1])==124?false:true;},_41=function(_42,_43){var _44=B(_3P(_3Z,B(unCStr(_42)))),_45=_44[1],_46=function(_47,_48){return new F(function(){return _1(_47,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_1(_43,new T(function(){return B(_1(_48,_3Y));})));})));}));});},_49=E(_44[2]);if(!_49[0]){return new F(function(){return _46(_45,_0);});}else{return E(E(_49[1])[1])==124?B(_46(_45,[1,_3W,_49[2]])):B(_46(_45,_0));}},_4a=function(_4b){return new F(function(){return _3M([0,new T(function(){return B(_41(_4b,_3L));})],_3J);});},_4c=new T(function(){return B(_4a("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_4d=function(_4e,_4f){while(1){var _4g=(function(_4h,_4i){var _4j=E(_4h);switch(_4j[0]){case 0:var _4k=E(_4i);if(!_4k[0]){return [0];}else{_4e=B(A(_4j[1],[_4k[1]]));_4f=_4k[2];return null;}break;case 1:var _4l=B(A(_4j[1],[_4i])),_4m=_4i;_4e=_4l;_4f=_4m;return null;case 2:return [0];case 3:return [1,[0,_4j[1],_4i],new T(function(){return B(_4d(_4j[2],_4i));})];default:return E(_4j[1]);}})(_4e,_4f);if(_4g!=null){return _4g;}}},_4n=function(_4o,_4p){var _4q=function(_4r){var _4s=E(_4p);if(_4s[0]==3){return [3,_4s[1],new T(function(){return B(_4n(_4o,_4s[2]));})];}else{var _4t=E(_4o);if(_4t[0]==2){return E(_4s);}else{var _4u=E(_4s);if(_4u[0]==2){return E(_4t);}else{var _4v=function(_4w){var _4x=E(_4u);if(_4x[0]==4){return [1,function(_4y){return [4,new T(function(){return B(_1(B(_4d(_4t,_4y)),_4x[1]));})];}];}else{var _4z=E(_4t);if(_4z[0]==1){var _4A=_4z[1],_4B=E(_4x);return _4B[0]==0?[1,function(_4C){return new F(function(){return _4n(B(A(_4A,[_4C])),_4B);});}]:[1,function(_4D){return new F(function(){return _4n(B(A(_4A,[_4D])),new T(function(){return B(A(_4B[1],[_4D]));}));});}];}else{var _4E=E(_4x);return _4E[0]==0?E(_4c):[1,function(_4F){return new F(function(){return _4n(_4z,new T(function(){return B(A(_4E[1],[_4F]));}));});}];}}},_4G=E(_4t);switch(_4G[0]){case 1:var _4H=E(_4u);if(_4H[0]==4){return [1,function(_4I){return [4,new T(function(){return B(_1(B(_4d(B(A(_4G[1],[_4I])),_4I)),_4H[1]));})];}];}else{return new F(function(){return _4v(_);});}break;case 4:var _4J=_4G[1],_4K=E(_4u);switch(_4K[0]){case 0:return [1,function(_4L){return [4,new T(function(){return B(_1(_4J,new T(function(){return B(_4d(_4K,_4L));})));})];}];case 1:return [1,function(_4M){return [4,new T(function(){return B(_1(_4J,new T(function(){return B(_4d(B(A(_4K[1],[_4M])),_4M));})));})];}];default:return [4,new T(function(){return B(_1(_4J,_4K[1]));})];}break;default:return new F(function(){return _4v(_);});}}}}},_4N=E(_4o);switch(_4N[0]){case 0:var _4O=E(_4p);if(!_4O[0]){return [0,function(_4P){return new F(function(){return _4n(B(A(_4N[1],[_4P])),new T(function(){return B(A(_4O[1],[_4P]));}));});}];}else{return new F(function(){return _4q(_);});}break;case 3:return [3,_4N[1],new T(function(){return B(_4n(_4N[2],_4p));})];default:return new F(function(){return _4q(_);});}},_4Q=[0,41],_4R=[1,_4Q,_0],_4S=[0,40],_4T=[1,_4S,_0],_4U=function(_4V,_4W){while(1){var _4X=E(_4V);if(!_4X[0]){return E(_4W)[0]==0?true:false;}else{var _4Y=E(_4W);if(!_4Y[0]){return false;}else{if(E(_4X[1])[1]!=E(_4Y[1])[1]){return false;}else{_4V=_4X[2];_4W=_4Y[2];continue;}}}}},_4Z=function(_50,_51){return E(_50)[1]!=E(_51)[1];},_52=function(_53,_54){return E(_53)[1]==E(_54)[1];},_55=[0,_52,_4Z],_56=function(_57,_58){while(1){var _59=E(_57);if(!_59[0]){return E(_58)[0]==0?true:false;}else{var _5a=E(_58);if(!_5a[0]){return false;}else{if(E(_59[1])[1]!=E(_5a[1])[1]){return false;}else{_57=_59[2];_58=_5a[2];continue;}}}}},_5b=function(_5c,_5d){return !B(_56(_5c,_5d))?true:false;},_5e=[0,_56,_5b],_5f=function(_5g,_5h){var _5i=E(_5g);switch(_5i[0]){case 0:return [0,function(_5j){return new F(function(){return _5f(B(A(_5i[1],[_5j])),_5h);});}];case 1:return [1,function(_5k){return new F(function(){return _5f(B(A(_5i[1],[_5k])),_5h);});}];case 2:return [2];case 3:return new F(function(){return _4n(B(A(_5h,[_5i[1]])),new T(function(){return B(_5f(_5i[2],_5h));}));});break;default:var _5l=function(_5m){var _5n=E(_5m);if(!_5n[0]){return [0];}else{var _5o=E(_5n[1]);return new F(function(){return _1(B(_4d(B(A(_5h,[_5o[1]])),_5o[2])),new T(function(){return B(_5l(_5n[2]));}));});}},_5p=B(_5l(_5i[1]));return _5p[0]==0?[2]:[4,_5p];}},_5q=[2],_5r=function(_5s){return [3,_5s,_5q];},_5t=function(_5u,_5v){var _5w=E(_5u);if(!_5w){return new F(function(){return A(_5v,[_5]);});}else{return [0,function(_5x){return E(new T(function(){return B(_5t(_5w-1|0,_5v));}));}];}},_5y=function(_5z,_5A,_5B){return function(_5C){return new F(function(){return A(function(_5D,_5E,_5F){while(1){var _5G=(function(_5H,_5I,_5J){var _5K=E(_5H);switch(_5K[0]){case 0:var _5L=E(_5I);if(!_5L[0]){return E(_5A);}else{_5D=B(A(_5K[1],[_5L[1]]));_5E=_5L[2];var _5M=_5J+1|0;_5F=_5M;return null;}break;case 1:var _5N=B(A(_5K[1],[_5I])),_5O=_5I,_5M=_5J;_5D=_5N;_5E=_5O;_5F=_5M;return null;case 2:return E(_5A);case 3:return function(_5P){return new F(function(){return _5t(_5J,function(_5Q){return E(new T(function(){return B(_5f(_5K,_5P));}));});});};default:return function(_5R){return new F(function(){return _5f(_5K,_5R);});};}})(_5D,_5E,_5F);if(_5G!=null){return _5G;}}},[new T(function(){return B(A(_5z,[_5r]));}),_5C,0,_5B]);});};},_5S=function(_5T){return new F(function(){return A(_5T,[_0]);});},_5U=function(_5V,_5W){var _5X=function(_5Y){var _5Z=E(_5Y);if(!_5Z[0]){return E(_5S);}else{var _60=_5Z[1];return !B(A(_5V,[_60]))?E(_5S):function(_61){return [0,function(_62){return E(new T(function(){return B(A(new T(function(){return B(_5X(_5Z[2]));}),[function(_63){return new F(function(){return A(_61,[[1,_60,_63]]);});}]));}));}];};}};return function(_64){return new F(function(){return A(_5X,[_64,_5W]);});};},_65=[6],_66=function(_67){return E(_67);},_68=function(_69){return new F(function(){return _66(_69);});},_6a=new T(function(){return B(unCStr("valDig: Bad base"));}),_6b=new T(function(){return B(err(_6a));}),_6c=function(_6d,_6e){var _6f=function(_6g,_6h){var _6i=E(_6g);if(!_6i[0]){return function(_6j){return new F(function(){return A(_6j,[new T(function(){return B(A(_6h,[_0]));})]);});};}else{var _6k=E(_6i[1])[1],_6l=function(_6m){return function(_6n){return [0,function(_6o){return E(new T(function(){return B(A(new T(function(){return B(_6f(_6i[2],function(_6p){return new F(function(){return A(_6h,[[1,_6m,_6p]]);});}));}),[_6n]));}));}];};};switch(E(E(_6d)[1])){case 8:if(48>_6k){return function(_6q){return new F(function(){return A(_6q,[new T(function(){return B(A(_6h,[_0]));})]);});};}else{if(_6k>55){return function(_6r){return new F(function(){return A(_6r,[new T(function(){return B(A(_6h,[_0]));})]);});};}else{return new F(function(){return _6l([0,_6k-48|0]);});}}break;case 10:if(48>_6k){return function(_6s){return new F(function(){return A(_6s,[new T(function(){return B(A(_6h,[_0]));})]);});};}else{if(_6k>57){return function(_6t){return new F(function(){return A(_6t,[new T(function(){return B(A(_6h,[_0]));})]);});};}else{return new F(function(){return _6l([0,_6k-48|0]);});}}break;case 16:if(48>_6k){if(97>_6k){if(65>_6k){return function(_6u){return new F(function(){return A(_6u,[new T(function(){return B(A(_6h,[_0]));})]);});};}else{if(_6k>70){return function(_6v){return new F(function(){return A(_6v,[new T(function(){return B(A(_6h,[_0]));})]);});};}else{return new F(function(){return _6l([0,(_6k-65|0)+10|0]);});}}}else{if(_6k>102){if(65>_6k){return function(_6w){return new F(function(){return A(_6w,[new T(function(){return B(A(_6h,[_0]));})]);});};}else{if(_6k>70){return function(_6x){return new F(function(){return A(_6x,[new T(function(){return B(A(_6h,[_0]));})]);});};}else{return new F(function(){return _6l([0,(_6k-65|0)+10|0]);});}}}else{return new F(function(){return _6l([0,(_6k-97|0)+10|0]);});}}}else{if(_6k>57){if(97>_6k){if(65>_6k){return function(_6y){return new F(function(){return A(_6y,[new T(function(){return B(A(_6h,[_0]));})]);});};}else{if(_6k>70){return function(_6z){return new F(function(){return A(_6z,[new T(function(){return B(A(_6h,[_0]));})]);});};}else{return new F(function(){return _6l([0,(_6k-65|0)+10|0]);});}}}else{if(_6k>102){if(65>_6k){return function(_6A){return new F(function(){return A(_6A,[new T(function(){return B(A(_6h,[_0]));})]);});};}else{if(_6k>70){return function(_6B){return new F(function(){return A(_6B,[new T(function(){return B(A(_6h,[_0]));})]);});};}else{return new F(function(){return _6l([0,(_6k-65|0)+10|0]);});}}}else{return new F(function(){return _6l([0,(_6k-97|0)+10|0]);});}}}else{return new F(function(){return _6l([0,_6k-48|0]);});}}break;default:return E(_6b);}}};return function(_6C){return new F(function(){return A(_6f,[_6C,_68,function(_6D){var _6E=E(_6D);return _6E[0]==0?[2]:B(A(_6e,[_6E]));}]);});};},_6F=[0,10],_6G=[0,1],_6H=[0,2147483647],_6I=function(_6J,_6K){while(1){var _6L=E(_6J);if(!_6L[0]){var _6M=_6L[1],_6N=E(_6K);if(!_6N[0]){var _6O=_6N[1],_6P=addC(_6M,_6O);if(!E(_6P[2])){return [0,_6P[1]];}else{_6J=[1,I_fromInt(_6M)];_6K=[1,I_fromInt(_6O)];continue;}}else{_6J=[1,I_fromInt(_6M)];_6K=_6N;continue;}}else{var _6Q=E(_6K);if(!_6Q[0]){_6J=_6L;_6K=[1,I_fromInt(_6Q[1])];continue;}else{return [1,I_add(_6L[1],_6Q[1])];}}}},_6R=new T(function(){return B(_6I(_6H,_6G));}),_6S=function(_6T){var _6U=E(_6T);if(!_6U[0]){var _6V=E(_6U[1]);return _6V==(-2147483648)?E(_6R):[0, -_6V];}else{return [1,I_negate(_6U[1])];}},_6W=[0,10],_6X=[0,0],_6Y=function(_6Z){return [0,_6Z];},_70=function(_71,_72){while(1){var _73=E(_71);if(!_73[0]){var _74=_73[1],_75=E(_72);if(!_75[0]){var _76=_75[1];if(!(imul(_74,_76)|0)){return [0,imul(_74,_76)|0];}else{_71=[1,I_fromInt(_74)];_72=[1,I_fromInt(_76)];continue;}}else{_71=[1,I_fromInt(_74)];_72=_75;continue;}}else{var _77=E(_72);if(!_77[0]){_71=_73;_72=[1,I_fromInt(_77[1])];continue;}else{return [1,I_mul(_73[1],_77[1])];}}}},_78=function(_79,_7a,_7b){while(1){var _7c=E(_7b);if(!_7c[0]){return E(_7a);}else{var _7d=B(_6I(B(_70(_7a,_79)),B(_6Y(E(_7c[1])[1]))));_7b=_7c[2];_7a=_7d;continue;}}},_7e=function(_7f){var _7g=new T(function(){return B(_4n(B(_4n([0,function(_7h){return E(E(_7h)[1])==45?[1,B(_6c(_6F,function(_7i){return new F(function(){return A(_7f,[[1,new T(function(){return B(_6S(B(_78(_6W,_6X,_7i))));})]]);});}))]:[2];}],[0,function(_7j){return E(E(_7j)[1])==43?[1,B(_6c(_6F,function(_7k){return new F(function(){return A(_7f,[[1,new T(function(){return B(_78(_6W,_6X,_7k));})]]);});}))]:[2];}])),new T(function(){return [1,B(_6c(_6F,function(_7l){return new F(function(){return A(_7f,[[1,new T(function(){return B(_78(_6W,_6X,_7l));})]]);});}))];})));});return new F(function(){return _4n([0,function(_7m){return E(E(_7m)[1])==101?E(_7g):[2];}],[0,function(_7n){return E(E(_7n)[1])==69?E(_7g):[2];}]);});},_7o=[0],_7p=function(_7q){return new F(function(){return A(_7q,[_7o]);});},_7r=function(_7s){return new F(function(){return A(_7s,[_7o]);});},_7t=function(_7u){return function(_7v){return E(E(_7v)[1])==46?[1,B(_6c(_6F,function(_7w){return new F(function(){return A(_7u,[[1,_7w]]);});}))]:[2];};},_7x=function(_7y){return [0,B(_7t(_7y))];},_7z=function(_7A){return new F(function(){return _6c(_6F,function(_7B){return [1,B(_5y(_7x,_7p,function(_7C){return [1,B(_5y(_7e,_7r,function(_7D){return new F(function(){return A(_7A,[[5,[1,_7B,_7C,_7D]]]);});}))];}))];});});},_7E=function(_7F){return [1,B(_7z(_7F))];},_7G=function(_7H){return E(E(_7H)[1]);},_7I=function(_7J,_7K,_7L){while(1){var _7M=E(_7L);if(!_7M[0]){return false;}else{if(!B(A(_7G,[_7J,_7K,_7M[1]]))){_7L=_7M[2];continue;}else{return true;}}}},_7N=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_7O=function(_7P){return new F(function(){return _7I(_55,_7P,_7N);});},_7Q=[0,8],_7R=[0,16],_7S=function(_7T){var _7U=function(_7V){return new F(function(){return A(_7T,[[5,[0,_7Q,_7V]]]);});},_7W=function(_7X){return new F(function(){return A(_7T,[[5,[0,_7R,_7X]]]);});};return function(_7Y){return E(E(_7Y)[1])==48?E([0,function(_7Z){switch(E(E(_7Z)[1])){case 79:return [1,B(_6c(_7Q,_7U))];case 88:return [1,B(_6c(_7R,_7W))];case 111:return [1,B(_6c(_7Q,_7U))];case 120:return [1,B(_6c(_7R,_7W))];default:return [2];}}]):[2];};},_80=function(_81){return [0,B(_7S(_81))];},_82=false,_83=true,_84=function(_85){var _86=new T(function(){return B(A(_85,[_7Q]));}),_87=new T(function(){return B(A(_85,[_7R]));});return function(_88){switch(E(E(_88)[1])){case 79:return E(_86);case 88:return E(_87);case 111:return E(_86);case 120:return E(_87);default:return [2];}};},_89=function(_8a){return [0,B(_84(_8a))];},_8b=[0,92],_8c=function(_8d){return new F(function(){return A(_8d,[_6F]);});},_8e=function(_8f,_8g){var _8h=jsShowI(_8f),_8i=_8h;return new F(function(){return _1(fromJSStr(_8i),_8g);});},_8j=function(_8k,_8l,_8m){if(_8l>=0){return new F(function(){return _8e(_8l,_8m);});}else{return _8k<=6?B(_8e(_8l,_8m)):[1,_F,new T(function(){var _8n=jsShowI(_8l),_8o=_8n;return B(_1(fromJSStr(_8o),[1,_E,_8m]));})];}},_8p=function(_8q){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_8j(9,_8q,_0));}))));});},_8r=function(_8s){var _8t=E(_8s);return _8t[0]==0?E(_8t[1]):I_toInt(_8t[1]);},_8u=function(_8v,_8w){var _8x=E(_8v);if(!_8x[0]){var _8y=_8x[1],_8z=E(_8w);return _8z[0]==0?_8y<=_8z[1]:I_compareInt(_8z[1],_8y)>=0;}else{var _8A=_8x[1],_8B=E(_8w);return _8B[0]==0?I_compareInt(_8A,_8B[1])<=0:I_compare(_8A,_8B[1])<=0;}},_8C=function(_8D){return [2];},_8E=function(_8F){var _8G=E(_8F);if(!_8G[0]){return E(_8C);}else{var _8H=_8G[1],_8I=E(_8G[2]);return _8I[0]==0?E(_8H):function(_8J){return new F(function(){return _4n(B(A(_8H,[_8J])),new T(function(){return B(A(new T(function(){return B(_8E(_8I));}),[_8J]));}));});};}},_8K=function(_8L){return [2];},_8M=function(_8N,_8O){var _8P=function(_8Q,_8R){var _8S=E(_8Q);if(!_8S[0]){return function(_8T){return new F(function(){return A(_8T,[_8N]);});};}else{var _8U=E(_8R);return _8U[0]==0?E(_8K):E(_8S[1])[1]!=E(_8U[1])[1]?E(_8K):function(_8V){return [0,function(_8W){return E(new T(function(){return B(A(new T(function(){return B(_8P(_8S[2],_8U[2]));}),[_8V]));}));}];};}};return function(_8X){return new F(function(){return A(_8P,[_8N,_8X,_8O]);});};},_8Y=new T(function(){return B(unCStr("SOH"));}),_8Z=[0,1],_90=function(_91){return [1,B(_8M(_8Y,function(_92){return E(new T(function(){return B(A(_91,[_8Z]));}));}))];},_93=new T(function(){return B(unCStr("SO"));}),_94=[0,14],_95=function(_96){return [1,B(_8M(_93,function(_97){return E(new T(function(){return B(A(_96,[_94]));}));}))];},_98=function(_99){return [1,B(_5y(_90,_95,_99))];},_9a=new T(function(){return B(unCStr("NUL"));}),_9b=[0,0],_9c=function(_9d){return [1,B(_8M(_9a,function(_9e){return E(new T(function(){return B(A(_9d,[_9b]));}));}))];},_9f=new T(function(){return B(unCStr("STX"));}),_9g=[0,2],_9h=function(_9i){return [1,B(_8M(_9f,function(_9j){return E(new T(function(){return B(A(_9i,[_9g]));}));}))];},_9k=new T(function(){return B(unCStr("ETX"));}),_9l=[0,3],_9m=function(_9n){return [1,B(_8M(_9k,function(_9o){return E(new T(function(){return B(A(_9n,[_9l]));}));}))];},_9p=new T(function(){return B(unCStr("EOT"));}),_9q=[0,4],_9r=function(_9s){return [1,B(_8M(_9p,function(_9t){return E(new T(function(){return B(A(_9s,[_9q]));}));}))];},_9u=new T(function(){return B(unCStr("ENQ"));}),_9v=[0,5],_9w=function(_9x){return [1,B(_8M(_9u,function(_9y){return E(new T(function(){return B(A(_9x,[_9v]));}));}))];},_9z=new T(function(){return B(unCStr("ACK"));}),_9A=[0,6],_9B=function(_9C){return [1,B(_8M(_9z,function(_9D){return E(new T(function(){return B(A(_9C,[_9A]));}));}))];},_9E=new T(function(){return B(unCStr("BEL"));}),_9F=[0,7],_9G=function(_9H){return [1,B(_8M(_9E,function(_9I){return E(new T(function(){return B(A(_9H,[_9F]));}));}))];},_9J=new T(function(){return B(unCStr("BS"));}),_9K=[0,8],_9L=function(_9M){return [1,B(_8M(_9J,function(_9N){return E(new T(function(){return B(A(_9M,[_9K]));}));}))];},_9O=new T(function(){return B(unCStr("HT"));}),_9P=[0,9],_9Q=function(_9R){return [1,B(_8M(_9O,function(_9S){return E(new T(function(){return B(A(_9R,[_9P]));}));}))];},_9T=new T(function(){return B(unCStr("LF"));}),_9U=[0,10],_9V=function(_9W){return [1,B(_8M(_9T,function(_9X){return E(new T(function(){return B(A(_9W,[_9U]));}));}))];},_9Y=new T(function(){return B(unCStr("VT"));}),_9Z=[0,11],_a0=function(_a1){return [1,B(_8M(_9Y,function(_a2){return E(new T(function(){return B(A(_a1,[_9Z]));}));}))];},_a3=new T(function(){return B(unCStr("FF"));}),_a4=[0,12],_a5=function(_a6){return [1,B(_8M(_a3,function(_a7){return E(new T(function(){return B(A(_a6,[_a4]));}));}))];},_a8=new T(function(){return B(unCStr("CR"));}),_a9=[0,13],_aa=function(_ab){return [1,B(_8M(_a8,function(_ac){return E(new T(function(){return B(A(_ab,[_a9]));}));}))];},_ad=new T(function(){return B(unCStr("SI"));}),_ae=[0,15],_af=function(_ag){return [1,B(_8M(_ad,function(_ah){return E(new T(function(){return B(A(_ag,[_ae]));}));}))];},_ai=new T(function(){return B(unCStr("DLE"));}),_aj=[0,16],_ak=function(_al){return [1,B(_8M(_ai,function(_am){return E(new T(function(){return B(A(_al,[_aj]));}));}))];},_an=new T(function(){return B(unCStr("DC1"));}),_ao=[0,17],_ap=function(_aq){return [1,B(_8M(_an,function(_ar){return E(new T(function(){return B(A(_aq,[_ao]));}));}))];},_as=new T(function(){return B(unCStr("DC2"));}),_at=[0,18],_au=function(_av){return [1,B(_8M(_as,function(_aw){return E(new T(function(){return B(A(_av,[_at]));}));}))];},_ax=new T(function(){return B(unCStr("DC3"));}),_ay=[0,19],_az=function(_aA){return [1,B(_8M(_ax,function(_aB){return E(new T(function(){return B(A(_aA,[_ay]));}));}))];},_aC=new T(function(){return B(unCStr("DC4"));}),_aD=[0,20],_aE=function(_aF){return [1,B(_8M(_aC,function(_aG){return E(new T(function(){return B(A(_aF,[_aD]));}));}))];},_aH=new T(function(){return B(unCStr("NAK"));}),_aI=[0,21],_aJ=function(_aK){return [1,B(_8M(_aH,function(_aL){return E(new T(function(){return B(A(_aK,[_aI]));}));}))];},_aM=new T(function(){return B(unCStr("SYN"));}),_aN=[0,22],_aO=function(_aP){return [1,B(_8M(_aM,function(_aQ){return E(new T(function(){return B(A(_aP,[_aN]));}));}))];},_aR=new T(function(){return B(unCStr("ETB"));}),_aS=[0,23],_aT=function(_aU){return [1,B(_8M(_aR,function(_aV){return E(new T(function(){return B(A(_aU,[_aS]));}));}))];},_aW=new T(function(){return B(unCStr("CAN"));}),_aX=[0,24],_aY=function(_aZ){return [1,B(_8M(_aW,function(_b0){return E(new T(function(){return B(A(_aZ,[_aX]));}));}))];},_b1=new T(function(){return B(unCStr("EM"));}),_b2=[0,25],_b3=function(_b4){return [1,B(_8M(_b1,function(_b5){return E(new T(function(){return B(A(_b4,[_b2]));}));}))];},_b6=new T(function(){return B(unCStr("SUB"));}),_b7=[0,26],_b8=function(_b9){return [1,B(_8M(_b6,function(_ba){return E(new T(function(){return B(A(_b9,[_b7]));}));}))];},_bb=new T(function(){return B(unCStr("ESC"));}),_bc=[0,27],_bd=function(_be){return [1,B(_8M(_bb,function(_bf){return E(new T(function(){return B(A(_be,[_bc]));}));}))];},_bg=new T(function(){return B(unCStr("FS"));}),_bh=[0,28],_bi=function(_bj){return [1,B(_8M(_bg,function(_bk){return E(new T(function(){return B(A(_bj,[_bh]));}));}))];},_bl=new T(function(){return B(unCStr("GS"));}),_bm=[0,29],_bn=function(_bo){return [1,B(_8M(_bl,function(_bp){return E(new T(function(){return B(A(_bo,[_bm]));}));}))];},_bq=new T(function(){return B(unCStr("RS"));}),_br=[0,30],_bs=function(_bt){return [1,B(_8M(_bq,function(_bu){return E(new T(function(){return B(A(_bt,[_br]));}));}))];},_bv=new T(function(){return B(unCStr("US"));}),_bw=[0,31],_bx=function(_by){return [1,B(_8M(_bv,function(_bz){return E(new T(function(){return B(A(_by,[_bw]));}));}))];},_bA=new T(function(){return B(unCStr("SP"));}),_bB=[0,32],_bC=function(_bD){return [1,B(_8M(_bA,function(_bE){return E(new T(function(){return B(A(_bD,[_bB]));}));}))];},_bF=new T(function(){return B(unCStr("DEL"));}),_bG=[0,127],_bH=function(_bI){return [1,B(_8M(_bF,function(_bJ){return E(new T(function(){return B(A(_bI,[_bG]));}));}))];},_bK=[1,_bH,_0],_bL=[1,_bC,_bK],_bM=[1,_bx,_bL],_bN=[1,_bs,_bM],_bO=[1,_bn,_bN],_bP=[1,_bi,_bO],_bQ=[1,_bd,_bP],_bR=[1,_b8,_bQ],_bS=[1,_b3,_bR],_bT=[1,_aY,_bS],_bU=[1,_aT,_bT],_bV=[1,_aO,_bU],_bW=[1,_aJ,_bV],_bX=[1,_aE,_bW],_bY=[1,_az,_bX],_bZ=[1,_au,_bY],_c0=[1,_ap,_bZ],_c1=[1,_ak,_c0],_c2=[1,_af,_c1],_c3=[1,_aa,_c2],_c4=[1,_a5,_c3],_c5=[1,_a0,_c4],_c6=[1,_9V,_c5],_c7=[1,_9Q,_c6],_c8=[1,_9L,_c7],_c9=[1,_9G,_c8],_ca=[1,_9B,_c9],_cb=[1,_9w,_ca],_cc=[1,_9r,_cb],_cd=[1,_9m,_cc],_ce=[1,_9h,_cd],_cf=[1,_9c,_ce],_cg=[1,_98,_cf],_ch=new T(function(){return B(_8E(_cg));}),_ci=[0,1114111],_cj=[0,34],_ck=[0,39],_cl=function(_cm){var _cn=new T(function(){return B(A(_cm,[_9F]));}),_co=new T(function(){return B(A(_cm,[_9K]));}),_cp=new T(function(){return B(A(_cm,[_9P]));}),_cq=new T(function(){return B(A(_cm,[_9U]));}),_cr=new T(function(){return B(A(_cm,[_9Z]));}),_cs=new T(function(){return B(A(_cm,[_a4]));}),_ct=new T(function(){return B(A(_cm,[_a9]));});return new F(function(){return _4n([0,function(_cu){switch(E(E(_cu)[1])){case 34:return E(new T(function(){return B(A(_cm,[_cj]));}));case 39:return E(new T(function(){return B(A(_cm,[_ck]));}));case 92:return E(new T(function(){return B(A(_cm,[_8b]));}));case 97:return E(_cn);case 98:return E(_co);case 102:return E(_cs);case 110:return E(_cq);case 114:return E(_ct);case 116:return E(_cp);case 118:return E(_cr);default:return [2];}}],new T(function(){return B(_4n([1,B(_5y(_89,_8c,function(_cv){return [1,B(_6c(_cv,function(_cw){var _cx=B(_78(new T(function(){return B(_6Y(E(_cv)[1]));}),_6X,_cw));return !B(_8u(_cx,_ci))?[2]:B(A(_cm,[new T(function(){var _cy=B(_8r(_cx));if(_cy>>>0>1114111){var _cz=B(_8p(_cy));}else{var _cz=[0,_cy];}var _cA=_cz,_cB=_cA,_cC=_cB;return _cC;})]));}))];}))],new T(function(){return B(_4n([0,function(_cD){return E(E(_cD)[1])==94?E([0,function(_cE){switch(E(E(_cE)[1])){case 64:return E(new T(function(){return B(A(_cm,[_9b]));}));case 65:return E(new T(function(){return B(A(_cm,[_8Z]));}));case 66:return E(new T(function(){return B(A(_cm,[_9g]));}));case 67:return E(new T(function(){return B(A(_cm,[_9l]));}));case 68:return E(new T(function(){return B(A(_cm,[_9q]));}));case 69:return E(new T(function(){return B(A(_cm,[_9v]));}));case 70:return E(new T(function(){return B(A(_cm,[_9A]));}));case 71:return E(_cn);case 72:return E(_co);case 73:return E(_cp);case 74:return E(_cq);case 75:return E(_cr);case 76:return E(_cs);case 77:return E(_ct);case 78:return E(new T(function(){return B(A(_cm,[_94]));}));case 79:return E(new T(function(){return B(A(_cm,[_ae]));}));case 80:return E(new T(function(){return B(A(_cm,[_aj]));}));case 81:return E(new T(function(){return B(A(_cm,[_ao]));}));case 82:return E(new T(function(){return B(A(_cm,[_at]));}));case 83:return E(new T(function(){return B(A(_cm,[_ay]));}));case 84:return E(new T(function(){return B(A(_cm,[_aD]));}));case 85:return E(new T(function(){return B(A(_cm,[_aI]));}));case 86:return E(new T(function(){return B(A(_cm,[_aN]));}));case 87:return E(new T(function(){return B(A(_cm,[_aS]));}));case 88:return E(new T(function(){return B(A(_cm,[_aX]));}));case 89:return E(new T(function(){return B(A(_cm,[_b2]));}));case 90:return E(new T(function(){return B(A(_cm,[_b7]));}));case 91:return E(new T(function(){return B(A(_cm,[_bc]));}));case 92:return E(new T(function(){return B(A(_cm,[_bh]));}));case 93:return E(new T(function(){return B(A(_cm,[_bm]));}));case 94:return E(new T(function(){return B(A(_cm,[_br]));}));case 95:return E(new T(function(){return B(A(_cm,[_bw]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_ch,[_cm]));})));})));}));});},_cF=function(_cG){return new F(function(){return A(_cG,[_5]);});},_cH=function(_cI){var _cJ=E(_cI);if(!_cJ[0]){return E(_cF);}else{var _cK=_cJ[2],_cL=E(E(_cJ[1])[1]);switch(_cL){case 9:return function(_cM){return [0,function(_cN){return E(new T(function(){return B(A(new T(function(){return B(_cH(_cK));}),[_cM]));}));}];};case 10:return function(_cO){return [0,function(_cP){return E(new T(function(){return B(A(new T(function(){return B(_cH(_cK));}),[_cO]));}));}];};case 11:return function(_cQ){return [0,function(_cR){return E(new T(function(){return B(A(new T(function(){return B(_cH(_cK));}),[_cQ]));}));}];};case 12:return function(_cS){return [0,function(_cT){return E(new T(function(){return B(A(new T(function(){return B(_cH(_cK));}),[_cS]));}));}];};case 13:return function(_cU){return [0,function(_cV){return E(new T(function(){return B(A(new T(function(){return B(_cH(_cK));}),[_cU]));}));}];};case 32:return function(_cW){return [0,function(_cX){return E(new T(function(){return B(A(new T(function(){return B(_cH(_cK));}),[_cW]));}));}];};case 160:return function(_cY){return [0,function(_cZ){return E(new T(function(){return B(A(new T(function(){return B(_cH(_cK));}),[_cY]));}));}];};default:var _d0=u_iswspace(_cL),_d1=_d0;return E(_d1)==0?E(_cF):function(_d2){return [0,function(_d3){return E(new T(function(){return B(A(new T(function(){return B(_cH(_cK));}),[_d2]));}));}];};}}},_d4=function(_d5){var _d6=new T(function(){return B(_d4(_d5));}),_d7=[1,function(_d8){return new F(function(){return A(_cH,[_d8,function(_d9){return E([0,function(_da){return E(E(_da)[1])==92?E(_d6):[2];}]);}]);});}];return new F(function(){return _4n([0,function(_db){return E(E(_db)[1])==92?E([0,function(_dc){var _dd=E(E(_dc)[1]);switch(_dd){case 9:return E(_d7);case 10:return E(_d7);case 11:return E(_d7);case 12:return E(_d7);case 13:return E(_d7);case 32:return E(_d7);case 38:return E(_d6);case 160:return E(_d7);default:var _de=u_iswspace(_dd),_df=_de;return E(_df)==0?[2]:E(_d7);}}]):[2];}],[0,function(_dg){var _dh=E(_dg);return E(_dh[1])==92?E(new T(function(){return B(_cl(function(_di){return new F(function(){return A(_d5,[[0,_di,_83]]);});}));})):B(A(_d5,[[0,_dh,_82]]));}]);});},_dj=function(_dk,_dl){return new F(function(){return _d4(function(_dm){var _dn=E(_dm),_do=E(_dn[1]);if(E(_do[1])==34){if(!E(_dn[2])){return E(new T(function(){return B(A(_dl,[[1,new T(function(){return B(A(_dk,[_0]));})]]));}));}else{return new F(function(){return _dj(function(_dp){return new F(function(){return A(_dk,[[1,_do,_dp]]);});},_dl);});}}else{return new F(function(){return _dj(function(_dq){return new F(function(){return A(_dk,[[1,_do,_dq]]);});},_dl);});}});});},_dr=new T(function(){return B(unCStr("_\'"));}),_ds=function(_dt){var _du=u_iswalnum(_dt),_dv=_du;return E(_dv)==0?B(_7I(_55,[0,_dt],_dr)):true;},_dw=function(_dx){return new F(function(){return _ds(E(_dx)[1]);});},_dy=new T(function(){return B(unCStr(",;()[]{}`"));}),_dz=new T(function(){return B(unCStr(".."));}),_dA=new T(function(){return B(unCStr("::"));}),_dB=new T(function(){return B(unCStr("->"));}),_dC=[0,64],_dD=[1,_dC,_0],_dE=[0,126],_dF=[1,_dE,_0],_dG=new T(function(){return B(unCStr("=>"));}),_dH=[1,_dG,_0],_dI=[1,_dF,_dH],_dJ=[1,_dD,_dI],_dK=[1,_dB,_dJ],_dL=new T(function(){return B(unCStr("<-"));}),_dM=[1,_dL,_dK],_dN=[0,124],_dO=[1,_dN,_0],_dP=[1,_dO,_dM],_dQ=[1,_8b,_0],_dR=[1,_dQ,_dP],_dS=[0,61],_dT=[1,_dS,_0],_dU=[1,_dT,_dR],_dV=[1,_dA,_dU],_dW=[1,_dz,_dV],_dX=function(_dY){return new F(function(){return _4n([1,function(_dZ){return E(_dZ)[0]==0?E(new T(function(){return B(A(_dY,[_65]));})):[2];}],new T(function(){return B(_4n([0,function(_e0){return E(E(_e0)[1])==39?E([0,function(_e1){var _e2=E(_e1);switch(E(_e2[1])){case 39:return [2];case 92:return E(new T(function(){return B(_cl(function(_e3){return [0,function(_e4){return E(E(_e4)[1])==39?E(new T(function(){return B(A(_dY,[[0,_e3]]));})):[2];}];}));}));default:return [0,function(_e5){return E(E(_e5)[1])==39?E(new T(function(){return B(A(_dY,[[0,_e2]]));})):[2];}];}}]):[2];}],new T(function(){return B(_4n([0,function(_e6){return E(E(_e6)[1])==34?E(new T(function(){return B(_dj(_68,_dY));})):[2];}],new T(function(){return B(_4n([0,function(_e7){return !B(_7I(_55,_e7,_dy))?[2]:B(A(_dY,[[2,[1,_e7,_0]]]));}],new T(function(){return B(_4n([0,function(_e8){return !B(_7I(_55,_e8,_7N))?[2]:[1,B(_5U(_7O,function(_e9){var _ea=[1,_e8,_e9];return !B(_7I(_5e,_ea,_dW))?B(A(_dY,[[4,_ea]])):B(A(_dY,[[2,_ea]]));}))];}],new T(function(){return B(_4n([0,function(_eb){var _ec=E(_eb),_ed=_ec[1],_ee=u_iswalpha(_ed),_ef=_ee;return E(_ef)==0?E(_ed)==95?[1,B(_5U(_dw,function(_eg){return new F(function(){return A(_dY,[[3,[1,_ec,_eg]]]);});}))]:[2]:[1,B(_5U(_dw,function(_eh){return new F(function(){return A(_dY,[[3,[1,_ec,_eh]]]);});}))];}],new T(function(){return [1,B(_5y(_80,_7E,_dY))];})));})));})));})));})));}));});},_ei=[0,0],_ej=function(_ek,_el){return function(_em){return new F(function(){return A(_cH,[_em,function(_en){return E(new T(function(){return B(_dX(function(_eo){var _ep=E(_eo);return _ep[0]==2?!B(_4U(_ep[1],_4T))?[2]:E(new T(function(){return B(A(_ek,[_ei,function(_eq){return [1,function(_er){return new F(function(){return A(_cH,[_er,function(_es){return E(new T(function(){return B(_dX(function(_et){var _eu=E(_et);return _eu[0]==2?!B(_4U(_eu[1],_4R))?[2]:E(new T(function(){return B(A(_el,[_eq]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_ev=function(_ew,_ex,_ey){var _ez=function(_eA,_eB){return new F(function(){return _4n([1,function(_eC){return new F(function(){return A(_cH,[_eC,function(_eD){return E(new T(function(){return B(_dX(function(_eE){var _eF=E(_eE);if(_eF[0]==4){var _eG=E(_eF[1]);if(!_eG[0]){return new F(function(){return A(_ew,[_eF,_eA,_eB]);});}else{return E(E(_eG[1])[1])==45?E(_eG[2])[0]==0?E([1,function(_eH){return new F(function(){return A(_cH,[_eH,function(_eI){return E(new T(function(){return B(_dX(function(_eJ){return new F(function(){return A(_ew,[_eJ,_eA,function(_eK){return new F(function(){return A(_eB,[new T(function(){return B(_6S(_eK));})]);});}]);});}));}));}]);});}]):B(A(_ew,[_eF,_eA,_eB])):B(A(_ew,[_eF,_eA,_eB]));}}else{return new F(function(){return A(_ew,[_eF,_eA,_eB]);});}}));}));}]);});}],new T(function(){return [1,B(_ej(_ez,_eB))];}));});};return new F(function(){return _ez(_ex,_ey);});},_eL=function(_eM,_eN){return [2];},_eO=function(_eP){var _eQ=E(_eP);return _eQ[0]==0?[1,new T(function(){return B(_78(new T(function(){return B(_6Y(E(_eQ[1])[1]));}),_6X,_eQ[2]));})]:E(_eQ[2])[0]==0?E(_eQ[3])[0]==0?[1,new T(function(){return B(_78(_6W,_6X,_eQ[1]));})]:[0]:[0];},_eR=function(_eS){var _eT=E(_eS);if(_eT[0]==5){var _eU=B(_eO(_eT[1]));return _eU[0]==0?E(_eL):function(_eV,_eW){return new F(function(){return A(_eW,[_eU[1]]);});};}else{return E(_eL);}},_eX=function(_eY,_eZ){return new F(function(){return _f0(_eZ);});},_f1=new T(function(){return B(unCStr("True"));}),_f2=new T(function(){return B(unCStr("False"));}),_f3=function(_f4){return function(_f5){return new F(function(){return A(_cH,[_f5,function(_f6){return E(new T(function(){return B(_dX(function(_f7){var _f8=E(_f7);if(_f8[0]==3){var _f9=_f8[1];return !B(_4U(_f9,_f2))?!B(_4U(_f9,_f1))?[2]:E(new T(function(){return B(A(_f4,[_83]));})):E(new T(function(){return B(A(_f4,[_82]));}));}else{return [2];}}));}));}]);});};},_f0=function(_fa){return new F(function(){return _4n([1,B(_f3(_fa))],new T(function(){return [1,B(_ej(_eX,_fa))];}));});},_fb=function(_fc,_fd){return new F(function(){return _fe(_fd);});},_ff=function(_fg){return new F(function(){return _4n([1,function(_fh){return new F(function(){return A(_cH,[_fh,function(_fi){return E(new T(function(){return B(_dX(function(_fj){var _fk=E(_fj);return _fk[0]==0?B(A(_fg,[_fk[1]])):[2];}));}));}]);});}],new T(function(){return [1,B(_ej(_fl,_fg))];}));});},_fl=function(_fm,_fn){return new F(function(){return _ff(_fn);});},_fo=[0,91],_fp=[1,_fo,_0],_fq=function(_fr,_fs){var _ft=function(_fu,_fv){return [1,function(_fw){return new F(function(){return A(_cH,[_fw,function(_fx){return E(new T(function(){return B(_dX(function(_fy){var _fz=E(_fy);if(_fz[0]==2){var _fA=E(_fz[1]);if(!_fA[0]){return [2];}else{var _fB=_fA[2];switch(E(E(_fA[1])[1])){case 44:return E(_fB)[0]==0?!E(_fu)?[2]:E(new T(function(){return B(A(_fr,[_ei,function(_fC){return new F(function(){return _ft(_83,function(_fD){return new F(function(){return A(_fv,[[1,_fC,_fD]]);});});});}]));})):[2];case 93:return E(_fB)[0]==0?E(new T(function(){return B(A(_fv,[_0]));})):[2];default:return [2];}}}else{return [2];}}));}));}]);});}];},_fE=function(_fF){return new F(function(){return _4n([1,function(_fG){return new F(function(){return A(_cH,[_fG,function(_fH){return E(new T(function(){return B(_dX(function(_fI){var _fJ=E(_fI);return _fJ[0]==2?!B(_4U(_fJ[1],_fp))?[2]:E(new T(function(){return B(_4n(B(_ft(_82,_fF)),new T(function(){return B(A(_fr,[_ei,function(_fK){return new F(function(){return _ft(_83,function(_fL){return new F(function(){return A(_fF,[[1,_fK,_fL]]);});});});}]));})));})):[2];}));}));}]);});}],new T(function(){return [1,B(_ej(function(_fM,_fN){return new F(function(){return _fE(_fN);});},_fF))];}));});};return new F(function(){return _fE(_fs);});},_fe=function(_fO){return new F(function(){return _4n(B(_4n([1,function(_fP){return new F(function(){return A(_cH,[_fP,function(_fQ){return E(new T(function(){return B(_dX(function(_fR){var _fS=E(_fR);return _fS[0]==1?B(A(_fO,[_fS[1]])):[2];}));}));}]);});}],new T(function(){return B(_fq(_fl,_fO));}))),new T(function(){return [1,B(_ej(_fb,_fO))];}));});},_fT=new T(function(){return B(unCStr("Todo"));}),_fU=[0,123],_fV=[1,_fU,_0],_fW=new T(function(){return B(unCStr("identifier"));}),_fX=[0,61],_fY=[1,_fX,_0],_fZ=[0,44],_g0=[1,_fZ,_0],_g1=new T(function(){return B(unCStr("task"));}),_g2=new T(function(){return B(unCStr("completed"));}),_g3=[1,_m,_0],_g4=function(_g5,_g6){return _g5>11?[2]:[1,function(_g7){return new F(function(){return A(_cH,[_g7,function(_g8){return E(new T(function(){return B(_dX(function(_g9){var _ga=E(_g9);return _ga[0]==3?!B(_4U(_ga[1],_fT))?[2]:E([1,function(_gb){return new F(function(){return A(_cH,[_gb,function(_gc){return E(new T(function(){return B(_dX(function(_gd){var _ge=E(_gd);return _ge[0]==2?!B(_4U(_ge[1],_fV))?[2]:E([1,function(_gf){return new F(function(){return A(_cH,[_gf,function(_gg){return E(new T(function(){return B(_dX(function(_gh){var _gi=E(_gh);return _gi[0]==3?!B(_4U(_gi[1],_fW))?[2]:E([1,function(_gj){return new F(function(){return A(_cH,[_gj,function(_gk){return E(new T(function(){return B(_dX(function(_gl){var _gm=E(_gl);return _gm[0]==2?!B(_4U(_gm[1],_fY))?[2]:E(new T(function(){return B(_ev(_eR,_ei,function(_gn){return [1,function(_go){return new F(function(){return A(_cH,[_go,function(_gp){return E(new T(function(){return B(_dX(function(_gq){var _gr=E(_gq);return _gr[0]==2?!B(_4U(_gr[1],_g0))?[2]:E([1,function(_gs){return new F(function(){return A(_cH,[_gs,function(_gt){return E(new T(function(){return B(_dX(function(_gu){var _gv=E(_gu);return _gv[0]==3?!B(_4U(_gv[1],_g1))?[2]:E([1,function(_gw){return new F(function(){return A(_cH,[_gw,function(_gx){return E(new T(function(){return B(_dX(function(_gy){var _gz=E(_gy);return _gz[0]==2?!B(_4U(_gz[1],_fY))?[2]:E(new T(function(){return B(_fe(function(_gA){return [1,function(_gB){return new F(function(){return A(_cH,[_gB,function(_gC){return E(new T(function(){return B(_dX(function(_gD){var _gE=E(_gD);return _gE[0]==2?!B(_4U(_gE[1],_g0))?[2]:E([1,function(_gF){return new F(function(){return A(_cH,[_gF,function(_gG){return E(new T(function(){return B(_dX(function(_gH){var _gI=E(_gH);return _gI[0]==3?!B(_4U(_gI[1],_g2))?[2]:E([1,function(_gJ){return new F(function(){return A(_cH,[_gJ,function(_gK){return E(new T(function(){return B(_dX(function(_gL){var _gM=E(_gL);return _gM[0]==2?!B(_4U(_gM[1],_fY))?[2]:E(new T(function(){return B(_f0(function(_gN){return [1,function(_gO){return new F(function(){return A(_cH,[_gO,function(_gP){return E(new T(function(){return B(_dX(function(_gQ){var _gR=E(_gQ);return _gR[0]==2?!B(_4U(_gR[1],_g3))?[2]:E(new T(function(){return B(A(_g6,[[0,_gn,_gA,_gN]]));})):[2];}));}));}]);});}];}));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];},_gS=function(_gT,_gU){return new F(function(){return _g4(E(_gT)[1],_gU);});},_gV=function(_gW,_gX){var _gY=function(_gZ){return function(_h0){return new F(function(){return _4n(B(A(new T(function(){return B(A(_gW,[_gZ]));}),[_h0])),new T(function(){return [1,B(_ej(_gY,_h0))];}));});};};return new F(function(){return _gY(_gX);});},_h1=function(_h2){return [1,function(_h3){return new F(function(){return A(_cH,[_h3,function(_h4){return E([3,_h2,_5q]);}]);});}];},_h5=new T(function(){return B(A(_gV,[_gS,_ei,_h1]));}),_h6=new T(function(){return B(unCStr("Tried to parse a non Todo instance as Todo"));}),_h7=new T(function(){return B(err(_h6));}),_h8=function(_h9){return new F(function(){return fromJSStr(E(_h9)[1]);});},_ha=function(_hb){while(1){var _hc=(function(_hd){var _he=E(_hd);if(!_he[0]){return [0];}else{var _hf=_he[2],_hg=E(_he[1]);if(!E(_hg[2])[0]){return [1,_hg[1],new T(function(){return B(_ha(_hf));})];}else{_hb=_hf;return null;}}})(_hb);if(_hc!=null){return _hc;}}},_hh=function(_hi){var _hj=E(_hi);return _hj[0]==1?new T(function(){var _hk=B(_ha(B(_4d(_h5,new T(function(){return B(_h8(_hj[1]));})))));return _hk[0]==0?E(_2U):E(_hk[2])[0]==0?E(_hk[1]):E(_2S);}):E(_h7);},_hl=function(_hm){return [1,B(_hh(_hm))];},_hn=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_ho=[0,_hn],_hp=[1,_0],_hq=function(_hr){var _hs=E(_hr);if(!_hs[0]){return E(_hp);}else{var _ht=B(_hq(_hs[2]));return _ht[0]==0?[0,_ht[1]]:[1,[1,B(_hh(_hs[1])),_ht[1]]];}},_hu=function(_hv){var _hw=E(_hv);return _hw[0]==3?B(_hq(_hw[1])):E(_ho);},_hx=[0,_2J,_2P,_hl,_hu],_hy=function(_hz){return E(E(_hz)[2]);},_hA=function(_hB,_hC){return [3,new T(function(){return B(_2L(new T(function(){return B(_hy(_hB));}),_hC));})];},_hD=[1,_0],_hE=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_hF=[0,_hE],_hG=function(_hH){return E(E(_hH)[4]);},_hI=function(_hJ,_hK){var _hL=E(_hK);if(_hL[0]==3){var _hM=function(_hN){var _hO=E(_hN);if(!_hO[0]){return E(_hD);}else{var _hP=B(A(new T(function(){return B(_hG(_hJ));}),[_hO[1]]));if(!_hP[0]){return [0,_hP[1]];}else{var _hQ=B(_hM(_hO[2]));return _hQ[0]==0?[0,_hQ[1]]:[1,[1,_hP[1],_hQ[1]]];}}};return new F(function(){return _hM(_hL[1]);});}else{return E(_hF);}},_hR=function(_hS){return [0,new T(function(){return B(_hy(_hS));}),function(_hT){return new F(function(){return _hA(_hS,_hT);});},new T(function(){return B(_hG(_hS));}),function(_hT){return new F(function(){return _hI(_hS,_hT);});}];},_hU=new T(function(){return B(_hR(_hx));}),_hV=function(_hW,_hX){return [0,function(_){var _hY=B(A(_hW,[_])),_hZ=_hY;return new T(function(){return B(A(_hX,[_hZ]));});}];},_i0=function(_i1,_i2,_){var _i3=jsGet(_i1,toJSStr(E(_i2))),_i4=_i3;return new T(function(){return fromJSStr(_i4);});},_i5=[2],_i6=function(_i7){return [2];},_i8=function(_i9,_ia,_ib){return function(_){var _ic=E(_i9)[1],_id=rMV(_ic),_ie=_id,_if=E(_ie);if(!_if[0]){var _=wMV(_ic,[0,_if[1],new T(function(){return B(_1(_if[2],[1,[0,_ia,function(_ig){return E(new T(function(){return B(A(_ib,[_5]));}));}],_0]));})]);return _i5;}else{var _ih=E(_if[1]);if(!_ih[0]){var _=wMV(_ic,[0,_ia,_0]);return new T(function(){return B(A(_ib,[_5]));});}else{var _=wMV(_ic,[1,_ih[2]]);return [1,[1,new T(function(){return B(A(_ib,[_5]));}),[1,new T(function(){return B(A(_ih[1],[_ia,_i6]));}),_0]]];}}};},_ii=function(_ij,_ik){var _il=E(_ij);if(!_il[0]){var _im=_il[1],_in=E(_ik);return _in[0]==0?_im==_in[1]:I_compareInt(_in[1],_im)==0?true:false;}else{var _io=_il[1],_ip=E(_ik);return _ip[0]==0?I_compareInt(_io,_ip[1])==0?true:false:I_compare(_io,_ip[1])==0?true:false;}},_iq=function(_ir,_is){var _it=E(_ir),_iu=E(_is),_iv=_iu[3];return !B(_ii(_it[1],_iu[1]))?true:!B(_4U(_it[2],_iu[2]))?true:!E(_it[3])?E(_iv):!E(_iv)?true:false;},_iw=function(_ix,_iy){return !E(_ix)?!E(_iy)?true:false:E(_iy);},_iz=function(_iA,_iB,_iC,_iD,_iE,_iF){return !B(_ii(_iA,_iD))?false:!B(_4U(_iB,_iE))?false:B(_iw(_iC,_iF));},_iG=function(_iH,_iI){var _iJ=E(_iH),_iK=E(_iI);return new F(function(){return _iz(_iJ[1],_iJ[2],_iJ[3],_iK[1],_iK[2],_iK[3]);});},_iL=[0,_iG,_iq],_iM=function(_iN,_iO,_iP){while(1){var _iQ=E(_iO);if(!_iQ[0]){return E(_iP)[0]==0?true:false;}else{var _iR=E(_iP);if(!_iR[0]){return false;}else{if(!B(A(_7G,[_iN,_iQ[1],_iR[1]]))){return false;}else{_iO=_iQ[2];_iP=_iR[2];continue;}}}}},_iS=[1,_0],_iT=function(_iU,_iV){return function(_){var _iW=E(_iU)[1],_iX=rMV(_iW),_iY=_iX,_iZ=E(_iY);if(!_iZ[0]){var _j0=_iZ[1],_j1=E(_iZ[2]);if(!_j1[0]){var _=wMV(_iW,_iS);return new T(function(){return B(A(_iV,[_j0]));});}else{var _j2=E(_j1[1]),_=wMV(_iW,[0,_j2[1],_j1[2]]);return [1,[1,new T(function(){return B(A(_iV,[_j0]));}),[1,new T(function(){return B(A(_j2[2],[_i6]));}),_0]]];}}else{var _=wMV(_iW,[1,new T(function(){return B(_1(_iZ[1],[1,function(_j3){return function(_j4){return E(new T(function(){return B(A(_iV,[_j3]));}));};},_0]));})]);return _i5;}};},_j5=function(_j6,_j7){while(1){var _j8=E(_j6);if(!_j8[0]){return E(_j7);}else{_j6=_j8[2];var _j9=_j7+1|0;_j7=_j9;continue;}}},_ja=function(_jb,_jc,_jd,_je){return new F(function(){return A(_jb,[function(_){var _jf=jsSet(E(_jc)[1],toJSStr(E(_jd)),toJSStr(E(_je)));return _5;}]);});},_jg=function(_jh){return new F(function(){return A(_jh,[_5]);});},_ji=function(_jj){return !E(E(_jj)[3])?true:false;},_jk=function(_jl,_jm){while(1){var _jn=E(_jm);if(!_jn[0]){return false;}else{if(!B(A(_jl,[_jn[1]]))){_jm=_jn[2];continue;}else{return true;}}}},_jo=function(_jp){return E(E(_jp)[3]);},_jq=function(_jr){var _js=B(A(_jr,[_])),_jt=_js;return E(_jt);},_ju=function(_jv){return new F(function(){return _jq(function(_){var _=0;return new F(function(){return eval(_jv);});});});},_jw=function(_){var _jx=B(A(_ju,["document",_])),_jy=_jx;return [0,_jy];},_jz=function(_){return new F(function(){return _jw(_);});},_jA=function(_){var _=0;return new F(function(){return _jz(_);});},_jB=new T(function(){return B(_jq(_jA));}),_jC=new T(function(){return [0,"blur"];}),_jD=new T(function(){return [0,"keyup"];}),_jE=new T(function(){return [0,"dblclick"];}),_jF=new T(function(){return [0,"click"];}),_jG=function(_jH,_jI){while(1){var _jJ=(function(_jK,_jL){var _jM=E(_jL);if(!_jM[0]){return [0];}else{var _jN=_jM[1],_jO=_jM[2];if(!B(A(_jK,[_jN]))){var _jP=_jK;_jI=_jO;_jH=_jP;return null;}else{return [1,_jN,new T(function(){return B(_jG(_jK,_jO));})];}}})(_jH,_jI);if(_jJ!=null){return _jJ;}}},_jQ=new T(function(){return B(_ju("(function(e) {e.focus();})"));}),_jR=function(_jS,_){var _jT=B(A(_jQ,[E(E(_jS)[1]),_])),_jU=_jT;return _5;},_jV=function(_jW,_){return new F(function(){return _jR(_jW,_);});},_jX=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_jY=new T(function(){return B(err(_jX));}),_jZ=function(_k0){var _k1=E(_k0);return _k1[0]==0?E(_jY):E(_k1[1]);},_k2=function(_k3){return E(_k3)[0]==0?true:false;},_k4=function(_k5){var _k6=String(_k5),_k7=_k6;return [0,_k7];},_k8=function(_){var _k9=B(A(_ju,["(function() {return location.hash.substring(1);})",_])),_ka=_k9;return new T(function(){return B(_k4(_ka));});},_kb=function(_){return new F(function(){return _k8(_);});},_kc=new T(function(){return B(unCStr("true"));}),_kd=new T(function(){return B(unCStr("completed"));}),_ke=new T(function(){return B(unCStr("value"));}),_kf=new T(function(){return B(unCStr("label"));}),_kg=new T(function(){return B(unCStr("todo-list"));}),_kh=[0,41],_ki=[1,_kh,_0],_kj=new T(function(){return B(unCStr("#clear-completed"));}),_kk=new T(function(){return B(unCStr("item"));}),_kl=new T(function(){return B(unCStr("items"));}),_km=new T(function(){return B(unCStr("#todo-count"));}),_kn=new T(function(){return B(unCStr("checked"));}),_ko=new T(function(){return B(unCStr("footer"));}),_kp=[1,_ko,_0],_kq=new T(function(){return B(unCStr("main"));}),_kr=[1,_kq,_kp],_ks=[0,35],_kt=new T(function(){return B(unCStr("selected"));}),_ku=new T(function(){return B(unCStr("href"));}),_kv=new T(function(){return B(unCStr("#filters li a"));}),_kw=new T(function(){return B(unCStr("hidden"));}),_kx=new T(function(){return B(unCStr("div"));}),_ky=new T(function(){return B(unCStr("toggle-all"));}),_kz=new T(function(){return B(unCStr("todos"));}),_kA=new T(function(){return [0,toJSStr(_0)];}),_kB=[0,93],_kC=[1,_kB,_0],_kD=new T(function(){return [0,toJSStr(_kC)];}),_kE=[0,125],_kF=[1,_kE,_0],_kG=new T(function(){return [0,toJSStr(_kF)];}),_kH=[0,58],_kI=[1,_kH,_0],_kJ=new T(function(){return [0,toJSStr(_kI)];}),_kK=[0,44],_kL=[1,_kK,_0],_kM=new T(function(){return [0,toJSStr(_kL)];}),_kN=new T(function(){return [0,"false"];}),_kO=function(_kP){var _kQ=jsShow(E(_kP)[1]),_kR=_kQ;return [0,_kR];},_kS=function(_kT){var _kU=jsStringify(E(_kT)[1]),_kV=_kU;return [0,_kV];},_kW=new T(function(){return [0,"null"];}),_kX=[0,91],_kY=[1,_kX,_0],_kZ=new T(function(){return [0,toJSStr(_kY)];}),_l0=[0,123],_l1=[1,_l0,_0],_l2=new T(function(){return [0,toJSStr(_l1)];}),_l3=[0,34],_l4=[1,_l3,_0],_l5=new T(function(){return [0,toJSStr(_l4)];}),_l6=new T(function(){return [0,"true"];}),_l7=function(_l8,_l9){var _la=E(_l9);switch(_la[0]){case 0:return [0,new T(function(){return B(_kO(_la[1]));}),_l8];case 1:return [0,new T(function(){return B(_kS(_la[1]));}),_l8];case 2:return !E(_la[1])?[0,_kN,_l8]:[0,_l6,_l8];case 3:var _lb=E(_la[1]);return _lb[0]==0?[0,_kZ,[1,_kD,_l8]]:[0,_kZ,new T(function(){var _lc=B(_l7(new T(function(){var _ld=function(_le){var _lf=E(_le);return _lf[0]==0?E([1,_kD,_l8]):[1,_kM,new T(function(){var _lg=B(_l7(new T(function(){return B(_ld(_lf[2]));}),_lf[1]));return [1,_lg[1],_lg[2]];})];};return B(_ld(_lb[2]));}),_lb[1]));return [1,_lc[1],_lc[2]];})];case 4:var _lh=E(_la[1]);if(!_lh[0]){return [0,_l2,[1,_kG,_l8]];}else{var _li=E(_lh[1]);return [0,_l2,[1,new T(function(){return B(_kS(_li[1]));}),[1,_kJ,new T(function(){var _lj=B(_l7(new T(function(){var _lk=function(_ll){var _lm=E(_ll);if(!_lm[0]){return E([1,_kG,_l8]);}else{var _ln=E(_lm[1]);return [1,_kM,[1,_l5,[1,_ln[1],[1,_l5,[1,_kJ,new T(function(){var _lo=B(_l7(new T(function(){return B(_lk(_lm[2]));}),_ln[2]));return [1,_lo[1],_lo[2]];})]]]]];}};return B(_lk(_lh[2]));}),_li[2]));return [1,_lj[1],_lj[2]];})]]];}break;default:return [0,_kW,_l8];}},_lp=function(_lq){var _lr=jsCat(new T(function(){var _ls=B(_l7(_0,_lq));return [1,_ls[1],_ls[2]];}),E(_kA)[1]),_lt=_lr;return E(_lt);},_lu=new T(function(){return [0,"(function(k,v) {localStorage.setItem(k,v);})"];}),_lv=function(_lw){return E(E(_lw)[1]);},_lx=function(_ly,_lz){return function(_lA,_){var _lB=B(A(new T(function(){return B(A(_ju,[E(_lu)[1],E(toJSStr(E(_lz)))]));}),[E(B(_lp(B(A(new T(function(){return B(_lv(_ly));}),[_lA]))))),_])),_lC=_lB;return _5;};},_lD=new T(function(){return B(_lx(_hU,_kz));}),_lE=new T(function(){return B(unCStr("/completed"));}),_lF=new T(function(){return B(unCStr("/active"));}),_lG=new T(function(){return B(unCStr("li"));}),_lH=new T(function(){return B(unCStr(".toggle"));}),_lI=new T(function(){return B(unCStr(".edit"));}),_lJ=new T(function(){return B(unCStr("editing"));}),_lK=function(_lL,_lM){var _lN=E(_lM);return [0,_lN[1],_lL,_lN[3]];},_lO=new T(function(){return B(_ju("(function(e,c,x){x?e.classList.add(c):e.classList.remove(c);})"));}),_lP=function(_){var _=0;return new F(function(){return A(_ju,["false",_]);});},_lQ=new T(function(){return B(_jq(_lP));}),_lR=function(_){var _=0;return new F(function(){return A(_ju,["true",_]);});},_lS=new T(function(){return B(_jq(_lR));}),_lT=function(_lU){return function(_lV){return function(_lW,_){var _lX=B(A(new T(function(){return B(A(new T(function(){return B(A(_lO,[E(E(_lU)[1])]));}),[E(toJSStr(E(_lV)))]));}),[!E(_lW)?E(_lQ):E(_lS),_])),_lY=_lX;return _5;};};},_lZ=function(_m0){var _m1=E(_m0);return [0,_m1[1],_m1[2],new T(function(){return !E(_m1[3])?true:false;})];},_m2=function(_m3,_m4,_m5){return new F(function(){return _2L(function(_m6){var _m7=E(_m6),_m8=E(_m3),_m9=_m8[3];return !B(_ii(_m7[1],_m8[1]))?E(_m7):!B(_4U(_m7[2],_m8[2]))?E(_m7):!E(_m7[3])?!E(_m9)?B(A(_m4,[_m8])):E(_m7):!E(_m9)?E(_m7):B(A(_m4,[_m8]));},_m5);});},_ma=new T(function(){return B(unCStr("template-todo"));}),_mb=new T(function(){return B(unCStr(" could be found!"));}),_mc=function(_md){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_1(_md,_mb));}))));});},_me=function(_mf,_mg){while(1){var _mh=(function(_mi,_mj){var _mk=E(_mi);if(!_mk[0]){return [0];}else{var _ml=_mk[2],_mm=E(_mj);if(!_mm[0]){return [0];}else{var _mn=_mm[2];if(!E(_mm[1])[0]){return [1,_mk[1],new T(function(){return B(_me(_ml,_mn));})];}else{_mf=_ml;_mg=_mn;return null;}}}})(_mf,_mg);if(_mh!=null){return _mh;}}},_mo=new T(function(){return B(unAppCStr("[]",_0));}),_mp=[1,_3q,_0],_mq=function(_mr){var _ms=E(_mr);return _ms[0]==0?E(_mp):[1,_3p,[1,_p,new T(function(){return B(_2q(_ms[1],[1,_p,new T(function(){return B(_mq(_ms[2]));})]));})]];},_mt=function(_mu,_mv){return new F(function(){return err(B(unAppCStr("Elements with the following IDs could not be found: ",new T(function(){var _mw=B(_me(_mv,_mu));return _mw[0]==0?E(_mo):[1,_3r,[1,_p,new T(function(){return B(_2q(_mw[1],[1,_p,new T(function(){return B(_mq(_mw[2]));})]));})]];}))));});},_mx=function(_my){return new F(function(){return err(B(unAppCStr("No element with selector ",new T(function(){return B(_1(_my,_mb));}))));});},_mz=new T(function(){return B(unCStr("innerHTML"));}),_mA=function(_mB,_mC){return new F(function(){return _iT(_mB,function(_mD){return [0,B(_i8(_mB,_mD,function(_mE){return E([0,function(_){var _mF=B(_kb(_)),_mG=_mF,_mH=new T(function(){return fromJSStr(E(_mG)[1]);}),_mI=new T(function(){return B(_jG(_ji,_mD));}),_mJ=[1,_ks,_mH],_mK=function(_mL){var _mM=E(_mL);return _mM[0]==0?E(_jg):function(_mN){return [0,function(_){var _mO=B(A(new T(function(){return B(A(_lT,[_mM[1],_kw,new T(function(){return B(_iM(_iL,_mD,_0));})]));}),[_])),_mP=_mO;return new T(function(){return B(A(new T(function(){return B(_mK(_mM[2]));}),[_mN]));});}];};},_mQ=new T(function(){return [0,B(_j5(_mI,0))];}),_mR=new T(function(){return [0,B(_j5(B(_jG(_jo,_mD)),0))];});return [0,function(_){var _mS=E(_jB)[1],_mT=jsQuerySelectorAll(_mS,toJSStr(E(_kv))),_mU=_mT;return new T(function(){var _mV=new T(function(){var _mW=function(_mX,_mY){var _mZ=E(_mX);if(!_mZ[0]){return new F(function(){return A(_mY,[_0]);});}else{return [0,function(_){var _n0=jsFind(toJSStr(E(_mZ[1]))),_n1=_n0;return new T(function(){return B(_mW(_mZ[2],function(_n2){return new F(function(){return A(_mY,[[1,_n1,_n2]]);});}));});}];}};return B((function(_n3,_n4,_n5){return [0,function(_){var _n6=jsFind(toJSStr(E(_n3))),_n7=_n6;return new T(function(){return B(_mW(_n4,function(_n8){return new F(function(){return A(_n5,[[1,_n7,_n8]]);});}));});}];})(_kq,_kp,function(_n9){if(!B(_jk(_k2,_n9))){return new F(function(){return A(_mK,[B(_2L(_jZ,_n9)),function(_na){return E([0,function(_){var _nb=E(_km),_nc=jsQuerySelector(_mS,toJSStr(_nb)),_nd=_nc;return new T(function(){var _ne=E(_nd);if(!_ne[0]){var _nf=B(_mx(_nb));}else{var _ng=_ne[1],_nf=[0,function(_){var _nh=B(A(_lT,[_ng,_kw,new T(function(){return E(E(_mQ)[1])==0?true:false;}),_])),_ni=_nh;return [0,function(_){var _nj=E(_mz),_nk=toJSStr(_nj),_nl=jsSet(E(_ng)[1],_nk,toJSStr(B(unAppCStr("<strong>",new T(function(){var _nm=E(_mQ)[1];return B(_1(B(_8j(0,_nm,_0)),new T(function(){return B(unAppCStr("</strong> ",new T(function(){return E(_nm)==1?E(_kk):E(_kl);})));})));})))));return [0,function(_){var _nn=E(_kj),_no=jsQuerySelector(_mS,toJSStr(_nn)),_np=_no;return new T(function(){var _nq=E(_np);if(!_nq[0]){var _nr=B(_mx(_nn));}else{var _ns=_nq[1],_nr=[0,function(_){var _nt=B(A(_lT,[_ns,_kw,new T(function(){return E(E(_mR)[1])==0?true:false;}),_])),_nu=_nt;return [0,function(_){var _nv=jsSet(E(_ns)[1],_nk,toJSStr(B(unAppCStr("Clear completed (",new T(function(){return B(_1(B(_8j(0,E(_mR)[1],_0)),_ki));})))));return [0,function(_){var _nw=E(_kg),_nx=jsFind(toJSStr(_nw)),_ny=_nx;return new T(function(){var _nz=E(_ny);if(!_nz[0]){var _nA=B(_mc(_nw));}else{var _nB=_nz[1],_nA=[0,function(_){var _nC=E(_nB)[1],_nD=jsQuerySelectorAll(_nC,toJSStr(E(_lG))),_nE=_nD;return new T(function(){var _nF=function(_nG,_nH){var _nI=E(_nG);if(!_nI[0]){return new F(function(){return A(_nH,[_0]);});}else{return [0,function(_){var _nJ=jsKillChild(E(_nI[1])[1],_nC);return new T(function(){return B(_nF(_nI[2],function(_nK){return new F(function(){return A(_nH,[[1,_5,_nK]]);});}));});}];}};return B(_nF(_nE,function(_nL){return E(new T(function(){var _nM=function(_nN,_nO){var _nP=E(_nN);if(!_nP[0]){return new F(function(){return A(_nO,[_0]);});}else{var _nQ=_nP[1],_nR=new T(function(){return E(E(_nQ)[3]);}),_nS=new T(function(){return E(E(_nQ)[2]);});return [0,function(_){var _nT=jsCreateElem(toJSStr(E(_kx))),_nU=_nT;return [0,function(_){var _nV=E(_ma),_nW=jsFind(toJSStr(_nV)),_nX=_nW;return new T(function(){var _nY=E(_nX);if(!_nY[0]){var _nZ=B(_mc(_nV));}else{var _nZ=[0,function(_){var _o0=jsGet(E(_nY[1])[1],toJSStr(_nj)),_o1=_o0;return [0,function(_){var _o2=jsSet(_nU,_nk,toJSStr(fromJSStr(_o1)));return [0,function(_){var _o3=E(_lG),_o4=jsQuerySelector(_nU,toJSStr(_o3)),_o5=_o4;return new T(function(){var _o6=E(_o5);if(!_o6[0]){var _o7=B(_mx(_o3));}else{var _o7=[0,function(_){var _o8=E(_o6[1]),_o9=_o8[1],_oa=E(_kf),_ob=jsQuerySelector(_o9,toJSStr(_oa)),_oc=_ob;return new T(function(){var _od=E(_oc);if(!_od[0]){var _oe=B(_mx(_oa));}else{var _oe=B(A(_ja,[_hV,_od[1],_nj,_nS,function(_of){return E([0,function(_){var _og=E(_lH),_oh=jsQuerySelector(_o9,toJSStr(_og)),_oi=_oh;return new T(function(){var _oj=E(_oi);if(!_oj[0]){var _ok=B(_mx(_og));}else{var _ok=[0,function(_){var _ol=function(_om){var _on=jsSet(E(_oj[1])[1],toJSStr(E(_kn)),toJSStr(_om));return [0,function(_){var _oo=E(_lI),_op=jsQuerySelector(_o9,toJSStr(_oo)),_oq=_op;return new T(function(){var _or=E(_oq);if(!_or[0]){var _os=B(_mx(_oo));}else{var _os=B(A(_ja,[_hV,_or[1],_ke,_nS,function(_ot){return E([0,function(_){var _ou=B(A(_lT,[_o8,_kd,_nR,_])),_ov=_ou;return [0,function(_){var _ow=jsSetCB(_o9,E(_jE)[1],function(_ox,_oy,_){var _oz=B(A(new T(function(){return B(_lT(_o8));}),[_lJ,_83,_])),_oA=_oz,_oB=E(_lI),_oC=jsQuerySelector(_o9,toJSStr(_oB)),_oD=_oC,_oE=E(_oD);return _oE[0]==0?B(_mx(_oB)):B(A(_jV,[_oE[1],_]));}),_oF=_ow;return [0,function(_){var _oG=E(_lI),_oH=jsQuerySelector(_o9,toJSStr(_oG)),_oI=_oH;return new T(function(){var _oJ=E(_oI);if(!_oJ[0]){var _oK=B(_mx(_oG));}else{var _oL=_oJ[1],_oK=[0,function(_){var _oM=E(_oL)[1],_oN=jsSetCB(_oM,E(_jD)[1],function(_oO,_){if(E(E(_oO)[1])==13){return new F(function(){return _d([0,function(_){var _oP=jsGet(E(_oL)[1],toJSStr(E(_ke))),_oQ=_oP;return new T(function(){var _oR=new T(function(){return fromJSStr(_oQ);});return [0,B(_iT(_mB,function(_oS){var _oT=new T(function(){return B(_m2(_nQ,function(_oU){return new F(function(){return _lK(_oR,_oU);});},_oS));});return [0,B(_i8(_mB,_oT,function(_oV){return E([0,function(_){var _oW=B(A(_lD,[_oT,_])),_oX=_oW;return [0,function(_){var _oY=jsQuerySelector(_o9,toJSStr(_oa)),_oZ=_oY;return new T(function(){var _p0=E(_oZ);if(!_p0[0]){var _p1=B(_mx(_oa));}else{var _p1=B(A(_ja,[_hV,_p0[1],_nj,_oR,function(_p2){return E([0,function(_){var _p3=B(A(_lT,[[0,_o9],_lJ,_82,_])),_p4=_p3;return _i5;}]);}]));}return _p1;});}];}]);}))];}))];});}],_0,_);});}else{return _5;}}),_p5=_oN;return [0,function(_){var _p6=jsSetCB(_oM,E(_jC)[1],function(_){return new F(function(){return _d([0,function(_){var _p7=jsGet(_oM,toJSStr(E(_ke))),_p8=_p7;return new T(function(){var _p9=new T(function(){return fromJSStr(_p8);});return [0,B(_iT(_mB,function(_pa){var _pb=new T(function(){return B(_m2(_nQ,function(_oU){return new F(function(){return _lK(_p9,_oU);});},_pa));});return [0,B(_i8(_mB,_pb,function(_pc){return E([0,function(_){var _pd=B(A(_lD,[_pb,_])),_pe=_pd;return [0,function(_){var _pf=jsQuerySelector(_o9,toJSStr(_oa)),_pg=_pf;return new T(function(){var _ph=E(_pg);if(!_ph[0]){var _pi=B(_mx(_oa));}else{var _pi=B(A(_ja,[_hV,_ph[1],_nj,_p9,function(_pj){return E([0,function(_){var _pk=B(A(_lT,[[0,_o9],_lJ,_82,_])),_pl=_pk;return _i5;}]);}]));}return _pi;});}];}]);}))];}))];});}],_0,_);});}),_pm=_p6;return [0,function(_){var _pn=E(_lH),_po=jsQuerySelector(_o9,toJSStr(_pn)),_pp=_po;return new T(function(){var _pq=E(_pp);if(!_pq[0]){var _pr=B(_mx(_pn));}else{var _pr=[0,function(_){var _ps=jsSetCB(E(_pq[1])[1],E(_jF)[1],function(_pt,_pu,_){return new F(function(){return (function(_){return new F(function(){return _d(new T(function(){return [0,B(_iT(_mB,function(_pv){var _pw=new T(function(){return B(_m2(_nQ,_lZ,_pv));});return [0,B(_i8(_mB,_pw,function(_px){return E([0,function(_){var _py=B(A(_lD,[_pw,_])),_pz=_py;return new T(function(){return [0,B(_mA(_mB,_i6))];});}]);}))];}))];}),_0,_);});})(_);});}),_pA=_ps;return [0,function(_){var _pB=jsAppendChild(_o9,E(_nB)[1]);return new T(function(){return B(_nM(_nP[2],function(_pC){return new F(function(){return A(_nO,[[1,_5,_pC]]);});}));});}];}];}return _pr;});}];}];}];}return _oK;});}];}];}]);}]));}return _os;});}];};if(!E(_nR)){return new F(function(){return _ol(_0);});}else{return new F(function(){return _ol(E(_kc));});}}];}return _ok;});}]);}]));}return _oe;});}];}return _o7;});}];}];}];}return _nZ;});}];}];}},_pD=function(_pE){return E([0,function(_){var _pF=E(_ky),_pG=jsFind(toJSStr(_pF)),_pH=_pG;return new T(function(){var _pI=E(_pH);if(!_pI[0]){var _pJ=B(_mc(_pF));}else{var _pJ=[0,function(_){var _pK=function(_pL){var _pM=jsSet(E(_pI[1])[1],toJSStr(E(_kn)),toJSStr(_pL));return new T(function(){return B(A(_mC,[_5]));});};if(!B(_iM(_iL,_mI,_0))){return new F(function(){return _pK(_0);});}else{return new F(function(){return _pK(E(_kc));});}}];}return _pJ;});}]);};if(!B(_4U(_mH,_lF))){if(!B(_4U(_mH,_lE))){var _pN=B(_nM(_mD,_pD));}else{var _pN=B(_nM(B(_jG(_jo,_mD)),_pD));}var _pO=_pN;}else{var _pO=B(_nM(_mI,_pD));}return _pO;}));}));});}];}return _nA;});}];}];}];}return _nr;});}];}];}];}return _nf;});}]);}]);});}else{return new F(function(){return _mt(_n9,_kr);});}}));}),_pP=E(_mU);if(!_pP[0]){var _pQ=E(_mV);}else{var _pQ=[0,function(_){var _pR=E(_pP[1]),_pS=E(_ku),_pT=jsGetAttr(_pR[1],toJSStr(_pS)),_pU=_pT;return [0,function(_){var _pV=B(A(_lT,[_pR,_kt,new T(function(){return B(_4U(_mJ,fromJSStr(_pU)));}),_])),_pW=_pV;return new T(function(){var _pX=function(_pY,_pZ){var _q0=E(_pY);if(!_q0[0]){return new F(function(){return A(_pZ,[_0]);});}else{return [0,function(_){var _q1=E(_q0[1]),_q2=jsGetAttr(_q1[1],toJSStr(_pS)),_q3=_q2;return [0,function(_){var _q4=B(A(_lT,[_q1,_kt,new T(function(){return B(_4U(_mJ,fromJSStr(_q3)));}),_])),_q5=_q4;return new T(function(){return B(_pX(_q0[2],function(_q6){return new F(function(){return A(_pZ,[[1,_q5,_q6]]);});}));});}];}];}};return B(_pX(_pP[2],function(_q7){return E(_mV);}));});}];}];}return _pQ;});}];}]);}))];});});},_q8=[0,1],_q9=function(_qa,_qb){while(1){var _qc=E(_qb);if(!_qc[0]){return E(_qa);}else{var _qd=_qc[1],_qe=!B(_8u(_qa,_qd))?E(_qa):E(_qd);_qb=_qc[2];_qa=_qe;continue;}}},_qf=function(_qg){return E(E(_qg)[1]);},_qh=new T(function(){return B(unCStr(": empty list"));}),_qi=new T(function(){return B(unCStr("Prelude."));}),_qj=function(_qk){return new F(function(){return err(B(_1(_qi,new T(function(){return B(_1(_qk,_qh));}))));});},_ql=new T(function(){return B(unCStr("maximum"));}),_qm=new T(function(){return B(_qj(_ql));}),_qn=function(_qo,_qp,_qq){return [0,[0,new T(function(){if(!B(_j5(_qq,0))){var _qr=E(_q8);}else{var _qs=B(_2L(_qf,_qq));if(!_qs[0]){var _qt=E(_qm);}else{var _qt=B(_6I(B(_q9(_qs[1],_qs[2])),_q8));}var _qr=_qt;}return _qr;}),_qo,_qp],_qq];},_qu=function(_qv){return _qv>0;},_qw=new T(function(){return B(_ju("(function(x) {return x === null;})"));}),_qx=new T(function(){return B(unCStr("No such value"));}),_qy=[0,_qx],_qz=new T(function(){return B(unCStr("Invalid JSON!"));}),_qA=[0,_qz],_qB=new T(function(){return [0,"(function(k) {return localStorage.getItem(k);})"];}),_qC=function(_qD){return E(E(_qD)[3]);},_qE=function(_qF,_qG,_){var _qH=B(A(_ju,[E(_qB)[1],E(toJSStr(E(_qG))),_])),_qI=_qH;return new T(function(){if(!B(_jq(function(_){var _=0,_qJ=B(A(_qw,[E(_qI),_])),_qK=_qJ;return new T(function(){return B(_qu(_qK));});}))){var _qL=String(_qI),_qM=_qL,_qN=jsParseJSON(_qM),_qO=_qN,_qP=E(_qO),_qQ=_qP[0]==0?E(_qA):B(A(_qC,[_qF,_qP[1]]));}else{var _qQ=E(_qy);}return _qQ;});},_qR=new T(function(){return B(unCStr("new-todo"));}),_qS=new T(function(){return B(unCStr("clear-completed"));}),_qT=function(_qU,_){return new F(function(){return _d(new T(function(){return B(A(_qU,[_i6]));}),_0,_);});},_qV=function(_qW,_qX){return new F(function(){return A(_qX,[_qT]);});},_qY=function(_qZ,_r0){return new F(function(){return A(_qZ,[_r0]);});},_r1=[0,_qY,_qV],_r2=function(_r3,_r4){return new F(function(){return A(_r4,[_r3]);});},_r5=function(_r6,_r7,_r8){return new F(function(){return A(_r6,[function(_r9){return new F(function(){return A(_r7,[_r9,_r8]);});}]);});},_ra=function(_rb,_rc,_rd){return new F(function(){return A(_rb,[function(_re){return E(new T(function(){return B(A(_rc,[_rd]));}));}]);});},_rf=function(_rg,_rh,_ri){return new F(function(){return _ra(_rg,_rh,_ri);});},_rj=function(_rk){return new F(function(){return err(_rk);});},_rl=[0,_r5,_rf,_r2,_rj],_rm=[0,_rl,_hV],_rn=[0,_],_ro=function(_rp){return E(E(_rp)[1]);},_rq=function(_rr){return E(E(_rr)[1]);},_rs=new T(function(){return B(_ju("(function(firsthash,cb){window.__old_hash = firsthash;window.onhashchange = function(e){var oldhash = window.__old_hash;var newhash = window.location.hash.split(\'#\')[1] || \'\';window.__old_hash = newhash;B(A(cb, [[0,oldhash],[0,newhash],0]));};})"));}),_rt=function(_ru){return function(_rv,_){var _rw=B(A(new T(function(){return B(A(_rs,[E(E(_ru)[1])]));}),[E(_rv),_])),_rx=_rw;return _5;};},_ry=function(_rz){return E(E(_rz)[2]);},_rA=function(_rB){return E(E(_rB)[2]);},_rC=function(_rD){return E(E(_rD)[1]);},_rE=new T(function(){return B(unCStr("Prelude.undefined"));}),_rF=new T(function(){return B(err(_rE));}),_rG=function(_rH,_rI,_rJ){var _rK=new T(function(){return B(_ro(_rH));});return function(_rL){return new F(function(){return A(new T(function(){return B(_rq(_rK));}),[new T(function(){return B(A(_ry,[_rH,_kb]));}),function(_rM){return new F(function(){return A(new T(function(){return B(_rq(_rK));}),[new T(function(){return B(A(new T(function(){var _rN=E(_rJ);return function(_rO){var _rP=E(_rK);return new F(function(){return A(_rP[1],[new T(function(){return B(A(new T(function(){return B(_rA(_rI));}),[new T(function(){return B(A(_rO,[_rF,_rF]));})]));}),function(_rQ){return new F(function(){return A(_rP[3],[function(_rR){return function(_rS){return new F(function(){return A(new T(function(){return B(_rC(_rI));}),[_rQ,new T(function(){return B(A(new T(function(){return B(A(_rO,[_rR]));}),[_rS]));})]);});};}]);});}]);});};}),[function(_rT,_rU){return new F(function(){return A(_rL,[new T(function(){return B(_h8(_rT));}),new T(function(){return B(_h8(_rU));})]);});}]));}),function(_rV){return new F(function(){return A(new T(function(){return B(_ry(_rH));}),[new T(function(){return B(A(new T(function(){return B(_rt(_rM));}),[_rV]));})]);});}]);});}]);});};},_rW=new T(function(){return B(_rG(_rm,_r1,_rn));}),_rX=function(_rY,_rZ){var _s0=E(_rZ);return [0,_s0[1],_s0[2],_rY];},_s1=function(_s2){return function(_){var _s3=B(_qE(_hU,_kz,_)),_s4=_s3;return [0,function(_){var _s5=nMV([0,new T(function(){var _s6=E(_s4);return _s6[0]==0?[0]:E(_s6[1]);}),_0]),_s7=_s5;return new T(function(){var _s8=[0,_s7],_s9=new T(function(){return [0,B(_mA(_s8,_i6))];});return [0,B(_mA(_s8,function(_sa){return E([0,function(_){var _sb=E(_qR),_sc=jsFind(toJSStr(_sb)),_sd=_sc;return new T(function(){var _se=E(_sd);if(!_se[0]){var _sf=B(_mc(_sb));}else{var _sg=_se[1],_sf=[0,function(_){var _sh=E(_sg),_si=jsSetCB(_sh[1],E(_jD)[1],function(_sj,_){var _sk=B(_i0(E(_sg)[1],_ke,_)),_sl=_sk;return E(E(_sj)[1])==13?B(_j5(_sl,0))<=0?_5:B(_d(new T(function(){return [0,B(_iT(_s8,function(_sm){var _sn=new T(function(){var _so=B(_qn(_sl,_82,_sm));return [1,_so[1],_so[2]];});return [0,B(_i8(_s8,_sn,function(_sp){return E([0,function(_){var _sq=B(A(_lD,[_sn,_])),_sr=_sq;return new T(function(){return B(A(_ja,[_hV,_sg,_ke,_0,function(_ss){return [0,B(_mA(_s8,_i6))];}]));});}]);}))];}))];}),_0,_)):_5;}),_st=_si;return [0,function(_){var _su=B(A(_jV,[_sh,_])),_sv=_su;return [0,function(_){var _sw=E(_ky),_sx=jsFind(toJSStr(_sw)),_sy=_sx;return new T(function(){var _sz=E(_sy);if(!_sz[0]){var _sA=B(_mc(_sw));}else{var _sB=_sz[1],_sA=[0,function(_){var _sC=E(_jF)[1],_sD=jsSetCB(E(_sB)[1],_sC,function(_sE,_sF,_){return new F(function(){return _d([0,function(_){var _sG=jsGet(E(_sB)[1],toJSStr(E(_kn))),_sH=_sG;return new T(function(){return [0,B(_iT(_s8,function(_sI){var _sJ=new T(function(){return B(_2L(function(_oU){return new F(function(){return _rX(new T(function(){return B(_4U(fromJSStr(_sH),_kc));}),_oU);});},_sI));});return [0,B(_i8(_s8,_sJ,function(_sK){return E([0,function(_){var _sL=B(A(_lD,[_sJ,_])),_sM=_sL;return _s9;}]);}))];}))];});}],_0,_);});}),_sN=_sD;return [0,function(_){var _sO=E(_qS),_sP=jsFind(toJSStr(_sO)),_sQ=_sP;return new T(function(){var _sR=E(_sQ);if(!_sR[0]){var _sS=B(_mc(_sO));}else{var _sS=[0,function(_){var _sT=jsSetCB(E(_sR[1])[1],_sC,function(_sU,_sV,_){return new F(function(){return (function(_){return new F(function(){return _d(new T(function(){return [0,B(_iT(_s8,function(_sW){var _sX=new T(function(){return B(_jG(_ji,_sW));});return [0,B(_i8(_s8,_sX,function(_sY){return E([0,function(_){var _sZ=B(A(_lD,[_sX,_])),_t0=_sZ;return _s9;}]);}))];}))];}),_0,_);});})(_);});}),_t1=_sT;return new T(function(){return B(A(_rW,[function(_t2,_t3,_t4){return [0,B(_mA(_s8,_t4))];},_s2]));});}];}return _sS;});}];}];}return _sA;});}];}];}];}return _sf;});}]);}))];});}];};},_t5=new T(function(){return [0,B(_s1(_i6))];}),_t6=function(_){return new F(function(){return _d(_t5,_0,_);});},_t7=function(_){return new F(function(){return _t6(_);});};
var hasteMain = function() {B(A(_t7, [0]));};window.onload = hasteMain;