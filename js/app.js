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

var _0=[0],_1=function(_2,_3){var _4=E(_2);return _4[0]==0?E(_3):[1,_4[1],new T(function(){return B(_1(_4[2],_3));})];},_5=0,_6=function(_7,_){while(1){var _8=E(_7);if(!_8[0]){return _5;}else{var _9=_8[2],_a=E(_8[1]);switch(_a[0]){case 0:var _b=B(A(_a[1],[_])),_c=_b;_7=B(_1(_9,[1,_c,_0]));continue;case 1:_7=B(_1(_9,_a[1]));continue;default:_7=_9;continue;}}}},_d=function(_e,_f,_){var _g=E(_e);switch(_g[0]){case 0:var _h=B(A(_g[1],[_])),_i=_h;return new F(function(){return _6(B(_1(_f,[1,_i,_0])),_);});break;case 1:return new F(function(){return _6(B(_1(_f,_g[1])),_);});break;default:return new F(function(){return _6(_f,_);});}},_j=function(_k,_l){return [0,function(_){var _m=B(A(_k,[_])),_n=_m;return new T(function(){return B(A(_l,[_n]));});}];},_o=function(_p,_q,_){var _r=jsGet(_p,toJSStr(E(_q))),_s=_r;return new T(function(){return fromJSStr(_s);});},_t=[2],_u=function(_v){return [2];},_w=function(_x,_y,_z){return function(_){var _A=E(_x)[1],_B=rMV(_A),_C=_B,_D=E(_C);if(!_D[0]){var _=wMV(_A,[0,_D[1],new T(function(){return B(_1(_D[2],[1,[0,_y,function(_E){return E(new T(function(){return B(A(_z,[_5]));}));}],_0]));})]);return _t;}else{var _F=E(_D[1]);if(!_F[0]){var _=wMV(_A,[0,_y,_0]);return new T(function(){return B(A(_z,[_5]));});}else{var _=wMV(_A,[1,_F[2]]);return [1,[1,new T(function(){return B(A(_z,[_5]));}),[1,new T(function(){return B(A(_F[1],[_y,_u]));}),_0]]];}}};},_G=function(_H,_I){var _J=E(_H);if(!_J[0]){var _K=_J[1],_L=E(_I);return _L[0]==0?_K==_L[1]:I_compareInt(_L[1],_K)==0?true:false;}else{var _M=_J[1],_N=E(_I);return _N[0]==0?I_compareInt(_M,_N[1])==0?true:false:I_compare(_M,_N[1])==0?true:false;}},_O=function(_P,_Q){while(1){var _R=E(_P);if(!_R[0]){return E(_Q)[0]==0?true:false;}else{var _S=E(_Q);if(!_S[0]){return false;}else{if(E(_R[1])[1]!=E(_S[1])[1]){return false;}else{_P=_R[2];_Q=_S[2];continue;}}}}},_T=function(_U,_V){var _W=E(_U),_X=E(_V),_Y=_X[3];return !B(_G(_W[1],_X[1]))?true:!B(_O(_W[2],_X[2]))?true:!E(_W[3])?E(_Y):!E(_Y)?true:false;},_Z=function(_10,_11){return !E(_10)?!E(_11)?true:false:E(_11);},_12=function(_13,_14,_15,_16,_17,_18){return !B(_G(_13,_16))?false:!B(_O(_14,_17))?false:B(_Z(_15,_18));},_19=function(_1a,_1b){var _1c=E(_1a),_1d=E(_1b);return new F(function(){return _12(_1c[1],_1c[2],_1c[3],_1d[1],_1d[2],_1d[3]);});},_1e=[0,_19,_T],_1f=function(_1g){return E(E(_1g)[1]);},_1h=function(_1i,_1j,_1k){while(1){var _1l=E(_1j);if(!_1l[0]){return E(_1k)[0]==0?true:false;}else{var _1m=E(_1k);if(!_1m[0]){return false;}else{if(!B(A(_1f,[_1i,_1l[1],_1m[1]]))){return false;}else{_1j=_1l[2];_1k=_1m[2];continue;}}}}},_1n=[1,_0],_1o=function(_1p,_1q){return function(_){var _1r=E(_1p)[1],_1s=rMV(_1r),_1t=_1s,_1u=E(_1t);if(!_1u[0]){var _1v=_1u[1],_1w=E(_1u[2]);if(!_1w[0]){var _=wMV(_1r,_1n);return new T(function(){return B(A(_1q,[_1v]));});}else{var _1x=E(_1w[1]),_=wMV(_1r,[0,_1x[1],_1w[2]]);return [1,[1,new T(function(){return B(A(_1q,[_1v]));}),[1,new T(function(){return B(A(_1x[2],[_u]));}),_0]]];}}else{var _=wMV(_1r,[1,new T(function(){return B(_1(_1u[1],[1,function(_1y){return function(_1z){return E(new T(function(){return B(A(_1q,[_1y]));}));};},_0]));})]);return _t;}};},_1A=function(_1B,_1C){while(1){var _1D=E(_1B);if(!_1D[0]){return E(_1C);}else{_1B=_1D[2];var _1E=_1C+1|0;_1C=_1E;continue;}}},_1F=function(_1G,_1H,_1I,_1J){return new F(function(){return A(_1G,[function(_){var _1K=jsSet(E(_1H)[1],toJSStr(E(_1I)),toJSStr(E(_1J)));return _5;}]);});},_1L=function(_1M,_1N){var _1O=jsShowI(_1M),_1P=_1O;return new F(function(){return _1(fromJSStr(_1P),_1N);});},_1Q=[0,41],_1R=[0,40],_1S=function(_1T,_1U,_1V){if(_1U>=0){return new F(function(){return _1L(_1U,_1V);});}else{return _1T<=6?B(_1L(_1U,_1V)):[1,_1R,new T(function(){var _1W=jsShowI(_1U),_1X=_1W;return B(_1(fromJSStr(_1X),[1,_1Q,_1V]));})];}},_1Y=false,_1Z=true,_20=function(_21){return new F(function(){return A(_21,[_5]);});},_22=function(_23){return !E(E(_23)[3])?true:false;},_24=function(_25,_26){while(1){var _27=E(_26);if(!_27[0]){return false;}else{if(!B(A(_25,[_27[1]]))){_26=_27[2];continue;}else{return true;}}}},_28=function(_29){return E(E(_29)[3]);},_2a=function(_2b){var _2c=B(A(_2b,[_])),_2d=_2c;return E(_2d);},_2e=function(_2f){return new F(function(){return _2a(function(_){var _=0;return new F(function(){return eval(_2f);});});});},_2g=function(_){var _2h=B(A(_2e,["document",_])),_2i=_2h;return [0,_2i];},_2j=function(_){return new F(function(){return _2g(_);});},_2k=function(_){var _=0;return new F(function(){return _2j(_);});},_2l=new T(function(){return B(_2a(_2k));}),_2m=new T(function(){return [0,"blur"];}),_2n=new T(function(){return [0,"keyup"];}),_2o=new T(function(){return [0,"dblclick"];}),_2p=new T(function(){return [0,"click"];}),_2q=function(_2r,_2s){while(1){var _2t=(function(_2u,_2v){var _2w=E(_2v);if(!_2w[0]){return [0];}else{var _2x=_2w[1],_2y=_2w[2];if(!B(A(_2u,[_2x]))){var _2z=_2u;_2s=_2y;_2r=_2z;return null;}else{return [1,_2x,new T(function(){return B(_2q(_2u,_2y));})];}}})(_2r,_2s);if(_2t!=null){return _2t;}}},_2A=new T(function(){return B(_2e("(function(e) {e.focus();})"));}),_2B=function(_2C,_){var _2D=B(A(_2A,[E(E(_2C)[1]),_])),_2E=_2D;return _5;},_2F=function(_2G,_){return new F(function(){return _2B(_2G,_);});},_2H=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_2I=new T(function(){return B(err(_2H));}),_2J=function(_2K){var _2L=E(_2K);return _2L[0]==0?E(_2I):E(_2L[1]);},_2M=function(_2N){return E(_2N)[0]==0?true:false;},_2O=function(_2P){var _2Q=String(_2P),_2R=_2Q;return [0,_2R];},_2S=function(_){var _2T=B(A(_2e,["(function() {return location.hash.substring(1);})",_])),_2U=_2T;return new T(function(){return B(_2O(_2U));});},_2V=function(_){return new F(function(){return _2S(_);});},_2W=new T(function(){return B(unCStr("true"));}),_2X=new T(function(){return B(unCStr("checked"));}),_2Y=new T(function(){return B(unCStr("value"));}),_2Z=new T(function(){return B(unCStr("label"));}),_30=new T(function(){return B(unCStr("todo-list"));}),_31=[0,41],_32=[1,_31,_0],_33=new T(function(){return B(unCStr("#clear-completed"));}),_34=new T(function(){return B(unCStr("item"));}),_35=new T(function(){return B(unCStr("items"));}),_36=new T(function(){return B(unCStr("#todo-count"));}),_37=new T(function(){return B(unCStr("toggle-all"));}),_38=new T(function(){return B(unCStr("footer"));}),_39=[1,_38,_0],_3a=[1,_37,_39],_3b=[0,35],_3c=new T(function(){return B(unCStr("selected"));}),_3d=new T(function(){return B(unCStr("href"));}),_3e=new T(function(){return B(unCStr("#filters li a"));}),_3f=new T(function(){return B(unCStr("hidden"));}),_3g=new T(function(){return B(unCStr("div"));}),_3h=new T(function(){return B(unCStr("/completed"));}),_3i=new T(function(){return B(unCStr("/active"));}),_3j=new T(function(){return B(unCStr("li"));}),_3k=new T(function(){return B(unCStr(".toggle"));}),_3l=new T(function(){return B(unCStr(".edit"));}),_3m=new T(function(){return B(unCStr("editing"));}),_3n=new T(function(){return B(unCStr("completed"));}),_3o=function(_3p,_3q){var _3r=E(_3q);return _3r[0]==0?[0]:[1,new T(function(){return B(A(_3p,[_3r[1]]));}),new T(function(){return B(_3o(_3p,_3r[2]));})];},_3s=function(_3t,_3u){var _3v=E(_3u);return [0,_3v[1],_3t,_3v[3]];},_3w=new T(function(){return B(_2e("(function(e,c,x){x?e.classList.add(c):e.classList.remove(c);})"));}),_3x=function(_){var _=0;return new F(function(){return A(_2e,["false",_]);});},_3y=new T(function(){return B(_2a(_3x));}),_3z=function(_){var _=0;return new F(function(){return A(_2e,["true",_]);});},_3A=new T(function(){return B(_2a(_3z));}),_3B=function(_3C){return function(_3D){return function(_3E,_){var _3F=B(A(new T(function(){return B(A(new T(function(){return B(A(_3w,[E(E(_3C)[1])]));}),[E(toJSStr(E(_3D)))]));}),[!E(_3E)?E(_3y):E(_3A),_])),_3G=_3F;return _5;};};},_3H=function(_3I){var _3J=E(_3I);return [0,_3J[1],_3J[2],new T(function(){return !E(_3J[3])?true:false;})];},_3K=function(_3L,_3M,_3N){return new F(function(){return _3o(function(_3O){var _3P=E(_3O),_3Q=E(_3L),_3R=_3Q[3];return !B(_G(_3P[1],_3Q[1]))?E(_3P):!B(_O(_3P[2],_3Q[2]))?E(_3P):!E(_3P[3])?!E(_3R)?B(A(_3M,[_3Q])):E(_3P):!E(_3R)?E(_3P):B(A(_3M,[_3Q]));},_3N);});},_3S=new T(function(){return B(unCStr("template-todo"));}),_3T=new T(function(){return B(unCStr(" could be found!"));}),_3U=function(_3V){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_1(_3V,_3T));}))));});},_3W=[0,34],_3X=function(_3Y,_3Z){while(1){var _40=(function(_41,_42){var _43=E(_41);if(!_43[0]){return [0];}else{var _44=_43[2],_45=E(_42);if(!_45[0]){return [0];}else{var _46=_45[2];if(!E(_45[1])[0]){return [1,_43[1],new T(function(){return B(_3X(_44,_46));})];}else{_3Y=_44;_3Z=_46;return null;}}}})(_3Y,_3Z);if(_40!=null){return _40;}}},_47=new T(function(){return B(unAppCStr("[]",_0));}),_48=[0,91],_49=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_4a=new T(function(){return B(err(_49));}),_4b=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_4c=new T(function(){return B(err(_4b));}),_4d=function(_4e,_4f){while(1){var _4g=E(_4e);if(!_4g[0]){return E(_4c);}else{var _4h=E(_4f);if(!_4h){return E(_4g[1]);}else{_4e=_4g[2];_4f=_4h-1|0;continue;}}}},_4i=new T(function(){return B(unCStr("ACK"));}),_4j=new T(function(){return B(unCStr("BEL"));}),_4k=new T(function(){return B(unCStr("BS"));}),_4l=new T(function(){return B(unCStr("SP"));}),_4m=[1,_4l,_0],_4n=new T(function(){return B(unCStr("US"));}),_4o=[1,_4n,_4m],_4p=new T(function(){return B(unCStr("RS"));}),_4q=[1,_4p,_4o],_4r=new T(function(){return B(unCStr("GS"));}),_4s=[1,_4r,_4q],_4t=new T(function(){return B(unCStr("FS"));}),_4u=[1,_4t,_4s],_4v=new T(function(){return B(unCStr("ESC"));}),_4w=[1,_4v,_4u],_4x=new T(function(){return B(unCStr("SUB"));}),_4y=[1,_4x,_4w],_4z=new T(function(){return B(unCStr("EM"));}),_4A=[1,_4z,_4y],_4B=new T(function(){return B(unCStr("CAN"));}),_4C=[1,_4B,_4A],_4D=new T(function(){return B(unCStr("ETB"));}),_4E=[1,_4D,_4C],_4F=new T(function(){return B(unCStr("SYN"));}),_4G=[1,_4F,_4E],_4H=new T(function(){return B(unCStr("NAK"));}),_4I=[1,_4H,_4G],_4J=new T(function(){return B(unCStr("DC4"));}),_4K=[1,_4J,_4I],_4L=new T(function(){return B(unCStr("DC3"));}),_4M=[1,_4L,_4K],_4N=new T(function(){return B(unCStr("DC2"));}),_4O=[1,_4N,_4M],_4P=new T(function(){return B(unCStr("DC1"));}),_4Q=[1,_4P,_4O],_4R=new T(function(){return B(unCStr("DLE"));}),_4S=[1,_4R,_4Q],_4T=new T(function(){return B(unCStr("SI"));}),_4U=[1,_4T,_4S],_4V=new T(function(){return B(unCStr("SO"));}),_4W=[1,_4V,_4U],_4X=new T(function(){return B(unCStr("CR"));}),_4Y=[1,_4X,_4W],_4Z=new T(function(){return B(unCStr("FF"));}),_50=[1,_4Z,_4Y],_51=new T(function(){return B(unCStr("VT"));}),_52=[1,_51,_50],_53=new T(function(){return B(unCStr("LF"));}),_54=[1,_53,_52],_55=new T(function(){return B(unCStr("HT"));}),_56=[1,_55,_54],_57=[1,_4k,_56],_58=[1,_4j,_57],_59=[1,_4i,_58],_5a=new T(function(){return B(unCStr("ENQ"));}),_5b=[1,_5a,_59],_5c=new T(function(){return B(unCStr("EOT"));}),_5d=[1,_5c,_5b],_5e=new T(function(){return B(unCStr("ETX"));}),_5f=[1,_5e,_5d],_5g=new T(function(){return B(unCStr("STX"));}),_5h=[1,_5g,_5f],_5i=new T(function(){return B(unCStr("SOH"));}),_5j=[1,_5i,_5h],_5k=new T(function(){return B(unCStr("NUL"));}),_5l=[1,_5k,_5j],_5m=[0,92],_5n=new T(function(){return B(unCStr("\\DEL"));}),_5o=new T(function(){return B(unCStr("\\a"));}),_5p=new T(function(){return B(unCStr("\\\\"));}),_5q=new T(function(){return B(unCStr("\\SO"));}),_5r=new T(function(){return B(unCStr("\\r"));}),_5s=new T(function(){return B(unCStr("\\f"));}),_5t=new T(function(){return B(unCStr("\\v"));}),_5u=new T(function(){return B(unCStr("\\n"));}),_5v=new T(function(){return B(unCStr("\\t"));}),_5w=new T(function(){return B(unCStr("\\b"));}),_5x=function(_5y,_5z){if(_5y<=127){var _5A=E(_5y);switch(_5A){case 92:return new F(function(){return _1(_5p,_5z);});break;case 127:return new F(function(){return _1(_5n,_5z);});break;default:if(_5A<32){var _5B=E(_5A);switch(_5B){case 7:return new F(function(){return _1(_5o,_5z);});break;case 8:return new F(function(){return _1(_5w,_5z);});break;case 9:return new F(function(){return _1(_5v,_5z);});break;case 10:return new F(function(){return _1(_5u,_5z);});break;case 11:return new F(function(){return _1(_5t,_5z);});break;case 12:return new F(function(){return _1(_5s,_5z);});break;case 13:return new F(function(){return _1(_5r,_5z);});break;case 14:return new F(function(){return _1(_5q,new T(function(){var _5C=E(_5z);if(!_5C[0]){var _5D=[0];}else{var _5D=E(E(_5C[1])[1])==72?B(unAppCStr("\\&",_5C)):E(_5C);}return _5D;}));});break;default:return new F(function(){return _1([1,_5m,new T(function(){var _5E=_5B;return _5E>=0?B(_4d(_5l,_5E)):E(_4a);})],_5z);});}}else{return [1,[0,_5A],_5z];}}}else{return [1,_5m,new T(function(){var _5F=jsShowI(_5y),_5G=_5F;return B(_1(fromJSStr(_5G),new T(function(){var _5H=E(_5z);if(!_5H[0]){var _5I=[0];}else{var _5J=E(_5H[1])[1];if(_5J<48){var _5K=E(_5H);}else{var _5K=_5J>57?E(_5H):B(unAppCStr("\\&",_5H));}var _5L=_5K,_5M=_5L,_5I=_5M;}return _5I;})));})];}},_5N=new T(function(){return B(unCStr("\\\""));}),_5O=function(_5P,_5Q){var _5R=E(_5P);if(!_5R[0]){return E(_5Q);}else{var _5S=_5R[2],_5T=E(E(_5R[1])[1]);if(_5T==34){return new F(function(){return _1(_5N,new T(function(){return B(_5O(_5S,_5Q));}));});}else{return new F(function(){return _5x(_5T,new T(function(){return B(_5O(_5S,_5Q));}));});}}},_5U=[0,93],_5V=[1,_5U,_0],_5W=[0,44],_5X=function(_5Y){var _5Z=E(_5Y);return _5Z[0]==0?E(_5V):[1,_5W,[1,_3W,new T(function(){return B(_5O(_5Z[1],[1,_3W,new T(function(){return B(_5X(_5Z[2]));})]));})]];},_60=function(_61,_62){return new F(function(){return err(B(unAppCStr("Elements with the following IDs could not be found: ",new T(function(){var _63=B(_3X(_62,_61));return _63[0]==0?E(_47):[1,_48,[1,_3W,new T(function(){return B(_5O(_63[1],[1,_3W,new T(function(){return B(_5X(_63[2]));})]));})]];}))));});},_64=function(_65){return new F(function(){return err(B(unAppCStr("No element with selector ",new T(function(){return B(_1(_65,_3T));}))));});},_66=new T(function(){return B(unCStr("innerHTML"));}),_67=function(_68,_69){return new F(function(){return _1o(_68,function(_6a){return [0,B(_w(_68,_6a,function(_6b){return E([0,function(_){var _6c=B(_2V(_)),_6d=_6c,_6e=new T(function(){return fromJSStr(E(_6d)[1]);}),_6f=new T(function(){return B(_2q(_22,_6a));}),_6g=[1,_3b,_6e],_6h=function(_6i){var _6j=E(_6i);return _6j[0]==0?E(_20):function(_6k){return [0,function(_){var _6l=B(A(new T(function(){return B(A(_3B,[_6j[1],_3f,new T(function(){return B(_1h(_1e,_6a,_0));})]));}),[_])),_6m=_6l;return new T(function(){return B(A(new T(function(){return B(_6h(_6j[2]));}),[_6k]));});}];};},_6n=new T(function(){return [0,B(_1A(_6f,0))];}),_6o=new T(function(){return [0,B(_1A(B(_2q(_28,_6a)),0))];});return [0,function(_){var _6p=E(_2l)[1],_6q=jsQuerySelectorAll(_6p,toJSStr(E(_3e))),_6r=_6q;return new T(function(){var _6s=new T(function(){var _6t=function(_6u,_6v){var _6w=E(_6u);if(!_6w[0]){return new F(function(){return A(_6v,[_0]);});}else{return [0,function(_){var _6x=jsFind(toJSStr(E(_6w[1]))),_6y=_6x;return new T(function(){return B(_6t(_6w[2],function(_6z){return new F(function(){return A(_6v,[[1,_6y,_6z]]);});}));});}];}};return B((function(_6A,_6B,_6C){return [0,function(_){var _6D=jsFind(toJSStr(E(_6A))),_6E=_6D;return new T(function(){return B(_6t(_6B,function(_6F){return new F(function(){return A(_6C,[[1,_6E,_6F]]);});}));});}];})(_37,_39,function(_6G){if(!B(_24(_2M,_6G))){return new F(function(){return A(_6h,[B(_3o(_2J,_6G)),function(_6H){return E([0,function(_){var _6I=E(_36),_6J=jsQuerySelector(_6p,toJSStr(_6I)),_6K=_6J;return new T(function(){var _6L=E(_6K);if(!_6L[0]){var _6M=B(_64(_6I));}else{var _6N=_6L[1],_6M=[0,function(_){var _6O=B(A(_3B,[_6N,_3f,new T(function(){return E(E(_6n)[1])==0?true:false;}),_])),_6P=_6O;return [0,function(_){var _6Q=E(_66),_6R=toJSStr(_6Q),_6S=jsSet(E(_6N)[1],_6R,toJSStr(B(unAppCStr("<strong>",new T(function(){var _6T=E(_6n)[1];return B(_1(B(_1S(0,_6T,_0)),new T(function(){return B(unAppCStr("</strong> ",new T(function(){return E(_6T)==1?E(_34):E(_35);})));})));})))));return [0,function(_){var _6U=E(_33),_6V=jsQuerySelector(_6p,toJSStr(_6U)),_6W=_6V;return new T(function(){var _6X=E(_6W);if(!_6X[0]){var _6Y=B(_64(_6U));}else{var _6Z=_6X[1],_6Y=[0,function(_){var _70=B(A(_3B,[_6Z,_3f,new T(function(){return E(E(_6o)[1])==0?true:false;}),_])),_71=_70;return [0,function(_){var _72=jsSet(E(_6Z)[1],_6R,toJSStr(B(unAppCStr("Clear completed (",new T(function(){return B(_1(B(_1S(0,E(_6o)[1],_0)),_32));})))));return [0,function(_){var _73=E(_30),_74=jsFind(toJSStr(_73)),_75=_74;return new T(function(){var _76=E(_75);if(!_76[0]){var _77=B(_3U(_73));}else{var _78=_76[1],_77=[0,function(_){var _79=E(_78)[1],_7a=jsQuerySelectorAll(_79,toJSStr(E(_3j))),_7b=_7a;return new T(function(){var _7c=function(_7d,_7e){var _7f=E(_7d);if(!_7f[0]){return new F(function(){return A(_7e,[_0]);});}else{return [0,function(_){var _7g=jsKillChild(E(_7f[1])[1],_79);return new T(function(){return B(_7c(_7f[2],function(_7h){return new F(function(){return A(_7e,[[1,_5,_7h]]);});}));});}];}};return B(_7c(_7b,function(_7i){return E(new T(function(){var _7j=function(_7k,_7l){var _7m=E(_7k);if(!_7m[0]){return new F(function(){return A(_7l,[_0]);});}else{var _7n=_7m[1],_7o=new T(function(){return E(E(_7n)[3]);}),_7p=new T(function(){return E(E(_7n)[2]);});return [0,function(_){var _7q=jsCreateElem(toJSStr(E(_3g))),_7r=_7q;return [0,function(_){var _7s=E(_3S),_7t=jsFind(toJSStr(_7s)),_7u=_7t;return new T(function(){var _7v=E(_7u);if(!_7v[0]){var _7w=B(_3U(_7s));}else{var _7w=[0,function(_){var _7x=jsGet(E(_7v[1])[1],toJSStr(_6Q)),_7y=_7x;return [0,function(_){var _7z=jsSet(_7r,_6R,toJSStr(fromJSStr(_7y)));return [0,function(_){var _7A=E(_3j),_7B=jsQuerySelector(_7r,toJSStr(_7A)),_7C=_7B;return new T(function(){var _7D=E(_7C);if(!_7D[0]){var _7E=B(_64(_7A));}else{var _7E=[0,function(_){var _7F=E(_7D[1]),_7G=_7F[1],_7H=E(_2Z),_7I=jsQuerySelector(_7G,toJSStr(_7H)),_7J=_7I;return new T(function(){var _7K=E(_7J);if(!_7K[0]){var _7L=B(_64(_7H));}else{var _7L=B(A(_1F,[_j,_7K[1],_6Q,_7p,function(_7M){return E([0,function(_){var _7N=E(_3k),_7O=jsQuerySelector(_7G,toJSStr(_7N)),_7P=_7O;return new T(function(){var _7Q=E(_7P);if(!_7Q[0]){var _7R=B(_64(_7N));}else{var _7R=[0,function(_){var _7S=function(_7T){var _7U=jsSet(E(_7Q[1])[1],toJSStr(E(_2X)),toJSStr(_7T));return [0,function(_){var _7V=E(_3l),_7W=jsQuerySelector(_7G,toJSStr(_7V)),_7X=_7W;return new T(function(){var _7Y=E(_7X);if(!_7Y[0]){var _7Z=B(_64(_7V));}else{var _7Z=B(A(_1F,[_j,_7Y[1],_2Y,_7p,function(_80){return E([0,function(_){var _81=B(A(_3B,[_7F,_3n,_7o,_])),_82=_81;return [0,function(_){var _83=jsSetCB(_7G,E(_2o)[1],function(_84,_85,_){var _86=B(A(new T(function(){return B(_3B(_7F));}),[_3m,_1Z,_])),_87=_86,_88=E(_3l),_89=jsQuerySelector(_7G,toJSStr(_88)),_8a=_89,_8b=E(_8a);return _8b[0]==0?B(_64(_88)):B(A(_2F,[_8b[1],_]));}),_8c=_83;return [0,function(_){var _8d=E(_3l),_8e=jsQuerySelector(_7G,toJSStr(_8d)),_8f=_8e;return new T(function(){var _8g=E(_8f);if(!_8g[0]){var _8h=B(_64(_8d));}else{var _8i=_8g[1],_8h=[0,function(_){var _8j=E(_8i)[1],_8k=jsSetCB(_8j,E(_2n)[1],function(_8l,_){if(E(E(_8l)[1])==13){return new F(function(){return _d([0,function(_){var _8m=jsGet(E(_8i)[1],toJSStr(E(_2Y))),_8n=_8m;return new T(function(){var _8o=new T(function(){return fromJSStr(_8n);});return [0,B(_1o(_68,function(_8p){return [0,B(_w(_68,new T(function(){return B(_3K(_7n,function(_8q){return new F(function(){return _3s(_8o,_8q);});},_8p));}),function(_8r){return E([0,function(_){var _8s=jsQuerySelector(_7G,toJSStr(_7H)),_8t=_8s;return new T(function(){var _8u=E(_8t);if(!_8u[0]){var _8v=B(_64(_7H));}else{var _8v=B(A(_1F,[_j,_8u[1],_6Q,_8o,function(_8w){return E([0,function(_){var _8x=B(A(_3B,[[0,_7G],_3m,_1Y,_])),_8y=_8x;return _t;}]);}]));}return _8v;});}]);}))];}))];});}],_0,_);});}else{return _5;}}),_8z=_8k;return [0,function(_){var _8A=jsSetCB(_8j,E(_2m)[1],function(_){return new F(function(){return _d([0,function(_){var _8B=jsGet(_8j,toJSStr(E(_2Y))),_8C=_8B;return new T(function(){var _8D=new T(function(){return fromJSStr(_8C);});return [0,B(_1o(_68,function(_8E){return [0,B(_w(_68,new T(function(){return B(_3K(_7n,function(_8q){return new F(function(){return _3s(_8D,_8q);});},_8E));}),function(_8F){return E([0,function(_){var _8G=jsQuerySelector(_7G,toJSStr(_7H)),_8H=_8G;return new T(function(){var _8I=E(_8H);if(!_8I[0]){var _8J=B(_64(_7H));}else{var _8J=B(A(_1F,[_j,_8I[1],_6Q,_8D,function(_8K){return E([0,function(_){var _8L=B(A(_3B,[[0,_7G],_3m,_1Y,_])),_8M=_8L;return _t;}]);}]));}return _8J;});}]);}))];}))];});}],_0,_);});}),_8N=_8A;return [0,function(_){var _8O=E(_3k),_8P=jsQuerySelector(_7G,toJSStr(_8O)),_8Q=_8P;return new T(function(){var _8R=E(_8Q);if(!_8R[0]){var _8S=B(_64(_8O));}else{var _8S=[0,function(_){var _8T=jsSetCB(E(_8R[1])[1],E(_2p)[1],function(_8U,_8V,_){return new F(function(){return (function(_){return new F(function(){return _d(new T(function(){return [0,B(_1o(_68,function(_8W){return [0,B(_w(_68,new T(function(){return B(_3K(_7n,_3H,_8W));}),function(_8X){return [0,B(_67(_68,_u))];}))];}))];}),_0,_);});})(_);});}),_8Y=_8T;return [0,function(_){var _8Z=jsAppendChild(_7G,E(_78)[1]);return new T(function(){return B(_7j(_7m[2],function(_90){return new F(function(){return A(_7l,[[1,_5,_90]]);});}));});}];}];}return _8S;});}];}];}];}return _8h;});}];}];}]);}]));}return _7Z;});}];};if(!E(_7o)){return new F(function(){return _7S(_0);});}else{return new F(function(){return _7S(E(_2W));});}}];}return _7R;});}]);}]));}return _7L;});}];}return _7E;});}];}];}];}return _7w;});}];}];}},_91=function(_92){return E([0,function(_){var _93=E(_37),_94=jsFind(toJSStr(_93)),_95=_94;return new T(function(){var _96=E(_95);if(!_96[0]){var _97=B(_3U(_93));}else{var _97=[0,function(_){var _98=function(_99){var _9a=jsSet(E(_96[1])[1],toJSStr(E(_2X)),toJSStr(_99));return new T(function(){return B(A(_69,[_5]));});};if(!B(_1h(_1e,_6f,_0))){return new F(function(){return _98(_0);});}else{return new F(function(){return _98(E(_2W));});}}];}return _97;});}]);};if(!B(_O(_6e,_3i))){if(!B(_O(_6e,_3h))){var _9b=B(_7j(_6a,_91));}else{var _9b=B(_7j(B(_2q(_28,_6a)),_91));}var _9c=_9b;}else{var _9c=B(_7j(_6f,_91));}return _9c;}));}));});}];}return _77;});}];}];}];}return _6Y;});}];}];}];}return _6M;});}]);}]);});}else{return new F(function(){return _60(_6G,_3a);});}}));}),_9d=E(_6r);if(!_9d[0]){var _9e=E(_6s);}else{var _9e=[0,function(_){var _9f=E(_9d[1]),_9g=E(_3d),_9h=jsGetAttr(_9f[1],toJSStr(_9g)),_9i=_9h;return [0,function(_){var _9j=B(A(_3B,[_9f,_3c,new T(function(){return B(_O(_6g,fromJSStr(_9i)));}),_])),_9k=_9j;return new T(function(){var _9l=function(_9m,_9n){var _9o=E(_9m);if(!_9o[0]){return new F(function(){return A(_9n,[_0]);});}else{return [0,function(_){var _9p=E(_9o[1]),_9q=jsGetAttr(_9p[1],toJSStr(_9g)),_9r=_9q;return [0,function(_){var _9s=B(A(_3B,[_9p,_3c,new T(function(){return B(_O(_6g,fromJSStr(_9r)));}),_])),_9t=_9s;return new T(function(){return B(_9l(_9o[2],function(_9u){return new F(function(){return A(_9n,[[1,_9t,_9u]]);});}));});}];}];}};return B(_9l(_9d[2],function(_9v){return E(_6s);}));});}];}];}return _9e;});}];}]);}))];});});},_9w=[0,1],_9x=function(_9y,_9z){var _9A=E(_9y);if(!_9A[0]){var _9B=_9A[1],_9C=E(_9z);return _9C[0]==0?_9B<=_9C[1]:I_compareInt(_9C[1],_9B)>=0;}else{var _9D=_9A[1],_9E=E(_9z);return _9E[0]==0?I_compareInt(_9D,_9E[1])<=0:I_compare(_9D,_9E[1])<=0;}},_9F=function(_9G,_9H){while(1){var _9I=E(_9H);if(!_9I[0]){return E(_9G);}else{var _9J=_9I[1],_9K=!B(_9x(_9G,_9J))?E(_9G):E(_9J);_9H=_9I[2];_9G=_9K;continue;}}},_9L=function(_9M){return E(E(_9M)[1]);},_9N=new T(function(){return B(unCStr(": empty list"));}),_9O=new T(function(){return B(unCStr("Prelude."));}),_9P=function(_9Q){return new F(function(){return err(B(_1(_9O,new T(function(){return B(_1(_9Q,_9N));}))));});},_9R=new T(function(){return B(unCStr("maximum"));}),_9S=new T(function(){return B(_9P(_9R));}),_9T=function(_9U,_9V){while(1){var _9W=E(_9U);if(!_9W[0]){var _9X=_9W[1],_9Y=E(_9V);if(!_9Y[0]){var _9Z=_9Y[1],_a0=addC(_9X,_9Z);if(!E(_a0[2])){return [0,_a0[1]];}else{_9U=[1,I_fromInt(_9X)];_9V=[1,I_fromInt(_9Z)];continue;}}else{_9U=[1,I_fromInt(_9X)];_9V=_9Y;continue;}}else{var _a1=E(_9V);if(!_a1[0]){_9U=_9W;_9V=[1,I_fromInt(_a1[1])];continue;}else{return [1,I_add(_9W[1],_a1[1])];}}}},_a2=function(_a3,_a4,_a5){return [0,[0,new T(function(){if(!B(_1A(_a5,0))){var _a6=E(_9w);}else{var _a7=B(_3o(_9L,_a5));if(!_a7[0]){var _a8=E(_9S);}else{var _a8=B(_9T(B(_9F(_a7[1],_a7[2])),_9w));}var _a6=_a8;}return _a6;}),_a3,_a4],_a5];},_a9=new T(function(){return B(unCStr("new-todo"));}),_aa=new T(function(){return B(unCStr("clear-completed"));}),_ab=function(_ac,_){return new F(function(){return _d(new T(function(){return B(A(_ac,[_u]));}),_0,_);});},_ad=function(_ae,_af){return new F(function(){return A(_af,[_ab]);});},_ag=function(_ah,_ai){return new F(function(){return A(_ah,[_ai]);});},_aj=[0,_ag,_ad],_ak=function(_al,_am){return new F(function(){return A(_am,[_al]);});},_an=function(_ao,_ap,_aq){return new F(function(){return A(_ao,[function(_ar){return new F(function(){return A(_ap,[_ar,_aq]);});}]);});},_as=function(_at,_au,_av){return new F(function(){return A(_at,[function(_aw){return E(new T(function(){return B(A(_au,[_av]));}));}]);});},_ax=function(_ay,_az,_aA){return new F(function(){return _as(_ay,_az,_aA);});},_aB=function(_aC){return new F(function(){return err(_aC);});},_aD=[0,_an,_ax,_ak,_aB],_aE=[0,_aD,_j],_aF=[0,_],_aG=function(_aH){return E(E(_aH)[1]);},_aI=function(_aJ){return E(E(_aJ)[1]);},_aK=function(_aL){return new F(function(){return fromJSStr(E(_aL)[1]);});},_aM=new T(function(){return B(_2e("(function(firsthash,cb){window.__old_hash = firsthash;window.onhashchange = function(e){var oldhash = window.__old_hash;var newhash = window.location.hash.split(\'#\')[1] || \'\';window.__old_hash = newhash;B(A(cb, [[0,oldhash],[0,newhash],0]));};})"));}),_aN=function(_aO){return function(_aP,_){var _aQ=B(A(new T(function(){return B(A(_aM,[E(E(_aO)[1])]));}),[E(_aP),_])),_aR=_aQ;return _5;};},_aS=function(_aT){return E(E(_aT)[2]);},_aU=function(_aV){return E(E(_aV)[2]);},_aW=function(_aX){return E(E(_aX)[1]);},_aY=new T(function(){return B(unCStr("Prelude.undefined"));}),_aZ=new T(function(){return B(err(_aY));}),_b0=function(_b1,_b2,_b3){var _b4=new T(function(){return B(_aG(_b1));});return function(_b5){return new F(function(){return A(new T(function(){return B(_aI(_b4));}),[new T(function(){return B(A(_aS,[_b1,_2V]));}),function(_b6){return new F(function(){return A(new T(function(){return B(_aI(_b4));}),[new T(function(){return B(A(new T(function(){var _b7=E(_b3);return function(_b8){var _b9=E(_b4);return new F(function(){return A(_b9[1],[new T(function(){return B(A(new T(function(){return B(_aU(_b2));}),[new T(function(){return B(A(_b8,[_aZ,_aZ]));})]));}),function(_ba){return new F(function(){return A(_b9[3],[function(_bb){return function(_bc){return new F(function(){return A(new T(function(){return B(_aW(_b2));}),[_ba,new T(function(){return B(A(new T(function(){return B(A(_b8,[_bb]));}),[_bc]));})]);});};}]);});}]);});};}),[function(_bd,_be){return new F(function(){return A(_b5,[new T(function(){return B(_aK(_bd));}),new T(function(){return B(_aK(_be));})]);});}]));}),function(_bf){return new F(function(){return A(new T(function(){return B(_aS(_b1));}),[new T(function(){return B(A(new T(function(){return B(_aN(_b6));}),[_bf]));})]);});}]);});}]);});};},_bg=new T(function(){return B(_b0(_aE,_aj,_aF));}),_bh=[0,_0,_0],_bi=function(_bj,_bk){var _bl=E(_bk);return [0,_bl[1],_bl[2],_bj];},_bm=function(_bn){return function(_){var _bo=nMV(_bh),_bp=_bo;return new T(function(){var _bq=[0,_bp];return [0,B(_67(_bq,function(_br){return E([0,function(_){var _bs=E(_a9),_bt=jsFind(toJSStr(_bs)),_bu=_bt;return new T(function(){var _bv=E(_bu);if(!_bv[0]){var _bw=B(_3U(_bs));}else{var _bx=_bv[1],_bw=[0,function(_){var _by=E(_bx),_bz=jsSetCB(_by[1],E(_2n)[1],function(_bA,_){var _bB=B(_o(E(_bx)[1],_2Y,_)),_bC=_bB;return E(E(_bA)[1])==13?B(_1A(_bC,0))<=0?_5:B(_d(new T(function(){return [0,B(_1o(_bq,function(_bD){return [0,B(_w(_bq,new T(function(){var _bE=B(_a2(_bC,_1Y,_bD));return [1,_bE[1],_bE[2]];}),function(_bF){return E(new T(function(){return B(A(new T(function(){return B(_1F(_j,_bx,_2Y,_0));}),[function(_bG){return [0,B(_67(_bq,_u))];}]));}));}))];}))];}),_0,_)):_5;}),_bH=_bz;return [0,function(_){var _bI=B(A(_2F,[_by,_])),_bJ=_bI;return [0,function(_){var _bK=E(_37),_bL=jsFind(toJSStr(_bK)),_bM=_bL;return new T(function(){var _bN=E(_bM);if(!_bN[0]){var _bO=B(_3U(_bK));}else{var _bP=_bN[1],_bO=[0,function(_){var _bQ=E(_2p)[1],_bR=jsSetCB(E(_bP)[1],_bQ,function(_bS,_bT,_){return new F(function(){return _d([0,function(_){var _bU=jsGet(E(_bP)[1],toJSStr(E(_2X))),_bV=_bU;return new T(function(){return [0,B(_1o(_bq,function(_bW){return [0,B(_w(_bq,new T(function(){return B(_3o(function(_8q){return new F(function(){return _bi(new T(function(){return B(_O(fromJSStr(_bV),_2W));}),_8q);});},_bW));}),function(_bX){return [0,B(_67(_bq,_u))];}))];}))];});}],_0,_);});}),_bY=_bR;return [0,function(_){var _bZ=E(_aa),_c0=jsFind(toJSStr(_bZ)),_c1=_c0;return new T(function(){var _c2=E(_c1);if(!_c2[0]){var _c3=B(_3U(_bZ));}else{var _c3=[0,function(_){var _c4=jsSetCB(E(_c2[1])[1],_bQ,function(_c5,_c6,_){return new F(function(){return (function(_){return new F(function(){return _d(new T(function(){return [0,B(_1o(_bq,function(_c7){return [0,B(_w(_bq,new T(function(){return B(_2q(_22,_c7));}),function(_c8){return [0,B(_67(_bq,_u))];}))];}))];}),_0,_);});})(_);});}),_c9=_c4;return new T(function(){return B(A(_bg,[function(_ca,_cb,_cc){return [0,B(_67(_bq,_cc))];},_bn]));});}];}return _c3;});}];}];}return _bO;});}];}];}];}return _bw;});}]);}))];});};},_cd=new T(function(){return [0,B(_bm(_u))];}),_ce=function(_){return new F(function(){return _d(_cd,_0,_);});},_cf=function(_){return new F(function(){return _ce(_);});};
var hasteMain = function() {B(A(_cf, [0]));};window.onload = hasteMain;