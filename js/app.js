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
    this.f = f;
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

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
        if(t.f != __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            t.x = f();
        }
        return t.x;
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
        f = __blackhole;
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
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
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
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
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

var _0=[0],_1=function(_2,_3){var _4=E(_2);return _4[0]==0?E(_3):[1,_4[1],new T(function(){return B(_1(_4[2],_3));})];},_5=0,_6=function(_7,_){while(1){var _8=E(_7);if(!_8[0]){return _5;}else{var _9=_8[2],_a=E(_8[1]);switch(_a[0]){case 0:var _b=B(A(_a[1],[_])),_c=_b;_7=B(_1(_9,[1,_c,_0]));continue;case 1:_7=B(_1(_9,_a[1]));continue;default:_7=_9;continue;}}}},_d=function(_e,_f,_){var _g=E(_e);switch(_g[0]){case 0:var _h=B(A(_g[1],[_])),_i=_h;return new F(function(){return _6(B(_1(_f,[1,_i,_0])),_);});break;case 1:return new F(function(){return _6(B(_1(_f,_g[1])),_);});break;default:return new F(function(){return _6(_f,_);});}},_j=new T(function(){return B(unCStr(", "));}),_k=new T(function(){return B(unCStr("identifier = "));}),_l=new T(function(){return B(unCStr("Todo {"));}),_m=[0,125],_n=new T(function(){return B(unCStr("completed = "));}),_o=new T(function(){return B(unCStr("task = "));}),_p=[0,34],_q=function(_r){while(1){var _s=E(_r);if(!_s[0]){_r=[1,I_fromInt(_s[1])];continue;}else{return new F(function(){return I_toString(_s[1]);});}}},_t=function(_u,_v){return new F(function(){return _1(fromJSStr(B(_q(_u))),_v);});},_w=function(_x,_y){var _z=E(_x);if(!_z[0]){var _A=_z[1],_B=E(_y);return _B[0]==0?_A<_B[1]:I_compareInt(_B[1],_A)>0;}else{var _C=_z[1],_D=E(_y);return _D[0]==0?I_compareInt(_C,_D[1])<0:I_compare(_C,_D[1])<0;}},_E=[0,41],_F=[0,40],_G=[0,0],_H=function(_I,_J,_K){return _I<=6?B(_t(_J,_K)):!B(_w(_J,_G))?B(_t(_J,_K)):[1,_F,new T(function(){return B(_1(fromJSStr(B(_q(_J))),[1,_E,_K]));})];},_L=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_M=new T(function(){return B(err(_L));}),_N=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_O=new T(function(){return B(err(_N));}),_P=function(_Q,_R){while(1){var _S=E(_Q);if(!_S[0]){return E(_O);}else{var _T=E(_R);if(!_T){return E(_S[1]);}else{_Q=_S[2];_R=_T-1|0;continue;}}}},_U=new T(function(){return B(unCStr("ACK"));}),_V=new T(function(){return B(unCStr("BEL"));}),_W=new T(function(){return B(unCStr("BS"));}),_X=new T(function(){return B(unCStr("SP"));}),_Y=[1,_X,_0],_Z=new T(function(){return B(unCStr("US"));}),_10=[1,_Z,_Y],_11=new T(function(){return B(unCStr("RS"));}),_12=[1,_11,_10],_13=new T(function(){return B(unCStr("GS"));}),_14=[1,_13,_12],_15=new T(function(){return B(unCStr("FS"));}),_16=[1,_15,_14],_17=new T(function(){return B(unCStr("ESC"));}),_18=[1,_17,_16],_19=new T(function(){return B(unCStr("SUB"));}),_1a=[1,_19,_18],_1b=new T(function(){return B(unCStr("EM"));}),_1c=[1,_1b,_1a],_1d=new T(function(){return B(unCStr("CAN"));}),_1e=[1,_1d,_1c],_1f=new T(function(){return B(unCStr("ETB"));}),_1g=[1,_1f,_1e],_1h=new T(function(){return B(unCStr("SYN"));}),_1i=[1,_1h,_1g],_1j=new T(function(){return B(unCStr("NAK"));}),_1k=[1,_1j,_1i],_1l=new T(function(){return B(unCStr("DC4"));}),_1m=[1,_1l,_1k],_1n=new T(function(){return B(unCStr("DC3"));}),_1o=[1,_1n,_1m],_1p=new T(function(){return B(unCStr("DC2"));}),_1q=[1,_1p,_1o],_1r=new T(function(){return B(unCStr("DC1"));}),_1s=[1,_1r,_1q],_1t=new T(function(){return B(unCStr("DLE"));}),_1u=[1,_1t,_1s],_1v=new T(function(){return B(unCStr("SI"));}),_1w=[1,_1v,_1u],_1x=new T(function(){return B(unCStr("SO"));}),_1y=[1,_1x,_1w],_1z=new T(function(){return B(unCStr("CR"));}),_1A=[1,_1z,_1y],_1B=new T(function(){return B(unCStr("FF"));}),_1C=[1,_1B,_1A],_1D=new T(function(){return B(unCStr("VT"));}),_1E=[1,_1D,_1C],_1F=new T(function(){return B(unCStr("LF"));}),_1G=[1,_1F,_1E],_1H=new T(function(){return B(unCStr("HT"));}),_1I=[1,_1H,_1G],_1J=[1,_W,_1I],_1K=[1,_V,_1J],_1L=[1,_U,_1K],_1M=new T(function(){return B(unCStr("ENQ"));}),_1N=[1,_1M,_1L],_1O=new T(function(){return B(unCStr("EOT"));}),_1P=[1,_1O,_1N],_1Q=new T(function(){return B(unCStr("ETX"));}),_1R=[1,_1Q,_1P],_1S=new T(function(){return B(unCStr("STX"));}),_1T=[1,_1S,_1R],_1U=new T(function(){return B(unCStr("SOH"));}),_1V=[1,_1U,_1T],_1W=new T(function(){return B(unCStr("NUL"));}),_1X=[1,_1W,_1V],_1Y=[0,92],_1Z=new T(function(){return B(unCStr("\\DEL"));}),_20=new T(function(){return B(unCStr("\\a"));}),_21=new T(function(){return B(unCStr("\\\\"));}),_22=new T(function(){return B(unCStr("\\SO"));}),_23=new T(function(){return B(unCStr("\\r"));}),_24=new T(function(){return B(unCStr("\\f"));}),_25=new T(function(){return B(unCStr("\\v"));}),_26=new T(function(){return B(unCStr("\\n"));}),_27=new T(function(){return B(unCStr("\\t"));}),_28=new T(function(){return B(unCStr("\\b"));}),_29=function(_2a,_2b){if(_2a<=127){var _2c=E(_2a);switch(_2c){case 92:return new F(function(){return _1(_21,_2b);});break;case 127:return new F(function(){return _1(_1Z,_2b);});break;default:if(_2c<32){var _2d=E(_2c);switch(_2d){case 7:return new F(function(){return _1(_20,_2b);});break;case 8:return new F(function(){return _1(_28,_2b);});break;case 9:return new F(function(){return _1(_27,_2b);});break;case 10:return new F(function(){return _1(_26,_2b);});break;case 11:return new F(function(){return _1(_25,_2b);});break;case 12:return new F(function(){return _1(_24,_2b);});break;case 13:return new F(function(){return _1(_23,_2b);});break;case 14:return new F(function(){return _1(_22,new T(function(){var _2e=E(_2b);if(!_2e[0]){var _2f=[0];}else{var _2f=E(E(_2e[1])[1])==72?B(unAppCStr("\\&",_2e)):E(_2e);}return _2f;}));});break;default:return new F(function(){return _1([1,_1Y,new T(function(){var _2g=_2d;return _2g>=0?B(_P(_1X,_2g)):E(_M);})],_2b);});}}else{return [1,[0,_2c],_2b];}}}else{return [1,_1Y,new T(function(){var _2h=jsShowI(_2a),_2i=_2h;return B(_1(fromJSStr(_2i),new T(function(){var _2j=E(_2b);if(!_2j[0]){var _2k=[0];}else{var _2l=E(_2j[1])[1];if(_2l<48){var _2m=E(_2j);}else{var _2m=_2l>57?E(_2j):B(unAppCStr("\\&",_2j));}var _2n=_2m,_2o=_2n,_2k=_2o;}return _2k;})));})];}},_2p=new T(function(){return B(unCStr("\\\""));}),_2q=function(_2r,_2s){var _2t=E(_2r);if(!_2t[0]){return E(_2s);}else{var _2u=_2t[2],_2v=E(E(_2t[1])[1]);if(_2v==34){return new F(function(){return _1(_2p,new T(function(){return B(_2q(_2u,_2s));}));});}else{return new F(function(){return _29(_2v,new T(function(){return B(_2q(_2u,_2s));}));});}}},_2w=new T(function(){return B(unCStr("True"));}),_2x=new T(function(){return B(unCStr("False"));}),_2y=function(_2z,_2A,_2B,_2C,_2D){var _2E=function(_2F){return new F(function(){return _1(_l,new T(function(){return B(_1(_k,new T(function(){return B(_H(0,_2A,new T(function(){return B(_1(_j,new T(function(){return B(_1(_o,[1,_p,new T(function(){return B(_2q(_2B,[1,_p,new T(function(){return B(_1(_j,new T(function(){return B(_1(_n,new T(function(){return !E(_2C)?B(_1(_2x,[1,_m,_2F])):B(_1(_2w,[1,_m,_2F]));})));})));})]));})]));})));})));})));}));});};return _2z<11?B(_2E(_2D)):[1,_F,new T(function(){return B(_2E([1,_E,_2D]));})];},_2G=function(_2H){return new T(function(){var _2I=E(_2H);return [0,toJSStr(B(_2y(0,_2I[1],_2I[2],_2I[3],_0)))];});},_2J=function(_2K){return [1,B(_2G(_2K))];},_2L=function(_2M,_2N){var _2O=E(_2N);return _2O[0]==0?[0]:[1,new T(function(){return B(A(_2M,[_2O[1]]));}),new T(function(){return B(_2L(_2M,_2O[2]));})];},_2P=function(_2Q){return [3,new T(function(){return B(_2L(_2J,_2Q));})];},_2R=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_2S=new T(function(){return B(err(_2R));}),_2T=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_2U=new T(function(){return B(err(_2T));}),_2V=new T(function(){return B(unCStr("Control.Exception.Base"));}),_2W=new T(function(){return B(unCStr("base"));}),_2X=new T(function(){return B(unCStr("PatternMatchFail"));}),_2Y=new T(function(){var _2Z=hs_wordToWord64(18445595),_30=_2Z,_31=hs_wordToWord64(52003073),_32=_31;return [0,_30,_32,[0,_30,_32,_2W,_2V,_2X],_0];}),_33=function(_34){return E(_2Y);},_35=function(_36){return E(E(_36)[1]);},_37=function(_38,_39,_3a){var _3b=B(A(_38,[_])),_3c=B(A(_39,[_])),_3d=hs_eqWord64(_3b[1],_3c[1]),_3e=_3d;if(!E(_3e)){return [0];}else{var _3f=hs_eqWord64(_3b[2],_3c[2]),_3g=_3f;return E(_3g)==0?[0]:[1,_3a];}},_3h=function(_3i){var _3j=E(_3i);return new F(function(){return _37(B(_35(_3j[1])),_33,_3j[2]);});},_3k=function(_3l){return E(E(_3l)[1]);},_3m=function(_3n,_3o){return new F(function(){return _1(E(_3n)[1],_3o);});},_3p=[0,44],_3q=[0,93],_3r=[0,91],_3s=function(_3t,_3u,_3v){var _3w=E(_3u);return _3w[0]==0?B(unAppCStr("[]",_3v)):[1,_3r,new T(function(){return B(A(_3t,[_3w[1],new T(function(){var _3x=function(_3y){var _3z=E(_3y);return _3z[0]==0?E([1,_3q,_3v]):[1,_3p,new T(function(){return B(A(_3t,[_3z[1],new T(function(){return B(_3x(_3z[2]));})]));})];};return B(_3x(_3w[2]));})]));})];},_3A=function(_3B,_3C){return new F(function(){return _3s(_3m,_3B,_3C);});},_3D=function(_3E,_3F,_3G){return new F(function(){return _1(E(_3F)[1],_3G);});},_3H=[0,_3D,_3k,_3A],_3I=new T(function(){return [0,_33,_3H,_3J,_3h];}),_3J=function(_3K){return [0,_3I,_3K];},_3L=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_3M=function(_3N,_3O){return new F(function(){return die(new T(function(){return B(A(_3O,[_3N]));}));});},_3P=function(_3Q,_3R){var _3S=E(_3R);if(!_3S[0]){return [0,_0,_0];}else{var _3T=_3S[1];if(!B(A(_3Q,[_3T]))){return [0,_0,_3S];}else{var _3U=new T(function(){var _3V=B(_3P(_3Q,_3S[2]));return [0,_3V[1],_3V[2]];});return [0,[1,_3T,new T(function(){return E(E(_3U)[1]);})],new T(function(){return E(E(_3U)[2]);})];}}},_3W=[0,32],_3X=[0,10],_3Y=[1,_3X,_0],_3Z=function(_40){return E(E(_40)[1])==124?false:true;},_41=function(_42,_43){var _44=B(_3P(_3Z,B(unCStr(_42)))),_45=_44[1],_46=function(_47,_48){return new F(function(){return _1(_47,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_1(_43,new T(function(){return B(_1(_48,_3Y));})));})));}));});},_49=E(_44[2]);if(!_49[0]){return new F(function(){return _46(_45,_0);});}else{return E(E(_49[1])[1])==124?B(_46(_45,[1,_3W,_49[2]])):B(_46(_45,_0));}},_4a=function(_4b){return new F(function(){return _3M([0,new T(function(){return B(_41(_4b,_3L));})],_3J);});},_4c=new T(function(){return B(_4a("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_4d=function(_4e,_4f){while(1){var _4g=(function(_4h,_4i){var _4j=E(_4h);switch(_4j[0]){case 0:var _4k=E(_4i);if(!_4k[0]){return [0];}else{_4e=B(A(_4j[1],[_4k[1]]));_4f=_4k[2];return null;}break;case 1:var _4l=B(A(_4j[1],[_4i])),_4m=_4i;_4e=_4l;_4f=_4m;return null;case 2:return [0];case 3:return [1,[0,_4j[1],_4i],new T(function(){return B(_4d(_4j[2],_4i));})];default:return E(_4j[1]);}})(_4e,_4f);if(_4g!=null){return _4g;}}},_4n=function(_4o,_4p){var _4q=function(_4r){var _4s=E(_4p);if(_4s[0]==3){return [3,_4s[1],new T(function(){return B(_4n(_4o,_4s[2]));})];}else{var _4t=E(_4o);if(_4t[0]==2){return E(_4s);}else{var _4u=E(_4s);if(_4u[0]==2){return E(_4t);}else{var _4v=function(_4w){var _4x=E(_4u);if(_4x[0]==4){return [1,function(_4y){return [4,new T(function(){return B(_1(B(_4d(_4t,_4y)),_4x[1]));})];}];}else{var _4z=E(_4t);if(_4z[0]==1){var _4A=_4z[1],_4B=E(_4x);return _4B[0]==0?[1,function(_4C){return new F(function(){return _4n(B(A(_4A,[_4C])),_4B);});}]:[1,function(_4D){return new F(function(){return _4n(B(A(_4A,[_4D])),new T(function(){return B(A(_4B[1],[_4D]));}));});}];}else{var _4E=E(_4x);return _4E[0]==0?E(_4c):[1,function(_4F){return new F(function(){return _4n(_4z,new T(function(){return B(A(_4E[1],[_4F]));}));});}];}}},_4G=E(_4t);switch(_4G[0]){case 1:var _4H=E(_4u);if(_4H[0]==4){return [1,function(_4I){return [4,new T(function(){return B(_1(B(_4d(B(A(_4G[1],[_4I])),_4I)),_4H[1]));})];}];}else{return new F(function(){return _4v(_);});}break;case 4:var _4J=_4G[1],_4K=E(_4u);switch(_4K[0]){case 0:return [1,function(_4L){return [4,new T(function(){return B(_1(_4J,new T(function(){return B(_4d(_4K,_4L));})));})];}];case 1:return [1,function(_4M){return [4,new T(function(){return B(_1(_4J,new T(function(){return B(_4d(B(A(_4K[1],[_4M])),_4M));})));})];}];default:return [4,new T(function(){return B(_1(_4J,_4K[1]));})];}break;default:return new F(function(){return _4v(_);});}}}}},_4N=E(_4o);switch(_4N[0]){case 0:var _4O=E(_4p);if(!_4O[0]){return [0,function(_4P){return new F(function(){return _4n(B(A(_4N[1],[_4P])),new T(function(){return B(A(_4O[1],[_4P]));}));});}];}else{return new F(function(){return _4q(_);});}break;case 3:return [3,_4N[1],new T(function(){return B(_4n(_4N[2],_4p));})];default:return new F(function(){return _4q(_);});}},_4Q=[0,41],_4R=[1,_4Q,_0],_4S=[0,40],_4T=[1,_4S,_0],_4U=function(_4V,_4W){while(1){var _4X=E(_4V);if(!_4X[0]){return E(_4W)[0]==0?true:false;}else{var _4Y=E(_4W);if(!_4Y[0]){return false;}else{if(E(_4X[1])[1]!=E(_4Y[1])[1]){return false;}else{_4V=_4X[2];_4W=_4Y[2];continue;}}}}},_4Z=function(_50,_51){return E(_50)[1]!=E(_51)[1];},_52=function(_53,_54){return E(_53)[1]==E(_54)[1];},_55=[0,_52,_4Z],_56=function(_57,_58){while(1){var _59=E(_57);if(!_59[0]){return E(_58)[0]==0?true:false;}else{var _5a=E(_58);if(!_5a[0]){return false;}else{if(E(_59[1])[1]!=E(_5a[1])[1]){return false;}else{_57=_59[2];_58=_5a[2];continue;}}}}},_5b=function(_5c,_5d){return !B(_56(_5c,_5d))?true:false;},_5e=[0,_56,_5b],_5f=function(_5g,_5h){var _5i=E(_5g);switch(_5i[0]){case 0:return [0,function(_5j){return new F(function(){return _5f(B(A(_5i[1],[_5j])),_5h);});}];case 1:return [1,function(_5k){return new F(function(){return _5f(B(A(_5i[1],[_5k])),_5h);});}];case 2:return [2];case 3:return new F(function(){return _4n(B(A(_5h,[_5i[1]])),new T(function(){return B(_5f(_5i[2],_5h));}));});break;default:var _5l=function(_5m){var _5n=E(_5m);if(!_5n[0]){return [0];}else{var _5o=E(_5n[1]);return new F(function(){return _1(B(_4d(B(A(_5h,[_5o[1]])),_5o[2])),new T(function(){return B(_5l(_5n[2]));}));});}},_5p=B(_5l(_5i[1]));return _5p[0]==0?[2]:[4,_5p];}},_5q=[2],_5r=function(_5s){return [3,_5s,_5q];},_5t=function(_5u,_5v){var _5w=E(_5u);if(!_5w){return new F(function(){return A(_5v,[_5]);});}else{return [0,function(_5x){return E(new T(function(){return B(_5t(_5w-1|0,_5v));}));}];}},_5y=function(_5z,_5A,_5B){return function(_5C){return new F(function(){return A(function(_5D,_5E,_5F){while(1){var _5G=(function(_5H,_5I,_5J){var _5K=E(_5H);switch(_5K[0]){case 0:var _5L=E(_5I);if(!_5L[0]){return E(_5A);}else{_5D=B(A(_5K[1],[_5L[1]]));_5E=_5L[2];var _5M=_5J+1|0;_5F=_5M;return null;}break;case 1:var _5N=B(A(_5K[1],[_5I])),_5O=_5I,_5M=_5J;_5D=_5N;_5E=_5O;_5F=_5M;return null;case 2:return E(_5A);case 3:return function(_5P){return new F(function(){return _5t(_5J,function(_5Q){return E(new T(function(){return B(_5f(_5K,_5P));}));});});};default:return function(_5R){return new F(function(){return _5f(_5K,_5R);});};}})(_5D,_5E,_5F);if(_5G!=null){return _5G;}}},[new T(function(){return B(A(_5z,[_5r]));}),_5C,0,_5B]);});};},_5S=function(_5T){return new F(function(){return A(_5T,[_0]);});},_5U=function(_5V,_5W){var _5X=function(_5Y){var _5Z=E(_5Y);if(!_5Z[0]){return E(_5S);}else{var _60=_5Z[1];return !B(A(_5V,[_60]))?E(_5S):function(_61){return [0,function(_62){return E(new T(function(){return B(A(new T(function(){return B(_5X(_5Z[2]));}),[function(_63){return new F(function(){return A(_61,[[1,_60,_63]]);});}]));}));}];};}};return function(_64){return new F(function(){return A(_5X,[_64,_5W]);});};},_65=[6],_66=function(_67){return E(_67);},_68=new T(function(){return B(unCStr("valDig: Bad base"));}),_69=new T(function(){return B(err(_68));}),_6a=function(_6b,_6c){var _6d=function(_6e,_6f){var _6g=E(_6e);if(!_6g[0]){return function(_6h){return new F(function(){return A(_6h,[new T(function(){return B(A(_6f,[_0]));})]);});};}else{var _6i=E(_6g[1])[1],_6j=function(_6k){return function(_6l){return [0,function(_6m){return E(new T(function(){return B(A(new T(function(){return B(_6d(_6g[2],function(_6n){return new F(function(){return A(_6f,[[1,_6k,_6n]]);});}));}),[_6l]));}));}];};};switch(E(E(_6b)[1])){case 8:if(48>_6i){return function(_6o){return new F(function(){return A(_6o,[new T(function(){return B(A(_6f,[_0]));})]);});};}else{if(_6i>55){return function(_6p){return new F(function(){return A(_6p,[new T(function(){return B(A(_6f,[_0]));})]);});};}else{return new F(function(){return _6j([0,_6i-48|0]);});}}break;case 10:if(48>_6i){return function(_6q){return new F(function(){return A(_6q,[new T(function(){return B(A(_6f,[_0]));})]);});};}else{if(_6i>57){return function(_6r){return new F(function(){return A(_6r,[new T(function(){return B(A(_6f,[_0]));})]);});};}else{return new F(function(){return _6j([0,_6i-48|0]);});}}break;case 16:if(48>_6i){if(97>_6i){if(65>_6i){return function(_6s){return new F(function(){return A(_6s,[new T(function(){return B(A(_6f,[_0]));})]);});};}else{if(_6i>70){return function(_6t){return new F(function(){return A(_6t,[new T(function(){return B(A(_6f,[_0]));})]);});};}else{return new F(function(){return _6j([0,(_6i-65|0)+10|0]);});}}}else{if(_6i>102){if(65>_6i){return function(_6u){return new F(function(){return A(_6u,[new T(function(){return B(A(_6f,[_0]));})]);});};}else{if(_6i>70){return function(_6v){return new F(function(){return A(_6v,[new T(function(){return B(A(_6f,[_0]));})]);});};}else{return new F(function(){return _6j([0,(_6i-65|0)+10|0]);});}}}else{return new F(function(){return _6j([0,(_6i-97|0)+10|0]);});}}}else{if(_6i>57){if(97>_6i){if(65>_6i){return function(_6w){return new F(function(){return A(_6w,[new T(function(){return B(A(_6f,[_0]));})]);});};}else{if(_6i>70){return function(_6x){return new F(function(){return A(_6x,[new T(function(){return B(A(_6f,[_0]));})]);});};}else{return new F(function(){return _6j([0,(_6i-65|0)+10|0]);});}}}else{if(_6i>102){if(65>_6i){return function(_6y){return new F(function(){return A(_6y,[new T(function(){return B(A(_6f,[_0]));})]);});};}else{if(_6i>70){return function(_6z){return new F(function(){return A(_6z,[new T(function(){return B(A(_6f,[_0]));})]);});};}else{return new F(function(){return _6j([0,(_6i-65|0)+10|0]);});}}}else{return new F(function(){return _6j([0,(_6i-97|0)+10|0]);});}}}else{return new F(function(){return _6j([0,_6i-48|0]);});}}break;default:return E(_69);}}};return function(_6A){return new F(function(){return A(_6d,[_6A,_66,function(_6B){var _6C=E(_6B);return _6C[0]==0?[2]:B(A(_6c,[_6C]));}]);});};},_6D=[0,10],_6E=[0,1],_6F=[0,2147483647],_6G=function(_6H,_6I){while(1){var _6J=E(_6H);if(!_6J[0]){var _6K=_6J[1],_6L=E(_6I);if(!_6L[0]){var _6M=_6L[1],_6N=addC(_6K,_6M);if(!E(_6N[2])){return [0,_6N[1]];}else{_6H=[1,I_fromInt(_6K)];_6I=[1,I_fromInt(_6M)];continue;}}else{_6H=[1,I_fromInt(_6K)];_6I=_6L;continue;}}else{var _6O=E(_6I);if(!_6O[0]){_6H=_6J;_6I=[1,I_fromInt(_6O[1])];continue;}else{return [1,I_add(_6J[1],_6O[1])];}}}},_6P=new T(function(){return B(_6G(_6F,_6E));}),_6Q=function(_6R){var _6S=E(_6R);if(!_6S[0]){var _6T=E(_6S[1]);return _6T==(-2147483648)?E(_6P):[0, -_6T];}else{return [1,I_negate(_6S[1])];}},_6U=[0,10],_6V=[0,0],_6W=function(_6X){return [0,_6X];},_6Y=function(_6Z,_70){while(1){var _71=E(_6Z);if(!_71[0]){var _72=_71[1],_73=E(_70);if(!_73[0]){var _74=_73[1];if(!(imul(_72,_74)|0)){return [0,imul(_72,_74)|0];}else{_6Z=[1,I_fromInt(_72)];_70=[1,I_fromInt(_74)];continue;}}else{_6Z=[1,I_fromInt(_72)];_70=_73;continue;}}else{var _75=E(_70);if(!_75[0]){_6Z=_71;_70=[1,I_fromInt(_75[1])];continue;}else{return [1,I_mul(_71[1],_75[1])];}}}},_76=function(_77,_78,_79){while(1){var _7a=E(_79);if(!_7a[0]){return E(_78);}else{var _7b=B(_6G(B(_6Y(_78,_77)),B(_6W(E(_7a[1])[1]))));_79=_7a[2];_78=_7b;continue;}}},_7c=function(_7d){var _7e=new T(function(){return B(_4n(B(_4n([0,function(_7f){return E(E(_7f)[1])==45?[1,B(_6a(_6D,function(_7g){return new F(function(){return A(_7d,[[1,new T(function(){return B(_6Q(B(_76(_6U,_6V,_7g))));})]]);});}))]:[2];}],[0,function(_7h){return E(E(_7h)[1])==43?[1,B(_6a(_6D,function(_7i){return new F(function(){return A(_7d,[[1,new T(function(){return B(_76(_6U,_6V,_7i));})]]);});}))]:[2];}])),new T(function(){return [1,B(_6a(_6D,function(_7j){return new F(function(){return A(_7d,[[1,new T(function(){return B(_76(_6U,_6V,_7j));})]]);});}))];})));});return new F(function(){return _4n([0,function(_7k){return E(E(_7k)[1])==101?E(_7e):[2];}],[0,function(_7l){return E(E(_7l)[1])==69?E(_7e):[2];}]);});},_7m=[0],_7n=function(_7o){return new F(function(){return A(_7o,[_7m]);});},_7p=function(_7q){return new F(function(){return A(_7q,[_7m]);});},_7r=function(_7s){return function(_7t){return E(E(_7t)[1])==46?[1,B(_6a(_6D,function(_7u){return new F(function(){return A(_7s,[[1,_7u]]);});}))]:[2];};},_7v=function(_7w){return [0,B(_7r(_7w))];},_7x=function(_7y){return new F(function(){return _6a(_6D,function(_7z){return [1,B(_5y(_7v,_7n,function(_7A){return [1,B(_5y(_7c,_7p,function(_7B){return new F(function(){return A(_7y,[[5,[1,_7z,_7A,_7B]]]);});}))];}))];});});},_7C=function(_7D){return [1,B(_7x(_7D))];},_7E=function(_7F){return E(E(_7F)[1]);},_7G=function(_7H,_7I,_7J){while(1){var _7K=E(_7J);if(!_7K[0]){return false;}else{if(!B(A(_7E,[_7H,_7I,_7K[1]]))){_7J=_7K[2];continue;}else{return true;}}}},_7L=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_7M=function(_7N){return new F(function(){return _7G(_55,_7N,_7L);});},_7O=[0,8],_7P=[0,16],_7Q=function(_7R){var _7S=function(_7T){return new F(function(){return A(_7R,[[5,[0,_7O,_7T]]]);});},_7U=function(_7V){return new F(function(){return A(_7R,[[5,[0,_7P,_7V]]]);});};return function(_7W){return E(E(_7W)[1])==48?E([0,function(_7X){switch(E(E(_7X)[1])){case 79:return [1,B(_6a(_7O,_7S))];case 88:return [1,B(_6a(_7P,_7U))];case 111:return [1,B(_6a(_7O,_7S))];case 120:return [1,B(_6a(_7P,_7U))];default:return [2];}}]):[2];};},_7Y=function(_7Z){return [0,B(_7Q(_7Z))];},_80=false,_81=true,_82=function(_83){var _84=new T(function(){return B(A(_83,[_7O]));}),_85=new T(function(){return B(A(_83,[_7P]));});return function(_86){switch(E(E(_86)[1])){case 79:return E(_84);case 88:return E(_85);case 111:return E(_84);case 120:return E(_85);default:return [2];}};},_87=function(_88){return [0,B(_82(_88))];},_89=[0,92],_8a=function(_8b){return new F(function(){return A(_8b,[_6D]);});},_8c=function(_8d,_8e){var _8f=jsShowI(_8d),_8g=_8f;return new F(function(){return _1(fromJSStr(_8g),_8e);});},_8h=function(_8i,_8j,_8k){if(_8j>=0){return new F(function(){return _8c(_8j,_8k);});}else{return _8i<=6?B(_8c(_8j,_8k)):[1,_F,new T(function(){var _8l=jsShowI(_8j),_8m=_8l;return B(_1(fromJSStr(_8m),[1,_E,_8k]));})];}},_8n=function(_8o){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_8h(9,_8o,_0));}))));});},_8p=function(_8q){var _8r=E(_8q);return _8r[0]==0?E(_8r[1]):I_toInt(_8r[1]);},_8s=function(_8t,_8u){var _8v=E(_8t);if(!_8v[0]){var _8w=_8v[1],_8x=E(_8u);return _8x[0]==0?_8w<=_8x[1]:I_compareInt(_8x[1],_8w)>=0;}else{var _8y=_8v[1],_8z=E(_8u);return _8z[0]==0?I_compareInt(_8y,_8z[1])<=0:I_compare(_8y,_8z[1])<=0;}},_8A=function(_8B){return [2];},_8C=function(_8D){var _8E=E(_8D);if(!_8E[0]){return E(_8A);}else{var _8F=_8E[1],_8G=E(_8E[2]);return _8G[0]==0?E(_8F):function(_8H){return new F(function(){return _4n(B(A(_8F,[_8H])),new T(function(){return B(A(new T(function(){return B(_8C(_8G));}),[_8H]));}));});};}},_8I=function(_8J){return [2];},_8K=function(_8L,_8M){var _8N=function(_8O,_8P){var _8Q=E(_8O);if(!_8Q[0]){return function(_8R){return new F(function(){return A(_8R,[_8L]);});};}else{var _8S=E(_8P);return _8S[0]==0?E(_8I):E(_8Q[1])[1]!=E(_8S[1])[1]?E(_8I):function(_8T){return [0,function(_8U){return E(new T(function(){return B(A(new T(function(){return B(_8N(_8Q[2],_8S[2]));}),[_8T]));}));}];};}};return function(_8V){return new F(function(){return A(_8N,[_8L,_8V,_8M]);});};},_8W=new T(function(){return B(unCStr("SOH"));}),_8X=[0,1],_8Y=function(_8Z){return [1,B(_8K(_8W,function(_90){return E(new T(function(){return B(A(_8Z,[_8X]));}));}))];},_91=new T(function(){return B(unCStr("SO"));}),_92=[0,14],_93=function(_94){return [1,B(_8K(_91,function(_95){return E(new T(function(){return B(A(_94,[_92]));}));}))];},_96=function(_97){return [1,B(_5y(_8Y,_93,_97))];},_98=new T(function(){return B(unCStr("NUL"));}),_99=[0,0],_9a=function(_9b){return [1,B(_8K(_98,function(_9c){return E(new T(function(){return B(A(_9b,[_99]));}));}))];},_9d=new T(function(){return B(unCStr("STX"));}),_9e=[0,2],_9f=function(_9g){return [1,B(_8K(_9d,function(_9h){return E(new T(function(){return B(A(_9g,[_9e]));}));}))];},_9i=new T(function(){return B(unCStr("ETX"));}),_9j=[0,3],_9k=function(_9l){return [1,B(_8K(_9i,function(_9m){return E(new T(function(){return B(A(_9l,[_9j]));}));}))];},_9n=new T(function(){return B(unCStr("EOT"));}),_9o=[0,4],_9p=function(_9q){return [1,B(_8K(_9n,function(_9r){return E(new T(function(){return B(A(_9q,[_9o]));}));}))];},_9s=new T(function(){return B(unCStr("ENQ"));}),_9t=[0,5],_9u=function(_9v){return [1,B(_8K(_9s,function(_9w){return E(new T(function(){return B(A(_9v,[_9t]));}));}))];},_9x=new T(function(){return B(unCStr("ACK"));}),_9y=[0,6],_9z=function(_9A){return [1,B(_8K(_9x,function(_9B){return E(new T(function(){return B(A(_9A,[_9y]));}));}))];},_9C=new T(function(){return B(unCStr("BEL"));}),_9D=[0,7],_9E=function(_9F){return [1,B(_8K(_9C,function(_9G){return E(new T(function(){return B(A(_9F,[_9D]));}));}))];},_9H=new T(function(){return B(unCStr("BS"));}),_9I=[0,8],_9J=function(_9K){return [1,B(_8K(_9H,function(_9L){return E(new T(function(){return B(A(_9K,[_9I]));}));}))];},_9M=new T(function(){return B(unCStr("HT"));}),_9N=[0,9],_9O=function(_9P){return [1,B(_8K(_9M,function(_9Q){return E(new T(function(){return B(A(_9P,[_9N]));}));}))];},_9R=new T(function(){return B(unCStr("LF"));}),_9S=[0,10],_9T=function(_9U){return [1,B(_8K(_9R,function(_9V){return E(new T(function(){return B(A(_9U,[_9S]));}));}))];},_9W=new T(function(){return B(unCStr("VT"));}),_9X=[0,11],_9Y=function(_9Z){return [1,B(_8K(_9W,function(_a0){return E(new T(function(){return B(A(_9Z,[_9X]));}));}))];},_a1=new T(function(){return B(unCStr("FF"));}),_a2=[0,12],_a3=function(_a4){return [1,B(_8K(_a1,function(_a5){return E(new T(function(){return B(A(_a4,[_a2]));}));}))];},_a6=new T(function(){return B(unCStr("CR"));}),_a7=[0,13],_a8=function(_a9){return [1,B(_8K(_a6,function(_aa){return E(new T(function(){return B(A(_a9,[_a7]));}));}))];},_ab=new T(function(){return B(unCStr("SI"));}),_ac=[0,15],_ad=function(_ae){return [1,B(_8K(_ab,function(_af){return E(new T(function(){return B(A(_ae,[_ac]));}));}))];},_ag=new T(function(){return B(unCStr("DLE"));}),_ah=[0,16],_ai=function(_aj){return [1,B(_8K(_ag,function(_ak){return E(new T(function(){return B(A(_aj,[_ah]));}));}))];},_al=new T(function(){return B(unCStr("DC1"));}),_am=[0,17],_an=function(_ao){return [1,B(_8K(_al,function(_ap){return E(new T(function(){return B(A(_ao,[_am]));}));}))];},_aq=new T(function(){return B(unCStr("DC2"));}),_ar=[0,18],_as=function(_at){return [1,B(_8K(_aq,function(_au){return E(new T(function(){return B(A(_at,[_ar]));}));}))];},_av=new T(function(){return B(unCStr("DC3"));}),_aw=[0,19],_ax=function(_ay){return [1,B(_8K(_av,function(_az){return E(new T(function(){return B(A(_ay,[_aw]));}));}))];},_aA=new T(function(){return B(unCStr("DC4"));}),_aB=[0,20],_aC=function(_aD){return [1,B(_8K(_aA,function(_aE){return E(new T(function(){return B(A(_aD,[_aB]));}));}))];},_aF=new T(function(){return B(unCStr("NAK"));}),_aG=[0,21],_aH=function(_aI){return [1,B(_8K(_aF,function(_aJ){return E(new T(function(){return B(A(_aI,[_aG]));}));}))];},_aK=new T(function(){return B(unCStr("SYN"));}),_aL=[0,22],_aM=function(_aN){return [1,B(_8K(_aK,function(_aO){return E(new T(function(){return B(A(_aN,[_aL]));}));}))];},_aP=new T(function(){return B(unCStr("ETB"));}),_aQ=[0,23],_aR=function(_aS){return [1,B(_8K(_aP,function(_aT){return E(new T(function(){return B(A(_aS,[_aQ]));}));}))];},_aU=new T(function(){return B(unCStr("CAN"));}),_aV=[0,24],_aW=function(_aX){return [1,B(_8K(_aU,function(_aY){return E(new T(function(){return B(A(_aX,[_aV]));}));}))];},_aZ=new T(function(){return B(unCStr("EM"));}),_b0=[0,25],_b1=function(_b2){return [1,B(_8K(_aZ,function(_b3){return E(new T(function(){return B(A(_b2,[_b0]));}));}))];},_b4=new T(function(){return B(unCStr("SUB"));}),_b5=[0,26],_b6=function(_b7){return [1,B(_8K(_b4,function(_b8){return E(new T(function(){return B(A(_b7,[_b5]));}));}))];},_b9=new T(function(){return B(unCStr("ESC"));}),_ba=[0,27],_bb=function(_bc){return [1,B(_8K(_b9,function(_bd){return E(new T(function(){return B(A(_bc,[_ba]));}));}))];},_be=new T(function(){return B(unCStr("FS"));}),_bf=[0,28],_bg=function(_bh){return [1,B(_8K(_be,function(_bi){return E(new T(function(){return B(A(_bh,[_bf]));}));}))];},_bj=new T(function(){return B(unCStr("GS"));}),_bk=[0,29],_bl=function(_bm){return [1,B(_8K(_bj,function(_bn){return E(new T(function(){return B(A(_bm,[_bk]));}));}))];},_bo=new T(function(){return B(unCStr("RS"));}),_bp=[0,30],_bq=function(_br){return [1,B(_8K(_bo,function(_bs){return E(new T(function(){return B(A(_br,[_bp]));}));}))];},_bt=new T(function(){return B(unCStr("US"));}),_bu=[0,31],_bv=function(_bw){return [1,B(_8K(_bt,function(_bx){return E(new T(function(){return B(A(_bw,[_bu]));}));}))];},_by=new T(function(){return B(unCStr("SP"));}),_bz=[0,32],_bA=function(_bB){return [1,B(_8K(_by,function(_bC){return E(new T(function(){return B(A(_bB,[_bz]));}));}))];},_bD=new T(function(){return B(unCStr("DEL"));}),_bE=[0,127],_bF=function(_bG){return [1,B(_8K(_bD,function(_bH){return E(new T(function(){return B(A(_bG,[_bE]));}));}))];},_bI=[1,_bF,_0],_bJ=[1,_bA,_bI],_bK=[1,_bv,_bJ],_bL=[1,_bq,_bK],_bM=[1,_bl,_bL],_bN=[1,_bg,_bM],_bO=[1,_bb,_bN],_bP=[1,_b6,_bO],_bQ=[1,_b1,_bP],_bR=[1,_aW,_bQ],_bS=[1,_aR,_bR],_bT=[1,_aM,_bS],_bU=[1,_aH,_bT],_bV=[1,_aC,_bU],_bW=[1,_ax,_bV],_bX=[1,_as,_bW],_bY=[1,_an,_bX],_bZ=[1,_ai,_bY],_c0=[1,_ad,_bZ],_c1=[1,_a8,_c0],_c2=[1,_a3,_c1],_c3=[1,_9Y,_c2],_c4=[1,_9T,_c3],_c5=[1,_9O,_c4],_c6=[1,_9J,_c5],_c7=[1,_9E,_c6],_c8=[1,_9z,_c7],_c9=[1,_9u,_c8],_ca=[1,_9p,_c9],_cb=[1,_9k,_ca],_cc=[1,_9f,_cb],_cd=[1,_9a,_cc],_ce=[1,_96,_cd],_cf=new T(function(){return B(_8C(_ce));}),_cg=[0,1114111],_ch=[0,34],_ci=[0,39],_cj=function(_ck){var _cl=new T(function(){return B(A(_ck,[_9D]));}),_cm=new T(function(){return B(A(_ck,[_9I]));}),_cn=new T(function(){return B(A(_ck,[_9N]));}),_co=new T(function(){return B(A(_ck,[_9S]));}),_cp=new T(function(){return B(A(_ck,[_9X]));}),_cq=new T(function(){return B(A(_ck,[_a2]));}),_cr=new T(function(){return B(A(_ck,[_a7]));});return new F(function(){return _4n([0,function(_cs){switch(E(E(_cs)[1])){case 34:return E(new T(function(){return B(A(_ck,[_ch]));}));case 39:return E(new T(function(){return B(A(_ck,[_ci]));}));case 92:return E(new T(function(){return B(A(_ck,[_89]));}));case 97:return E(_cl);case 98:return E(_cm);case 102:return E(_cq);case 110:return E(_co);case 114:return E(_cr);case 116:return E(_cn);case 118:return E(_cp);default:return [2];}}],new T(function(){return B(_4n([1,B(_5y(_87,_8a,function(_ct){return [1,B(_6a(_ct,function(_cu){var _cv=B(_76(new T(function(){return B(_6W(E(_ct)[1]));}),_6V,_cu));return !B(_8s(_cv,_cg))?[2]:B(A(_ck,[new T(function(){var _cw=B(_8p(_cv));if(_cw>>>0>1114111){var _cx=B(_8n(_cw));}else{var _cx=[0,_cw];}var _cy=_cx,_cz=_cy,_cA=_cz;return _cA;})]));}))];}))],new T(function(){return B(_4n([0,function(_cB){return E(E(_cB)[1])==94?E([0,function(_cC){switch(E(E(_cC)[1])){case 64:return E(new T(function(){return B(A(_ck,[_99]));}));case 65:return E(new T(function(){return B(A(_ck,[_8X]));}));case 66:return E(new T(function(){return B(A(_ck,[_9e]));}));case 67:return E(new T(function(){return B(A(_ck,[_9j]));}));case 68:return E(new T(function(){return B(A(_ck,[_9o]));}));case 69:return E(new T(function(){return B(A(_ck,[_9t]));}));case 70:return E(new T(function(){return B(A(_ck,[_9y]));}));case 71:return E(_cl);case 72:return E(_cm);case 73:return E(_cn);case 74:return E(_co);case 75:return E(_cp);case 76:return E(_cq);case 77:return E(_cr);case 78:return E(new T(function(){return B(A(_ck,[_92]));}));case 79:return E(new T(function(){return B(A(_ck,[_ac]));}));case 80:return E(new T(function(){return B(A(_ck,[_ah]));}));case 81:return E(new T(function(){return B(A(_ck,[_am]));}));case 82:return E(new T(function(){return B(A(_ck,[_ar]));}));case 83:return E(new T(function(){return B(A(_ck,[_aw]));}));case 84:return E(new T(function(){return B(A(_ck,[_aB]));}));case 85:return E(new T(function(){return B(A(_ck,[_aG]));}));case 86:return E(new T(function(){return B(A(_ck,[_aL]));}));case 87:return E(new T(function(){return B(A(_ck,[_aQ]));}));case 88:return E(new T(function(){return B(A(_ck,[_aV]));}));case 89:return E(new T(function(){return B(A(_ck,[_b0]));}));case 90:return E(new T(function(){return B(A(_ck,[_b5]));}));case 91:return E(new T(function(){return B(A(_ck,[_ba]));}));case 92:return E(new T(function(){return B(A(_ck,[_bf]));}));case 93:return E(new T(function(){return B(A(_ck,[_bk]));}));case 94:return E(new T(function(){return B(A(_ck,[_bp]));}));case 95:return E(new T(function(){return B(A(_ck,[_bu]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_cf,[_ck]));})));})));}));});},_cD=function(_cE){return new F(function(){return A(_cE,[_5]);});},_cF=function(_cG){var _cH=E(_cG);if(!_cH[0]){return E(_cD);}else{var _cI=_cH[2],_cJ=E(E(_cH[1])[1]);switch(_cJ){case 9:return function(_cK){return [0,function(_cL){return E(new T(function(){return B(A(new T(function(){return B(_cF(_cI));}),[_cK]));}));}];};case 10:return function(_cM){return [0,function(_cN){return E(new T(function(){return B(A(new T(function(){return B(_cF(_cI));}),[_cM]));}));}];};case 11:return function(_cO){return [0,function(_cP){return E(new T(function(){return B(A(new T(function(){return B(_cF(_cI));}),[_cO]));}));}];};case 12:return function(_cQ){return [0,function(_cR){return E(new T(function(){return B(A(new T(function(){return B(_cF(_cI));}),[_cQ]));}));}];};case 13:return function(_cS){return [0,function(_cT){return E(new T(function(){return B(A(new T(function(){return B(_cF(_cI));}),[_cS]));}));}];};case 32:return function(_cU){return [0,function(_cV){return E(new T(function(){return B(A(new T(function(){return B(_cF(_cI));}),[_cU]));}));}];};case 160:return function(_cW){return [0,function(_cX){return E(new T(function(){return B(A(new T(function(){return B(_cF(_cI));}),[_cW]));}));}];};default:var _cY=u_iswspace(_cJ),_cZ=_cY;return E(_cZ)==0?E(_cD):function(_d0){return [0,function(_d1){return E(new T(function(){return B(A(new T(function(){return B(_cF(_cI));}),[_d0]));}));}];};}}},_d2=function(_d3){var _d4=new T(function(){return B(_d2(_d3));}),_d5=[1,function(_d6){return new F(function(){return A(_cF,[_d6,function(_d7){return E([0,function(_d8){return E(E(_d8)[1])==92?E(_d4):[2];}]);}]);});}];return new F(function(){return _4n([0,function(_d9){return E(E(_d9)[1])==92?E([0,function(_da){var _db=E(E(_da)[1]);switch(_db){case 9:return E(_d5);case 10:return E(_d5);case 11:return E(_d5);case 12:return E(_d5);case 13:return E(_d5);case 32:return E(_d5);case 38:return E(_d4);case 160:return E(_d5);default:var _dc=u_iswspace(_db),_dd=_dc;return E(_dd)==0?[2]:E(_d5);}}]):[2];}],[0,function(_de){var _df=E(_de);return E(_df[1])==92?E(new T(function(){return B(_cj(function(_dg){return new F(function(){return A(_d3,[[0,_dg,_81]]);});}));})):B(A(_d3,[[0,_df,_80]]));}]);});},_dh=function(_di,_dj){return new F(function(){return _d2(function(_dk){var _dl=E(_dk),_dm=E(_dl[1]);if(E(_dm[1])==34){if(!E(_dl[2])){return E(new T(function(){return B(A(_dj,[[1,new T(function(){return B(A(_di,[_0]));})]]));}));}else{return new F(function(){return _dh(function(_dn){return new F(function(){return A(_di,[[1,_dm,_dn]]);});},_dj);});}}else{return new F(function(){return _dh(function(_do){return new F(function(){return A(_di,[[1,_dm,_do]]);});},_dj);});}});});},_dp=new T(function(){return B(unCStr("_\'"));}),_dq=function(_dr){var _ds=u_iswalnum(_dr),_dt=_ds;return E(_dt)==0?B(_7G(_55,[0,_dr],_dp)):true;},_du=function(_dv){return new F(function(){return _dq(E(_dv)[1]);});},_dw=new T(function(){return B(unCStr(",;()[]{}`"));}),_dx=new T(function(){return B(unCStr(".."));}),_dy=new T(function(){return B(unCStr("::"));}),_dz=new T(function(){return B(unCStr("->"));}),_dA=[0,64],_dB=[1,_dA,_0],_dC=[0,126],_dD=[1,_dC,_0],_dE=new T(function(){return B(unCStr("=>"));}),_dF=[1,_dE,_0],_dG=[1,_dD,_dF],_dH=[1,_dB,_dG],_dI=[1,_dz,_dH],_dJ=new T(function(){return B(unCStr("<-"));}),_dK=[1,_dJ,_dI],_dL=[0,124],_dM=[1,_dL,_0],_dN=[1,_dM,_dK],_dO=[1,_89,_0],_dP=[1,_dO,_dN],_dQ=[0,61],_dR=[1,_dQ,_0],_dS=[1,_dR,_dP],_dT=[1,_dy,_dS],_dU=[1,_dx,_dT],_dV=function(_dW){return new F(function(){return _4n([1,function(_dX){return E(_dX)[0]==0?E(new T(function(){return B(A(_dW,[_65]));})):[2];}],new T(function(){return B(_4n([0,function(_dY){return E(E(_dY)[1])==39?E([0,function(_dZ){var _e0=E(_dZ);switch(E(_e0[1])){case 39:return [2];case 92:return E(new T(function(){return B(_cj(function(_e1){return [0,function(_e2){return E(E(_e2)[1])==39?E(new T(function(){return B(A(_dW,[[0,_e1]]));})):[2];}];}));}));default:return [0,function(_e3){return E(E(_e3)[1])==39?E(new T(function(){return B(A(_dW,[[0,_e0]]));})):[2];}];}}]):[2];}],new T(function(){return B(_4n([0,function(_e4){return E(E(_e4)[1])==34?E(new T(function(){return B(_dh(_66,_dW));})):[2];}],new T(function(){return B(_4n([0,function(_e5){return !B(_7G(_55,_e5,_dw))?[2]:B(A(_dW,[[2,[1,_e5,_0]]]));}],new T(function(){return B(_4n([0,function(_e6){return !B(_7G(_55,_e6,_7L))?[2]:[1,B(_5U(_7M,function(_e7){var _e8=[1,_e6,_e7];return !B(_7G(_5e,_e8,_dU))?B(A(_dW,[[4,_e8]])):B(A(_dW,[[2,_e8]]));}))];}],new T(function(){return B(_4n([0,function(_e9){var _ea=E(_e9),_eb=_ea[1],_ec=u_iswalpha(_eb),_ed=_ec;return E(_ed)==0?E(_eb)==95?[1,B(_5U(_du,function(_ee){return new F(function(){return A(_dW,[[3,[1,_ea,_ee]]]);});}))]:[2]:[1,B(_5U(_du,function(_ef){return new F(function(){return A(_dW,[[3,[1,_ea,_ef]]]);});}))];}],new T(function(){return [1,B(_5y(_7Y,_7C,_dW))];})));})));})));})));})));}));});},_eg=[0,0],_eh=function(_ei,_ej){return function(_ek){return new F(function(){return A(_cF,[_ek,function(_el){return E(new T(function(){return B(_dV(function(_em){var _en=E(_em);return _en[0]==2?!B(_4U(_en[1],_4T))?[2]:E(new T(function(){return B(A(_ei,[_eg,function(_eo){return [1,function(_ep){return new F(function(){return A(_cF,[_ep,function(_eq){return E(new T(function(){return B(_dV(function(_er){var _es=E(_er);return _es[0]==2?!B(_4U(_es[1],_4R))?[2]:E(new T(function(){return B(A(_ej,[_eo]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_et=function(_eu,_ev,_ew){var _ex=function(_ey,_ez){return new F(function(){return _4n([1,function(_eA){return new F(function(){return A(_cF,[_eA,function(_eB){return E(new T(function(){return B(_dV(function(_eC){var _eD=E(_eC);if(_eD[0]==4){var _eE=E(_eD[1]);if(!_eE[0]){return new F(function(){return A(_eu,[_eD,_ey,_ez]);});}else{return E(E(_eE[1])[1])==45?E(_eE[2])[0]==0?E([1,function(_eF){return new F(function(){return A(_cF,[_eF,function(_eG){return E(new T(function(){return B(_dV(function(_eH){return new F(function(){return A(_eu,[_eH,_ey,function(_eI){return new F(function(){return A(_ez,[new T(function(){return B(_6Q(_eI));})]);});}]);});}));}));}]);});}]):B(A(_eu,[_eD,_ey,_ez])):B(A(_eu,[_eD,_ey,_ez]));}}else{return new F(function(){return A(_eu,[_eD,_ey,_ez]);});}}));}));}]);});}],new T(function(){return [1,B(_eh(_ex,_ez))];}));});};return new F(function(){return _ex(_ev,_ew);});},_eJ=function(_eK,_eL){return [2];},_eM=function(_eN){var _eO=E(_eN);return _eO[0]==0?[1,new T(function(){return B(_76(new T(function(){return B(_6W(E(_eO[1])[1]));}),_6V,_eO[2]));})]:E(_eO[2])[0]==0?E(_eO[3])[0]==0?[1,new T(function(){return B(_76(_6U,_6V,_eO[1]));})]:[0]:[0];},_eP=function(_eQ){var _eR=E(_eQ);if(_eR[0]==5){var _eS=B(_eM(_eR[1]));return _eS[0]==0?E(_eJ):function(_eT,_eU){return new F(function(){return A(_eU,[_eS[1]]);});};}else{return E(_eJ);}},_eV=function(_eW,_eX){return new F(function(){return _eY(_eX);});},_eZ=new T(function(){return B(unCStr("True"));}),_f0=new T(function(){return B(unCStr("False"));}),_f1=function(_f2){return function(_f3){return new F(function(){return A(_cF,[_f3,function(_f4){return E(new T(function(){return B(_dV(function(_f5){var _f6=E(_f5);if(_f6[0]==3){var _f7=_f6[1];return !B(_4U(_f7,_f0))?!B(_4U(_f7,_eZ))?[2]:E(new T(function(){return B(A(_f2,[_81]));})):E(new T(function(){return B(A(_f2,[_80]));}));}else{return [2];}}));}));}]);});};},_eY=function(_f8){return new F(function(){return _4n([1,B(_f1(_f8))],new T(function(){return [1,B(_eh(_eV,_f8))];}));});},_f9=function(_fa,_fb){return new F(function(){return _fc(_fb);});},_fd=function(_fe){return new F(function(){return _4n([1,function(_ff){return new F(function(){return A(_cF,[_ff,function(_fg){return E(new T(function(){return B(_dV(function(_fh){var _fi=E(_fh);return _fi[0]==0?B(A(_fe,[_fi[1]])):[2];}));}));}]);});}],new T(function(){return [1,B(_eh(_fj,_fe))];}));});},_fj=function(_fk,_fl){return new F(function(){return _fd(_fl);});},_fm=[0,91],_fn=[1,_fm,_0],_fo=function(_fp,_fq){var _fr=function(_fs,_ft){return [1,function(_fu){return new F(function(){return A(_cF,[_fu,function(_fv){return E(new T(function(){return B(_dV(function(_fw){var _fx=E(_fw);if(_fx[0]==2){var _fy=E(_fx[1]);if(!_fy[0]){return [2];}else{var _fz=_fy[2];switch(E(E(_fy[1])[1])){case 44:return E(_fz)[0]==0?!E(_fs)?[2]:E(new T(function(){return B(A(_fp,[_eg,function(_fA){return new F(function(){return _fr(_81,function(_fB){return new F(function(){return A(_ft,[[1,_fA,_fB]]);});});});}]));})):[2];case 93:return E(_fz)[0]==0?E(new T(function(){return B(A(_ft,[_0]));})):[2];default:return [2];}}}else{return [2];}}));}));}]);});}];},_fC=function(_fD){return new F(function(){return _4n([1,function(_fE){return new F(function(){return A(_cF,[_fE,function(_fF){return E(new T(function(){return B(_dV(function(_fG){var _fH=E(_fG);return _fH[0]==2?!B(_4U(_fH[1],_fn))?[2]:E(new T(function(){return B(_4n(B(_fr(_80,_fD)),new T(function(){return B(A(_fp,[_eg,function(_fI){return new F(function(){return _fr(_81,function(_fJ){return new F(function(){return A(_fD,[[1,_fI,_fJ]]);});});});}]));})));})):[2];}));}));}]);});}],new T(function(){return [1,B(_eh(function(_fK,_fL){return new F(function(){return _fC(_fL);});},_fD))];}));});};return new F(function(){return _fC(_fq);});},_fc=function(_fM){return new F(function(){return _4n(B(_4n([1,function(_fN){return new F(function(){return A(_cF,[_fN,function(_fO){return E(new T(function(){return B(_dV(function(_fP){var _fQ=E(_fP);return _fQ[0]==1?B(A(_fM,[_fQ[1]])):[2];}));}));}]);});}],new T(function(){return B(_fo(_fj,_fM));}))),new T(function(){return [1,B(_eh(_f9,_fM))];}));});},_fR=new T(function(){return B(unCStr("Todo"));}),_fS=[0,123],_fT=[1,_fS,_0],_fU=new T(function(){return B(unCStr("identifier"));}),_fV=[0,61],_fW=[1,_fV,_0],_fX=[0,44],_fY=[1,_fX,_0],_fZ=new T(function(){return B(unCStr("task"));}),_g0=new T(function(){return B(unCStr("completed"));}),_g1=[1,_m,_0],_g2=function(_g3,_g4){return _g3>11?[2]:[1,function(_g5){return new F(function(){return A(_cF,[_g5,function(_g6){return E(new T(function(){return B(_dV(function(_g7){var _g8=E(_g7);return _g8[0]==3?!B(_4U(_g8[1],_fR))?[2]:E([1,function(_g9){return new F(function(){return A(_cF,[_g9,function(_ga){return E(new T(function(){return B(_dV(function(_gb){var _gc=E(_gb);return _gc[0]==2?!B(_4U(_gc[1],_fT))?[2]:E([1,function(_gd){return new F(function(){return A(_cF,[_gd,function(_ge){return E(new T(function(){return B(_dV(function(_gf){var _gg=E(_gf);return _gg[0]==3?!B(_4U(_gg[1],_fU))?[2]:E([1,function(_gh){return new F(function(){return A(_cF,[_gh,function(_gi){return E(new T(function(){return B(_dV(function(_gj){var _gk=E(_gj);return _gk[0]==2?!B(_4U(_gk[1],_fW))?[2]:E(new T(function(){return B(_et(_eP,_eg,function(_gl){return [1,function(_gm){return new F(function(){return A(_cF,[_gm,function(_gn){return E(new T(function(){return B(_dV(function(_go){var _gp=E(_go);return _gp[0]==2?!B(_4U(_gp[1],_fY))?[2]:E([1,function(_gq){return new F(function(){return A(_cF,[_gq,function(_gr){return E(new T(function(){return B(_dV(function(_gs){var _gt=E(_gs);return _gt[0]==3?!B(_4U(_gt[1],_fZ))?[2]:E([1,function(_gu){return new F(function(){return A(_cF,[_gu,function(_gv){return E(new T(function(){return B(_dV(function(_gw){var _gx=E(_gw);return _gx[0]==2?!B(_4U(_gx[1],_fW))?[2]:E(new T(function(){return B(_fc(function(_gy){return [1,function(_gz){return new F(function(){return A(_cF,[_gz,function(_gA){return E(new T(function(){return B(_dV(function(_gB){var _gC=E(_gB);return _gC[0]==2?!B(_4U(_gC[1],_fY))?[2]:E([1,function(_gD){return new F(function(){return A(_cF,[_gD,function(_gE){return E(new T(function(){return B(_dV(function(_gF){var _gG=E(_gF);return _gG[0]==3?!B(_4U(_gG[1],_g0))?[2]:E([1,function(_gH){return new F(function(){return A(_cF,[_gH,function(_gI){return E(new T(function(){return B(_dV(function(_gJ){var _gK=E(_gJ);return _gK[0]==2?!B(_4U(_gK[1],_fW))?[2]:E(new T(function(){return B(_eY(function(_gL){return [1,function(_gM){return new F(function(){return A(_cF,[_gM,function(_gN){return E(new T(function(){return B(_dV(function(_gO){var _gP=E(_gO);return _gP[0]==2?!B(_4U(_gP[1],_g1))?[2]:E(new T(function(){return B(A(_g4,[[0,_gl,_gy,_gL]]));})):[2];}));}));}]);});}];}));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];},_gQ=function(_gR,_gS){return new F(function(){return _g2(E(_gR)[1],_gS);});},_gT=function(_gU,_gV){var _gW=function(_gX){return function(_gY){return new F(function(){return _4n(B(A(new T(function(){return B(A(_gU,[_gX]));}),[_gY])),new T(function(){return [1,B(_eh(_gW,_gY))];}));});};};return new F(function(){return _gW(_gV);});},_gZ=function(_h0){return [1,function(_h1){return new F(function(){return A(_cF,[_h1,function(_h2){return E([3,_h0,_5q]);}]);});}];},_h3=new T(function(){return B(A(_gT,[_gQ,_eg,_gZ]));}),_h4=new T(function(){return B(unCStr("Tried to parse a non Todo instance as Todo"));}),_h5=new T(function(){return B(err(_h4));}),_h6=function(_h7){return new F(function(){return fromJSStr(E(_h7)[1]);});},_h8=function(_h9){while(1){var _ha=(function(_hb){var _hc=E(_hb);if(!_hc[0]){return [0];}else{var _hd=_hc[2],_he=E(_hc[1]);if(!E(_he[2])[0]){return [1,_he[1],new T(function(){return B(_h8(_hd));})];}else{_h9=_hd;return null;}}})(_h9);if(_ha!=null){return _ha;}}},_hf=function(_hg){var _hh=E(_hg);return _hh[0]==1?new T(function(){var _hi=B(_h8(B(_4d(_h3,new T(function(){return B(_h6(_hh[1]));})))));return _hi[0]==0?E(_2U):E(_hi[2])[0]==0?E(_hi[1]):E(_2S);}):E(_h5);},_hj=function(_hk){return [1,B(_hf(_hk))];},_hl=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_hm=[0,_hl],_hn=[1,_0],_ho=function(_hp){var _hq=E(_hp);if(!_hq[0]){return E(_hn);}else{var _hr=B(_ho(_hq[2]));return _hr[0]==0?[0,_hr[1]]:[1,[1,B(_hf(_hq[1])),_hr[1]]];}},_hs=function(_ht){var _hu=E(_ht);return _hu[0]==3?B(_ho(_hu[1])):E(_hm);},_hv=[0,_2J,_2P,_hj,_hs],_hw=function(_hx){return E(E(_hx)[2]);},_hy=function(_hz,_hA){return [3,new T(function(){return B(_2L(new T(function(){return B(_hw(_hz));}),_hA));})];},_hB=[1,_0],_hC=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_hD=[0,_hC],_hE=function(_hF){return E(E(_hF)[4]);},_hG=function(_hH,_hI){var _hJ=E(_hI);if(_hJ[0]==3){var _hK=function(_hL){var _hM=E(_hL);if(!_hM[0]){return E(_hB);}else{var _hN=B(A(new T(function(){return B(_hE(_hH));}),[_hM[1]]));if(!_hN[0]){return [0,_hN[1]];}else{var _hO=B(_hK(_hM[2]));return _hO[0]==0?[0,_hO[1]]:[1,[1,_hN[1],_hO[1]]];}}};return new F(function(){return _hK(_hJ[1]);});}else{return E(_hD);}},_hP=function(_hQ){return [0,new T(function(){return B(_hw(_hQ));}),function(_hR){return new F(function(){return _hy(_hQ,_hR);});},new T(function(){return B(_hE(_hQ));}),function(_hR){return new F(function(){return _hG(_hQ,_hR);});}];},_hS=new T(function(){return B(_hP(_hv));}),_hT=function(_hU,_hV){return [0,function(_){var _hW=B(A(_hU,[_])),_hX=_hW;return new T(function(){return B(A(_hV,[_hX]));});}];},_hY=function(_hZ,_i0,_){var _i1=jsGet(_hZ,toJSStr(E(_i0))),_i2=_i1;return new T(function(){return fromJSStr(_i2);});},_i3=[2],_i4=function(_i5){return [2];},_i6=function(_i7,_i8,_i9){return function(_){var _ia=E(_i7)[1],_ib=rMV(_ia),_ic=_ib,_id=E(_ic);if(!_id[0]){var _=wMV(_ia,[0,_id[1],new T(function(){return B(_1(_id[2],[1,[0,_i8,function(_ie){return E(new T(function(){return B(A(_i9,[_5]));}));}],_0]));})]);return _i3;}else{var _if=E(_id[1]);if(!_if[0]){var _=wMV(_ia,[0,_i8,_0]);return new T(function(){return B(A(_i9,[_5]));});}else{var _=wMV(_ia,[1,_if[2]]);return [1,[1,new T(function(){return B(A(_i9,[_5]));}),[1,new T(function(){return B(A(_if[1],[_i8,_i4]));}),_0]]];}}};},_ig=function(_ih,_ii){var _ij=E(_ih);if(!_ij[0]){var _ik=_ij[1],_il=E(_ii);return _il[0]==0?_ik==_il[1]:I_compareInt(_il[1],_ik)==0?true:false;}else{var _im=_ij[1],_in=E(_ii);return _in[0]==0?I_compareInt(_im,_in[1])==0?true:false:I_compare(_im,_in[1])==0?true:false;}},_io=function(_ip,_iq){var _ir=E(_ip),_is=E(_iq),_it=_is[3];return !B(_ig(_ir[1],_is[1]))?true:!B(_4U(_ir[2],_is[2]))?true:!E(_ir[3])?E(_it):!E(_it)?true:false;},_iu=function(_iv,_iw){return !E(_iv)?!E(_iw)?true:false:E(_iw);},_ix=function(_iy,_iz,_iA,_iB,_iC,_iD){return !B(_ig(_iy,_iB))?false:!B(_4U(_iz,_iC))?false:B(_iu(_iA,_iD));},_iE=function(_iF,_iG){var _iH=E(_iF),_iI=E(_iG);return new F(function(){return _ix(_iH[1],_iH[2],_iH[3],_iI[1],_iI[2],_iI[3]);});},_iJ=[0,_iE,_io],_iK=function(_iL,_iM,_iN){while(1){var _iO=E(_iM);if(!_iO[0]){return E(_iN)[0]==0?true:false;}else{var _iP=E(_iN);if(!_iP[0]){return false;}else{if(!B(A(_7E,[_iL,_iO[1],_iP[1]]))){return false;}else{_iM=_iO[2];_iN=_iP[2];continue;}}}}},_iQ=[1,_0],_iR=function(_iS,_iT){return function(_){var _iU=E(_iS)[1],_iV=rMV(_iU),_iW=_iV,_iX=E(_iW);if(!_iX[0]){var _iY=_iX[1],_iZ=E(_iX[2]);if(!_iZ[0]){var _=wMV(_iU,_iQ);return new T(function(){return B(A(_iT,[_iY]));});}else{var _j0=E(_iZ[1]),_=wMV(_iU,[0,_j0[1],_iZ[2]]);return [1,[1,new T(function(){return B(A(_iT,[_iY]));}),[1,new T(function(){return B(A(_j0[2],[_i4]));}),_0]]];}}else{var _=wMV(_iU,[1,new T(function(){return B(_1(_iX[1],[1,function(_j1){return function(_j2){return E(new T(function(){return B(A(_iT,[_j1]));}));};},_0]));})]);return _i3;}};},_j3=function(_j4,_j5){while(1){var _j6=E(_j4);if(!_j6[0]){return E(_j5);}else{_j4=_j6[2];var _j7=_j5+1|0;_j5=_j7;continue;}}},_j8=function(_j9,_ja,_jb,_jc){return new F(function(){return A(_j9,[function(_){var _jd=jsSet(E(_ja)[1],toJSStr(E(_jb)),toJSStr(E(_jc)));return _5;}]);});},_je=function(_jf){return new F(function(){return A(_jf,[_5]);});},_jg=function(_jh){return !E(E(_jh)[3])?true:false;},_ji=function(_jj,_jk){while(1){var _jl=E(_jk);if(!_jl[0]){return false;}else{if(!B(A(_jj,[_jl[1]]))){_jk=_jl[2];continue;}else{return true;}}}},_jm=function(_jn){return E(E(_jn)[3]);},_jo=function(_jp){var _jq=B(A(_jp,[_])),_jr=_jq;return E(_jr);},_js=function(_jt){return new F(function(){return _jo(function(_){var _=0;return new F(function(){return eval(_jt);});});});},_ju=function(_){var _jv=B(A(_js,["document",_])),_jw=_jv;return [0,_jw];},_jx=function(_){return new F(function(){return _ju(_);});},_jy=function(_){var _=0;return new F(function(){return _jx(_);});},_jz=new T(function(){return B(_jo(_jy));}),_jA=new T(function(){return [0,"blur"];}),_jB=new T(function(){return [0,"keyup"];}),_jC=new T(function(){return [0,"dblclick"];}),_jD=new T(function(){return [0,"click"];}),_jE=function(_jF,_jG){while(1){var _jH=(function(_jI,_jJ){var _jK=E(_jJ);if(!_jK[0]){return [0];}else{var _jL=_jK[1],_jM=_jK[2];if(!B(A(_jI,[_jL]))){var _jN=_jI;_jG=_jM;_jF=_jN;return null;}else{return [1,_jL,new T(function(){return B(_jE(_jI,_jM));})];}}})(_jF,_jG);if(_jH!=null){return _jH;}}},_jO=new T(function(){return B(_js("(function(e) {e.focus();})"));}),_jP=function(_jQ,_){var _jR=B(A(_jO,[E(E(_jQ)[1]),_])),_jS=_jR;return _5;},_jT=function(_jU,_){return new F(function(){return _jP(_jU,_);});},_jV=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_jW=new T(function(){return B(err(_jV));}),_jX=function(_jY){var _jZ=E(_jY);return _jZ[0]==0?E(_jW):E(_jZ[1]);},_k0=function(_k1){return E(_k1)[0]==0?true:false;},_k2=function(_k3){var _k4=String(_k3),_k5=_k4;return [0,_k5];},_k6=function(_){var _k7=B(A(_js,["(function() {return location.hash.substring(1);})",_])),_k8=_k7;return new T(function(){return B(_k2(_k8));});},_k9=function(_){return new F(function(){return _k6(_);});},_ka=new T(function(){return B(unCStr("true"));}),_kb=new T(function(){return B(unCStr("completed"));}),_kc=new T(function(){return B(unCStr("value"));}),_kd=new T(function(){return B(unCStr("label"));}),_ke=new T(function(){return B(unCStr("li"));}),_kf=new T(function(){return B(unCStr("todo-list"));}),_kg=[0,41],_kh=[1,_kg,_0],_ki=new T(function(){return B(unCStr("#clear-completed"));}),_kj=new T(function(){return B(unCStr("item"));}),_kk=new T(function(){return B(unCStr("items"));}),_kl=new T(function(){return B(unCStr("checked"));}),_km=new T(function(){return B(unCStr("#todo-count"));}),_kn=new T(function(){return B(unCStr("footer"));}),_ko=[1,_kn,_0],_kp=new T(function(){return B(unCStr("main"));}),_kq=[1,_kp,_ko],_kr=[0,35],_ks=new T(function(){return B(unCStr("selected"));}),_kt=new T(function(){return B(unCStr("href"));}),_ku=new T(function(){return B(unCStr("#filters li a"));}),_kv=new T(function(){return B(unCStr("hidden"));}),_kw=new T(function(){return B(unCStr("toggle-all"));}),_kx=new T(function(){return B(unCStr("div"));}),_ky=new T(function(){return B(unCStr("todos"));}),_kz=new T(function(){return [0,toJSStr(_0)];}),_kA=[0,93],_kB=[1,_kA,_0],_kC=new T(function(){return [0,toJSStr(_kB)];}),_kD=[0,125],_kE=[1,_kD,_0],_kF=new T(function(){return [0,toJSStr(_kE)];}),_kG=[0,58],_kH=[1,_kG,_0],_kI=new T(function(){return [0,toJSStr(_kH)];}),_kJ=[0,44],_kK=[1,_kJ,_0],_kL=new T(function(){return [0,toJSStr(_kK)];}),_kM=new T(function(){return [0,"false"];}),_kN=function(_kO){var _kP=jsShow(E(_kO)[1]),_kQ=_kP;return [0,_kQ];},_kR=function(_kS){var _kT=jsStringify(E(_kS)[1]),_kU=_kT;return [0,_kU];},_kV=new T(function(){return [0,"null"];}),_kW=[0,91],_kX=[1,_kW,_0],_kY=new T(function(){return [0,toJSStr(_kX)];}),_kZ=[0,123],_l0=[1,_kZ,_0],_l1=new T(function(){return [0,toJSStr(_l0)];}),_l2=[0,34],_l3=[1,_l2,_0],_l4=new T(function(){return [0,toJSStr(_l3)];}),_l5=new T(function(){return [0,"true"];}),_l6=function(_l7,_l8){var _l9=E(_l8);switch(_l9[0]){case 0:return [0,new T(function(){return B(_kN(_l9[1]));}),_l7];case 1:return [0,new T(function(){return B(_kR(_l9[1]));}),_l7];case 2:return !E(_l9[1])?[0,_kM,_l7]:[0,_l5,_l7];case 3:var _la=E(_l9[1]);return _la[0]==0?[0,_kY,[1,_kC,_l7]]:[0,_kY,new T(function(){var _lb=B(_l6(new T(function(){var _lc=function(_ld){var _le=E(_ld);return _le[0]==0?E([1,_kC,_l7]):[1,_kL,new T(function(){var _lf=B(_l6(new T(function(){return B(_lc(_le[2]));}),_le[1]));return [1,_lf[1],_lf[2]];})];};return B(_lc(_la[2]));}),_la[1]));return [1,_lb[1],_lb[2]];})];case 4:var _lg=E(_l9[1]);if(!_lg[0]){return [0,_l1,[1,_kF,_l7]];}else{var _lh=E(_lg[1]);return [0,_l1,[1,new T(function(){return B(_kR(_lh[1]));}),[1,_kI,new T(function(){var _li=B(_l6(new T(function(){var _lj=function(_lk){var _ll=E(_lk);if(!_ll[0]){return E([1,_kF,_l7]);}else{var _lm=E(_ll[1]);return [1,_kL,[1,_l4,[1,_lm[1],[1,_l4,[1,_kI,new T(function(){var _ln=B(_l6(new T(function(){return B(_lj(_ll[2]));}),_lm[2]));return [1,_ln[1],_ln[2]];})]]]]];}};return B(_lj(_lg[2]));}),_lh[2]));return [1,_li[1],_li[2]];})]]];}break;default:return [0,_kV,_l7];}},_lo=function(_lp){var _lq=jsCat(new T(function(){var _lr=B(_l6(_0,_lp));return [1,_lr[1],_lr[2]];}),E(_kz)[1]),_ls=_lq;return E(_ls);},_lt=new T(function(){return [0,"(function(k,v) {localStorage.setItem(k,v);})"];}),_lu=function(_lv){return E(E(_lv)[1]);},_lw=function(_lx,_ly){return function(_lz,_){var _lA=B(A(new T(function(){return B(A(_js,[E(_lt)[1],E(toJSStr(E(_ly)))]));}),[E(B(_lo(B(A(new T(function(){return B(_lu(_lx));}),[_lz]))))),_])),_lB=_lA;return _5;};},_lC=new T(function(){return B(_lw(_hS,_ky));}),_lD=new T(function(){return B(unCStr("/completed"));}),_lE=new T(function(){return B(unCStr("/active"));}),_lF=new T(function(){return B(unCStr(".toggle"));}),_lG=new T(function(){return B(unCStr(".edit"));}),_lH=new T(function(){return B(unCStr(".destroy"));}),_lI=new T(function(){return B(unCStr("editing"));}),_lJ=function(_lK,_lL){var _lM=E(_lL);return [0,_lM[1],_lK,_lM[3]];},_lN=new T(function(){return B(_js("(function(e,c,x){x?e.classList.add(c):e.classList.remove(c);})"));}),_lO=function(_){var _=0;return new F(function(){return A(_js,["false",_]);});},_lP=new T(function(){return B(_jo(_lO));}),_lQ=function(_){var _=0;return new F(function(){return A(_js,["true",_]);});},_lR=new T(function(){return B(_jo(_lQ));}),_lS=function(_lT){return function(_lU){return function(_lV,_){var _lW=B(A(new T(function(){return B(A(new T(function(){return B(A(_lN,[E(E(_lT)[1])]));}),[E(toJSStr(E(_lU)))]));}),[!E(_lV)?E(_lP):E(_lR),_])),_lX=_lW;return _5;};};},_lY=function(_lZ){var _m0=E(_lZ);return [0,_m0[1],_m0[2],new T(function(){return !E(_m0[3])?true:false;})];},_m1=function(_m2,_m3,_m4){return new F(function(){return _2L(function(_m5){var _m6=E(_m5),_m7=E(_m2),_m8=_m7[3];return !B(_ig(_m6[1],_m7[1]))?E(_m6):!B(_4U(_m6[2],_m7[2]))?E(_m6):!E(_m6[3])?!E(_m8)?B(A(_m3,[_m7])):E(_m6):!E(_m8)?E(_m6):B(A(_m3,[_m7]));},_m4);});},_m9=new T(function(){return B(unCStr("template-todo"));}),_ma=new T(function(){return B(unCStr(" could be found!"));}),_mb=function(_mc){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_1(_mc,_ma));}))));});},_md=function(_me,_mf){while(1){var _mg=(function(_mh,_mi){var _mj=E(_mh);if(!_mj[0]){return [0];}else{var _mk=_mj[2],_ml=E(_mi);if(!_ml[0]){return [0];}else{var _mm=_ml[2];if(!E(_ml[1])[0]){return [1,_mj[1],new T(function(){return B(_md(_mk,_mm));})];}else{_me=_mk;_mf=_mm;return null;}}}})(_me,_mf);if(_mg!=null){return _mg;}}},_mn=new T(function(){return B(unAppCStr("[]",_0));}),_mo=[1,_3q,_0],_mp=function(_mq){var _mr=E(_mq);return _mr[0]==0?E(_mo):[1,_3p,[1,_p,new T(function(){return B(_2q(_mr[1],[1,_p,new T(function(){return B(_mp(_mr[2]));})]));})]];},_ms=function(_mt,_mu){return new F(function(){return err(B(unAppCStr("Elements with the following IDs could not be found: ",new T(function(){var _mv=B(_md(_mu,_mt));return _mv[0]==0?E(_mn):[1,_3r,[1,_p,new T(function(){return B(_2q(_mv[1],[1,_p,new T(function(){return B(_mp(_mv[2]));})]));})]];}))));});},_mw=new T(function(){return B(unCStr("innerHTML"));}),_mx=function(_my,_mz){var _mA=new T(function(){return [0,B(_mx(_my,_i4))];});return new F(function(){return _iR(_my,function(_mB){return [0,B(_i6(_my,_mB,function(_mC){return E([0,function(_){var _mD=B(_k9(_)),_mE=_mD,_mF=new T(function(){return fromJSStr(E(_mE)[1]);}),_mG=[1,_kr,_mF],_mH=new T(function(){return B(_jE(_jg,_mB));}),_mI=function(_mJ){var _mK=E(_mJ);return _mK[0]==0?E(_je):function(_mL){return [0,function(_){var _mM=B(A(new T(function(){return B(A(_lS,[_mK[1],_kv,new T(function(){return B(_iK(_iJ,_mB,_0));})]));}),[_])),_mN=_mM;return new T(function(){return B(A(new T(function(){return B(_mI(_mK[2]));}),[_mL]));});}];};},_mO=new T(function(){return [0,B(_j3(_mH,0))];}),_mP=new T(function(){return [0,B(_j3(B(_jE(_jm,_mB)),0))];});return [0,function(_){var _mQ=E(_jz)[1],_mR=jsQuerySelectorAll(_mQ,toJSStr(E(_ku))),_mS=_mR;return new T(function(){var _mT=new T(function(){var _mU=function(_mV,_mW){var _mX=E(_mV);if(!_mX[0]){return new F(function(){return A(_mW,[_0]);});}else{return [0,function(_){var _mY=jsFind(toJSStr(E(_mX[1]))),_mZ=_mY;return new T(function(){return B(_mU(_mX[2],function(_n0){return new F(function(){return A(_mW,[[1,_mZ,_n0]]);});}));});}];}};return B((function(_n1,_n2,_n3){return [0,function(_){var _n4=jsFind(toJSStr(E(_n1))),_n5=_n4;return new T(function(){return B(_mU(_n2,function(_n6){return new F(function(){return A(_n3,[[1,_n5,_n6]]);});}));});}];})(_kp,_ko,function(_n7){if(!B(_ji(_k0,_n7))){return new F(function(){return A(_mI,[B(_2L(_jX,_n7)),function(_n8){return E([0,function(_){var _n9=jsQuerySelectorAll(_mQ,toJSStr(E(_km))),_na=_n9;return new T(function(){var _nb=function(_nc){var _nd=E(_nc);if(!_nd[0]){return function(_ne){return new F(function(){return A(_ne,[_5]);});};}else{var _nf=_nd[1];return function(_ng){return [0,function(_){var _nh=B(A(new T(function(){return B(A(_lS,[_nf,_kv,new T(function(){return E(E(_mO)[1])==0?true:false;})]));}),[_])),_ni=_nh;return [0,function(_){var _nj=jsSet(E(_nf)[1],toJSStr(E(_mw)),toJSStr(E(new T(function(){return B(unAppCStr("<strong>",new T(function(){var _nk=E(_mO)[1];return B(_1(B(_8h(0,_nk,_0)),new T(function(){return B(unAppCStr("</strong> ",new T(function(){return E(_nk)==1?E(_kj):E(_kk);})));})));})));}))));return new T(function(){return B(A(new T(function(){return B(_nb(_nd[2]));}),[_ng]));});}];}];};}};return B(A(_nb,[_na,function(_nl){return E([0,function(_){var _nm=jsQuerySelectorAll(_mQ,toJSStr(E(_ki))),_nn=_nm;return new T(function(){var _no=function(_np){var _nq=E(_np);if(!_nq[0]){return function(_nr){return new F(function(){return A(_nr,[_5]);});};}else{var _ns=_nq[1];return function(_nt){return [0,function(_){var _nu=B(A(new T(function(){return B(A(_lS,[_ns,_kv,new T(function(){return E(E(_mP)[1])==0?true:false;})]));}),[_])),_nv=_nu;return [0,function(_){var _nw=jsSet(E(_ns)[1],toJSStr(E(_mw)),toJSStr(E(new T(function(){return B(unAppCStr("Clear completed (",new T(function(){return B(_1(B(_8h(0,E(_mP)[1],_0)),_kh));})));}))));return new T(function(){return B(A(new T(function(){return B(_no(_nq[2]));}),[_nt]));});}];}];};}};return B(A(_no,[_nn,function(_nx){return E([0,function(_){var _ny=E(_kf),_nz=jsFind(toJSStr(_ny)),_nA=_nz;return new T(function(){var _nB=E(_nA);if(!_nB[0]){var _nC=B(_mb(_ny));}else{var _nD=_nB[1],_nC=[0,function(_){var _nE=E(_nD)[1],_nF=jsQuerySelectorAll(_nE,toJSStr(E(_ke))),_nG=_nF;return new T(function(){var _nH=function(_nI,_nJ){var _nK=E(_nI);if(!_nK[0]){return new F(function(){return A(_nJ,[_5]);});}else{return [0,function(_){var _nL=jsKillChild(E(_nK[1])[1],_nE);return new T(function(){return B(_nH(_nK[2],_nJ));});}];}};return B(_nH(_nG,function(_nM){return E(new T(function(){var _nN=function(_nO,_nP){var _nQ=E(_nO);if(!_nQ[0]){return new F(function(){return A(_nP,[_0]);});}else{var _nR=_nQ[1],_nS=function(_nT,_nU,_nV,_nW,_){return new F(function(){return _d([0,function(_){var _nX=jsGet(_nV,toJSStr(E(_kc))),_nY=_nX;return new T(function(){var _nZ=new T(function(){return fromJSStr(_nY);});return [0,B(_iR(_my,function(_o0){var _o1=new T(function(){return B(_m1(_nR,function(_o2){return new F(function(){return _lJ(_nZ,_o2);});},_o0));});return [0,B(_i6(_my,_o1,function(_o3){return E([0,function(_){var _o4=B(A(_lC,[_o1,_])),_o5=_o4;return [0,function(_){var _o6=jsQuerySelectorAll(_nT,toJSStr(E(_kd))),_o7=_o6;return new T(function(){var _o8=function(_o9){var _oa=E(_o9);return _oa[0]==0?function(_ob){return new F(function(){return A(_ob,[_5]);});}:function(_oc){return new F(function(){return A(new T(function(){return B(_j8(_hT,_oa[1],_mw,_nZ));}),[function(_od){return E(new T(function(){return B(A(new T(function(){return B(_o8(_oa[2]));}),[_oc]));}));}]);});};};return B(A(_o8,[_o7,function(_oe){return E([0,function(_){var _of=B(A(_lS,[[0,_nT],_lI,_80,_])),_og=_of;return _i3;}]);}]));});}];}]);}))];}))];});}],_0,_);});},_oh=function(_oi,_oj,_ok,_){return new F(function(){return _d([0,function(_){var _ol=jsGet(E(_ok)[1],toJSStr(E(_kc))),_om=_ol;return new T(function(){var _on=new T(function(){return fromJSStr(_om);});return [0,B(_iR(_my,function(_oo){var _op=new T(function(){return B(_m1(_nR,function(_o2){return new F(function(){return _lJ(_on,_o2);});},_oo));});return [0,B(_i6(_my,_op,function(_oq){return E([0,function(_){var _or=B(A(_lC,[_op,_])),_os=_or;return [0,function(_){var _ot=jsQuerySelectorAll(_oi,toJSStr(E(_kd))),_ou=_ot;return new T(function(){var _ov=function(_ow){var _ox=E(_ow);return _ox[0]==0?function(_oy){return new F(function(){return A(_oy,[_5]);});}:function(_oz){return new F(function(){return A(new T(function(){return B(_j8(_hT,_ox[1],_mw,_on));}),[function(_oA){return E(new T(function(){return B(A(new T(function(){return B(_ov(_ox[2]));}),[_oz]));}));}]);});};};return B(A(_ov,[_ou,function(_oB){return E([0,function(_){var _oC=B(A(_lS,[[0,_oi],_lI,_80,_])),_oD=_oC;return _i3;}]);}]));});}];}]);}))];}))];});}],_0,_);});},_oE=new T(function(){return E(E(_nR)[3]);}),_oF=function(_oG){var _oH=E(_oG);return _oH[0]==0?function(_oI){return new F(function(){return A(_oI,[_5]);});}:function(_oJ){return new F(function(){return A(new T(function(){return B(_j8(_hT,_oH[1],_kl,new T(function(){return !E(_oE)?[0]:E(_ka);})));}),[function(_oK){return E(new T(function(){return B(A(new T(function(){return B(_oF(_oH[2]));}),[_oJ]));}));}]);});};},_oL=new T(function(){return E(E(_nR)[2]);}),_oM=function(_oN){var _oO=E(_oN);return _oO[0]==0?function(_oP){return new F(function(){return A(_oP,[_5]);});}:function(_oQ){return new F(function(){return A(new T(function(){return B(_j8(_hT,_oO[1],_kc,_oL));}),[function(_oR){return E(new T(function(){return B(A(new T(function(){return B(_oM(_oO[2]));}),[_oQ]));}));}]);});};},_oS=function(_oT){var _oU=E(_oT);return _oU[0]==0?function(_oV){return new F(function(){return A(_oV,[_5]);});}:function(_oW){return new F(function(){return A(new T(function(){return B(_j8(_hT,_oU[1],_mw,_oL));}),[function(_oX){return E(new T(function(){return B(A(new T(function(){return B(_oS(_oU[2]));}),[_oW]));}));}]);});};};return [0,function(_){var _oY=jsCreateElem(toJSStr(E(_kx))),_oZ=_oY;return [0,function(_){var _p0=E(_m9),_p1=jsFind(toJSStr(_p0)),_p2=_p1;return new T(function(){var _p3=E(_p2);if(!_p3[0]){var _p4=B(_mb(_p0));}else{var _p4=[0,function(_){var _p5=E(_mw),_p6=jsGet(E(_p3[1])[1],toJSStr(_p5)),_p7=_p6;return [0,function(_){var _p8=jsSet(_oZ,toJSStr(_p5),toJSStr(fromJSStr(_p7)));return [0,function(_){var _p9=jsQuerySelectorAll(_oZ,toJSStr(E(_ke))),_pa=_p9;return new T(function(){var _pb=function(_pc){var _pd=E(_pc);if(!_pd[0]){return function(_pe){return new F(function(){return A(_pe,[_0]);});};}else{var _pf=_pd[1];return function(_pg){return [0,function(_){var _ph=E(_pf),_pi=_ph[1],_pj=jsQuerySelectorAll(_pi,toJSStr(E(_kd))),_pk=_pj;return new T(function(){return B(A(_oS,[_pk,function(_pl){return E([0,function(_){var _pm=jsQuerySelectorAll(_pi,toJSStr(E(_lF))),_pn=_pm;return new T(function(){return B(A(_oF,[_pn,function(_po){return E([0,function(_){var _pp=jsQuerySelectorAll(_pi,toJSStr(E(_lG))),_pq=_pp;return new T(function(){return B(A(_oM,[_pq,function(_pr){return E([0,function(_){var _ps=B(A(new T(function(){return B(A(_lS,[_pf,_kb,_oE]));}),[_])),_pt=_ps;return new T(function(){return B(A(new T(function(){return B(_pb(_pd[2]));}),[function(_pu){return new F(function(){return A(_pg,[[1,_ph,_pu]]);});}]));});}]);}]));});}]);}]));});}]);}]));});}];};}};return B(A(_pb,[_pa,function(_pv){var _pw=new T(function(){return B(_P(_pv,0));});return [0,function(_){var _px=E(_pw)[1],_py=jsSetCB(_px,E(_jC)[1],function(_pz,_pA,_){var _pB=B(A(new T(function(){return B(_lS(_pw));}),[_lI,_81,_])),_pC=_pB,_pD=jsQuerySelectorAll(E(_pw)[1],toJSStr(E(_lG))),_pE=_pD;return new F(function(){return (function(_pF,_){while(1){var _pG=E(_pF);if(!_pG[0]){return _5;}else{var _pH=B(A(_jT,[_pG[1],_])),_pI=_pH;_pF=_pG[2];continue;}}})(_pE,_);});}),_pJ=_py,_pK=[0,function(_){var _pL=jsAppendChild(_px,E(_nD)[1]);return new T(function(){return B(_nN(_nQ[2],function(_pM){return new F(function(){return A(_nP,[[1,_5,_pM]]);});}));});}],_pN=[0,function(_){var _pO=jsQuerySelectorAll(_px,toJSStr(E(_lF))),_pP=_pO;return new T(function(){var _pQ=E(_pP);if(!_pQ[0]){var _pR=E(_pK);}else{var _pS=function(_pT,_pU,_){return new F(function(){return (function(_){return new F(function(){return _d(new T(function(){return [0,B(_iR(_my,function(_pV){var _pW=new T(function(){return B(_m1(_nR,_lY,_pV));});return [0,B(_i6(_my,_pW,function(_pX){return E([0,function(_){var _pY=B(A(_lC,[_pW,_])),_pZ=_pY;return _mA;}]);}))];}))];}),_0,_);});})(_);});},_pR=[0,function(_){var _q0=E(_jD)[1],_q1=jsSetCB(E(_pQ[1])[1],_q0,_pS),_q2=_q1;return new T(function(){var _q3=function(_q4,_q5){var _q6=E(_q4);if(!_q6[0]){return new F(function(){return A(_q5,[_5]);});}else{return [0,function(_){var _q7=jsSetCB(E(_q6[1])[1],_q0,_pS),_q8=_q7;return new T(function(){return B(_q3(_q6[2],_q5));});}];}};return B(_q3(_pQ[2],function(_q9){return E(_pK);}));});}];}return _pR;});}],_qa=[0,function(_){var _qb=jsQuerySelectorAll(_px,toJSStr(E(_lG))),_qc=_qb;return new T(function(){var _qd=E(_qc);if(!_qd[0]){var _qe=E(_pN);}else{var _qf=_qd[1],_qe=[0,function(_){var _qg=E(_qf)[1],_qh=E(_jB)[1],_qi=jsSetCB(_qg,_qh,function(_qj,_){return E(E(_qj)[1])==13?B(_oh(_px,_,_qf,_)):_5;}),_qk=_qi;return [0,function(_){var _ql=E(_jA)[1],_qm=jsSetCB(_qg,_ql,function(_){return new F(function(){return _nS(_px,_,_qg,_,_);});}),_qn=_qm;return new T(function(){var _qo=function(_qp,_qq){var _qr=E(_qp);if(!_qr[0]){return new F(function(){return A(_qq,[_5]);});}else{var _qs=_qr[1];return [0,function(_){var _qt=E(_qs)[1],_qu=jsSetCB(_qt,_qh,function(_qv,_){return E(E(_qv)[1])==13?B(_oh(_px,_,_qs,_)):_5;}),_qw=_qu;return [0,function(_){var _qx=jsSetCB(_qt,_ql,function(_){return new F(function(){return _nS(_px,_,_qt,_,_);});}),_qy=_qx;return new T(function(){return B(_qo(_qr[2],_qq));});}];}];}};return B(_qo(_qd[2],function(_qz){return E(_pN);}));});}];}];}return _qe;});}];return [0,function(_){var _qA=jsQuerySelectorAll(_px,toJSStr(E(_lH))),_qB=_qA;return new T(function(){var _qC=E(_qB);if(!_qC[0]){var _qD=E(_qa);}else{var _qE=function(_qF,_qG,_){return new F(function(){return (function(_){return new F(function(){return _d(new T(function(){return [0,B(_iR(_my,function(_qH){var _qI=new T(function(){return B(_jE(function(_qJ){return new F(function(){return _io(_qJ,_nR);});},_qH));});return [0,B(_i6(_my,_qI,function(_qK){return E([0,function(_){var _qL=B(A(_lC,[_qI,_])),_qM=_qL;return _mA;}]);}))];}))];}),_0,_);});})(_);});},_qD=[0,function(_){var _qN=E(_jD)[1],_qO=jsSetCB(E(_qC[1])[1],_qN,_qE),_qP=_qO;return new T(function(){var _qQ=function(_qR,_qS){var _qT=E(_qR);if(!_qT[0]){return new F(function(){return A(_qS,[_5]);});}else{return [0,function(_){var _qU=jsSetCB(E(_qT[1])[1],_qN,_qE),_qV=_qU;return new T(function(){return B(_qQ(_qT[2],_qS));});}];}};return B(_qQ(_qC[2],function(_qW){return E(_qa);}));});}];}return _qD;});}];}];}]));});}];}];}];}return _p4;});}];}];}},_qX=function(_qY){return E([0,function(_){var _qZ=E(_kw),_r0=jsFind(toJSStr(_qZ)),_r1=_r0;return new T(function(){var _r2=E(_r1);if(!_r2[0]){var _r3=B(_mb(_qZ));}else{var _r3=[0,function(_){var _r4=function(_r5){var _r6=jsSet(E(_r2[1])[1],toJSStr(E(_kl)),toJSStr(_r5));return new T(function(){return B(A(_mz,[_5]));});};if(!B(_iK(_iJ,_mH,_0))){return new F(function(){return _r4(_0);});}else{return new F(function(){return _r4(E(_ka));});}}];}return _r3;});}]);};if(!B(_4U(_mF,_lE))){if(!B(_4U(_mF,_lD))){var _r7=B(_nN(_mB,_qX));}else{var _r7=B(_nN(B(_jE(_jm,_mB)),_qX));}var _r8=_r7;}else{var _r8=B(_nN(_mH,_qX));}return _r8;}));}));});}];}return _nC;});}]);}]));});}]);}]));});}]);}]);});}else{return new F(function(){return _ms(_n7,_kq);});}}));}),_r9=E(_mS);if(!_r9[0]){var _ra=E(_mT);}else{var _ra=[0,function(_){var _rb=E(_r9[1]),_rc=E(_kt),_rd=jsGetAttr(_rb[1],toJSStr(_rc)),_re=_rd;return [0,function(_){var _rf=B(A(_lS,[_rb,_ks,new T(function(){return B(_4U(_mG,fromJSStr(_re)));}),_])),_rg=_rf;return new T(function(){var _rh=function(_ri,_rj){var _rk=E(_ri);if(!_rk[0]){return new F(function(){return A(_rj,[_5]);});}else{return [0,function(_){var _rl=E(_rk[1]),_rm=jsGetAttr(_rl[1],toJSStr(_rc)),_rn=_rm;return [0,function(_){var _ro=B(A(_lS,[_rl,_ks,new T(function(){return B(_4U(_mG,fromJSStr(_rn)));}),_])),_rp=_ro;return new T(function(){return B(_rh(_rk[2],_rj));});}];}];}};return B(_rh(_r9[2],function(_rq){return E(_mT);}));});}];}];}return _ra;});}];}]);}))];});});},_rr=[0,1],_rs=function(_rt,_ru){while(1){var _rv=E(_ru);if(!_rv[0]){return E(_rt);}else{var _rw=_rv[1],_rx=!B(_8s(_rt,_rw))?E(_rt):E(_rw);_ru=_rv[2];_rt=_rx;continue;}}},_ry=function(_rz){return E(E(_rz)[1]);},_rA=new T(function(){return B(unCStr(": empty list"));}),_rB=new T(function(){return B(unCStr("Prelude."));}),_rC=function(_rD){return new F(function(){return err(B(_1(_rB,new T(function(){return B(_1(_rD,_rA));}))));});},_rE=new T(function(){return B(unCStr("maximum"));}),_rF=new T(function(){return B(_rC(_rE));}),_rG=function(_rH,_rI,_rJ){return [0,[0,new T(function(){if(!B(_j3(_rJ,0))){var _rK=E(_rr);}else{var _rL=B(_2L(_ry,_rJ));if(!_rL[0]){var _rM=E(_rF);}else{var _rM=B(_6G(B(_rs(_rL[1],_rL[2])),_rr));}var _rK=_rM;}return _rK;}),_rH,_rI],_rJ];},_rN=function(_rO){return _rO>0;},_rP=new T(function(){return B(_js("(function(x) {return x === null;})"));}),_rQ=new T(function(){return B(unCStr("No such value"));}),_rR=[0,_rQ],_rS=new T(function(){return B(unCStr("Invalid JSON!"));}),_rT=[0,_rS],_rU=new T(function(){return [0,"(function(k) {return localStorage.getItem(k);})"];}),_rV=function(_rW){return E(E(_rW)[3]);},_rX=function(_rY,_rZ,_){var _s0=B(A(_js,[E(_rU)[1],E(toJSStr(E(_rZ))),_])),_s1=_s0;return new T(function(){if(!B(_jo(function(_){var _=0,_s2=B(A(_rP,[E(_s1),_])),_s3=_s2;return new T(function(){return B(_rN(_s3));});}))){var _s4=String(_s1),_s5=_s4,_s6=jsParseJSON(_s5),_s7=_s6,_s8=E(_s7),_s9=_s8[0]==0?E(_rT):B(A(_rV,[_rY,_s8[1]]));}else{var _s9=E(_rR);}return _s9;});},_sa=new T(function(){return B(unCStr("new-todo"));}),_sb=new T(function(){return B(unCStr("clear-completed"));}),_sc=function(_sd,_){return new F(function(){return _d(new T(function(){return B(A(_sd,[_i4]));}),_0,_);});},_se=function(_sf,_sg){return new F(function(){return A(_sg,[_sc]);});},_sh=function(_si,_sj){return new F(function(){return A(_si,[_sj]);});},_sk=[0,_sh,_se],_sl=function(_sm,_sn){return new F(function(){return A(_sn,[_sm]);});},_so=function(_sp,_sq,_sr){return new F(function(){return A(_sp,[function(_ss){return new F(function(){return A(_sq,[_ss,_sr]);});}]);});},_st=function(_su,_sv,_sw){return new F(function(){return A(_su,[function(_sx){return E(new T(function(){return B(A(_sv,[_sw]));}));}]);});},_sy=function(_sz,_sA,_sB){return new F(function(){return _st(_sz,_sA,_sB);});},_sC=function(_sD){return new F(function(){return err(_sD);});},_sE=[0,_so,_sy,_sl,_sC],_sF=[0,_sE,_hT],_sG=[0,_],_sH=function(_sI){return E(E(_sI)[1]);},_sJ=function(_sK){return E(E(_sK)[1]);},_sL=new T(function(){return B(_js("(function(firsthash,cb){window.__old_hash = firsthash;window.onhashchange = function(e){var oldhash = window.__old_hash;var newhash = window.location.hash.split(\'#\')[1] || \'\';window.__old_hash = newhash;B(A(cb, [[0,oldhash],[0,newhash],0]));};})"));}),_sM=function(_sN){return function(_sO,_){var _sP=B(A(new T(function(){return B(A(_sL,[E(E(_sN)[1])]));}),[E(_sO),_])),_sQ=_sP;return _5;};},_sR=function(_sS){return E(E(_sS)[2]);},_sT=function(_sU){return E(E(_sU)[2]);},_sV=function(_sW){return E(E(_sW)[1]);},_sX=new T(function(){return B(unCStr("Prelude.undefined"));}),_sY=new T(function(){return B(err(_sX));}),_sZ=function(_t0,_t1,_t2){var _t3=new T(function(){return B(_sH(_t0));});return function(_t4){return new F(function(){return A(new T(function(){return B(_sJ(_t3));}),[new T(function(){return B(A(_sR,[_t0,_k9]));}),function(_t5){return new F(function(){return A(new T(function(){return B(_sJ(_t3));}),[new T(function(){return B(A(new T(function(){var _t6=E(_t2);return function(_t7){var _t8=E(_t3);return new F(function(){return A(_t8[1],[new T(function(){return B(A(new T(function(){return B(_sT(_t1));}),[new T(function(){return B(A(_t7,[_sY,_sY]));})]));}),function(_t9){return new F(function(){return A(_t8[3],[function(_ta){return function(_tb){return new F(function(){return A(new T(function(){return B(_sV(_t1));}),[_t9,new T(function(){return B(A(new T(function(){return B(A(_t7,[_ta]));}),[_tb]));})]);});};}]);});}]);});};}),[function(_tc,_td){return new F(function(){return A(_t4,[new T(function(){return B(_h6(_tc));}),new T(function(){return B(_h6(_td));})]);});}]));}),function(_te){return new F(function(){return A(new T(function(){return B(_sR(_t0));}),[new T(function(){return B(A(new T(function(){return B(_sM(_t5));}),[_te]));})]);});}]);});}]);});};},_tf=new T(function(){return B(_sZ(_sF,_sk,_sG));}),_tg=function(_th,_ti){var _tj=E(_ti);return [0,_tj[1],_tj[2],_th];},_tk=function(_tl){return function(_){var _tm=B(_rX(_hS,_ky,_)),_tn=_tm;return [0,function(_){var _to=nMV([0,new T(function(){var _tp=E(_tn);return _tp[0]==0?[0]:E(_tp[1]);}),_0]),_tq=_to;return new T(function(){var _tr=[0,_tq],_ts=new T(function(){return [0,B(_mx(_tr,_i4))];});return [0,B(_mx(_tr,function(_tt){return E([0,function(_){var _tu=E(_sa),_tv=jsFind(toJSStr(_tu)),_tw=_tv;return new T(function(){var _tx=E(_tw);if(!_tx[0]){var _ty=B(_mb(_tu));}else{var _tz=_tx[1],_ty=[0,function(_){var _tA=E(_tz),_tB=jsSetCB(_tA[1],E(_jB)[1],function(_tC,_){var _tD=B(_hY(E(_tz)[1],_kc,_)),_tE=_tD;return E(E(_tC)[1])==13?B(_j3(_tE,0))<=0?_5:B(_d(new T(function(){return [0,B(_iR(_tr,function(_tF){var _tG=new T(function(){var _tH=B(_rG(_tE,_80,_tF));return [1,_tH[1],_tH[2]];});return [0,B(_i6(_tr,_tG,function(_tI){return E([0,function(_){var _tJ=B(A(_lC,[_tG,_])),_tK=_tJ;return new T(function(){return B(A(_j8,[_hT,_tz,_kc,_0,function(_tL){return [0,B(_mx(_tr,_i4))];}]));});}]);}))];}))];}),_0,_)):_5;}),_tM=_tB;return [0,function(_){var _tN=B(A(_jT,[_tA,_])),_tO=_tN;return [0,function(_){var _tP=E(_kw),_tQ=jsFind(toJSStr(_tP)),_tR=_tQ;return new T(function(){var _tS=E(_tR);if(!_tS[0]){var _tT=B(_mb(_tP));}else{var _tU=_tS[1],_tT=[0,function(_){var _tV=E(_jD)[1],_tW=jsSetCB(E(_tU)[1],_tV,function(_tX,_tY,_){return new F(function(){return _d([0,function(_){var _tZ=jsGet(E(_tU)[1],toJSStr(E(_kl))),_u0=_tZ;return new T(function(){return [0,B(_iR(_tr,function(_u1){var _u2=new T(function(){return B(_2L(function(_o2){return new F(function(){return _tg(new T(function(){return B(_4U(fromJSStr(_u0),_ka));}),_o2);});},_u1));});return [0,B(_i6(_tr,_u2,function(_u3){return E([0,function(_){var _u4=B(A(_lC,[_u2,_])),_u5=_u4;return _ts;}]);}))];}))];});}],_0,_);});}),_u6=_tW;return [0,function(_){var _u7=E(_sb),_u8=jsFind(toJSStr(_u7)),_u9=_u8;return new T(function(){var _ua=E(_u9);if(!_ua[0]){var _ub=B(_mb(_u7));}else{var _ub=[0,function(_){var _uc=jsSetCB(E(_ua[1])[1],_tV,function(_ud,_ue,_){return new F(function(){return (function(_){return new F(function(){return _d(new T(function(){return [0,B(_iR(_tr,function(_uf){var _ug=new T(function(){return B(_jE(_jg,_uf));});return [0,B(_i6(_tr,_ug,function(_uh){return E([0,function(_){var _ui=B(A(_lC,[_ug,_])),_uj=_ui;return _ts;}]);}))];}))];}),_0,_);});})(_);});}),_uk=_uc;return new T(function(){return B(A(_tf,[function(_ul,_um,_un){return [0,B(_mx(_tr,_un))];},_tl]));});}];}return _ub;});}];}];}return _tT;});}];}];}];}return _ty;});}]);}))];});}];};},_uo=new T(function(){return [0,B(_tk(_i4))];}),_up=function(_){return new F(function(){return _d(_uo,_0,_);});},_uq=function(_){return new F(function(){return _up(_);});};
var hasteMain = function() {B(A(_uq, [0]));};window.onload = hasteMain;