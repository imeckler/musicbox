Elm.Native.Smooth = {};
Elm.Native.Smooth.make = function(elm) {
   elm.Native = elm.Native || {};
   elm.Native.Smooth = elm.Native.Smooth || {};
   if (elm.Native.Smooth.values) {
     return elm.Native.Smooth.values;
   }

	function mapListToArray(f, xs) {
		var out = [];
		while (xs.ctor !== '[]') {
			out.push(f(xs._0));
			xs = xs._1;
		}
		return out;
	}

  function tupleToArray(tup) {
    var res = [];
    var i = 0;
    var x;
    while (true) {
      x = tup['_' + i];
      if (x === undefined) { return res; }
      else {
        res.push(x);
        ++i;
      }
    }
  }

  function arrayToTuple(arr) {
    var res = { ctor : '_Tuple' + arr.length };
    for (var i = 0; i < arr.length; ++i) {
      res['_' + i] = arr[i];
    }
    return res;
  }

  function smoothCubic(clip, xs) {
     var tension = Smooth.CUBIC_TENSION_DEFAULT;
     return Smooth(mapListToArray(function(x){return x;}, xs), {
       method: 'cubic',
       cubicTension: tension,
       clip: clip
     });
   }

  function smoothCubicTup(clip, xs) {
     var tension = Smooth.CUBIC_TENSION_DEFAULT;
     var f = Smooth(mapListToArray(tupleToArray, xs), {
       method: 'cubic',
       cubicTension: tension,
       clip: clip
     });
     return function(t){ return arrayToTuple(f(t)); };
   }

   return Elm.Native.Smooth.values = {
     smoothCubic    : F2(smoothCubic),
     smoothCubicTup : F2(smoothCubicTup)
   };
};
