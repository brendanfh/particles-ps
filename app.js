// Generated by psc-bundle 0.9.1
var PS = {};
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var Control_Category = PS["Control.Category"];
  var $$const = function (a) {
      return function (v) {
          return a;
      };
  };
  var apply = function (f) {
      return function (x) {
          return f(x);
      };
  };
  exports["apply"] = apply;
  exports["const"] = $$const;
})(PS["Data.Function"] = PS["Data.Function"] || {});
(function(exports) {
    "use strict";

  // module Data.Unit

  exports.unit = {};
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
    "use strict";

  // module Data.Show

  exports.showIntImpl = function (n) {
    return n.toString();
  };
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Data.Show"];     
  var Show = function (show) {
      this.show = show;
  };                                                 
  var showInt = new Show($foreign.showIntImpl);
  var show = function (dict) {
      return dict.show;
  };
  exports["Show"] = Show;
  exports["show"] = show;
  exports["showInt"] = showInt;
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Data.Unit"];
  var Data_Show = PS["Data.Show"];
  exports["unit"] = $foreign.unit;
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Data.Functor"];
  var Data_Function = PS["Data.Function"];
  var Data_Unit = PS["Data.Unit"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];        
  var Functor = function (map) {
      this.map = map;
  };
  var map = function (dict) {
      return dict.map;
  };
  var $$void = function (dictFunctor) {
      return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
  };
  exports["Functor"] = Functor;
  exports["map"] = map;
  exports["void"] = $$void;
})(PS["Data.Functor"] = PS["Data.Functor"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Control.Apply"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Function = PS["Data.Function"];
  var Control_Category = PS["Control.Category"];        
  var Apply = function (__superclass_Data$dotFunctor$dotFunctor_0, apply) {
      this["__superclass_Data.Functor.Functor_0"] = __superclass_Data$dotFunctor$dotFunctor_0;
      this.apply = apply;
  };                      
  var apply = function (dict) {
      return dict.apply;
  };
  exports["Apply"] = Apply;
  exports["apply"] = apply;
})(PS["Control.Apply"] = PS["Control.Apply"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var Control_Apply = PS["Control.Apply"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var Applicative = function (__superclass_Control$dotApply$dotApply_0, pure) {
      this["__superclass_Control.Apply.Apply_0"] = __superclass_Control$dotApply$dotApply_0;
      this.pure = pure;
  };
  var pure = function (dict) {
      return dict.pure;
  };
  var liftA1 = function (dictApplicative) {
      return function (f) {
          return function (a) {
              return Control_Apply.apply(dictApplicative["__superclass_Control.Apply.Apply_0"]())(pure(dictApplicative)(f))(a);
          };
      };
  };
  exports["Applicative"] = Applicative;
  exports["liftA1"] = liftA1;
  exports["pure"] = pure;
})(PS["Control.Applicative"] = PS["Control.Applicative"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Control.Bind"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Category = PS["Control.Category"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];        
  var Bind = function (__superclass_Control$dotApply$dotApply_0, bind) {
      this["__superclass_Control.Apply.Apply_0"] = __superclass_Control$dotApply$dotApply_0;
      this.bind = bind;
  };                     
  var bind = function (dict) {
      return dict.bind;
  };
  exports["Bind"] = Bind;
  exports["bind"] = bind;
})(PS["Control.Bind"] = PS["Control.Bind"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Data_Functor = PS["Data.Functor"];        
  var Monad = function (__superclass_Control$dotApplicative$dotApplicative_0, __superclass_Control$dotBind$dotBind_1) {
      this["__superclass_Control.Applicative.Applicative_0"] = __superclass_Control$dotApplicative$dotApplicative_0;
      this["__superclass_Control.Bind.Bind_1"] = __superclass_Control$dotBind$dotBind_1;
  };
  var ap = function (dictMonad) {
      return function (f) {
          return function (a) {
              return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(f)(function (v) {
                  return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(a)(function (v1) {
                      return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(v(v1));
                  });
              });
          };
      };
  };
  exports["Monad"] = Monad;
  exports["ap"] = ap;
})(PS["Control.Monad"] = PS["Control.Monad"] || {});
(function(exports) {
    "use strict";

  // module Control.Monad.Eff

  exports.pureE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };

  exports.foreachE = function (as) {
    return function (f) {
      return function () {
        for (var i = 0, l = as.length; i < l; i++) {
          f(as[i])();
        }
      };
    };
  };
})(PS["Control.Monad.Eff"] = PS["Control.Monad.Eff"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Control.Monad.Eff"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad = PS["Control.Monad"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var monadEff = new Control_Monad.Monad(function () {
      return applicativeEff;
  }, function () {
      return bindEff;
  });
  var bindEff = new Control_Bind.Bind(function () {
      return applyEff;
  }, $foreign.bindE);
  var applyEff = new Control_Apply.Apply(function () {
      return functorEff;
  }, Control_Monad.ap(monadEff));
  var applicativeEff = new Control_Applicative.Applicative(function () {
      return applyEff;
  }, $foreign.pureE);
  var functorEff = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEff));
  exports["functorEff"] = functorEff;
  exports["applyEff"] = applyEff;
  exports["applicativeEff"] = applicativeEff;
  exports["bindEff"] = bindEff;
  exports["monadEff"] = monadEff;
  exports["foreachE"] = $foreign.foreachE;
})(PS["Control.Monad.Eff"] = PS["Control.Monad.Eff"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Control.Monad.Eff.Random

  exports.random = Math.random;
})(PS["Control.Monad.Eff.Random"] = PS["Control.Monad.Eff.Random"] || {});
(function(exports) {
    "use strict";

  // module Data.Int

  exports.fromNumberImpl = function (just) {
    return function (nothing) {
      return function (n) {
        /* jshint bitwise: false */
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };

  exports.toNumber = function (n) {
    return n;
  };
})(PS["Data.Int"] = PS["Data.Int"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var otherwise = true;
  exports["otherwise"] = otherwise;
})(PS["Data.Boolean"] = PS["Data.Boolean"] || {});
(function(exports) {
    "use strict";

  // module Data.Bounded

  exports.topInt = 2147483647;
  exports.bottomInt = -2147483648;
})(PS["Data.Bounded"] = PS["Data.Bounded"] || {});
(function(exports) {
    "use strict";

  // module Data.Eq

  exports.refEq = function (r1) {
    return function (r2) {
      return r1 === r2;
    };
  };
})(PS["Data.Eq"] = PS["Data.Eq"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Data.Eq"];
  var Data_Unit = PS["Data.Unit"];
  var Data_Void = PS["Data.Void"];        
  var Eq = function (eq) {
      this.eq = eq;
  };                                    
  var eqNumber = new Eq($foreign.refEq);
  var eqInt = new Eq($foreign.refEq);    
  var eq = function (dict) {
      return dict.eq;
  };
  exports["Eq"] = Eq;
  exports["eq"] = eq;
  exports["eqInt"] = eqInt;
  exports["eqNumber"] = eqNumber;
})(PS["Data.Eq"] = PS["Data.Eq"] || {});
(function(exports) {
    "use strict";

  // module Data.Ord.Unsafe

  exports.unsafeCompareImpl = function (lt) {
    return function (eq) {
      return function (gt) {
        return function (x) {
          return function (y) {
            return x < y ? lt : x > y ? gt : eq;
          };
        };
      };
    };
  };
})(PS["Data.Ord.Unsafe"] = PS["Data.Ord.Unsafe"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var Data_Eq = PS["Data.Eq"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];        
  var LT = (function () {
      function LT() {

      };
      LT.value = new LT();
      return LT;
  })();
  var GT = (function () {
      function GT() {

      };
      GT.value = new GT();
      return GT;
  })();
  var EQ = (function () {
      function EQ() {

      };
      EQ.value = new EQ();
      return EQ;
  })();
  exports["LT"] = LT;
  exports["GT"] = GT;
  exports["EQ"] = EQ;
})(PS["Data.Ordering"] = PS["Data.Ordering"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Data.Ord.Unsafe"];
  var Data_Ordering = PS["Data.Ordering"];        
  var unsafeCompare = $foreign.unsafeCompareImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value);
  exports["unsafeCompare"] = unsafeCompare;
})(PS["Data.Ord.Unsafe"] = PS["Data.Ord.Unsafe"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Data.Ord"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Function = PS["Data.Function"];
  var Data_Ord_Unsafe = PS["Data.Ord.Unsafe"];
  var Data_Ordering = PS["Data.Ordering"];
  var Data_Ring = PS["Data.Ring"];
  var Data_Unit = PS["Data.Unit"];
  var Data_Void = PS["Data.Void"];
  var Data_Semiring = PS["Data.Semiring"];        
  var Ord = function (__superclass_Data$dotEq$dotEq_0, compare) {
      this["__superclass_Data.Eq.Eq_0"] = __superclass_Data$dotEq$dotEq_0;
      this.compare = compare;
  }; 
  var ordNumber = new Ord(function () {
      return Data_Eq.eqNumber;
  }, Data_Ord_Unsafe.unsafeCompare);
  var ordInt = new Ord(function () {
      return Data_Eq.eqInt;
  }, Data_Ord_Unsafe.unsafeCompare);
  var compare = function (dict) {
      return dict.compare;
  };
  var max = function (dictOrd) {
      return function (x) {
          return function (y) {
              var $27 = compare(dictOrd)(x)(y);
              if ($27 instanceof Data_Ordering.LT) {
                  return y;
              };
              if ($27 instanceof Data_Ordering.EQ) {
                  return x;
              };
              if ($27 instanceof Data_Ordering.GT) {
                  return x;
              };
              throw new Error("Failed pattern match at Data.Ord line 122, column 3 - line 125, column 12: " + [ $27.constructor.name ]);
          };
      };
  };
  var min = function (dictOrd) {
      return function (x) {
          return function (y) {
              var $28 = compare(dictOrd)(x)(y);
              if ($28 instanceof Data_Ordering.LT) {
                  return x;
              };
              if ($28 instanceof Data_Ordering.EQ) {
                  return x;
              };
              if ($28 instanceof Data_Ordering.GT) {
                  return y;
              };
              throw new Error("Failed pattern match at Data.Ord line 113, column 3 - line 116, column 12: " + [ $28.constructor.name ]);
          };
      };
  };
  var clamp = function (dictOrd) {
      return function (low) {
          return function (hi) {
              return function (x) {
                  return min(dictOrd)(hi)(max(dictOrd)(low)(x));
              };
          };
      };
  };
  exports["Ord"] = Ord;
  exports["clamp"] = clamp;
  exports["compare"] = compare;
  exports["max"] = max;
  exports["min"] = min;
  exports["ordInt"] = ordInt;
  exports["ordNumber"] = ordNumber;
})(PS["Data.Ord"] = PS["Data.Ord"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Data.Bounded"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Unit = PS["Data.Unit"];
  var Data_Ordering = PS["Data.Ordering"];        
  var Bounded = function (__superclass_Data$dotOrd$dotOrd_0, bottom, top) {
      this["__superclass_Data.Ord.Ord_0"] = __superclass_Data$dotOrd$dotOrd_0;
      this.bottom = bottom;
      this.top = top;
  };
  var top = function (dict) {
      return dict.top;
  };                                                 
  var boundedInt = new Bounded(function () {
      return Data_Ord.ordInt;
  }, $foreign.bottomInt, $foreign.topInt);
  var bottom = function (dict) {
      return dict.bottom;
  };
  exports["Bounded"] = Bounded;
  exports["bottom"] = bottom;
  exports["top"] = top;
  exports["boundedInt"] = boundedInt;
})(PS["Data.Bounded"] = PS["Data.Bounded"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Extend = PS["Control.Extend"];
  var Control_Monad = PS["Control.Monad"];
  var Control_MonadZero = PS["Control.MonadZero"];
  var Control_Plus = PS["Control.Plus"];
  var Data_Bounded = PS["Data.Bounded"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Functor_Invariant = PS["Data.Functor.Invariant"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Ordering = PS["Data.Ordering"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Data_Unit = PS["Data.Unit"];
  var Control_Category = PS["Control.Category"];        
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var fromJust = function (dictPartial) {
      return function (v) {
          var __unused = function (dictPartial1) {
              return function ($dollar29) {
                  return $dollar29;
              };
          };
          return __unused(dictPartial)((function () {
              if (v instanceof Just) {
                  return v.value0;
              };
              throw new Error("Failed pattern match at Data.Maybe line 283, column 1 - line 283, column 21: " + [ v.constructor.name ]);
          })());
      };
  };
  exports["Just"] = Just;
  exports["Nothing"] = Nothing;
  exports["fromJust"] = fromJust;
})(PS["Data.Maybe"] = PS["Data.Maybe"] || {});
(function(exports) {
    "use strict";        

  exports.floor = Math.floor;
})(PS["Math"] = PS["Math"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Math"];
  exports["floor"] = $foreign.floor;
})(PS["Math"] = PS["Math"] || {});
(function(exports) {
    "use strict";

  // module Partial.Unsafe

  exports.unsafePartial = function (f) {
    return f();
  };
})(PS["Partial.Unsafe"] = PS["Partial.Unsafe"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Partial.Unsafe"];
  var Partial = PS["Partial"];
  exports["unsafePartial"] = $foreign.unsafePartial;
})(PS["Partial.Unsafe"] = PS["Partial.Unsafe"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Data.Int"];
  var Data_Boolean = PS["Data.Boolean"];
  var Data_BooleanAlgebra = PS["Data.BooleanAlgebra"];
  var Data_Bounded = PS["Data.Bounded"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Function = PS["Data.Function"];
  var Data_Int_Bits = PS["Data.Int.Bits"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Ord = PS["Data.Ord"];
  var $$Math = PS["Math"];
  var Partial_Unsafe = PS["Partial.Unsafe"];
  var Data_HeytingAlgebra = PS["Data.HeytingAlgebra"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var fromNumber = $foreign.fromNumberImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  var unsafeClamp = function (x) {
      if (x >= $foreign.toNumber(Data_Bounded.top(Data_Bounded.boundedInt))) {
          return Data_Bounded.top(Data_Bounded.boundedInt);
      };
      if (x <= $foreign.toNumber(Data_Bounded.bottom(Data_Bounded.boundedInt))) {
          return Data_Bounded.bottom(Data_Bounded.boundedInt);
      };
      if (Data_Boolean.otherwise) {
          return Partial_Unsafe.unsafePartial(function (dictPartial) {
              return Data_Maybe.fromJust(dictPartial)(fromNumber(x));
          });
      };
      throw new Error("Failed pattern match at Data.Int line 64, column 1 - line 67, column 56: " + [ x.constructor.name ]);
  };
  var floor = function ($4) {
      return unsafeClamp($$Math.floor($4));
  };
  exports["floor"] = floor;
  exports["fromNumber"] = fromNumber;
  exports["toNumber"] = $foreign.toNumber;
})(PS["Data.Int"] = PS["Data.Int"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Control.Monad.Eff.Random"];
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Int = PS["Data.Int"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Applicative = PS["Control.Applicative"];
  var Data_Semiring = PS["Data.Semiring"];
  var Data_Ring = PS["Data.Ring"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Ord = PS["Data.Ord"];
  var randomInt = function (low) {
      return function (high) {
          return function __do() {
              var v = $foreign.random();
              var asNumber = ((Data_Int.toNumber(high) - Data_Int.toNumber(low)) + 1) * v + Data_Int.toNumber(low);
              return Data_Function.apply(Control_Applicative.pure(Control_Monad_Eff.applicativeEff))(Data_Int.floor(asNumber))();
          };
      };
  };
  exports["randomInt"] = randomInt;
  exports["random"] = $foreign.random;
})(PS["Control.Monad.Eff.Random"] = PS["Control.Monad.Eff.Random"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Control.Monad.Eff.Ref

  exports.newRef = function (val) {
    return function () {
      return { value: val };
    };
  };

  exports.readRef = function (ref) {
    return function () {
      return ref.value;
    };
  };

  exports.writeRef = function (ref) {
    return function (val) {
      return function () {
        ref.value = val;
        return {};
      };
    };
  };
})(PS["Control.Monad.Eff.Ref"] = PS["Control.Monad.Eff.Ref"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Control.Monad.Eff.Ref"];
  var Prelude = PS["Prelude"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Unit = PS["Data.Unit"];
  exports["newRef"] = $foreign.newRef;
  exports["readRef"] = $foreign.readRef;
  exports["writeRef"] = $foreign.writeRef;
})(PS["Control.Monad.Eff.Ref"] = PS["Control.Monad.Eff.Ref"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  // module Data.Array

  //------------------------------------------------------------------------------
  // Array creation --------------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.range = function (start) {
    return function (end) {
      var step = start > end ? -1 : 1;
      var result = [];
      for (var i = start, n = 0; i !== end; i += step) {
        result[n++] = i;
      }
      result[n] = i;
      return result;
    };
  };
})(PS["Data.Array"] = PS["Data.Array"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Data.Array"];
  var Prelude = PS["Prelude"];
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Lazy = PS["Control.Lazy"];
  var Data_Foldable = PS["Data.Foldable"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Traversable = PS["Data.Traversable"];
  var Data_Tuple = PS["Data.Tuple"];
  var Data_Unfoldable = PS["Data.Unfoldable"];
  var Partial_Unsafe = PS["Partial.Unsafe"];
  var Data_Function = PS["Data.Function"];
  var Data_Ordering = PS["Data.Ordering"];
  var Data_Ring = PS["Data.Ring"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Eq = PS["Data.Eq"];
  var Data_HeytingAlgebra = PS["Data.HeytingAlgebra"];
  var Control_Apply = PS["Control.Apply"];
  var Data_Functor = PS["Data.Functor"];
  var Control_Applicative = PS["Control.Applicative"];
  var Data_Boolean = PS["Data.Boolean"];
  var Data_Semiring = PS["Data.Semiring"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Control_Bind = PS["Control.Bind"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Control_Category = PS["Control.Category"];
  exports["range"] = $foreign.range;
})(PS["Data.Array"] = PS["Data.Array"] || {});
(function(exports) {
    "use strict";

  exports.runFn3 = function (fn) {
    return function (a) {
      return function (b) {
        return function (c) {
          return fn(a, b, c);
        };
      };
    };
  };
})(PS["Data.Function.Uncurried"] = PS["Data.Function.Uncurried"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Data.Function.Uncurried"];
  var Data_Unit = PS["Data.Unit"];
  exports["runFn3"] = $foreign.runFn3;
})(PS["Data.Function.Uncurried"] = PS["Data.Function.Uncurried"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  exports.getCanvasElementByIdImpl = function(id, Just, Nothing) {
      return function() {
          var el = document.getElementById(id);
          if (el && el instanceof HTMLCanvasElement) {
              return Just(el);
          } else {
              return Nothing;
          }
      };
  };

  exports.getContext2D = function(c) {
      return function() {
          return c.getContext('2d');
      };
  };

  exports.setCanvasWidth = function(width) {
      return function(canvas) {
          return function() {
              canvas.width = width;
              return canvas;
          };
      };
  };

  exports.setCanvasHeight = function(height) {
      return function(canvas) {
          return function() {
              canvas.height = height;
              return canvas;
          };
      };
  };

  exports.setFillStyle = function(style) {
      return function(ctx) {
          return function() {
              ctx.fillStyle = style;
              return ctx;
          };
      };
  };

  exports.fillRect = function(ctx) {
      return function(r) {
          return function() {
              ctx.fillRect(r.x, r.y, r.w, r.h);
              return ctx;
          };
      };
  };
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Graphics.Canvas"];
  var Prelude = PS["Prelude"];
  var Data_ArrayBuffer_Types = PS["Data.ArrayBuffer.Types"];
  var Data_Function_Uncurried = PS["Data.Function.Uncurried"];
  var Data_Maybe = PS["Data.Maybe"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Exception_Unsafe = PS["Control.Monad.Eff.Exception.Unsafe"];
  var Data_Show = PS["Data.Show"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Applicative = PS["Control.Applicative"];
  var Data_Function = PS["Data.Function"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Functor = PS["Data.Functor"];
  var setCanvasDimensions = function (d) {
      return function (ce) {
          return Control_Bind.bind(Control_Monad_Eff.bindEff)($foreign.setCanvasHeight(d.height)(ce))($foreign.setCanvasWidth(d.width));
      };
  };
  var getCanvasElementById = function (elId) {
      return Data_Function_Uncurried.runFn3($foreign.getCanvasElementByIdImpl)(elId)(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  };
  exports["getCanvasElementById"] = getCanvasElementById;
  exports["setCanvasDimensions"] = setCanvasDimensions;
  exports["fillRect"] = $foreign.fillRect;
  exports["getContext2D"] = $foreign.getContext2D;
  exports["setFillStyle"] = $foreign.setFillStyle;
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function(exports) {
    "use strict";

  //module Main

  exports.requestAnimationFrame = function(f) {
      return function() {
          return window.requestAnimationFrame(function(dt) {
              return f();
          });
      };
  };

  exports.mapE = function(arr) {
      return function(f) {
          return function() {
              var result = [];
              for(var i=0, l=arr.length; i<l; i++) {
                  result.push(f(arr[i])());
              }
              return result;
          }
      }
  }
})(PS["Main"] = PS["Main"] || {});
(function(exports) {
  // Generated by psc version 0.9.1
  "use strict";
  var $foreign = PS["Main"];
  var Prelude = PS["Prelude"];
  var Partial_Unsafe = PS["Partial.Unsafe"];
  var Data_Array = PS["Data.Array"];
  var Data_Maybe = PS["Data.Maybe"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Console = PS["Control.Monad.Eff.Console"];
  var Control_Monad_Eff_Ref = PS["Control.Monad.Eff.Ref"];
  var Control_Monad_Eff_Random = PS["Control.Monad.Eff.Random"];
  var Graphics_Canvas = PS["Graphics.Canvas"];
  var Control_Bind = PS["Control.Bind"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Control_Applicative = PS["Control.Applicative"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Semiring = PS["Data.Semiring"];
  var Data_Ring = PS["Data.Ring"];
  var Data_Boolean = PS["Data.Boolean"];        
  var initialParticles = $foreign.mapE(Data_Array.range(0)(500))(function (v) {
      return function __do() {
          var v1 = Control_Monad_Eff_Random.randomInt(0)(255)();
          var c = "hsla(" + (Data_Show.show(Data_Show.showInt)(v1) + ", 100%, 50%, 0.5)");
          return {
              x: 400.0, 
              y: 300.0, 
              vx: 0.0, 
              vy: 0.0, 
              c: c
          };
      };
  });
  var main = Data_Function.apply(Data_Functor["void"](Control_Monad_Eff.functorEff))(Data_Function.apply(Partial_Unsafe.unsafePartial)(function (dictPartial) {
      return function __do() {
          var v = Graphics_Canvas.getCanvasElementById("testcanvas")();
          var __unused = function (dictPartial1) {
              return function ($dollar15) {
                  return $dollar15;
              };
          };
          return __unused(dictPartial)((function () {
              if (v instanceof Data_Maybe.Just) {
                  return function __do() {
                      Graphics_Canvas.setCanvasDimensions({
                          width: 800.0, 
                          height: 600.0
                      })(v.value0)();
                      var v1 = Graphics_Canvas.getContext2D(v.value0)();
                      var v2 = initialParticles();
                      var v3 = Control_Monad_Eff_Ref.newRef(v2)();
                      var update = function (particles) {
                          var updateParticle = function (v4) {
                              return function (p) {
                                  var __unused = function (dictPartial1) {
                                      return function ($dollar13) {
                                          return $dollar13;
                                      };
                                  };
                                  return __unused(dictPartial)((function () {
                                      if (v4.length === 3) {
                                          var wrapEdges = function (v5) {
                                              return function (particle) {
                                                  var wrap = function (__copy_l) {
                                                      return function (__copy_h1) {
                                                          return function (__copy_v6) {
                                                              var l = __copy_l;
                                                              var h1 = __copy_h1;
                                                              var v6 = __copy_v6;
                                                              tco: while (true) {
                                                                  if (v6 < l) {
                                                                      var __tco_l = l;
                                                                      var __tco_h1 = h1;
                                                                      var __tco_v6 = v6 + h1;
                                                                      l = __tco_l;
                                                                      h1 = __tco_h1;
                                                                      v6 = __tco_v6;
                                                                      continue tco;
                                                                  };
                                                                  if (v6 > h1) {
                                                                      var __tco_l = l;
                                                                      var __tco_h1 = h1;
                                                                      var __tco_v6 = v6 - h1;
                                                                      l = __tco_l;
                                                                      h1 = __tco_h1;
                                                                      v6 = __tco_v6;
                                                                      continue tco;
                                                                  };
                                                                  if (Data_Boolean.otherwise) {
                                                                      return v6;
                                                                  };
                                                                  throw new Error("Failed pattern match at Main line 68, column 29 - line 71, column 48: " + [ l.constructor.name, h1.constructor.name, v6.constructor.name ]);
                                                              };
                                                          };
                                                      };
                                                  };
                                                  var $32 = {};
                                                  for (var $33 in particle) {
                                                      if (particle.hasOwnProperty($33)) {
                                                          $32[$33] = particle[$33];
                                                      };
                                                  };
                                                  $32.x = wrap(0.0)(v5.w)(particle.x);
                                                  $32.y = wrap(0.0)(v5.h)(particle.y);
                                                  return $32;
                                              };
                                          };
                                          var p2 = (function () {
                                              var $36 = v4[0] > 0.97;
                                              if ($36) {
                                                  var $37 = {};
                                                  for (var $38 in p) {
                                                      if (p.hasOwnProperty($38)) {
                                                          $37[$38] = p[$38];
                                                      };
                                                  };
                                                  $37.x = p.x + p.vx;
                                                  $37.y = p.y + p.vy;
                                                  $37.vx = Data_Ord.clamp(Data_Ord.ordNumber)(-5.0)(5.0)((p.vx + v4[1] * 1.0) - 0.5);
                                                  $37.vy = Data_Ord.clamp(Data_Ord.ordNumber)(-5.0)(5.0)((p.vy + v4[2] * 1.0) - 0.5);
                                                  return $37;
                                              };
                                              if (!$36) {
                                                  var $39 = {};
                                                  for (var $40 in p) {
                                                      if (p.hasOwnProperty($40)) {
                                                          $39[$40] = p[$40];
                                                      };
                                                  };
                                                  $39.x = p.x + p.vx;
                                                  $39.y = p.y + p.vy;
                                                  return $39;
                                              };
                                              throw new Error("Failed pattern match at Main line 55, column 26 - line 61, column 51: " + [ $36.constructor.name ]);
                                          })();
                                          return wrapEdges({
                                              w: 800.0, 
                                              h: 600.0
                                          })(p2);
                                      };
                                      throw new Error("Failed pattern match at Main line 54, column 13 - line 73, column 57: " + [ v4.constructor.name, p.constructor.name ]);
                                  })());
                              };
                          };
                          return $foreign.mapE(particles)(function (p) {
                              return function __do() {
                                  var v4 = Control_Monad_Eff_Random.random();
                                  var v5 = Control_Monad_Eff_Random.random();
                                  var v6 = Control_Monad_Eff_Random.random();
                                  return updateParticle([ v4, v5, v6 ])(p);
                              };
                          });
                      };
                      var drawParticles = function (particles) {
                          return Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_Eff.foreachE(particles)(function (p) {
                              return Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
                                  Graphics_Canvas.setFillStyle(p.c)(v1)();
                                  return Graphics_Canvas.fillRect(v1)({
                                      x: p.x, 
                                      y: p.y, 
                                      w: 10.0, 
                                      h: 10.0
                                  })();
                              });
                          }));
                      };
                      var clearCanvas = Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
                          Graphics_Canvas.setFillStyle("black")(v1)();
                          return Graphics_Canvas.fillRect(v1)({
                              x: 0.0, 
                              y: 0.0, 
                              w: 800.0, 
                              h: 600.0
                          })();
                      });
                      var loop = Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
                          var v4 = Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Monad_Eff_Ref.readRef(v3))(update)();
                          clearCanvas();
                          drawParticles(v4)();
                          Control_Monad_Eff_Ref.writeRef(v3)(v4)();
                          return $foreign.requestAnimationFrame(loop)();
                      });
                      return loop();
                  };
              };
              throw new Error("Failed pattern match at Main line 36, column 8 - line 98, column 5: " + [ v.constructor.name ]);
          })())();
      };
  }));
  exports["initialParticles"] = initialParticles;
  exports["main"] = main;
  exports["mapE"] = $foreign.mapE;
  exports["requestAnimationFrame"] = $foreign.requestAnimationFrame;
})(PS["Main"] = PS["Main"] || {});
PS["Main"].main();