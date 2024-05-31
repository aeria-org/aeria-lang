// Generated by purs version 0.15.15
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Plus from "../Control.Plus/index.js";
import * as Data_Distributive from "../Data.Distributive/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Functor_Invariant from "../Data.Functor.Invariant/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var Star = function (x) {
    return x;
};
var semigroupoidStar = function (dictBind) {
    var bind = Control_Bind.bind(dictBind);
    return {
        compose: function (v) {
            return function (v1) {
                return function (x) {
                    return bind(v1(x))(v);
                };
            };
        }
    };
};
var profunctorStar = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return {
        dimap: function (f) {
            return function (g) {
                return function (v) {
                    var $127 = map(g);
                    return function ($128) {
                        return $127(v(f($128)));
                    };
                };
            };
        }
    };
};
var strongStar = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    var profunctorStar1 = profunctorStar(dictFunctor);
    return {
        first: function (v) {
            return function (v1) {
                return map(function (v2) {
                    return new Data_Tuple.Tuple(v2, v1.value1);
                })(v(v1.value0));
            };
        },
        second: function (v) {
            return function (v1) {
                return map(Data_Tuple.Tuple.create(v1.value0))(v(v1.value1));
            };
        },
        Profunctor0: function () {
            return profunctorStar1;
        }
    };
};
var newtypeStar = {
    Coercible0: function () {
        return undefined;
    }
};
var invariantStar = function (dictInvariant) {
    var imap = Data_Functor_Invariant.imap(dictInvariant);
    return {
        imap: function (f) {
            return function (g) {
                return function (v) {
                    var $129 = imap(f)(g);
                    return function ($130) {
                        return $129(v($130));
                    };
                };
            };
        }
    };
};
var hoistStar = function (f) {
    return function (v) {
        return function ($131) {
            return f(v($131));
        };
    };
};
var functorStar = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return {
        map: function (f) {
            return function (v) {
                var $132 = map(f);
                return function ($133) {
                    return $132(v($133));
                };
            };
        }
    };
};
var distributiveStar = function (dictDistributive) {
    var collect = Data_Distributive.collect(dictDistributive);
    var functorStar1 = functorStar(dictDistributive.Functor0());
    return {
        distribute: function (dictFunctor) {
            var collect1 = collect(dictFunctor);
            return function (f) {
                return function (a) {
                    return collect1(function (v) {
                        return v(a);
                    })(f);
                };
            };
        },
        collect: function (dictFunctor) {
            var map = Data_Functor.map(dictFunctor);
            return function (f) {
                var $134 = Data_Distributive.distribute(distributiveStar(dictDistributive))(dictFunctor);
                var $135 = map(f);
                return function ($136) {
                    return $134($135($136));
                };
            };
        },
        Functor0: function () {
            return functorStar1;
        }
    };
};
var closedStar = function (dictDistributive) {
    var distribute = Data_Distributive.distribute(dictDistributive)(Data_Functor.functorFn);
    var profunctorStar1 = profunctorStar(dictDistributive.Functor0());
    return {
        closed: function (v) {
            return function (g) {
                return distribute(function ($137) {
                    return v(g($137));
                });
            };
        },
        Profunctor0: function () {
            return profunctorStar1;
        }
    };
};
var choiceStar = function (dictApplicative) {
    var Functor0 = (dictApplicative.Apply0()).Functor0();
    var map = Data_Functor.map(Functor0);
    var pure = Control_Applicative.pure(dictApplicative);
    var profunctorStar1 = profunctorStar(Functor0);
    return {
        left: function (v) {
            return Data_Either.either((function () {
                var $138 = map(Data_Either.Left.create);
                return function ($139) {
                    return $138(v($139));
                };
            })())(function ($140) {
                return pure(Data_Either.Right.create($140));
            });
        },
        right: function (v) {
            return Data_Either.either(function ($141) {
                return pure(Data_Either.Left.create($141));
            })((function () {
                var $142 = map(Data_Either.Right.create);
                return function ($143) {
                    return $142(v($143));
                };
            })());
        },
        Profunctor0: function () {
            return profunctorStar1;
        }
    };
};
var categoryStar = function (dictMonad) {
    var semigroupoidStar1 = semigroupoidStar(dictMonad.Bind1());
    return {
        identity: Control_Applicative.pure(dictMonad.Applicative0()),
        Semigroupoid0: function () {
            return semigroupoidStar1;
        }
    };
};
var applyStar = function (dictApply) {
    var apply = Control_Apply.apply(dictApply);
    var functorStar1 = functorStar(dictApply.Functor0());
    return {
        apply: function (v) {
            return function (v1) {
                return function (a) {
                    return apply(v(a))(v1(a));
                };
            };
        },
        Functor0: function () {
            return functorStar1;
        }
    };
};
var bindStar = function (dictBind) {
    var bind = Control_Bind.bind(dictBind);
    var applyStar1 = applyStar(dictBind.Apply0());
    return {
        bind: function (v) {
            return function (f) {
                return function (x) {
                    return bind(v(x))(function (a) {
                        var v1 = f(a);
                        return v1(x);
                    });
                };
            };
        },
        Apply0: function () {
            return applyStar1;
        }
    };
};
var applicativeStar = function (dictApplicative) {
    var pure = Control_Applicative.pure(dictApplicative);
    var applyStar1 = applyStar(dictApplicative.Apply0());
    return {
        pure: function (a) {
            return function (v) {
                return pure(a);
            };
        },
        Apply0: function () {
            return applyStar1;
        }
    };
};
var monadStar = function (dictMonad) {
    var applicativeStar1 = applicativeStar(dictMonad.Applicative0());
    var bindStar1 = bindStar(dictMonad.Bind1());
    return {
        Applicative0: function () {
            return applicativeStar1;
        },
        Bind1: function () {
            return bindStar1;
        }
    };
};
var altStar = function (dictAlt) {
    var alt = Control_Alt.alt(dictAlt);
    var functorStar1 = functorStar(dictAlt.Functor0());
    return {
        alt: function (v) {
            return function (v1) {
                return function (a) {
                    return alt(v(a))(v1(a));
                };
            };
        },
        Functor0: function () {
            return functorStar1;
        }
    };
};
var plusStar = function (dictPlus) {
    var empty = Control_Plus.empty(dictPlus);
    var altStar1 = altStar(dictPlus.Alt0());
    return {
        empty: function (v) {
            return empty;
        },
        Alt0: function () {
            return altStar1;
        }
    };
};
var alternativeStar = function (dictAlternative) {
    var applicativeStar1 = applicativeStar(dictAlternative.Applicative0());
    var plusStar1 = plusStar(dictAlternative.Plus1());
    return {
        Applicative0: function () {
            return applicativeStar1;
        },
        Plus1: function () {
            return plusStar1;
        }
    };
};
var monadPlusStar = function (dictMonadPlus) {
    var monadStar1 = monadStar(dictMonadPlus.Monad0());
    var alternativeStar1 = alternativeStar(dictMonadPlus.Alternative1());
    return {
        Monad0: function () {
            return monadStar1;
        },
        Alternative1: function () {
            return alternativeStar1;
        }
    };
};
export {
    Star,
    hoistStar,
    newtypeStar,
    semigroupoidStar,
    categoryStar,
    functorStar,
    invariantStar,
    applyStar,
    applicativeStar,
    bindStar,
    monadStar,
    altStar,
    plusStar,
    alternativeStar,
    monadPlusStar,
    distributiveStar,
    profunctorStar,
    strongStar,
    choiceStar,
    closedStar
};
