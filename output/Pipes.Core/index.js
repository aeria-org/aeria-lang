// Generated by purs version 0.15.15
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Pipes_Internal from "../Pipes.Internal/index.js";
var runEffectRec = function (dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map = Data_Functor.map(((Monad0.Bind1()).Apply0()).Functor0());
    var pure = Control_Applicative.pure(Monad0.Applicative0());
    var go = function (v) {
        if (v instanceof Pipes_Internal.Request) {
            return map(Control_Monad_Rec_Class.Done.create)(Pipes_Internal.closed(v.value0));
        };
        if (v instanceof Pipes_Internal.Respond) {
            return map(Control_Monad_Rec_Class.Done.create)(Pipes_Internal.closed(v.value0));
        };
        if (v instanceof Pipes_Internal.Pure) {
            return pure(new Control_Monad_Rec_Class.Done(v.value0));
        };
        if (v instanceof Pipes_Internal.M) {
            return map(Control_Monad_Rec_Class.Loop.create)(v.value0);
        };
        throw new Error("Failed pattern match at Pipes.Core (line 104, column 3 - line 104, column 39): " + [ v.constructor.name ]);
    };
    return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go);
};
var runEffect = function (dictMonad) {
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var pure = Control_Applicative.pure(dictMonad.Applicative0());
    var go = function (p) {
        if (p instanceof Pipes_Internal.Request) {
            return Pipes_Internal.closed(p.value0);
        };
        if (p instanceof Pipes_Internal.Respond) {
            return Pipes_Internal.closed(p.value0);
        };
        if (p instanceof Pipes_Internal.M) {
            return bind(p.value0)(go);
        };
        if (p instanceof Pipes_Internal.Pure) {
            return pure(p.value0);
        };
        throw new Error("Failed pattern match at Pipes.Core (line 95, column 12 - line 99, column 30): " + [ p.constructor.name ]);
    };
    return go;
};
var respond = function (dictMonad) {
    return function (a) {
        return new Pipes_Internal.Respond(a, Pipes_Internal.Pure.create);
    };
};
var request = function (dictMonad) {
    return function (a$prime) {
        return new Pipes_Internal.Request(a$prime, Pipes_Internal.Pure.create);
    };
};
var reflect = function (dictMonad) {
    var map = Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0());
    var go = function (p) {
        if (p instanceof Pipes_Internal.Request) {
            return new Pipes_Internal.Respond(p.value0, function ($124) {
                return go(p.value1($124));
            });
        };
        if (p instanceof Pipes_Internal.Respond) {
            return new Pipes_Internal.Request(p.value0, function ($125) {
                return go(p.value1($125));
            });
        };
        if (p instanceof Pipes_Internal.M) {
            return new Pipes_Internal.M(map(go)(p.value0));
        };
        if (p instanceof Pipes_Internal.Pure) {
            return new Pipes_Internal.Pure(p.value0);
        };
        throw new Error("Failed pattern match at Pipes.Core (line 234, column 12 - line 238, column 33): " + [ p.constructor.name ]);
    };
    return go;
};
var push = function (dictMonad) {
    var go = function (a) {
        return new Pipes_Internal.Respond(a, function (a$prime) {
            return new Pipes_Internal.Request(a$prime, go);
        });
    };
    return go;
};
var pull = function (dictMonad) {
    var go = function (a$prime) {
        return new Pipes_Internal.Request(a$prime, function (a) {
            return new Pipes_Internal.Respond(a, go);
        });
    };
    return go;
};
var composeResponse = function (dictMonad) {
    var bind = Control_Bind.bind(Pipes_Internal.bindProxy(dictMonad));
    var map = Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0());
    return function (p0) {
        return function (fb) {
            var go = function (p) {
                if (p instanceof Pipes_Internal.Request) {
                    return new Pipes_Internal.Request(p.value0, function ($126) {
                        return go(p.value1($126));
                    });
                };
                if (p instanceof Pipes_Internal.Respond) {
                    return bind(fb(p.value0))(function ($127) {
                        return go(p.value1($127));
                    });
                };
                if (p instanceof Pipes_Internal.M) {
                    return new Pipes_Internal.M(map(go)(p.value0));
                };
                if (p instanceof Pipes_Internal.Pure) {
                    return new Pipes_Internal.Pure(p.value0);
                };
                throw new Error("Failed pattern match at Pipes.Core (line 137, column 12 - line 141, column 33): " + [ p.constructor.name ]);
            };
            return go(p0);
        };
    };
};
var composeResponse$prime = function (dictMonad) {
    var composeResponse1 = composeResponse(dictMonad);
    return function (fa) {
        return function (fb) {
            return function (a) {
                return composeResponse1(fa(a))(fb);
            };
        };
    };
};
var flippedComposeResponse$prime = function (dictMonad) {
    var composeResponse$prime1 = composeResponse$prime(dictMonad);
    return function (p1) {
        return function (p2) {
            return composeResponse$prime1(p2)(p1);
        };
    };
};
var flippedComposeResponse = function (dictMonad) {
    var composeResponse1 = composeResponse(dictMonad);
    return function (f) {
        return function (p) {
            return composeResponse1(p)(f);
        };
    };
};
var composeRequest = function (dictMonad) {
    var bind = Control_Bind.bind(Pipes_Internal.bindProxy(dictMonad));
    var map = Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0());
    return function (fb$prime) {
        return function (p0) {
            var go = function (p) {
                if (p instanceof Pipes_Internal.Request) {
                    return bind(fb$prime(p.value0))(function ($128) {
                        return go(p.value1($128));
                    });
                };
                if (p instanceof Pipes_Internal.Respond) {
                    return new Pipes_Internal.Respond(p.value0, function ($129) {
                        return go(p.value1($129));
                    });
                };
                if (p instanceof Pipes_Internal.M) {
                    return new Pipes_Internal.M(map(go)(p.value0));
                };
                if (p instanceof Pipes_Internal.Pure) {
                    return new Pipes_Internal.Pure(p.value0);
                };
                throw new Error("Failed pattern match at Pipes.Core (line 163, column 12 - line 167, column 33): " + [ p.constructor.name ]);
            };
            return go(p0);
        };
    };
};
var composeRequest$prime = function (dictMonad) {
    var composeRequest1 = composeRequest(dictMonad);
    return function (fb$prime) {
        return function (fc$prime) {
            return function (c$prime) {
                return composeRequest1(fb$prime)(fc$prime(c$prime));
            };
        };
    };
};
var flippedComposeRequest$prime = function (dictMonad) {
    var composeRequest$prime1 = composeRequest$prime(dictMonad);
    return function (p1) {
        return function (p2) {
            return composeRequest$prime1(p2)(p1);
        };
    };
};
var flippedComposeRequest = function (dictMonad) {
    var composeRequest1 = composeRequest(dictMonad);
    return function (p) {
        return function (f) {
            return composeRequest1(f)(p);
        };
    };
};
var composePush$prime = function (dictMonad) {
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var pure = Control_Applicative.pure(dictMonad.Applicative0());
    return function (p) {
        return function (fb) {
            if (p instanceof Pipes_Internal.Request) {
                return new Pipes_Internal.Request(p.value0, function (a) {
                    return composePush$prime(dictMonad)(p.value1(a))(fb);
                });
            };
            if (p instanceof Pipes_Internal.Respond) {
                return composePull$prime(dictMonad)(p.value1)(fb(p.value0));
            };
            if (p instanceof Pipes_Internal.M) {
                return new Pipes_Internal.M(bind(p.value0)(function (p$prime) {
                    return pure(composePush$prime(dictMonad)(p$prime)(fb));
                }));
            };
            if (p instanceof Pipes_Internal.Pure) {
                return new Pipes_Internal.Pure(p.value0);
            };
            throw new Error("Failed pattern match at Pipes.Core (line 222, column 21 - line 226, column 29): " + [ p.constructor.name ]);
        };
    };
};
var composePull$prime = function (dictMonad) {
    var map = Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0());
    return function (fb$prime) {
        return function (p) {
            if (p instanceof Pipes_Internal.Request) {
                return composePush$prime(dictMonad)(fb$prime(p.value0))(p.value1);
            };
            if (p instanceof Pipes_Internal.Respond) {
                return new Pipes_Internal.Respond(p.value0, function ($130) {
                    return (function (v) {
                        return composePull$prime(dictMonad)(fb$prime)(v);
                    })(p.value1($130));
                });
            };
            if (p instanceof Pipes_Internal.M) {
                return new Pipes_Internal.M(map(function (v) {
                    return composePull$prime(dictMonad)(fb$prime)(v);
                })(p.value0));
            };
            if (p instanceof Pipes_Internal.Pure) {
                return new Pipes_Internal.Pure(p.value0);
            };
            throw new Error("Failed pattern match at Pipes.Core (line 197, column 22 - line 201, column 29): " + [ p.constructor.name ]);
        };
    };
};
var composePush = function (dictMonad) {
    var composePush$prime1 = composePush$prime(dictMonad);
    return function (fa) {
        return function (fb) {
            return function (a) {
                return composePush$prime1(fa(a))(fb);
            };
        };
    };
};
var flippedComposePush = function (dictMonad) {
    var composePush1 = composePush(dictMonad);
    return function (p1) {
        return function (p2) {
            return composePush1(p2)(p1);
        };
    };
};
var flippedComposePush$prime = function (dictMonad) {
    var composePush$prime1 = composePush$prime(dictMonad);
    return function (k) {
        return function (p) {
            return composePush$prime1(p)(k);
        };
    };
};
var flippedComposePull$prime = function (dictMonad) {
    var composePull$prime1 = composePull$prime(dictMonad);
    return function (k) {
        return function (p) {
            return composePull$prime1(p)(k);
        };
    };
};
var composePull = function (dictMonad) {
    var composePull$prime1 = composePull$prime(dictMonad);
    return function (fb$prime) {
        return function (fc$prime) {
            return function (c$prime) {
                return composePull$prime1(fb$prime)(fc$prime(c$prime));
            };
        };
    };
};
var flippedComposePull = function (dictMonad) {
    var composePull1 = composePull(dictMonad);
    return function (p1) {
        return function (p2) {
            return composePull1(p2)(p1);
        };
    };
};
export {
    runEffect,
    runEffectRec,
    respond,
    composeResponse$prime,
    composeResponse,
    request,
    composeRequest$prime,
    composeRequest,
    push,
    composePush,
    composePush$prime,
    pull,
    composePull,
    composePull$prime,
    reflect,
    flippedComposeResponse$prime,
    flippedComposeRequest$prime,
    flippedComposePush,
    flippedComposePush$prime,
    flippedComposePull,
    flippedComposePull$prime,
    flippedComposeResponse,
    flippedComposeRequest
};
export {
    closed
} from "../Pipes.Internal/index.js";
