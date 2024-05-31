// Generated by purs version 0.15.15
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Control_Monad_ST_Class from "../Control.Monad.ST.Class/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Array_ST from "../Data.Array.ST/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect_Aff from "../Effect.Aff/index.js";
import * as Effect_Aff_Class from "../Effect.Aff.Class/index.js";
import * as Effect_Class from "../Effect.Class/index.js";
import * as Effect_Exception from "../Effect.Exception/index.js";
import * as Effect_Ref from "../Effect.Ref/index.js";
import * as Node_Buffer from "../Node.Buffer/index.js";
import * as Node_Encoding from "../Node.Encoding/index.js";
import * as Node_EventEmitter from "../Node.EventEmitter/index.js";
import * as Node_Stream from "../Node.Stream/index.js";
var $runtime_lazy = function (name, moduleName, init) {
    var state = 0;
    var val;
    return function (lineNumber) {
        if (state === 2) return val;
        if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
        state = 1;
        val = init();
        state = 2;
        return val;
    };
};
var pure = /* #__PURE__ */ Control_Applicative.pure(Effect.applicativeEffect);
var tailRecM = /* #__PURE__ */ Control_Monad_Rec_Class.tailRecM(Control_Monad_Rec_Class.monadRecEffect);
var join = /* #__PURE__ */ Control_Bind.join(Effect.bindEffect);
var bindFlipped = /* #__PURE__ */ Control_Bind.bindFlipped(Effect.bindEffect);
var liftST = /* #__PURE__ */ Control_Monad_ST_Class.liftST(Control_Monad_ST_Class.monadSTEffect);
var mempty = /* #__PURE__ */ Data_Monoid.mempty(/* #__PURE__ */ Effect.monoidEffect(Data_Monoid.monoidUnit));
var $$void = /* #__PURE__ */ Data_Functor["void"](Effect.functorEffect);
var applySecond = /* #__PURE__ */ Control_Apply.applySecond(Effect.applyEffect);
var map = /* #__PURE__ */ Data_Functor.map(Effect.functorEffect);
var pure1 = /* #__PURE__ */ Control_Applicative.pure(Control_Applicative.applicativeArray);
var write = function (dictMonadAff) {
    var liftAff = Effect_Aff_Class.liftAff(dictMonadAff);
    return function (w) {
        return function (bs) {
            return liftAff(Effect_Aff.makeAff(function (complete) {
                return function __do() {
                    var removeDrain = Effect_Ref["new"](pure(Data_Unit.unit))();
                    var oneWrite = function (i$prime) {
                        return Data_Function.flip(tailRecM)(i$prime)(function (i) {
                            var v = Data_Array.index(bs)(i);
                            if (v instanceof Data_Maybe.Nothing) {
                                return function __do() {
                                    complete(new Data_Either.Right(Data_Unit.unit))();
                                    return new Control_Monad_Rec_Class.Done(Data_Unit.unit);
                                };
                            };
                            if (v instanceof Data_Maybe.Just) {
                                return function __do() {
                                    var nobackpressure = Node_Stream["write$prime"](w)(v.value0)(function (v1) {
                                        if (v1 instanceof Data_Maybe.Nothing) {
                                            return pure(Data_Unit.unit);
                                        };
                                        if (v1 instanceof Data_Maybe.Just) {
                                            return complete(new Data_Either.Left(v1.value0));
                                        };
                                        throw new Error("Failed pattern match at Node.Stream.Aff (line 465, column 49 - line 469, column 34): " + [ v1.constructor.name ]);
                                    })();
                                    if (nobackpressure) {
                                        return new Control_Monad_Rec_Class.Loop(i + 1 | 0);
                                    };
                                    var removeDrain$prime = Node_EventEmitter.once(Node_Stream.drainH)(oneWrite(i + 1 | 0))(w)();
                                    Effect_Ref.write(removeDrain$prime)(removeDrain)();
                                    return new Control_Monad_Rec_Class.Done(Data_Unit.unit);
                                };
                            };
                            throw new Error("Failed pattern match at Node.Stream.Aff (line 456, column 7 - line 476, column 29): " + [ v.constructor.name ]);
                        });
                    };
                    oneWrite(0)();
                    return Effect_Aff.effectCanceler(join(Effect_Ref.read(removeDrain)));
                };
            }));
        };
    };
};
var toStringUTF8 = function (dictMonadEffect) {
    var liftEffect = Effect_Class.liftEffect(dictMonadEffect);
    return function (bs) {
        return liftEffect(bindFlipped(Node_Buffer.toString(Node_Encoding.UTF8.value))(Node_Buffer.concat(bs)));
    };
};
var readableToBuffers = function (dictMonadAff) {
    var liftAff = Effect_Aff_Class.liftAff(dictMonadAff);
    return function (r) {
        return liftAff(Effect_Aff.makeAff(function (complete) {
            return function __do() {
                var bufs = liftST(Data_Array_ST["new"])();
                var dataRef = Effect_Ref["new"](mempty)();
                var removeData = join(Effect_Ref.read(dataRef));
                var removeError = Node_EventEmitter.once(Node_Stream.errorH)(function (err) {
                    return function __do() {
                        removeData();
                        return complete(new Data_Either.Left(err))();
                    };
                })(r)();
                var removeClose = Node_EventEmitter.once(Node_Stream.closeH)(function __do() {
                    removeError();
                    removeData();
                    var result = liftST(Data_Array_ST.unsafeFreeze(bufs))();
                    return complete(new Data_Either.Right(result))();
                })(r)();
                var removeEnd = Node_EventEmitter.once(Node_Stream.endH)(function __do() {
                    removeError();
                    removeClose();
                    removeData();
                    var result = liftST(Data_Array_ST.unsafeFreeze(bufs))();
                    return complete(new Data_Either.Right(result))();
                })(r)();
                var rmData = Node_EventEmitter.on(Node_Stream.dataH)(function (buf) {
                    return $$void(liftST(Data_Array_ST.push(buf)(bufs)));
                })(r)();
                Effect_Ref.write(rmData)(dataRef)();
                return Effect_Aff.effectCanceler(function __do() {
                    removeError();
                    removeClose();
                    removeEnd();
                    return removeData();
                });
            };
        }));
    };
};
var readableToString = function (dictMonadAff) {
    var MonadEffect0 = dictMonadAff.MonadEffect0();
    var bind1 = Control_Bind.bind((MonadEffect0.Monad0()).Bind1());
    var readableToBuffers1 = readableToBuffers(dictMonadAff);
    var liftEffect = Effect_Class.liftEffect(MonadEffect0);
    return function (r) {
        return function (enc) {
            return bind1(readableToBuffers1(r))(function (bufs) {
                return liftEffect(bindFlipped(Node_Buffer.toString(enc))(Node_Buffer.concat(bufs)));
            });
        };
    };
};
var readableToStringUtf8 = function (dictMonadAff) {
    var readableToString1 = readableToString(dictMonadAff);
    return function (r) {
        return readableToString1(r)(Node_Encoding.UTF8.value);
    };
};
var readSome = function (dictMonadAff) {
    var liftAff = Effect_Aff_Class.liftAff(dictMonadAff);
    return function (r) {
        return liftAff(Effect_Aff.makeAff(function (complete) {
            return function __do() {
                var isReadable = Node_Stream.readable(r)();
                if (isReadable) {
                    var bufs = liftST(Data_Array_ST["new"])();
                    var removeError = Node_EventEmitter.once(Node_Stream.errorH)(function (err) {
                        return complete(new Data_Either.Left(err));
                    })(r)();
                    var removeClose = Node_EventEmitter.once(Node_Stream.closeH)(function __do() {
                        removeError();
                        var ret = liftST(Data_Array_ST.unsafeFreeze(bufs))();
                        return complete(new Data_Either.Right({
                            buffers: ret,
                            readagain: false
                        }))();
                    })(r)();
                    var removeEnd = Node_EventEmitter.once(Node_Stream.endH)(function __do() {
                        removeError();
                        removeClose();
                        var ret = liftST(Data_Array_ST.unsafeFreeze(bufs))();
                        return complete(new Data_Either.Right({
                            buffers: ret,
                            readagain: false
                        }))();
                    })(r)();
                    var cleanupRethrow = function (err) {
                        return function __do() {
                            removeError();
                            removeClose();
                            removeEnd();
                            complete(new Data_Either.Left(err))();
                            return Effect_Aff.nonCanceler;
                        };
                    };
                    return Effect_Exception.catchException(cleanupRethrow)(function __do() {
                        (function () {
                            while (!(function __do() {
                                var v = Node_Stream.read(r)();
                                if (v instanceof Data_Maybe.Nothing) {
                                    return true;
                                };
                                if (v instanceof Data_Maybe.Just) {
                                    $$void(liftST(Data_Array_ST.push(v.value0)(bufs)))();
                                    return false;
                                };
                                throw new Error("Failed pattern match at Node.Stream.Aff (line 221, column 27 - line 225, column 23): " + [ v.constructor.name ]);
                            })()) {

                            };
                            return {};
                        })();
                        var ret1 = liftST(Data_Array_ST.unsafeFreeze(bufs))();
                        var readagain = Node_Stream.readable(r)();
                        var $64 = readagain && Data_Array.length(ret1) === 0;
                        if ($64) {
                            var removeReadable = Node_EventEmitter.once(Node_Stream.readableH)(function __do() {
                                (function () {
                                    while (!(function __do() {
                                        var v = Node_Stream.read(r)();
                                        if (v instanceof Data_Maybe.Nothing) {
                                            return true;
                                        };
                                        if (v instanceof Data_Maybe.Just) {
                                            $$void(liftST(Data_Array_ST.push(v.value0)(bufs)))();
                                            return false;
                                        };
                                        throw new Error("Failed pattern match at Node.Stream.Aff (line 238, column 31 - line 242, column 27): " + [ v.constructor.name ]);
                                    })()) {

                                    };
                                    return {};
                                })();
                                var ret2 = liftST(Data_Array_ST.unsafeFreeze(bufs))();
                                removeError();
                                removeClose();
                                removeEnd();
                                var readagain2 = Node_Stream.readable(r)();
                                return complete(new Data_Either.Right({
                                    buffers: ret2,
                                    readagain: readagain2
                                }))();
                            })(r)();
                            return Effect_Aff.effectCanceler(function __do() {
                                removeError();
                                removeClose();
                                removeEnd();
                                return removeReadable();
                            });
                        };
                        removeError();
                        removeClose();
                        removeEnd();
                        complete(new Data_Either.Right({
                            buffers: ret1,
                            readagain: readagain
                        }))();
                        return Effect_Aff.nonCanceler;
                    })();
                };
                complete(new Data_Either.Right({
                    buffers: [  ],
                    readagain: false
                }))();
                return Effect_Aff.nonCanceler;
            };
        }));
    };
};
var readN = function (dictMonadAff) {
    var liftAff = Effect_Aff_Class.liftAff(dictMonadAff);
    return function (r) {
        return function (n) {
            return liftAff(Effect_Aff.makeAff(function (complete) {
                var $67 = n < 0;
                if ($67) {
                    return applySecond(complete(new Data_Either.Left(Effect_Exception.error("read bytes must be > 0"))))(pure(Effect_Aff.nonCanceler));
                };
                return function __do() {
                    var isReadable = Node_Stream.readable(r)();
                    if (isReadable) {
                        var redRef = Effect_Ref["new"](0)();
                        var bufs = liftST(Data_Array_ST["new"])();
                        var removeReadable = Effect_Ref["new"](pure(Data_Unit.unit))();
                        var removeError = Node_EventEmitter.once(Node_Stream.errorH)(function (err) {
                            return function __do() {
                                join(Effect_Ref.read(removeReadable))();
                                return complete(new Data_Either.Left(err))();
                            };
                        })(r)();
                        var removeClose = Node_EventEmitter.once(Node_Stream.closeH)(function __do() {
                            removeError();
                            join(Effect_Ref.read(removeReadable))();
                            var ret = liftST(Data_Array_ST.unsafeFreeze(bufs))();
                            return complete(new Data_Either.Right({
                                buffers: ret,
                                readagain: false
                            }))();
                        })(r)();
                        var removeEnd = Node_EventEmitter.once(Node_Stream.endH)(function __do() {
                            removeError();
                            removeClose();
                            var ret = liftST(Data_Array_ST.unsafeFreeze(bufs))();
                            return complete(new Data_Either.Right({
                                buffers: ret,
                                readagain: false
                            }))();
                        })(r)();
                        var tryToRead = function (continuation) {
                            return function __do() {
                                (function () {
                                    while (!(function __do() {
                                        var red = Effect_Ref.read(redRef)();
                                        var v = Node_Stream["read$prime"](r)(n - red | 0)();
                                        if (v instanceof Data_Maybe.Nothing) {
                                            return true;
                                        };
                                        if (v instanceof Data_Maybe.Just) {
                                            liftST(Data_Array_ST.push(v.value0)(bufs))();
                                            var s = Node_Buffer.size(v.value0)();
                                            var red$prime = Effect_Ref.modify(function (v1) {
                                                return v1 + s | 0;
                                            })(redRef)();
                                            var $70 = red$prime >= n;
                                            if ($70) {
                                                return true;
                                            };
                                            return false;
                                        };
                                        throw new Error("Failed pattern match at Node.Stream.Aff (line 401, column 42 - line 410, column 29): " + [ v.constructor.name ]);
                                    })()) {

                                    };
                                    return {};
                                })();
                                var red = Effect_Ref.read(redRef)();
                                var $72 = red >= n;
                                if ($72) {
                                    removeError();
                                    removeClose();
                                    removeEnd();
                                    var ret = liftST(Data_Array_ST.unsafeFreeze(bufs))();
                                    var readagain = Node_Stream.readable(r)();
                                    return complete(new Data_Either.Right({
                                        buffers: ret,
                                        readagain: readagain
                                    }))();
                                };
                                return continuation(Data_Unit.unit)();
                            };
                        };
                        var waitToRead = function (v) {
                            return function __do() {
                                var removeReadable$prime = Node_EventEmitter.once(Node_Stream.readableH)(tryToRead(waitToRead))(r)();
                                return Effect_Ref.write(removeReadable$prime)(removeReadable)();
                            };
                        };
                        var cleanupRethrow = function (err) {
                            return function __do() {
                                removeError();
                                removeClose();
                                removeEnd();
                                join(Effect_Ref.read(removeReadable))();
                                complete(new Data_Either.Left(err))();
                                return Effect_Aff.nonCanceler;
                            };
                        };
                        return Effect_Exception.catchException(cleanupRethrow)(function __do() {
                            tryToRead(waitToRead)();
                            return Effect_Aff.effectCanceler(function __do() {
                                removeError();
                                removeClose();
                                removeEnd();
                                return join(Effect_Ref.read(removeReadable))();
                            });
                        })();
                    };
                    complete(new Data_Either.Right({
                        buffers: [  ],
                        readagain: false
                    }))();
                    return Effect_Aff.nonCanceler;
                };
            }));
        };
    };
};
var readAll = function (dictMonadAff) {
    var liftAff = Effect_Aff_Class.liftAff(dictMonadAff);
    return function (r) {
        return liftAff(Effect_Aff.makeAff(function (complete) {
            return function __do() {
                var isReadable = Node_Stream.readable(r)();
                if (isReadable) {
                    var bufs = liftST(Data_Array_ST["new"])();
                    var removeReadable = Effect_Ref["new"](pure(Data_Unit.unit))();
                    var removeError = Node_EventEmitter.once(Node_Stream.errorH)(function (err) {
                        return function __do() {
                            join(Effect_Ref.read(removeReadable))();
                            return complete(new Data_Either.Left(err))();
                        };
                    })(r)();
                    var removeClose = Node_EventEmitter.once(Node_Stream.closeH)(function __do() {
                        removeError();
                        join(Effect_Ref.read(removeReadable))();
                        var ret = liftST(Data_Array_ST.unsafeFreeze(bufs))();
                        return complete(new Data_Either.Right(ret))();
                    })(r)();
                    var removeEnd = Node_EventEmitter.once(Node_Stream.endH)(function __do() {
                        removeError();
                        removeClose();
                        var ret = liftST(Data_Array_ST.unsafeFreeze(bufs))();
                        return complete(new Data_Either.Right(ret))();
                    })(r)();
                    var cleanupRethrow = function (err) {
                        return function __do() {
                            removeError();
                            removeClose();
                            removeEnd();
                            join(Effect_Ref.read(removeReadable))();
                            complete(new Data_Either.Left(err))();
                            return Effect_Aff.nonCanceler;
                        };
                    };
                    return Effect_Exception.catchException(cleanupRethrow)(function __do() {
                        (function () {
                            while (!(function __do() {
                                var v = Node_Stream.read(r)();
                                if (v instanceof Data_Maybe.Nothing) {
                                    return true;
                                };
                                if (v instanceof Data_Maybe.Just) {
                                    $$void(liftST(Data_Array_ST.push(v.value0)(bufs)))();
                                    return false;
                                };
                                throw new Error("Failed pattern match at Node.Stream.Aff (line 311, column 27 - line 315, column 23): " + [ v.constructor.name ]);
                            })()) {

                            };
                            return {};
                        })();
                        var $lazy_waitToRead = $runtime_lazy("waitToRead", "Node.Stream.Aff", function () {
                            return function __do() {
                                var removeReadable$prime = Node_EventEmitter.once(Node_Stream.readableH)(function __do() {
                                    (function () {
                                        while (!(function __do() {
                                            var v = Node_Stream.read(r)();
                                            if (v instanceof Data_Maybe.Nothing) {
                                                return true;
                                            };
                                            if (v instanceof Data_Maybe.Just) {
                                                liftST(Data_Array_ST.push(v.value0)(bufs))();
                                                return false;
                                            };
                                            throw new Error("Failed pattern match at Node.Stream.Aff (line 324, column 33 - line 328, column 29): " + [ v.constructor.name ]);
                                        })()) {

                                        };
                                        return {};
                                    })();
                                    return $lazy_waitToRead(329)();
                                })(r)();
                                return Effect_Ref.write(removeReadable$prime)(removeReadable)();
                            };
                        });
                        var waitToRead = $lazy_waitToRead(319);
                        waitToRead();
                        return Effect_Aff.effectCanceler(function __do() {
                            removeError();
                            removeClose();
                            removeEnd();
                            return join(Effect_Ref.read(removeReadable))();
                        });
                    })();
                };
                complete(new Data_Either.Right([  ]))();
                return Effect_Aff.nonCanceler;
            };
        }));
    };
};
var fromStringUTF8 = function (dictMonadEffect) {
    var liftEffect = Effect_Class.liftEffect(dictMonadEffect);
    return function (s) {
        return liftEffect(map(pure1)(Node_Buffer.fromString(s)(Node_Encoding.UTF8.value)));
    };
};
var end = function (dictMonadAff) {
    var liftAff = Effect_Aff_Class.liftAff(dictMonadAff);
    return function (w) {
        return liftAff(Effect_Aff.makeAff(function (complete) {
            return function __do() {
                Node_Stream["end$prime"](w)(function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return complete(new Data_Either.Right(Data_Unit.unit));
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return complete(new Data_Either.Left(v.value0));
                    };
                    throw new Error("Failed pattern match at Node.Stream.Aff (line 497, column 19 - line 499, column 36): " + [ v.constructor.name ]);
                })();
                return Effect_Aff.nonCanceler;
            };
        }));
    };
};
export {
    readableToStringUtf8,
    readableToString,
    readableToBuffers,
    readSome,
    readAll,
    readN,
    write,
    end,
    toStringUTF8,
    fromStringUTF8
};
