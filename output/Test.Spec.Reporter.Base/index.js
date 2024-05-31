// Generated by purs version 0.15.15
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_State_Class from "../Control.Monad.State.Class/index.js";
import * as Control_Monad_State_Trans from "../Control.Monad.State.Trans/index.js";
import * as Control_Monad_Trans_Class from "../Control.Monad.Trans.Class/index.js";
import * as Control_Monad_Writer from "../Control.Monad.Writer/index.js";
import * as Control_Monad_Writer_Trans from "../Control.Monad.Writer.Trans/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Generic_Rep from "../Data.Generic.Rep/index.js";
import * as Data_HeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Show_Generic from "../Data.Show.Generic/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Data_Void from "../Data.Void/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect_Aff from "../Effect.Aff/index.js";
import * as Effect_Class from "../Effect.Class/index.js";
import * as Effect_Exception from "../Effect.Exception/index.js";
import * as Pipes from "../Pipes/index.js";
import * as Pipes_Internal from "../Pipes.Internal/index.js";
import * as Test_Spec_Console from "../Test.Spec.Console/index.js";
import * as Test_Spec_Result from "../Test.Spec.Result/index.js";
import * as Test_Spec_Runner_Event from "../Test.Spec.Runner.Event/index.js";
import * as Test_Spec_Style from "../Test.Spec.Style/index.js";
import * as Test_Spec_Summary from "../Test.Spec.Summary/index.js";
import * as Test_Spec_Tree from "../Test.Spec.Tree/index.js";
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
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit);
var lift = /* #__PURE__ */ Control_Monad_Trans_Class.lift(Pipes_Internal.monadTransProxy);
var genericShowArgsArgument = /* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(Data_Show.showString);
var genericShowArgsProduct = /* #__PURE__ */ Data_Show_Generic.genericShowArgsProduct(genericShowArgsArgument);
var add = /* #__PURE__ */ Data_Semiring.add(Data_Semiring.semiringInt);
var intercalate = /* #__PURE__ */ Data_Foldable.intercalate(Data_Foldable.foldableArray)(Data_Monoid.monoidString);
var append = /* #__PURE__ */ Data_Semigroup.append(Data_Semigroup.semigroupArray);
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showInt);
var monadWriterT = /* #__PURE__ */ Control_Monad_Writer_Trans.monadWriterT(Data_Monoid.monoidString)(Data_Identity.monadIdentity);
var bindStateT = /* #__PURE__ */ Control_Monad_State_Trans.bindStateT(monadWriterT);
var bind = /* #__PURE__ */ Control_Bind.bind(bindStateT);
var monadStateStateT = /* #__PURE__ */ Control_Monad_State_Trans.monadStateStateT(monadWriterT);
var get = /* #__PURE__ */ Control_Monad_State_Class.get(monadStateStateT);
var all = /* #__PURE__ */ Data_Foldable.all(Data_Map_Internal.foldableMap)(Data_HeytingAlgebra.heytingAlgebraBoolean);
var discard1 = /* #__PURE__ */ discard(bindStateT);
var put = /* #__PURE__ */ Control_Monad_State_Class.put(monadStateStateT);
var applicativeStateT = /* #__PURE__ */ Control_Monad_State_Trans.applicativeStateT(monadWriterT);
var when = /* #__PURE__ */ Control_Applicative.when(applicativeStateT);
var for_ = /* #__PURE__ */ Data_Foldable.for_(applicativeStateT)(Data_Foldable.foldableArray);
var toUnfoldable = /* #__PURE__ */ Data_Map_Internal.toUnfoldable(Data_Unfoldable.unfoldableArray);
var pure = /* #__PURE__ */ Control_Applicative.pure(applicativeStateT);
var ordArray = /* #__PURE__ */ Data_Ord.ordArray(Test_Spec_Tree.pathItemOrd);
var insert = /* #__PURE__ */ Data_Map_Internal.insert(ordArray);
var update = /* #__PURE__ */ Data_Map_Internal.update(ordArray);
var gets = /* #__PURE__ */ Control_Monad_State_Class.gets(monadStateStateT);
var lookup = /* #__PURE__ */ Data_Map_Internal.lookup(ordArray);
var unless = /* #__PURE__ */ Control_Applicative.unless(applicativeStateT);
var execStateT = /* #__PURE__ */ Control_Monad_State_Trans.execStateT(/* #__PURE__ */ Control_Monad_Writer_Trans.functorWriterT(Data_Identity.functorIdentity));
var liftEffect = /* #__PURE__ */ Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
var voidLeft = /* #__PURE__ */ Data_Functor.voidLeft(Effect.functorEffect);
var pure1 = /* #__PURE__ */ Control_Applicative.pure(Effect_Aff.applicativeAff);
var RunningTest = /* #__PURE__ */ (function () {
    function RunningTest(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    RunningTest.create = function (value0) {
        return function (value1) {
            return new RunningTest(value0, value1);
        };
    };
    return RunningTest;
})();
var RunningPending = /* #__PURE__ */ (function () {
    function RunningPending(value0) {
        this.value0 = value0;
    };
    RunningPending.create = function (value0) {
        return new RunningPending(value0);
    };
    return RunningPending;
})();
var RunningSuite = /* #__PURE__ */ (function () {
    function RunningSuite(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    RunningSuite.create = function (value0) {
        return function (value1) {
            return new RunningSuite(value0, value1);
        };
    };
    return RunningSuite;
})();
var scanWithStateM = function (dictMonad) {
    var bindProxy = Pipes_Internal.bindProxy(dictMonad);
    var bind1 = Control_Bind.bind(bindProxy);
    var $$await = Pipes["await"](dictMonad);
    var discard2 = discard(bindProxy);
    var $$yield = Pipes["yield"](dictMonad);
    var lift1 = lift(dictMonad);
    return function (step) {
        return function (begin) {
            var go = function (x) {
                return bind1($$await)(function (a) {
                    return discard2($$yield(a))(function () {
                        return bind1(lift1(step(x)(a)))(function (x$prime) {
                            return go(x$prime);
                        });
                    });
                });
            };
            return bind1(lift1(begin))(function (x) {
                return go(x);
            });
        };
    };
};
var scanWithStateM1 = /* #__PURE__ */ scanWithStateM(Effect_Aff.monadAff);
var runningItemGeneric = {
    to: function (x) {
        if (x instanceof Data_Generic_Rep.Inl) {
            return new RunningTest(x.value0.value0, x.value0.value1);
        };
        if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inl) {
            return new RunningPending(x.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inr) {
            return new RunningSuite(x.value0.value0.value0, x.value0.value0.value1);
        };
        throw new Error("Failed pattern match at Test.Spec.Reporter.Base (line 106, column 1 - line 106, column 60): " + [ x.constructor.name ]);
    },
    from: function (x) {
        if (x instanceof RunningTest) {
            return new Data_Generic_Rep.Inl(new Data_Generic_Rep.Product(x.value0, x.value1));
        };
        if (x instanceof RunningPending) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0));
        };
        if (x instanceof RunningSuite) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Product(x.value0, x.value1)));
        };
        throw new Error("Failed pattern match at Test.Spec.Reporter.Base (line 106, column 1 - line 106, column 60): " + [ x.constructor.name ]);
    }
};
var runningItemShow = {
    show: /* #__PURE__ */ Data_Show_Generic.genericShow(runningItemGeneric)(/* #__PURE__ */ Data_Show_Generic.genericShowSum(/* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ genericShowArgsProduct(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(/* #__PURE__ */ Data_Maybe.showMaybe(Test_Spec_Result.showResult))))({
        reflectSymbol: function () {
            return "RunningTest";
        }
    }))(/* #__PURE__ */ Data_Show_Generic.genericShowSum(/* #__PURE__ */ Data_Show_Generic.genericShowConstructor(genericShowArgsArgument)({
        reflectSymbol: function () {
            return "RunningPending";
        }
    }))(/* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ genericShowArgsProduct(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(Data_Show.showBoolean)))({
        reflectSymbol: function () {
            return "RunningSuite";
        }
    }))))
};
var printFailures = function (dictMonadWriter) {
    var Monad1 = (dictMonadWriter.MonadTell1()).Monad1();
    var applicativeStateT1 = Control_Monad_State_Trans.applicativeStateT(Monad1);
    var traverse_ = Data_Foldable.traverse_(applicativeStateT1)(Data_Foldable.foldableArray);
    var bindStateT1 = Control_Monad_State_Trans.bindStateT(Monad1);
    var bind1 = Control_Bind.bind(bindStateT1);
    var modify = Control_Monad_State_Class.modify(Control_Monad_State_Trans.monadStateStateT(Monad1));
    var discard2 = discard(bindStateT1);
    var tellLn = Test_Spec_Console.tellLn(Control_Monad_State_Trans.monadWriterStateT(dictMonadWriter));
    var pure2 = Control_Applicative.pure(applicativeStateT1);
    var evalStateT = Control_Monad_State_Trans.evalStateT(((Monad1.Bind1()).Apply0()).Functor0());
    return function (xs$prime) {
        var $lazy_go = $runtime_lazy("go", "Test.Spec.Reporter.Base", function () {
            return traverse_(function (v) {
                if (v instanceof Test_Spec_Tree.Node && v.value0 instanceof Data_Either.Left) {
                    return $lazy_go(64)(v.value1);
                };
                if (v instanceof Test_Spec_Tree.Node && v.value0 instanceof Data_Either.Right) {
                    return Data_Void.absurd(v.value0.value0);
                };
                if (v instanceof Test_Spec_Tree.Leaf && (v.value1 instanceof Data_Maybe.Just && v.value1.value0 instanceof Test_Spec_Result.Failure)) {
                    return bind1(modify(add(1)))(function (i) {
                        var label = intercalate(" ")(append(Test_Spec_Tree.parentSuiteName(v.value0.value1))([ v.value0.value0 ]));
                        return discard2(tellLn(show(i) + (") " + label)))(function () {
                            return tellLn(Test_Spec_Style.styled(Test_Spec_Style.red)(Test_Spec_Style.indent(2) + Effect_Exception.message(v.value1.value0.value0)));
                        });
                    });
                };
                if (v instanceof Test_Spec_Tree.Leaf) {
                    return pure2(Data_Unit.unit);
                };
                throw new Error("Failed pattern match at Test.Spec.Reporter.Base (line 63, column 20 - line 71, column 30): " + [ v.constructor.name ]);
            });
        });
        var go = $lazy_go(62);
        return evalStateT(go(Test_Spec_Tree.annotateWithPaths(xs$prime)))(0);
    };
};
var defaultUpdate = function (opts) {
    return function (e) {
        var modifyRunningItems = function (f) {
            var runningItemIsFinished = function (v) {
                if (v instanceof RunningPending) {
                    return true;
                };
                if (v instanceof RunningTest) {
                    return Data_Maybe.isJust(v.value1);
                };
                if (v instanceof RunningSuite) {
                    return v.value1;
                };
                throw new Error("Failed pattern match at Test.Spec.Reporter.Base (line 158, column 33 - line 161, column 46): " + [ v.constructor.name ]);
            };
            return bind(get)(function (s) {
                var nextRunningItems = f(opts.getRunningItems(s));
                var allFinished = all(runningItemIsFinished)(nextRunningItems);
                return discard1(put(opts.putRunningItems((function () {
                    if (allFinished) {
                        return Data_Map_Internal.empty;
                    };
                    return nextRunningItems;
                })())(s)))(function () {
                    return when(allFinished)(for_(toUnfoldable(nextRunningItems))(Data_Tuple.uncurry(opts.printFinishedItem)));
                });
            });
        };
        var baseUpdate = function (v) {
            if (v instanceof Test_Spec_Runner_Event.Suite && v.value0 instanceof Test_Spec_Runner_Event.Sequential) {
                return pure(Data_Unit.unit);
            };
            if (v instanceof Test_Spec_Runner_Event.Suite && v.value0 instanceof Test_Spec_Runner_Event.Parallel) {
                return modifyRunningItems(insert(v.value1)(new RunningSuite(v.value2, false)));
            };
            if (v instanceof Test_Spec_Runner_Event.SuiteEnd) {
                return modifyRunningItems(Data_Function.flip(update)(v.value0)(function (v1) {
                    if (v1 instanceof RunningSuite) {
                        return new Data_Maybe.Just(new RunningSuite(v1.value0, true));
                    };
                    return Data_Maybe.Nothing.value;
                }));
            };
            if (v instanceof Test_Spec_Runner_Event.Test && v.value0 instanceof Test_Spec_Runner_Event.Sequential) {
                return pure(Data_Unit.unit);
            };
            if (v instanceof Test_Spec_Runner_Event.Test && v.value0 instanceof Test_Spec_Runner_Event.Parallel) {
                return modifyRunningItems(insert(v.value1)(new RunningTest(v.value2, Data_Maybe.Nothing.value)));
            };
            if (v instanceof Test_Spec_Runner_Event.TestEnd) {
                return bind(gets(opts.getRunningItems))(function (runningItem) {
                    var v1 = lookup(v.value0)(runningItem);
                    if (v1 instanceof Data_Maybe.Just && v1.value0 instanceof RunningTest) {
                        return modifyRunningItems(insert(v.value0)(new RunningTest(v1.value0.value0, new Data_Maybe.Just(v.value2))));
                    };
                    return pure(Data_Unit.unit);
                });
            };
            if (v instanceof Test_Spec_Runner_Event.Pending) {
                return bind(gets(opts.getRunningItems))(function (runningItem) {
                    return unless(Data_Map_Internal.isEmpty(runningItem))(modifyRunningItems(insert(v.value0)(new RunningPending(v.value1))));
                });
            };
            if (v instanceof Test_Spec_Runner_Event.End) {
                return pure(Data_Unit.unit);
            };
            if (v instanceof Test_Spec_Runner_Event.Start) {
                return pure(Data_Unit.unit);
            };
            throw new Error("Failed pattern match at Test.Spec.Reporter.Base (line 121, column 18 - line 146, column 33): " + [ v.constructor.name ]);
        };
        return discard1(baseUpdate(e))(function () {
            return opts.update(e);
        });
    };
};
var defaultSummary = function (dictMonadWriter) {
    var Monad1 = (dictMonadWriter.MonadTell1()).Monad1();
    var discard2 = discard(Monad1.Bind1());
    var when1 = Control_Applicative.when(Monad1.Applicative0());
    var tellLn = Test_Spec_Console.tellLn(dictMonadWriter);
    var printFailures1 = printFailures(dictMonadWriter);
    return function (xs) {
        return discard2((function () {
            var v = Test_Spec_Summary.summarize(xs);
            return discard2(when1(v.passed > 0)(tellLn(Test_Spec_Style.styled(Test_Spec_Style.green)(show(v.passed) + " passing"))))(function () {
                return discard2(when1(v.pending > 0)(tellLn(Test_Spec_Style.styled(Test_Spec_Style.cyan)(show(v.pending) + " pending"))))(function () {
                    return when1(v.failed > 0)(tellLn(Test_Spec_Style.styled(Test_Spec_Style.red)(show(v.failed) + " failed")));
                });
            });
        })())(function () {
            return discard2(tellLn(""))(function () {
                return printFailures1(xs);
            });
        });
    };
};
var defaultReporter = function (initialState) {
    return function (onEvent) {
        return scanWithStateM1(function (s) {
            return function (e) {
                var v = Control_Monad_Writer.runWriter(execStateT(onEvent(e))(s));
                return liftEffect(voidLeft(Test_Spec_Console.write(v.value1))(v.value0));
            };
        })(pure1(initialState));
    };
};
export {
    defaultSummary,
    defaultReporter,
    defaultUpdate,
    RunningTest,
    RunningPending,
    RunningSuite,
    runningItemGeneric,
    runningItemShow
};
