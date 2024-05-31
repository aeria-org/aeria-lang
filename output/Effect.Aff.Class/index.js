// Generated by purs version 0.15.15
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Monad_Cont_Trans from "../Control.Monad.Cont.Trans/index.js";
import * as Control_Monad_Except_Trans from "../Control.Monad.Except.Trans/index.js";
import * as Control_Monad_List_Trans from "../Control.Monad.List.Trans/index.js";
import * as Control_Monad_Maybe_Trans from "../Control.Monad.Maybe.Trans/index.js";
import * as Control_Monad_RWS_Trans from "../Control.Monad.RWS.Trans/index.js";
import * as Control_Monad_Reader_Trans from "../Control.Monad.Reader.Trans/index.js";
import * as Control_Monad_State_Trans from "../Control.Monad.State.Trans/index.js";
import * as Control_Monad_Trans_Class from "../Control.Monad.Trans.Class/index.js";
import * as Control_Monad_Writer_Trans from "../Control.Monad.Writer.Trans/index.js";
import * as Effect_Aff from "../Effect.Aff/index.js";
var lift = /* #__PURE__ */ Control_Monad_Trans_Class.lift(Control_Monad_Cont_Trans.monadTransContT);
var lift1 = /* #__PURE__ */ Control_Monad_Trans_Class.lift(Control_Monad_Except_Trans.monadTransExceptT);
var lift2 = /* #__PURE__ */ Control_Monad_Trans_Class.lift(Control_Monad_List_Trans.monadTransListT);
var lift3 = /* #__PURE__ */ Control_Monad_Trans_Class.lift(Control_Monad_Maybe_Trans.monadTransMaybeT);
var lift4 = /* #__PURE__ */ Control_Monad_Trans_Class.lift(Control_Monad_Reader_Trans.monadTransReaderT);
var lift5 = /* #__PURE__ */ Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT);
var monadAffAff = {
    liftAff: /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn),
    MonadEffect0: function () {
        return Effect_Aff.monadEffectAff;
    }
};
var liftAff = function (dict) {
    return dict.liftAff;
};
var monadAffContT = function (dictMonadAff) {
    var MonadEffect0 = dictMonadAff.MonadEffect0();
    var monadEffectContT = Control_Monad_Cont_Trans.monadEffectContT(MonadEffect0);
    return {
        liftAff: (function () {
            var $65 = lift(MonadEffect0.Monad0());
            var $66 = liftAff(dictMonadAff);
            return function ($67) {
                return $65($66($67));
            };
        })(),
        MonadEffect0: function () {
            return monadEffectContT;
        }
    };
};
var monadAffExceptT = function (dictMonadAff) {
    var MonadEffect0 = dictMonadAff.MonadEffect0();
    var monadEffectExceptT = Control_Monad_Except_Trans.monadEffectExceptT(MonadEffect0);
    return {
        liftAff: (function () {
            var $68 = lift1(MonadEffect0.Monad0());
            var $69 = liftAff(dictMonadAff);
            return function ($70) {
                return $68($69($70));
            };
        })(),
        MonadEffect0: function () {
            return monadEffectExceptT;
        }
    };
};
var monadAffListT = function (dictMonadAff) {
    var MonadEffect0 = dictMonadAff.MonadEffect0();
    var monadEffectListT = Control_Monad_List_Trans.monadEffectListT(MonadEffect0);
    return {
        liftAff: (function () {
            var $71 = lift2(MonadEffect0.Monad0());
            var $72 = liftAff(dictMonadAff);
            return function ($73) {
                return $71($72($73));
            };
        })(),
        MonadEffect0: function () {
            return monadEffectListT;
        }
    };
};
var monadAffMaybe = function (dictMonadAff) {
    var MonadEffect0 = dictMonadAff.MonadEffect0();
    var monadEffectMaybe = Control_Monad_Maybe_Trans.monadEffectMaybe(MonadEffect0);
    return {
        liftAff: (function () {
            var $74 = lift3(MonadEffect0.Monad0());
            var $75 = liftAff(dictMonadAff);
            return function ($76) {
                return $74($75($76));
            };
        })(),
        MonadEffect0: function () {
            return monadEffectMaybe;
        }
    };
};
var monadAffRWS = function (dictMonadAff) {
    var MonadEffect0 = dictMonadAff.MonadEffect0();
    var Monad0 = MonadEffect0.Monad0();
    var liftAff1 = liftAff(dictMonadAff);
    return function (dictMonoid) {
        var monadEffectRWS = Control_Monad_RWS_Trans.monadEffectRWS(dictMonoid)(MonadEffect0);
        return {
            liftAff: (function () {
                var $77 = Control_Monad_Trans_Class.lift(Control_Monad_RWS_Trans.monadTransRWST(dictMonoid))(Monad0);
                return function ($78) {
                    return $77(liftAff1($78));
                };
            })(),
            MonadEffect0: function () {
                return monadEffectRWS;
            }
        };
    };
};
var monadAffReader = function (dictMonadAff) {
    var MonadEffect0 = dictMonadAff.MonadEffect0();
    var monadEffectReader = Control_Monad_Reader_Trans.monadEffectReader(MonadEffect0);
    return {
        liftAff: (function () {
            var $79 = lift4(MonadEffect0.Monad0());
            var $80 = liftAff(dictMonadAff);
            return function ($81) {
                return $79($80($81));
            };
        })(),
        MonadEffect0: function () {
            return monadEffectReader;
        }
    };
};
var monadAffState = function (dictMonadAff) {
    var MonadEffect0 = dictMonadAff.MonadEffect0();
    var monadEffectState = Control_Monad_State_Trans.monadEffectState(MonadEffect0);
    return {
        liftAff: (function () {
            var $82 = lift5(MonadEffect0.Monad0());
            var $83 = liftAff(dictMonadAff);
            return function ($84) {
                return $82($83($84));
            };
        })(),
        MonadEffect0: function () {
            return monadEffectState;
        }
    };
};
var monadAffWriter = function (dictMonadAff) {
    var MonadEffect0 = dictMonadAff.MonadEffect0();
    var Monad0 = MonadEffect0.Monad0();
    var liftAff1 = liftAff(dictMonadAff);
    return function (dictMonoid) {
        var monadEffectWriter = Control_Monad_Writer_Trans.monadEffectWriter(dictMonoid)(MonadEffect0);
        return {
            liftAff: (function () {
                var $85 = Control_Monad_Trans_Class.lift(Control_Monad_Writer_Trans.monadTransWriterT(dictMonoid))(Monad0);
                return function ($86) {
                    return $85(liftAff1($86));
                };
            })(),
            MonadEffect0: function () {
                return monadEffectWriter;
            }
        };
    };
};
export {
    liftAff,
    monadAffAff,
    monadAffContT,
    monadAffExceptT,
    monadAffListT,
    monadAffMaybe,
    monadAffReader,
    monadAffRWS,
    monadAffState,
    monadAffWriter
};
