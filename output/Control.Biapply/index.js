// Generated by purs version 0.15.15
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Bifunctor from "../Data.Bifunctor/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var biapplyTuple = {
    biapply: function (v) {
        return function (v1) {
            return new Data_Tuple.Tuple(v.value0(v1.value0), v.value1(v1.value1));
        };
    },
    Bifunctor0: function () {
        return Data_Bifunctor.bifunctorTuple;
    }
};
var biapply = function (dict) {
    return dict.biapply;
};
var biapplyFirst = function (dictBiapply) {
    var biapply1 = biapply(dictBiapply);
    var bimap = Data_Bifunctor.bimap(dictBiapply.Bifunctor0());
    return function (a) {
        return function (b) {
            return biapply1(bimap(Data_Function["const"](identity))(Data_Function["const"](identity))(a))(b);
        };
    };
};
var biapplySecond = function (dictBiapply) {
    var biapply1 = biapply(dictBiapply);
    var bimap = Data_Bifunctor.bimap(dictBiapply.Bifunctor0());
    return function (a) {
        return function (b) {
            return biapply1(bimap(Data_Function["const"])(Data_Function["const"])(a))(b);
        };
    };
};
var bilift2 = function (dictBiapply) {
    var biapply1 = biapply(dictBiapply);
    var bimap = Data_Bifunctor.bimap(dictBiapply.Bifunctor0());
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return biapply1(bimap(f)(g)(a))(b);
                };
            };
        };
    };
};
var bilift3 = function (dictBiapply) {
    var biapply1 = biapply(dictBiapply);
    var bimap = Data_Bifunctor.bimap(dictBiapply.Bifunctor0());
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return function (c) {
                        return biapply1(biapply1(bimap(f)(g)(a))(b))(c);
                    };
                };
            };
        };
    };
};
export {
    biapply,
    biapplyFirst,
    biapplySecond,
    bilift2,
    bilift3,
    biapplyTuple
};
