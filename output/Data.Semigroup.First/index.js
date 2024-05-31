// Generated by purs version 0.15.15
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Show from "../Data.Show/index.js";
var First = function (x) {
    return x;
};
var showFirst = function (dictShow) {
    var show = Data_Show.show(dictShow);
    return {
        show: function (v) {
            return "(First " + (show(v) + ")");
        }
    };
};
var semigroupFirst = {
    append: function (x) {
        return function (v) {
            return x;
        };
    }
};
var ordFirst = function (dictOrd) {
    return dictOrd;
};
var functorFirst = {
    map: function (f) {
        return function (m) {
            return f(m);
        };
    }
};
var eqFirst = function (dictEq) {
    return dictEq;
};
var eq1First = {
    eq1: function (dictEq) {
        return Data_Eq.eq(eqFirst(dictEq));
    }
};
var ord1First = {
    compare1: function (dictOrd) {
        return Data_Ord.compare(ordFirst(dictOrd));
    },
    Eq10: function () {
        return eq1First;
    }
};
var boundedFirst = function (dictBounded) {
    return dictBounded;
};
var applyFirst = {
    apply: function (v) {
        return function (v1) {
            return v(v1);
        };
    },
    Functor0: function () {
        return functorFirst;
    }
};
var bindFirst = {
    bind: function (v) {
        return function (f) {
            return f(v);
        };
    },
    Apply0: function () {
        return applyFirst;
    }
};
var applicativeFirst = {
    pure: First,
    Apply0: function () {
        return applyFirst;
    }
};
var monadFirst = {
    Applicative0: function () {
        return applicativeFirst;
    },
    Bind1: function () {
        return bindFirst;
    }
};
export {
    First,
    eqFirst,
    eq1First,
    ordFirst,
    ord1First,
    boundedFirst,
    showFirst,
    functorFirst,
    applyFirst,
    applicativeFirst,
    bindFirst,
    monadFirst,
    semigroupFirst
};
