// Generated by purs version 0.15.15
import * as Aeria_Syntax_Tree from "../Aeria.Syntax.Tree/index.js";
import * as Data_Generic_Rep from "../Data.Generic.Rep/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_String_Utils from "../Data.String.Utils/index.js";
var show = /* #__PURE__ */ Data_Show.show(/* #__PURE__ */ Data_Show.showArray(Aeria_Syntax_Tree.showTyp));
var show1 = /* #__PURE__ */ Data_Show.show(Aeria_Syntax_Tree.showTyp);
var show2 = /* #__PURE__ */ Data_Show.show(/* #__PURE__ */ Data_List_Types.showList(Data_Show.showString));
var PropertyTypeDoesNotExpectAttributes = /* #__PURE__ */ (function () {
    function PropertyTypeDoesNotExpectAttributes() {

    };
    PropertyTypeDoesNotExpectAttributes.value = new PropertyTypeDoesNotExpectAttributes();
    return PropertyTypeDoesNotExpectAttributes;
})();
var PropertyTypeDoesNotExpectType = /* #__PURE__ */ (function () {
    function PropertyTypeDoesNotExpectType() {

    };
    PropertyTypeDoesNotExpectType.value = new PropertyTypeDoesNotExpectType();
    return PropertyTypeDoesNotExpectType;
})();
var UndefinedReference = /* #__PURE__ */ (function () {
    function UndefinedReference(value0) {
        this.value0 = value0;
    };
    UndefinedReference.create = function (value0) {
        return new UndefinedReference(value0);
    };
    return UndefinedReference;
})();
var TypeMismatch = /* #__PURE__ */ (function () {
    function TypeMismatch(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TypeMismatch.create = function (value0) {
        return function (value1) {
            return new TypeMismatch(value0, value1);
        };
    };
    return TypeMismatch;
})();
var ArrayTypeMismatch = /* #__PURE__ */ (function () {
    function ArrayTypeMismatch(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ArrayTypeMismatch.create = function (value0) {
        return function (value1) {
            return new ArrayTypeMismatch(value0, value1);
        };
    };
    return ArrayTypeMismatch;
})();
var UndefinedAttribute = /* #__PURE__ */ (function () {
    function UndefinedAttribute(value0) {
        this.value0 = value0;
    };
    UndefinedAttribute.create = function (value0) {
        return new UndefinedAttribute(value0);
    };
    return UndefinedAttribute;
})();
var AttributeLiteralMustBe = /* #__PURE__ */ (function () {
    function AttributeLiteralMustBe(value0) {
        this.value0 = value0;
    };
    AttributeLiteralMustBe.create = function (value0) {
        return new AttributeLiteralMustBe(value0);
    };
    return AttributeLiteralMustBe;
})();
var ExpectedProperty = /* #__PURE__ */ (function () {
    function ExpectedProperty() {

    };
    ExpectedProperty.value = new ExpectedProperty();
    return ExpectedProperty;
})();
var NotProperty = /* #__PURE__ */ (function () {
    function NotProperty(value0) {
        this.value0 = value0;
    };
    NotProperty.create = function (value0) {
        return new NotProperty(value0);
    };
    return NotProperty;
})();
var ExprError = /* #__PURE__ */ (function () {
    function ExprError(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ExprError.create = function (value0) {
        return function (value1) {
            return new ExprError(value0, value1);
        };
    };
    return ExprError;
})();
var UndefinedProperty = /* #__PURE__ */ (function () {
    function UndefinedProperty(value0) {
        this.value0 = value0;
    };
    UndefinedProperty.create = function (value0) {
        return new UndefinedProperty(value0);
    };
    return UndefinedProperty;
})();
var PropertyIsAlreadyInUse = /* #__PURE__ */ (function () {
    function PropertyIsAlreadyInUse(value0) {
        this.value0 = value0;
    };
    PropertyIsAlreadyInUse.create = function (value0) {
        return new PropertyIsAlreadyInUse(value0);
    };
    return PropertyIsAlreadyInUse;
})();
var PropertyError = /* #__PURE__ */ (function () {
    function PropertyError(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    PropertyError.create = function (value0) {
        return function (value1) {
            return new PropertyError(value0, value1);
        };
    };
    return PropertyError;
})();
var FiltersPresetsError = /* #__PURE__ */ (function () {
    function FiltersPresetsError(value0) {
        this.value0 = value0;
    };
    FiltersPresetsError.create = function (value0) {
        return new FiltersPresetsError(value0);
    };
    return FiltersPresetsError;
})();
var LayoutComponentError = /* #__PURE__ */ (function () {
    function LayoutComponentError(value0) {
        this.value0 = value0;
    };
    LayoutComponentError.create = function (value0) {
        return new LayoutComponentError(value0);
    };
    return LayoutComponentError;
})();
var UndefinedFunction = /* #__PURE__ */ (function () {
    function UndefinedFunction(value0) {
        this.value0 = value0;
    };
    UndefinedFunction.create = function (value0) {
        return new UndefinedFunction(value0);
    };
    return UndefinedFunction;
})();
var UndefinedStrategy = /* #__PURE__ */ (function () {
    function UndefinedStrategy(value0) {
        this.value0 = value0;
    };
    UndefinedStrategy.create = function (value0) {
        return new UndefinedStrategy(value0);
    };
    return UndefinedStrategy;
})();
var showExprError = {
    show: function (v) {
        if (v instanceof ExpectedProperty) {
            return "Expected a property";
        };
        if (v instanceof NotProperty) {
            return "Not a property: \"" + (v.value0.value1 + "\"");
        };
        throw new Error("Failed pattern match at Aeria.Semantic.Error (line 36, column 1 - line 38, column 82): " + [ v.constructor.name ]);
    }
};
var show3 = /* #__PURE__ */ Data_Show.show(showExprError);
var ppPropertyName = function (v) {
    return v.value1;
};
var ppCollectionName = function (v) {
    return Data_String_Utils.ucfirst(v.value1);
};
var ppAttributeName = function (v) {
    return v.value1;
};
var showPropertyError = {
    show: function (v) {
        if (v instanceof PropertyTypeDoesNotExpectAttributes) {
            return "Property type does not expect attributes";
        };
        if (v instanceof PropertyTypeDoesNotExpectType) {
            return "Property type does not expect a type";
        };
        if (v instanceof UndefinedReference) {
            return "Undefined reference to collection: " + ppCollectionName(v.value0);
        };
        if (v instanceof TypeMismatch) {
            return "Type mismatch, expected: " + (show(v.value0) + (", got: " + show1(v.value1)));
        };
        if (v instanceof ArrayTypeMismatch) {
            return "Array type mismatch, expected: " + (show1(v.value0) + (", got: " + show1(v.value1)));
        };
        if (v instanceof UndefinedAttribute) {
            return "Undefined attribute: " + ppAttributeName(v.value0);
        };
        if (v instanceof AttributeLiteralMustBe) {
            return "Attribute literal must be one of: " + show2(v.value0);
        };
        throw new Error("Failed pattern match at Aeria.Semantic.Error (line 21, column 1 - line 28, column 93): " + [ v.constructor.name ]);
    }
};
var show4 = /* #__PURE__ */ Data_Show.show(showPropertyError);
var showSemanticError = {
    show: function (v) {
        if (v instanceof ExprError) {
            return show3(v.value1);
        };
        if (v instanceof UndefinedProperty) {
            return "Undefined property: \"" + (v.value0.value1 + "\"");
        };
        if (v instanceof PropertyError) {
            return "Error in property \"" + (ppPropertyName(v.value0.value0.name) + ("\": " + show4(v.value1)));
        };
        if (v instanceof FiltersPresetsError) {
            return "\"Filters\" is required";
        };
        if (v instanceof LayoutComponentError) {
            return "\"Component name\" is required";
        };
        if (v instanceof UndefinedFunction) {
            return "Undefined function: \"" + (v.value0.value1 + "\"");
        };
        if (v instanceof UndefinedStrategy) {
            return "Undefined strategy: \"" + (v.value0 + "\"");
        };
        if (v instanceof PropertyIsAlreadyInUse) {
            return "PropertyIsAlreadyInUse";
        };
        throw new Error("Failed pattern match at Aeria.Semantic.Error (line 52, column 1 - line 60, column 61): " + [ v.constructor.name ]);
    }
};
var genericSemanticError = {
    to: function (x) {
        if (x instanceof Data_Generic_Rep.Inl) {
            return new ExprError(x.value0.value0, x.value0.value1);
        };
        if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inl) {
            return new UndefinedProperty(x.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0 instanceof Data_Generic_Rep.Inl)) {
            return new PropertyIsAlreadyInUse(x.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0 instanceof Data_Generic_Rep.Inl))) {
            return new PropertyError(x.value0.value0.value0.value0.value0, x.value0.value0.value0.value0.value1);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl)))) {
            return new FiltersPresetsError(x.value0.value0.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl))))) {
            return new LayoutComponentError(x.value0.value0.value0.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl)))))) {
            return new UndefinedFunction(x.value0.value0.value0.value0.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr)))))) {
            return new UndefinedStrategy(x.value0.value0.value0.value0.value0.value0.value0);
        };
        throw new Error("Failed pattern match at Aeria.Semantic.Error (line 50, column 1 - line 50, column 64): " + [ x.constructor.name ]);
    },
    from: function (x) {
        if (x instanceof ExprError) {
            return new Data_Generic_Rep.Inl(new Data_Generic_Rep.Product(x.value0, x.value1));
        };
        if (x instanceof UndefinedProperty) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0));
        };
        if (x instanceof PropertyIsAlreadyInUse) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0)));
        };
        if (x instanceof PropertyError) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(new Data_Generic_Rep.Product(x.value0, x.value1)))));
        };
        if (x instanceof FiltersPresetsError) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0)))));
        };
        if (x instanceof LayoutComponentError) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0))))));
        };
        if (x instanceof UndefinedFunction) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0)))))));
        };
        if (x instanceof UndefinedStrategy) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(x.value0)))))));
        };
        throw new Error("Failed pattern match at Aeria.Semantic.Error (line 50, column 1 - line 50, column 64): " + [ x.constructor.name ]);
    }
};
var genericPropertyError = {
    to: function (x) {
        if (x instanceof Data_Generic_Rep.Inl) {
            return PropertyTypeDoesNotExpectAttributes.value;
        };
        if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inl) {
            return PropertyTypeDoesNotExpectType.value;
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0 instanceof Data_Generic_Rep.Inl)) {
            return new UndefinedReference(x.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0 instanceof Data_Generic_Rep.Inl))) {
            return new TypeMismatch(x.value0.value0.value0.value0.value0, x.value0.value0.value0.value0.value1);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl)))) {
            return new ArrayTypeMismatch(x.value0.value0.value0.value0.value0.value0, x.value0.value0.value0.value0.value0.value1);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl))))) {
            return new UndefinedAttribute(x.value0.value0.value0.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr))))) {
            return new AttributeLiteralMustBe(x.value0.value0.value0.value0.value0.value0);
        };
        throw new Error("Failed pattern match at Aeria.Semantic.Error (line 19, column 1 - line 19, column 64): " + [ x.constructor.name ]);
    },
    from: function (x) {
        if (x instanceof PropertyTypeDoesNotExpectAttributes) {
            return new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value);
        };
        if (x instanceof PropertyTypeDoesNotExpectType) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value));
        };
        if (x instanceof UndefinedReference) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0)));
        };
        if (x instanceof TypeMismatch) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(new Data_Generic_Rep.Product(x.value0, x.value1)))));
        };
        if (x instanceof ArrayTypeMismatch) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(new Data_Generic_Rep.Product(x.value0, x.value1))))));
        };
        if (x instanceof UndefinedAttribute) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0))))));
        };
        if (x instanceof AttributeLiteralMustBe) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(x.value0))))));
        };
        throw new Error("Failed pattern match at Aeria.Semantic.Error (line 19, column 1 - line 19, column 64): " + [ x.constructor.name ]);
    }
};
var genericExprError = {
    to: function (x) {
        if (x instanceof Data_Generic_Rep.Inl) {
            return ExpectedProperty.value;
        };
        if (x instanceof Data_Generic_Rep.Inr) {
            return new NotProperty(x.value0);
        };
        throw new Error("Failed pattern match at Aeria.Semantic.Error (line 34, column 1 - line 34, column 56): " + [ x.constructor.name ]);
    },
    from: function (x) {
        if (x instanceof ExpectedProperty) {
            return new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value);
        };
        if (x instanceof NotProperty) {
            return new Data_Generic_Rep.Inr(x.value0);
        };
        throw new Error("Failed pattern match at Aeria.Semantic.Error (line 34, column 1 - line 34, column 56): " + [ x.constructor.name ]);
    }
};
export {
    PropertyTypeDoesNotExpectAttributes,
    PropertyTypeDoesNotExpectType,
    UndefinedReference,
    TypeMismatch,
    ArrayTypeMismatch,
    UndefinedAttribute,
    AttributeLiteralMustBe,
    ExpectedProperty,
    NotProperty,
    ExprError,
    UndefinedProperty,
    PropertyIsAlreadyInUse,
    PropertyError,
    FiltersPresetsError,
    LayoutComponentError,
    UndefinedFunction,
    UndefinedStrategy,
    ppPropertyName,
    ppCollectionName,
    ppAttributeName,
    genericPropertyError,
    showPropertyError,
    genericExprError,
    showExprError,
    genericSemanticError,
    showSemanticError
};
