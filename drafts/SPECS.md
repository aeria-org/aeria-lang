# Specs

## BNF (Backus-Naur Form)

```go
<language> ::= <mixin> | <collection> | <router>

// mixin
<mixin> ::= "mixin" <mixin-path>
<mixin-path> ::= <relative-path> | <github-path> | <npm-path>

<relative-path> ::= "'./" <path> ".aeria'"
<github-path> ::= "'github:" <organization> "/" <mixin> "'"
<npm-path> ::= "'npm:@" <organization> "/" <mixin> "'"

// types
<primite-types> ::= "str" | "number" | "enum" | "date" | "datetime"
<array-type> ::= (<primite-types> | <object-type> | <collection-name>) "[]"
<object-type> ::= "{" <property-list> "}"
<types> ::= <primite-types> | <array-type> | <object-type>

// values
<values> ::= <number> | <string> | <boolean> | <identifier>

<string> ::= <any_string>
<number> ::= <any_number>
<boolean> ::= false | true
<identifier> ::= <letter> | <identifier> <letter_or_digit>

<letter> ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"
<letter_or_digit> ::= <letter> | <digit>
<digit> ::= "0" | "1" | ... | "9"

// conditions
<cond> ::= "(" <cond> ")" | <cond-expr>
<cond-expr> ::= <values> <cond-oper> <values>
<cond-oper> ::= "<" | ">" | "<=" | ">=" | "=="

// collection
<collection> ::= "collection" <collection-name> "{" <collection-body> "}"
<collection-name> ::= <capitalized-word>
<collection-body> ::= <required> <properties> <getters> <table>

<property-name> ::= <lowercase-word>

// required
<required> ::= "required" "{" <required-properties> "}" | ε
<required-properties> ::= <property-name> <required-attributes> | <property-name> <required-attributes>  <required-properties>
<required-attributes> ::= "@cond" "(" <cond> ")" | ε

// properties
<properties> ::= "properties" "{" <property-list> "}"
<property-list> ::= <property> | <property> <property-list>

<property> ::= <property-name> <types> <property-attributes>
<property-attributes> ::= "@" <attribute-name> "(" <attribute-value> ")" | ε
<attribute-name> ::= "indexes" | "constraints" | "options" | "cond" | "default"
<attribute-value> ::= "[" <values> "," <values> "]" | <values>

// getters
<getters> ::= "getters" "{" <getter-list> "}" | ε
<getter-list> ::= <getter> | <getter> <getter-list>
<getter> ::= <getter-name> <macro>
<getter-name> ::= <lowercase-word>

<macro> ::= "@" "macro" ":" <identifier> <string>

// table
<table> ::= "table" "[" <table-properties> "]" | ε
<table-properties> ::= <property-name> | <property-name> "," <table-properties>

// docs
<router> ::= "router" "{" <route> "}"
<route> ::= <http-method> <route-path> <types> "{" <documentation> "}"
<http-method> ::= "POST" | "GET" | "PUT" | "DELETE"
<route-path> ::= "/" <collection-name> "/" <route-action>
<route-action> ::= <lowercase-word>
<documentation> ::= "documentation" "@text" <string> "@end"
```
