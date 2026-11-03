/*
 * Cleaned JSX parser — standalone, no semantic predicates.
 */

parser grammar JSXParser;

options {
    tokenVocab = JSXLexer;
}

jsxElement
    : jsxFragment
    | JsxOpen JsxName jsxAttributes? JsxOpeningSlashEnd
    | JsxOpen JsxName jsxAttributes? JsxOpeningEnd jsxChildren? JsxClose JsxClosingName JsxClosingEnd
    ;

jsxFragment
    : JsxOpen JsxOpeningEnd jsxChildren? JsxClose JsxClosingEnd
    ;

jsxAttributes
    : jsxAttribute+
    ;

jsxAttribute
    : JsxName (JsxAssign jsxAttributeValue)?
    ;

jsxAttributeValue
    : JsxString
    | JsxOpeningOpenBrace jsxExpression ExpressionCloseBrace
    ;

jsxChildren
    : jsxChild+
    ;

jsxChild
    : HtmlChardata
    | JsxOpenBrace jsxExpression ExpressionCloseBrace
    | jsxElement
    ;

jsxExpression
    : (ExpressionText | ExpressionOpenBrace jsxExpression ExpressionCloseBrace)*
    ;
