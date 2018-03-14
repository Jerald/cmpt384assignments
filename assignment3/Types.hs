-- For holding the various types we'll need to use.
-- Storing them here lets all our files use them.

-- Add the other data types to the list once made
module Types where

-- IMPORTANT
-- NOTE: The "Bottom" value, which corresponds to erroneuous evaluation, it being referred to as "null" in this file.
-- IMPORTANT

-- Notes for reading the EBNF:
-- Literal brackets (bounded in quotes) are just what they are.
-- Unbounded brackets are operators. Curly braces, {}, are the repetition operator, or the equivalent of the regex ()*
-- Square brackets, [], are the option operator, or the equivalent of the regex ()?

-- Template token type from the coursys. We can change if needed.
-- comment: (^;;;.*$)+
-- numeral: -?[0-9]+
-- alphanumeric_symbol: [A-Za-z](-?[A-Za-z0-9])*
-- special_symbol: [+-*/<>=&|!@#$%?:]+
-- lparen: \(
-- rparen: \)
-- lbrak: \[
-- rbrak: \]
-- lbrace: \{
-- rbrace: \}
-- equal: =
-- semicolon: ;
-- arrow: -->
-- quote-mark: "
-- colon: :
data Token = Comment [Char] | NumToken Int | AlphaNumToken [Char] | SpecialToken [Char] |
             LParen | RParen | LBrack | RBrack | LBrace | RBrace | Equal | Semicolon | Arrow | Quote | Colon deriving (Show, Eq)


-- Tamplate SLisp domains taken from coursys. We can change if needed

-- Defines: <S-expression>  ::= <atom> | <list>
-- <list>                   ::= '(' {<S-expression>} ')'
-- <numeric-atom>           = -?[0-9]+
-- <symbolic-atom>          = [A-Za-z](-?[A-Za-z0-9])*|[+-*/<>=&|!@#$%?:]+
data SExpression = NumAtom Int | SymAtom [Char] | List [SExpression] deriving Show

-- Some grouping of alpha-numeric things which starts with a letter and can contain dashes
-- Defines: <identifier>    = [A-Za-z](-?[A-Za-z0-9])*
type Identifier = [Char]

-- <clause>                 ::= <expression> '-->' <expression>
data CondClause = Clause SmLispExpr SmLispExpr deriving Show

-- <local-definition>       ::= <identifier> '=' <expression>
data LocalDef = Binding Identifier SmLispExpr deriving Show

-- Defines: <expression>    ::= <value> | <variable> | <function-call> | <conditional-expression> | <let-expression>
-- <value>                  ::= <numeric-atom> | '"' <symbolic-atom> '"' | <list>
-- Note: <value> is nearly equivalent to <S-expression>, so one is used in place of <value>
-- <function-call>          ::= <identifier> '[' <expression> {';' <expression>} ']'
-- <conditional-expression> ::= '[' <clause> {';' <clause>} ']'
-- <let-expression>         ::= '{' <local-definition> {';' <local-definition>} ':' <expression> '}'
data SmLispExpr = SExpr SExpression | Variable Identifier | FnCall Identifier [SmLispExpr] | CondExpr [CondClause] | LetExpr [LocalDef] SmLispExpr deriving Show
-- Conditional Expression: returns the first LHS value which evaluates to T, or returns null if any LHS evaluates to a non-boolean value. Returns false if no LHS is true.
-- Let Expression: evaluates the provided expression in the context of the local-definitions being applied. If any definition evaluates to null, returns null.

-- Defines: <definition>    ::= <constant-definition> | <function-definition>
-- <constant-definition>    ::= [<comment>] <identifier> '=' <expression>
-- <function-definition>    ::= [<comment>] <identifier> '[' <parameter> {';' <parameter> }']' '=' <expression>
-- <parameter>              ::= <identifier>
data Definition = ConstantDef Identifier SmLispExpr | FunctionDef Identifier [Identifier] SmLispExpr deriving Show

-- A SLisp "program" is simply defined as a list of definitions.
-- This "program" is then used as the context by which to evaluate the literal passed program.
type SmLispProgram = [Definition]
