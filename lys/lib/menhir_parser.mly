// SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
// SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
//
// SPDX-License-Identifier: GPL-3.0-only

%token KWDEF
%token KWLET
%token KWIN
%token KWIF
%token KWTHEN
%token KWELSE
%token KWINT
%token KWFLOAT
%token KWBOOL
%token KWSTRING
%token KWUNIT
%token KWMATCH
%token KWAS

%token EQUAL
%token COLONEQUAL
%token COLON
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token COMMA
%token ARROW
%token SEMICOLON
%token UNIT
%token BACKSLASH
%token ELLIPSIS

%token TRIANGLE
%token BARBAR
%token ANDAND
%token EQUALEQUAL
%token BANGEQUAL
%token GT
%token GEQ
%token LT
%token LEQ
%token PLUSPLUS
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token PERCENT
%token STARSTAR
%token BANG

%token <string> COMMENT
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> STRING
%token <Ast.uid> UID
%token <Ast.lid> LID

%token EOF

%start <Ast.program> program

%{ open Ast %}

%%

let located(x) == ~ = x; { { loc= { start= $startofs(x); fin= $endofs(x) }; value= x } }

let program := ~ = list(declaration); EOF; < >

let declaration :=
  | ~ = COMMENT; < Comment >
  | KWDEF; ~ = binding; < ValueBinding >
  | KWDEF; id = located(UID); COLONEQUAL; body = typing; { TypeDefinition {id; body} }
  | KWDEF; id = located(LID); parameters = nonempty_list(parameter); signature = signature; EQUAL; body = expression; { FunctionDefinition {id; parameters; signature; body} }
  | KWDEF; id = located(UID); COLONEQUAL; polymorphics = list(located(LID)); LBRACE; variants = nonempty_list(variant); RBRACE; { AdtDefinition {id; polymorphics; variants} }

let binding == id = located(LID); signature = signature; EQUAL; body = expression; { {id; signature; body} }

let signature == option(preceded(COLON, typing))

let parameter :=
  | ~ = located(LID); < ALid >
  | LPAREN; p = parameter; COMMA; ps = separated_list(COMMA, parameter); RPAREN; { ATuple (p :: ps) }

let typing_atom :=
  | KWINT; { TInt }
  | KWFLOAT; { TFloat }
  | KWBOOL; { TBool }
  | KWSTRING; { TString }
  | KWUNIT; { TUnit }
  | id = located(UID); typing = option(typing_atom); { TConstructor {id; typing} }
  | ~ = located(LID); < TPolymorphic >
  | LPAREN; t = typing; COMMA; ts = separated_list(COMMA, typing); RPAREN; { TTuple (t :: ts) }
  | LBRACKET; t = typing; RBRACKET; < TList >

let typing :=
  | typing_atom
  | l = typing; ARROW; r = typing_atom; { TFunction {l; r} }

let expression_atom :=
  | ~ = INT; < Int >
  | ~ = FLOAT; < Float >
  | ~ = BOOL; < Bool >
  | ~ = STRING; < String >
  | UNIT; { Unit }
  | ~ = located(UID); < Uid >
  | ~ = located(LID); < Lid >
  | LPAREN; e = expression; COMMA; es = separated_list(COMMA, expression); RPAREN; { Tuple (e :: es) }
  | LBRACKET; ~ = separated_list(COMMA, expression); RBRACKET; < List >
  | LPAREN; body = expression; signature = signature; RPAREN; { Expression {body; signature} }

let application :=
  | expression_atom
  | body = application; argument = expression_atom; { Application {body; argument} }

let expression :=
  | KWLET; bindings = separated_nonempty_list(SEMICOLON, binding); KWIN; body = expression; { Let {bindings; body} }
  | KWIF; predicate = expression; KWTHEN; truthy = expression; KWELSE; falsy = expression; { If {predicate; truthy; falsy} }
  | KWMATCH; body = expression; LBRACE; cases = nonempty_list(case); RBRACE; { Match {body; cases} }
  | BACKSLASH; parameters = nonempty_list(parameter); ARROW; body = expression; { Lambda {parameters; body} }
  | expression_pipe

let expression_pipe :=
  | expression_or
  | l = expression_pipe; TRIANGLE; r = expression_or; { BinaryOperation {l; operator= BPipe; r} }

let expression_or :=
  | expression_and
  | l = expression_or; BARBAR; r = expression_and; { BinaryOperation {l; operator= BOr; r} }

let expression_and :=
  | expression_equality
  | l = expression_and; ANDAND; r = expression_equality; { BinaryOperation {l; operator= BAnd; r} }

let expression_equality :=
  | expression_compare
  | l = expression_equality; EQUALEQUAL; r = expression_compare; { BinaryOperation {l; operator= BEqual; r} }
  | l = expression_equality; BANGEQUAL; r = expression_compare; { BinaryOperation {l; operator= BNotEqual; r} }

let expression_compare :=
  | expression_concatenate
  | l = expression_compare; GT; r = expression_concatenate; { BinaryOperation {l; operator= BGreaterThan; r} }
  | l = expression_compare; GEQ; r = expression_concatenate; { BinaryOperation {l; operator= BGreaterOrEqual; r} }
  | l = expression_compare; LT; r = expression_concatenate; { BinaryOperation {l; operator= BLessThan; r} }
  | l = expression_compare; LEQ; r = expression_concatenate; { BinaryOperation {l; operator= BLessOrEqual; r} }

let expression_concatenate :=
  | expression_addition
  | l = expression_concatenate; PLUSPLUS; r = expression_addition; { BinaryOperation {l; operator= BConcatenate; r} }

let expression_addition :=
  | expression_multiplication
  | l = expression_addition; PLUS; r = expression_multiplication; { BinaryOperation {l; operator= BAdd; r} }
  | l = expression_addition; MINUS; r = expression_multiplication; { BinaryOperation {l; operator= BSubstract; r} }

let expression_multiplication :=
  | expression_exponentiation
  | l = expression_multiplication; STAR; r = expression_exponentiation; { BinaryOperation {l; operator= BMultiply; r} }
  | l = expression_multiplication; SLASH; r = expression_exponentiation; { BinaryOperation {l; operator= BDivide; r} }
  | l = expression_multiplication; PERCENT; r = expression_exponentiation; { BinaryOperation {l; operator= BModulo; r} }

let expression_exponentiation :=
  | expression_unary
  | l = expression_unary; STARSTAR; r = expression_exponentiation; { BinaryOperation {l; operator= BExponentiate; r} }

let expression_unary :=
  | application
  | PLUS; body = expression_unary; { UnaryOperation {operator= UPlus; body} }
  | MINUS; body = expression_unary; { UnaryOperation {operator= UMinus; body} }
  | BANG; body = expression_unary; { UnaryOperation {operator= UNot; body} }

let variant == id = located(UID); typing = option(preceded(KWAS, typing)); SEMICOLON; { {id; typing} } 

let case == pattern = pattern; guard = option(preceded(KWIF, expression)); ARROW; body = expression; SEMICOLON; { {pattern; guard; body} }

let pattern_atom :=
  | ~ = INT; < PInt >
  | ~ = FLOAT; < PFloat >
  | ~ = BOOL; < PBool >
  | ~ = STRING; < PString >
  | ~ = located(LID); < PLid >
  | LPAREN; p = pattern_atom; COMMA; ps = separated_list(COMMA, pattern_atom); RPAREN; { PTuple (p :: ps) }
  | LBRACKET; ~ = separated_list(COMMA, pattern_atom); RBRACKET; < PList >
  | LBRACKET; ps = separated_nonempty_list(COMMA, pattern_atom); ELLIPSIS; p = located(LID); RBRACKET; { PListSpread (ps @ [PLid p]) }
  | id = located(UID); pattern = option(pattern_atom); { PConstructor {id; pattern} }

let pattern :=
  | pattern_atom
  | l = pattern; SEMICOLON; r = pattern_atom; { POr {l; r} }
