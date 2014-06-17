{
module SPLParse where
import SPLData
import Data.Char
}

%name parseProgram
%tokentype { Token }
%error { parseError }

%token
      int             { TokenInt $$ }
      id              { TokenId $$ }
      define          { TokenDefine }
      class           { TokenClass }
      instance        { TokenInstance }
      ':'             { TokenColon }
      '='             { TokenEq }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '('             { TokenOP }
      ')'             { TokenCP }
      ','             { TokenComma }
      inl             { TokenInl }
      inr             { TokenInr }
      mref            { TokenMRef }
      match           { TokenMatch }
      end             { TokenEnd }
      with            { TokenWith }
      '|'             { TokenPipe }
      '<'             { TokenLT }
      '>'             { TokenGT }
      assign          { TokenAssign }
      forall          { TokenForAll }
      '['             { TokenOB }
      ']'             { TokenCB }
      '{'             { TokenOC }
      '}'             { TokenCC }
      arrow           { TokenArrow }
      fatArrow        { TokenFatArrow }
      '@'             { TokenAt }
-- Types
      numberT         { TokenNumberT }
      bottomT         { TokenBottomT }
      arrayT          { TokenArrayT }

%nonassoc '['
%nonassoc with
%nonassoc id '('
%left arrow
%right assign
%left '='
%nonassoc inl inr
%left '+' '-'
%left '*' '/'
%nonassoc '@'
%left arrayT

%%

top :: { [Statement] }
    : statements { reverse $1 }

statements :: { [Statement] }
           : statement            { [$1] }
           | statements statement { $2 : $1 }

statement :: { Statement }
          : functionDefinition    { $1 }
          | classDefinition       { $1 }
          | instanceDefinition    { $1 }
          | expression            { ExprS $1 }

functionDefinition :: { Statement }
functionDefinition : define id '<' maybeIds '>' '[' maybeConstrainedIds ']' '(' maybeTypedIds ')' ':' typeNoParens expression end
                     { FunctionDef $2 $4 $7 $10 $13 $14 }
classDefinition    :: { Statement }
classDefinition    : class id id '(' typeConstraints ')' typeTaggedIds end
                     { ClassS $2 $3 $5 $7 }
instanceDefinition :: { Statement }
instanceDefinition : instance id id typeScheme idExprPairs end
                     { InstanceS $2 $3 $4 $5 }

expression :: { Expr }
expression : int { NumberE $ fromIntegral $1 }
           | '(' expression ',' expression ')' { PairE $2 $4 }
           | unit { UnitE }
           | inl expression { InLeftE $2 }
           | inr expression { InRightE $2 }
           | valIdRef { $1 }
           | expression '+' expression { PrimOpAppE (PrimOpE Plus) $1 $3 }
           | expression '-' expression { PrimOpAppE (PrimOpE Minus) $1 $3 }
           | expression '*' expression { PrimOpAppE (PrimOpE Star) $1 $3 }
           | expression '/' expression { PrimOpAppE (PrimOpE Slash) $1 $3 }
           | expression '=' expression { PrimOpAppE (PrimOpE Equal) $1 $3 }
           | expression '@' '<' maybeTypes '>' '[' maybeExpressions ']' '(' expressions ')'
             { AppE $1 $4 $7 $10 }
           | mref id expression { MethodRef $2 $3 }
           | match expression with matchPairs end { MatchE $2 $4 }
           | '{' expression '|' id '<' expression '}' { ArrayCompE $4 $6 $2 }
           | expression '[' expression ']' { SubscriptE $1 $3 }
           | expression '[' expression ']' assign expression { SubscriptUpdateE $1 $3 $6 }
           | '(' expression ')' { $2 }

typeScheme :: { TypeScheme }
typeScheme : typ { QuantifiedConstraintsTS [] [] $1 }
           | '(' forall ids typ ')'
             { QuantifiedConstraintsTS $3 [] $4 }
           | '(' forall ids '[' typeConstraints ']' typ ')'
             { QuantifiedConstraintsTS $3 $5 $7 }

typeConstraint :: { TypeConstraint }
               : '(' id typ ')' { ($2, $3) }

typ :: { Type }
typ : typeNoParens { $1 }
    | '(' typ ')'  { $2 }

typeNoParens :: { Type }
typeNoParens : numberT { NumberT }
             | typ '*' typ { ProductT $1 $3 }
             | unit { UnitT }
             | typ '+' typ { SumT $1 $3 }
             | bottomT { BottomT }
               -- I think it picks up the int's precedence before the arrayT so
               -- I must explicitly request the arrayT's precedence
             | arrayT int typ %prec arrayT { ArrayT $2 $3 }
             | typ arrow typ { FunctionT [$1] $3 }
             | '(' typ types ')' arrow typ { FunctionT ($2 : $3) $6 }
             | typeIdRef { $1 }

matchPair : matchPat fatArrow expression { ($1, $3) }
matchPat :: { Pattern }
         : id ':' typ { PatternVarP $1 $3 }
         | '(' matchPat ',' matchPat ')' { PatternPairP $2 $4 }
         | unit { PatternUnitP }
         | inl matchPat { PatternInLeftP $2 }
         | inr matchPat { PatternInRightP $2 }

typeTaggedId : id ':' typ { ($1, $3) }

idExprPair : id '=' expression { ($1, $3) }

valIdRef :: { Expr }
         : id { VarE $1 }
typeIdRef :: { Type }
         : id { TypeVar $1 }

ids :: { [String] }
ids : id { [$1] }
    | ids id { $2 : $1 }

constrainedId :: { ConstrainedId }
              : id ':' typeConstraint { ($1, $3) }

typedId :: { TypedId }
        : id ':' typ { ($1, $3) }

constrainedIds :: { [ConstrainedId] }
               : constrainedId { [$1] }
               | constrainedId constrainedIds { $1 : $2 }

typedIds :: { [TypedId] }
         : typedId { [$1] }
         | typedId typedIds { $1 : $2 }

maybeTypedIds :: { [TypedId] }
              : { [] }
              | typedIds { $1 }

maybeConstrainedIds :: { [ConstrainedId] }
                    : { [] }
                    | constrainedIds { $1 }

maybeIds :: { [String] }
maybeIds : { [] }
         | ids { $1 }

types :: { [Type] }
      : typ       { [$1] }
      | types typ { $2 : $1 }

maybeTypes :: { [Type] }
           : { [] }
           | types { $1 }

matchPairs : matchPairsRev { reverse $1 }

matchPairsRev : matchPair            { [$1] }
              | matchPairs matchPair { $2 : $1 }

typeConstraints : typeConstraint                 { [$1] }
                | typeConstraints typeConstraint { $2 : $1 }







typeTaggedIds : typeTaggedId               { [$1] }
              | typeTaggedIds typeTaggedId { $2 : $1 }

idExprPairs : idExprPair             { [$1] }
            | idExprPairs idExprPair { $2 : $1 }

expressions : expression             { [$1] }
            | expressions expression { $2 : $1 }

maybeExpressions : { [] }
                 | expressions { $1 }

unit : '(' ')' { undefined }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
  = TokenLet
  | TokenIn
  | TokenInt Integer
  | TokenId String
  | TokenDefine
  | TokenClass
  | TokenInstance
  | TokenColon
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenOP
  | TokenCP
  | TokenComma
  | TokenInl
  | TokenInr
  | TokenMRef
  | TokenMatch
  | TokenEnd
  | TokenWith
  | TokenPipe
  | TokenLT
  | TokenGT
  | TokenAssign
  | TokenForAll
  | TokenOB
  | TokenCB
  | TokenOC
  | TokenCC
  | TokenArrow
  | TokenFatArrow
  | TokenAt
-- Types
  | TokenNumberT
  | TokenBottomT
  | TokenArrayT
  deriving Show

isId :: Char -> Bool
isId c = c == '_' || isAlpha c

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isId c = lexId (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('=':'>':cs) = TokenFatArrow : lexer cs
lexer (':':'=':cs) = TokenAssign : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('|':cs) = TokenPipe : lexer cs
lexer ('<':cs) = TokenLT : lexer cs
lexer ('>':cs) = TokenGT : lexer cs
lexer ('[':cs) = TokenOB : lexer cs
lexer (']':cs) = TokenCB : lexer cs
lexer ('{':cs) = TokenOC : lexer cs
lexer ('}':cs) = TokenCC : lexer cs
lexer ('@':cs) = TokenAt : lexer cs
lexer (':':cs) = TokenColon : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexId cs =
   case span isId cs of
      ("inl" ,    rest) -> TokenInl      : lexer rest
      ("inr" ,    rest) -> TokenInr      : lexer rest
      ("mref" ,   rest) -> TokenMRef     : lexer rest
      ("match" ,  rest) -> TokenMatch    : lexer rest
      ("end" ,    rest) -> TokenEnd      : lexer rest
      ("with"   , rest) -> TokenWith     : lexer rest
      ("define" , rest) -> TokenDefine   : lexer rest
      ("class" ,  rest) -> TokenClass    : lexer rest
      ("instance",rest) -> TokenInstance : lexer rest
      ("forall" , rest) -> TokenForAll   : lexer rest
      ("array" ,  rest) -> TokenArrayT   : lexer rest
      ("bottom" , rest) -> TokenBottomT  : lexer rest
      ("number" , rest) -> TokenNumberT  : lexer rest
      (id ,       rest) -> TokenId id    : lexer rest

readProgram = parseProgram . lexer
}
