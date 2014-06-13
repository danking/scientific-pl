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
      with            { TokenWith }
      '|'             { TokenPipe }
      '<'             { TokenLT }
      assign          { TokenAssign }
      forall          { TokenForAll }
      '['             { TokenOB }
      ']'             { TokenCB }
      '{'             { TokenOC }
      '}'             { TokenCC }
      unit            { TokenUnit }
      arrow           { TokenArrow }
      fatArrow        { TokenFatArrow }
-- Types
      numberT         { TokenNumberT }
      bottomT         { TokenBottomT }
      arrayT          { TokenArrayT }
      arrowT          { TokenArrowT }

%nonassoc ':' '['
%nonassoc STMT
%nonassoc id
%left arrowT
%right assign
%nonassoc inl inr
%left '+' '-'
%left '*' '/'
%left arrayT

%%

statements :: { [Statement] }
           : statement            { [$1] }
           | statements statement { $2 : $1 }

statement :: { Statement }
          : functionDefinition    { $1 }
          | classDefinition       { $1 }
          | instanceDefinition    { $1 }
          | expression            { ExprS $1 }

functionDefinition :: { Statement }
functionDefinition : define id '(' ids ')' typeScheme expression
                     { FunctionDef $2 $4 $6 $7 }
classDefinition    :: { Statement }
classDefinition    : class id id '(' typeConstraints ')' typeTaggedIds %prec STMT
                     { ClassS $2 $3 $5 $7 }
instanceDefinition :: { Statement }
instanceDefinition : instance id id typeScheme idExprPairs %prec STMT
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
           | expression ':' types '[' expressions ']' '(' expressions ')'
             { AppE $1 $3 $5 $8 }
           | mref id expression { MethodRef $2 $3 }
           | match expression with matchPairs { MatchE $2 $4 }
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

typeConstraint : '(' id typ ')' { ($2, $3) }

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
             | typ arrowT typ { FunctionT [$1] $3 }
             | '(' typ types ')' arrowT typ { FunctionT ($2 : $3) $6 }
             | typeIdRef { $1 }
-- matchPair :

typeTaggedId : id ':' typ { ($1, $3) }

idExprPair : id '=' expression { ($1, $3) }

valIdRef :: { Expr }
         : id { VarE $1 }
typeIdRef :: { Type }
         : id { TypeVar $1 }

ids :: { [String] }
ids : id { [$1] }
    | ids id { $2 : $1 }

types :: { [Type] }
      : typ       { [$1] }
      | types typ { $2 : $1 }

matchPairs : matchPair            { [$1] }
           | matchPairs matchPair { $2 : $1 }

typeConstraints : typeConstraint                 { [$1] }
                | typeConstraints typeConstraint { $2 : $1 }







typeTaggedIds : typeTaggedId               { [$1] }
              | typeTaggedIds typeTaggedId { $2 : $1 }

idExprPairs : idExprPair             { [$1] }
            | idExprPairs idExprPair { $2 : $1 }

expressions : expression             { [$1] }
            | expressions expression { $2 : $1 }

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
  | TokenPipe
  | TokenLT
  | TokenAssign
  | TokenForAll
  | TokenOB
  | TokenCB
  | TokenOC
  | TokenCC
  | TokenUnit
  | TokenArrow
  | TokenFatArrow
-- Types
  | TokenNumberT
  | TokenUnitT
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
lexer ('(':')':cs) = TokenUnit : lexer cs
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
lexer ('[':cs) = TokenOB : lexer cs
lexer (']':cs) = TokenCB : lexer cs
lexer ('{':cs) = TokenOC : lexer cs
lexer ('}':cs) = TokenCC : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexId cs =
   case span isId cs of
      ("inl" ,    rest) -> TokenInl      : lexer rest
      ("inr" ,    rest) -> TokenInr      : lexer rest
      ("mref" ,   rest) -> TokenMRef     : lexer rest
      ("match" ,  rest) -> TokenMatch    : lexer rest
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
