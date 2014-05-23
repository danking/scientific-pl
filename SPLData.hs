{-# LANGUAGE TypeFamilies #-}
module SPLData where

type TypeId = String

data Type = IntegerT
          | ProductT Type Type
          | UnitT
          | SumT Type Type
          | BottomT
          | ArrayT Integer Type
          | FunctionT [Type] Type
          | DistributionT Type
          | TypeVar TypeId
          deriving (Eq, Show, Read)

type TypeConstraintID = String

type TypeConstraint = (TypeConstraintID, Type)

data TypeScheme = QuantifiedConstraintsTS [TypeId] [TypeConstraint] Type
                deriving (Eq, Show, Read)

type Id = String

data PrimOp = Plus | Minus | Star | Slash
            deriving (Eq, Show, Read)

data Statement = ExprS Expr
               | FailS Fail
               | FunctionDef Id [Id] TypeScheme Expr
               | ClassS TypeConstraintID TypeId [TypeConstraint] [(Id, Type)]
               | InstanceS Id TypeConstraintID TypeId [(Id, Expr)]
               | SequenceS Statement Statement
  deriving (Eq, Show, Read)

data Expr = IntegerE         Integer
          | PairE            Expr Expr
          | UnitE
          | FailE            String
          | InLeftE          Expr
          | InRightE         Expr
          | VarE             Id
          | PrimOpE          PrimOp
          | PrimOpAppE       Expr Expr Expr
          | AppE             Expr [Type] [Expr] [Expr]
          | MatchE           Expr [(Pattern, Expr)]
          | SubscriptE       Expr Expr
          | SubscriptUpdateE Expr Expr Expr
          deriving (Eq, Show, Read)

data Value = IntegerV Integer
           | PairV Value Value
           | UnitV
           | InLeftV Value
           | InRightV Value
           | FunctionV Id [Id] TypeScheme Expr
           | PrimOpV PrimOp
           deriving (Eq, Show, Read)

data Fail = Fail String
          deriving (Eq, Show, Read)

data Pattern = PatternVarP Id Type
             | PatternTupleP Pattern Pattern
             | PatternUnitP
             | PatternInLeftP Pattern
             | PatternInRightP Pattern
             deriving (Eq, Show, Read)
