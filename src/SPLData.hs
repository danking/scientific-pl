{-# LANGUAGE TypeFamilies #-}
module SPLData where

type TypeId = String

data Type = NumberT
          | ProductT Type Type
          | UnitT
          | SumT Type Type
          | BottomT
          | ArrayT Integer Type
          | FunctionT [Type] Type
          | TypeVar TypeId
          deriving (Eq, Show, Read)

type TypeClassId = String

type TypeConstraint = (TypeClassId, Type)

type ConstrainedId = (Id, TypeConstraint)
type TypedId = (Id, Type)

data TypeScheme = QuantifiedConstraintsTS [TypeId] [TypeConstraint] Type
                deriving (Eq, Show, Read)

type Id = String

data PrimOp = Plus | Minus | Star | Slash | Equal
            deriving (Eq, Show, Read)

data Statement = ExprS Expr
               | FailS String
               | FunctionDef Id [Id] [ConstrainedId] [TypedId] Type Expr
               | ClassS TypeClassId TypeId [TypeConstraint] [(Id, Type)]
                 -- (InstanceS instanceName className instantiatedType methods)
               | InstanceS Id TypeClassId TypeScheme [(Id, Expr)]
               | SequenceS Statement Statement
  deriving (Eq, Show, Read)

data Expr = NumberE          Rational
          | PairE            Expr Expr
          | UnitE
          | FailE            String
          | InLeftE          Expr
          | InRightE         Expr
          | VarE             Id
          | PrimOpE          PrimOp
          | PrimOpAppE       Expr Expr Expr
            -- (TypeAppE procedure quantifiedTypes interfaces arguments)
          | AppE             Expr [Type] [Expr] [Expr]
          | MethodRef        Id Expr
          | MatchE           Expr [(Pattern, Expr)]
          | ArrayCompE       Id Expr Expr
          | SubscriptE       Expr Expr
          | SubscriptUpdateE Expr Expr Expr
          deriving (Eq, Show, Read)

data Instance = Instance { className :: TypeClassId
                         , instantiatedType :: TypeScheme
                         , methods :: [(Id, Value)]
                         }
              deriving (Eq, Show, Read)

data Function = Function { instanceParams :: [Id]
                         , valueParams :: [Id]
                         , functionType :: TypeScheme
                         , body :: Expr
                         }
              deriving (Eq, Show, Read)

type ArrayPtr = Integer

data Value = NumberV Rational
           | PairV Value Value
           | UnitV
           | InLeftV Value
           | InRightV Value
           | FunctionV Function
           | PrimOpV PrimOp
           | InstanceV Instance
           | ArrayV ArrayPtr
           deriving (Eq, Show, Read)

data Fail = Fail String
          deriving (Eq, Show, Read)

data Pattern = PatternVarP Id Type
             | PatternNumberP Rational
             | PatternPairP Pattern Pattern
             | PatternUnitP
             | PatternInLeftP Pattern
             | PatternInRightP Pattern
             deriving (Eq, Show, Read)
