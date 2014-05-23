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

data Atom = IntegerE Integer
          | PrimOpE Id Expr Expr
          | PairE Expr Expr
          | UnitE
          | InLeftE Expr
          | InRightE Expr

data Expr = VarE Id
          | TypeInstantiateE Expr [Type] [Expr]
          | AppE Expr [Expr]
          | MatchE Expr [(Pattern, Expr)]
          | SubscriptE Expr Expr
          | SubscriptUpdateE Expr Expr Expr
            -- base types
          | Atom
          | FailE String
          deriving (Eq, Show, Read)

data Pattern = PatternVarP Id Type
             | PatternTupleP Pattern Pattern
             | PatternUnitP
             | PatternInLeftP Pattern
             | PatternInRightP Pattern
             deriving (Eq, Show, Read)

data Statement = Expr
               | FunctionDefinitionS Id [Id] TypeScheme Expr
               | ClassS TypeConstraintID TypeId [TypeConstraint] [(Id, Type)]
               | InstanceS Id TypeConstraintID TypeId [(Id, Expr)]
               | SequenceS Expr Expr
               deriving (Eq, Show, Read)
