{-#LANGUAGE GADTs-}
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

data Atom
data Fail
data Expr
data Statement

data Term a = IntegerE         :: Integer -> Term Atom
            | PrimOpE          :: PrimOp -> Term a -> Term a -> Term Atom
            | PairE            :: Term a -> Term a -> Term Atom
            | UnitE            :: Term Atom
            | InLeftE          :: Term a -> Term Atom
            | InRightE         :: Term a -> Term Atom
              -- Fail
            | FailE            :: String -> Term Fail
              -- Expr
            | VarE             :: Id -> Term Expr
            | TypeInstantiateE :: Term a -> [Type] -> [Term a] -> Term Expr
            | AppE             :: Term a -> [Term a] -> Term Expr
            | MatchE           :: Term a -> [(Pattern, Term a)] -> Term Expr
            | SubscriptE       :: Term a -> Term a -> Term Expr
            | SubscriptUpdateE :: Term a -> Term a -> Term a -> Term Expr
              -- Statements
            | FunctionDefS     :: Id -> [Id] -> TypeScheme -> (Term a) -> Term Statement
            | ClassS           :: TypeConstraintID -> TypeId -> [TypeConstraint] -> [(Id, Type)] -> Term Statement
            | InstanceS        :: Id -> TypeConstraintID -> TypeId -> [(Id, (Term a))] -> Term Statement
            | SequenceS        :: (Term a) -> (Term a) -> Term Statement
            deriving (Eq, Show, Read)

data Pattern = PatternVarP Id Type
             | PatternTupleP Pattern Pattern
             | PatternUnitP
             | PatternInLeftP Pattern
             | PatternInRightP Pattern
             deriving (Eq, Show, Read)
