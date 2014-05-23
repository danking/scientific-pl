module SPLEval where

import SPLData
import Data.Map.Strict

type Store = Map Id Atom

type Result = Either Fail [Value]

evalE :: Expr -> Store -> Value
eval (IntegerE i) = IntegerV i
eval (PrimOpE op left right) = handlePrimOp op (eval left) (eval right)
eval _ = undefined

evalS :: Statement -> Store -> Value

handlePrimOp :: PrimOp -> Atom -> Atom
handlePrimOp Plus  (IntegerE left) (IntegerE right) = left + right
handlePrimOp Minus (IntegerE left) (IntegerE right) = left - right
handlePrimOp Star  (IntegerE left) (IntegerE right) = left * right
handlePrimOp Slash (IntegerE left) (IntegerE right) = left / right
handlePrimOp op left right = Fail
