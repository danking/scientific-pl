{-# LANGUAGE NamedFieldPuns #-}
module SPLEval where

import SPLData
import qualified Data.Map.Strict as M
import Data.Foldable (find)
import Data.Array.IArray ((!), listArray, (//), Array, bounds)
import Data.Ratio (denominator, numerator)
import Control.Monad.Reader

-------------------------------------------------------------------------------
-- Type Aliases

type Store = M.Map Id Value
type TypeStore = M.Map TypeId Type
type FailMsg1 a = a -> String
type FailMsg2 a b = a -> b -> String

-------------------------------------------------------------------------------
-- Eval

-- |The evalE function evaluates an expression into a value or a failure string.
evalE :: Expr -> Monae Value
evalE (NumberE n) = ret (NumberV n)
evalE (PairE left right) = do l <- evalE left
                              r <- evalE right
                              ret $ PairV l r
evalE (UnitE) = ret $ UnitV
evalE (FailE message) = die message
evalE (InLeftE left) = fmap InLeftV $ evalE left
evalE (InRightE right) = fmap InRightV $ evalE right
evalE (VarE identifier) = lookupVar identifier
evalE (PrimOpE primOp) = ret $ PrimOpV primOp
evalE (PrimOpAppE primOp left right) =
  do p <- evalE primOp
     l <- evalE left
     r <- evalE right
     handlePrimOp p l r
evalE (AppE procedure types instances arguments) =
  do p <- evalE procedure
     is <- mapM evalE instances
     args <- mapM evalE arguments
     applyE p types is args
evalE (MethodRef identifier inst) = getMethod identifier =<< (asInstance =<< evalE inst)
evalE (ArrayCompE identifier bound body) =
  buildArray =<< (asInteger errMessage) =<< evalE bound
  where errMessage v = "evalE: Expected an integer for the array size but instead received "
                       ++ v ++ "."
        arrayContents len = [ extendVar identifier (NumberV $ toRational index) $ evalE body
                            | index <- [0..len]]
        doneArrayContents :: Integer -> Monae [Value]
        doneArrayContents len = sequence $ arrayContents len
        buildArray len = fmap (ArrayV . (listArray (0,len-1))) $ doneArrayContents len
-- FIXME: MatchE
evalE (MatchE value patternResultPairs) = undefined
evalE (SubscriptE array index) =
  do array <- (asArray arrayFail) =<< evalE array
     index <- (asInteger numberFail) =<< evalE index
     maybeArraySubscript array index
  where arrayFail v = "evalE: Expected an array, but got " ++ v ++ "."
        numberFail v = "evalE: Expected a number, but got " ++ v ++ "."
evalE (SubscriptUpdateE array index value) =
  do array <- (asArray arrayFail) =<< evalE array
     index <- (asInteger numberFail) =<< evalE index
     value <- evalE value
     ret $ ArrayV $ array // [(index, value)]
  where arrayFail v = "evalE: Expected an array, but got " ++ v ++ "."
        numberFail v = "evalE: Expected a number, but got " ++ v ++ "."

evalS = undefined

-------------------------------------------------------------------------------
-- Apply

applyE :: Value -> [Type] -> [Value] -> [Value] -> Monae Value
applyE (FunctionV Function { instanceParams , valueParams , body })
       types instanceArgs valueArgs =
  clearEnv $ bindArgs $ evalE body
  where bindArgs c = bindMany instanceParams instanceArgs $ bindMany valueParams valueArgs c
applyE notAFunctionV _ _ _ =
  die $ "applyE: Expected a Function, got " ++ show notAFunctionV ++ "."

-------------------------------------------------------------------------------
-- Types

typeSchemeSubst :: TypeScheme -> [Type] -> [Value] -> Monae Type
typeSchemeSubst (QuantifiedConstraintsTS typeIds typeConstraints flatType) typeArguments instances =
  typeSubst flatType $ M.fromList $ zip typeIds typeArguments

typeSubst :: Type -> TypeStore -> Monae Type
typeSubst (NumberT) _ = ret NumberT
typeSubst (ProductT left right) ts = do left' <- typeSubst left ts
                                        right' <- typeSubst right ts
                                        ret $ ProductT left' right'
typeSubst (UnitT) _ = ret UnitT
typeSubst (SumT left right) ts = do left' <- (typeSubst left ts)
                                    right' <- (typeSubst right ts)
                                    ret (SumT left' right')
typeSubst (BottomT) _ = ret BottomT
typeSubst (ArrayT dimensionality t) ts = do t' <- (typeSubst t ts)
                                            ret (ArrayT dimensionality t')
typeSubst (FunctionT parameters body) ts = do parameters' <- (mapM (\x -> typeSubst x ts) parameters)
                                              body' <- (typeSubst body ts)
                                              ret (FunctionT parameters' body')
typeSubst (TypeVar identifier) ts = case M.lookup identifier ts of
  (Just t) -> ret t
  Nothing -> die $ "typeSubst: Type variable " ++ show identifier ++ "is unbound."

-------------------------------------------------------------------------------
-- Primitive Operations

handlePrimOp :: Value -> Value -> Value -> Monae Value
handlePrimOp (PrimOpV Plus)  (NumberV left) (NumberV right) = ret $ NumberV $ left + right
handlePrimOp (PrimOpV Minus) (NumberV left) (NumberV right) = ret $ NumberV $ left - right
handlePrimOp (PrimOpV Star)  (NumberV left) (NumberV right) = ret $ NumberV $ left * right
handlePrimOp (PrimOpV Slash) (NumberV left) (NumberV right) = ret $ NumberV $ left / right
handlePrimOp op _ _ = die $ "unknown primop" ++ show op

-------------------------------------------------------------------------------
-- Environment Things

bindMany :: [Id] -> [Value] -> Monae t -> Monae t
bindMany ids values c =
  foldl (\c (id, value) -> extendVar id value c) c (zip ids values)

-------------------------------------------------------------------------------
-- Coercions of Values

asArray :: FailMsg1 String -> Value -> Monae (Array Integer Value)
asArray _ (ArrayV arr) = ret arr
asArray message notAnArray = die $ message $ show notAnArray

asInteger :: FailMsg1 String -> Value -> Monae Integer
asInteger = thread maybeToInteger asRational

asInstance :: Value -> Monae Instance
asInstance (InstanceV i) = ret i
asInstance notAnInstance = die $ "Expected a type class instance but received a "
                                 ++ show notAnInstance ++ "."

asRational :: FailMsg1 String -> Value -> Monae Rational
asRational _ (NumberV n) = ret n
asRational message notANumber = die $ message $ show notANumber

maybeToInteger :: FailMsg1 String -> Rational -> Monae Integer
maybeToInteger message r = case denominator r of
  1 -> ret $ numerator r
  _ -> die $ message $ show r

-------------------------------------------------------------------------------
-- Data Helper Procedures

getMethod :: Id -> Instance -> Monae Value
getMethod identifier inst@(Instance { methods }) =
  case (find ((== identifier) . fst) methods) of
    Just (_, method) -> ret method
    Nothing -> die $ "getMethod: No method named " ++ show identifier
                     ++ " in instance " ++ show inst ++ "."

maybeArraySubscript :: Array Integer Value -> Integer -> Monae Value
maybeArraySubscript array index | (low <= index) && (index < high) = ret $ array ! index
                                | otherwise = die $ "evalE: array index, " ++ show index
                                            ++ ", out of bounds, ["
                                            ++ show low ++ "," ++ show high ++ "]"
  where (low, high) = bounds array


-------------------------------------------------------------------------------
-- Baby's First Monad

type Monae = ReaderT Store (Either Fail)

runMonae :: Monae t -> Store -> Either Fail t
runMonae x env = runReaderT x env

die :: String -> Monae a
die message = lift $ Left $ Fail message

ret :: a -> Monae a
ret = return

lookupVar :: Id -> Monae Value
lookupVar v = do store <- ask
                 case M.lookup v store of
                   Just val -> ret val
                   Nothing -> die $ "evalE: Variable " ++ show v ++ " is unbound."

extendVar :: Id -> Value -> Monae t -> Monae t
extendVar x v = local (\store -> M.insert x v store)

clearEnv :: Monae t -> Monae t
clearEnv = local (\_ -> M.empty)

-------------------------------------------------------------------------------
-- General Combinators

thread :: (Monad m) => (x -> b -> m c) -> (x -> a -> m b) -> x -> a -> m c
thread f g x = (f x) <=< (g x)