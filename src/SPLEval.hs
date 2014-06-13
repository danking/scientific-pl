{-# LANGUAGE NamedFieldPuns #-}
module SPLEval where

import SPLData
import qualified Data.Map.Strict as M
import Data.Foldable (find)
import Data.Array.IArray ((!), listArray, (//), Array, bounds)
import Data.Ratio (denominator, numerator)
import Control.Monad.Reader
import Control.Monad.State

-------------------------------------------------------------------------------
-- Type Aliases

data Heap = Heap { heapNextPtr :: ArrayPtr
                 , heapMap :: M.Map ArrayPtr (Array Integer Value)
                 }
emptyHeap :: Heap
emptyHeap = Heap { heapNextPtr = 0, heapMap = M.empty }

type Store = M.Map Id Value
emptyStore :: Store
emptyStore = M.empty

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
        doneArrayContents :: Integer -> Monae (Array Integer Value)
        doneArrayContents len = fmap (listArray (0,len-1)) $ sequence $ arrayContents len
        buildArray len = newArray =<< (doneArrayContents len)
evalE (MatchE value patternResultPairs) =
  do v <- evalE value
     findMatchAndEval v patternResultPairs
evalE (SubscriptE array index) =
  do array <- (asArray arrayFail) =<< evalE array
     index <- (asInteger numberFail) =<< evalE index
     maybeArraySubscript array index
  where arrayFail v = "evalE: Expected an array, but got " ++ v ++ "."
        numberFail v = "evalE: Expected a number, but got " ++ v ++ "."
evalE (SubscriptUpdateE array index value) =
  do arrayPtr <- (getArrayPtr arrayFail) =<< evalE array
     index <- (asInteger numberFail) =<< evalE index
     value <- evalE value
     updateArray arrayPtr index value
     ret $ UnitV
  where arrayFail v = "evalE: Expected an array, but got " ++ v ++ "."
        numberFail v = "evalE: Expected a number, but got " ++ v ++ "."

evalS = undefined

-------------------------------------------------------------------------------
-- Apply

applyE :: Value -> [Type] -> [Value] -> [Value] -> Monae Value
applyE (FunctionV Function { instanceParams , valueParams , body })
       _ instanceArgs valueArgs =
  clearEnv $ bindArgs $ evalE body
  where bindArgs c = bindMany instanceParams instanceArgs $ bindMany valueParams valueArgs c
applyE notAFunctionV _ _ _ =
  die $ "applyE: Expected a Function, got " ++ show notAFunctionV ++ "."

-------------------------------------------------------------------------------
-- Match

findMatchAndEval :: Value -> [(Pattern, Expr)] -> Monae Value
findMatchAndEval v pairs =
  do (pattern, consequent) <- getPair v pairs
     bindingPairs <- ret $ getPatternBindings v pattern []
     extendVars bindingPairs $ evalE consequent

getPair :: Value -> [(Pattern, Expr)] -> Monae (Pattern, Expr)
getPair v pairs =
  case find ((patternMatch v) . fst) pairs of
    Just pair -> ret pair
    Nothing -> die $ "evalE: no matching pattern for "
                     ++ show v ++ " in "
                     ++ show pairs

patternMatch :: Value -> Pattern -> Bool
patternMatch UnitV PatternUnitP = True
patternMatch _ (PatternVarP _ _) = True
patternMatch (PairV left right) (PatternPairP left' right') =
  patternMatch left left' && patternMatch right right'
patternMatch (InLeftV v) (PatternInLeftP p) = patternMatch v p
patternMatch (InRightV v) (PatternInRightP p) = patternMatch v p

getPatternBindings :: Value -> Pattern -> [(Id, Value)] -> [(Id, Value)]
getPatternBindings v (PatternVarP id _) bindings = (id, v) : bindings
getPatternBindings (PairV left right) (PatternPairP left' right') bindings =
  (getPatternBindings right right' (getPatternBindings left left' bindings))
getPatternBindings (InLeftV v) (PatternInLeftP p) bindings =
  getPatternBindings v p bindings
getPatternBindings (InRightV v) (PatternInRightP p) bindings =
  getPatternBindings v p bindings


-------------------------------------------------------------------------------
-- Types

typeSchemeSubst :: TypeScheme -> [Type] -> [Value] -> Monae Type
typeSchemeSubst (QuantifiedConstraintsTS typeIds _ flatType) typeArguments _ =
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
handlePrimOp (PrimOpV Equal) (NumberV left) (NumberV right) =
  ret $ case left == right of
    True -> InLeftV UnitV
    False -> InRightV UnitV
handlePrimOp op _ _ = die $ "unknown primop" ++ show op

-------------------------------------------------------------------------------
-- Environment Things

bindMany :: [Id] -> [Value] -> Monae t -> Monae t
bindMany ids values c =
  foldl (\c (id, value) -> extendVar id value c) c (zip ids values)

-------------------------------------------------------------------------------
-- Coercions of Values

asArray :: FailMsg1 String -> Value -> Monae (Array Integer Value)
asArray _ (ArrayV ptr) = getArray ptr
asArray message notAnArray = die $ message $ show notAnArray

getArrayPtr :: FailMsg1 String -> Value -> Monae ArrayPtr
getArrayPtr _ (ArrayV ptr) = ret ptr
getArrayPtr message notAnArray = die $ message $ show notAnArray

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
maybeArraySubscript array index | (low <= index) && (index <= high) = ret $ array ! index
                                | otherwise = die $ "evalE: array index, " ++ show index
                                            ++ ", out of bounds, ["
                                            ++ show low ++ "," ++ show high ++ "]"
  where (low, high) = bounds array


-------------------------------------------------------------------------------
-- Baby's First Monad

data Environments = Environments { globalEnv :: Store
                                 , localEnv :: Store
                                 }
type Monae = StateT Heap (ReaderT Environments (Either Fail))

runMonae :: Monae t -> Heap -> Store -> Store -> Either Fail (t, Heap)
runMonae x heap gEnv lEnv = runReaderT (runStateT x heap) $ Environments gEnv lEnv

die :: String -> Monae a
die message = lift $ lift $ Left $ Fail message

ret :: a -> Monae a
ret = return

lookupVar :: Id -> Monae Value
lookupVar v = do env <- ask
                 case M.lookup v $ localEnv env of
                   Just val -> ret val
                   Nothing -> case M.lookup v $ globalEnv env of
                                Just val -> ret val
                                Nothing -> die $ "evalE: Variable "
                                               ++ show v
                                               ++ " is unbound."
extendVar :: Id -> Value -> Monae t -> Monae t
extendVar x v =
  local (\env -> Environments { globalEnv = globalEnv env
                              , localEnv = M.insert x v $ localEnv env
                              })

extendVars :: [(Id, Value)] -> Monae t -> Monae t
extendVars bindings =
  local (\env -> Environments { globalEnv = globalEnv env
                              , localEnv = M.union (M.fromList bindings) $ localEnv env
                              })

getArray :: ArrayPtr -> Monae (Array Integer Value)
getArray ptr = do heap <- get
                  case M.lookup ptr $ heapMap heap of
                    Just arr -> ret arr
                    Nothing -> die $ "evalE: INTERNAL ERROR MY HAIR IS ON FIRE!!!"

newArray :: (Array Integer Value) -> Monae Value
newArray initialValue = do heap <- get
                           put Heap { heapMap = M.insert (heapNextPtr heap) initialValue $ heapMap heap
                                    , heapNextPtr = 1 + (heapNextPtr heap)
                                    }
                           ret $ ArrayV $ heapNextPtr heap

updateArray :: ArrayPtr -> Integer -> Value -> Monae ()
updateArray ptr index value =
  do heap <- get
     case M.lookup ptr (heapMap heap) of
       Just array -> put $ Heap { heapMap = M.insert ptr (array // [(index, value)]) $ heapMap heap
                                , heapNextPtr = heapNextPtr heap
                                }
       Nothing -> die $ "evalE: INTERNAL ERROR MY HAIR IS STILL ON FIRE!"

clearEnv :: Monae t -> Monae t
clearEnv = local (\env -> Environments { globalEnv = globalEnv env
                                       , localEnv = emptyStore
                                       })
-------------------------------------------------------------------------------
-- General Combinators

thread :: (Monad m) => (x -> b -> m c) -> (x -> a -> m b) -> x -> a -> m c
thread f g x = (f x) <=< (g x)
