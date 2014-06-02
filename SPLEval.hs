module SPLEval where

import SPLData
import qualified Data.Map.Strict as M
import Control.Applicative
import Data.Foldable (find)

type Store = M.Map Id Value
type TypeStore = M.Map TypeId Type

-- |The evalE function evaluates an expression into a value or a failure string.
evalE :: Expr -> Store -> Either Fail Value
evalE (NumberE n) _ = Right (NumberV n)
evalE (PairE left right) s = pure PairV <*> (evalE left s)
                                        <*> (evalE right s)
evalE (UnitE) _ = Right $ UnitV
evalE (FailE message) _ = Left $ Fail message
evalE (InLeftE left) s = fmap InLeftV $ evalE left s
evalE (InRightE right) s = fmap InRightV $ evalE right s
evalE (VarE identifier) s = case (M.lookup identifier s) of
  (Just value) -> Right value
  Nothing -> Left $ Fail $ "evalE: Variable " ++ show identifier ++ " is unbound."
evalE (PrimOpE primOp) _ = Right $ PrimOpV primOp
evalE (PrimOpAppE primOp left right) s = do p <- (evalE primOp s)
                                            l <- (evalE left s)
                                            r <- (evalE right s)
                                            handlePrimOp p l r
evalE (AppE procedure types instances arguments) s =
  do p <- (evalE procedure s)
     is <- mapM (\e -> evalE e s) instances
     args <- mapM (\e -> evalE e s) arguments
     applyE p types is args s
evalE (MethodRef identifier inst) s =
  do inst <- evalE inst s
     case inst of
       (InstanceV _ _ _ pairs) -> case (find ((== identifier) . fst) pairs) of
                                    (Just (_, procedure)) -> Right procedure
                                    Nothing -> Left $ Fail $ "No method named "
                                                           ++ show identifier
                                                           ++ "in instance "
                                                           ++ show inst
       somethingElse -> Left $ Fail $ "evalE: expected a typeclass instance, but got "
                                      ++ show somethingElse
evalE (MatchE value patternResultPairs) s = undefined
evalE (SubscriptE array index) s = undefined
evalE (SubscriptUpdateE array index value) s = undefined

evalS = undefined

applyE :: Value -> [Type] -> [Value] -> [Value] -> Store -> Either Fail Value
applyE (FunctionV instanceParameters valueParameters typeScheme body)
       types
       instanceArguments
       valueArguments
       s = evalE body s'
  where s' = bindMany instanceParameters instanceArguments $ bindMany valueParameters valueArguments s
applyE notAFunctionV _ _ _ _ =
  Left $ Fail $ "applyE: Expected a Function, got " ++ show notAFunctionV ++ "."

-- help
typeSchemeSubst :: TypeScheme -> [Type] -> [Value] -> Either Fail Type
typeSchemeSubst (QuantifiedConstraintsTS typeIds typeConstraints flatType) typeArguments instances =
  typeSubst flatType $ M.fromList $ zip typeIds typeArguments

typeSubst :: Type -> TypeStore -> Either Fail Type
typeSubst (NumberT) _ = Right NumberT
typeSubst (ProductT left right) ts = do left' <- typeSubst left ts
                                        right' <- typeSubst right ts
                                        Right $ ProductT left' right'
typeSubst (UnitT) _ = Right UnitT
typeSubst (SumT left right) ts = do left' <- (typeSubst left ts)
                                    right' <- (typeSubst right ts)
                                    Right (SumT left' right')
typeSubst (BottomT) _ = Right BottomT
typeSubst (ArrayT dimensionality t) ts = do t' <- (typeSubst t ts)
                                            Right (ArrayT dimensionality t')
typeSubst (FunctionT parameters body) ts = do parameters' <- (mapM (\x -> typeSubst x ts) parameters)
                                              body' <- (typeSubst body ts)
                                              Right (FunctionT parameters' body')
typeSubst (TypeVar identifier) ts = case M.lookup identifier ts of
  (Just t) -> Right t
  Nothing -> Left $ Fail $ "typeSubst: Type variable " ++ show identifier ++ "is unbound."

bindMany :: [Id] -> [Value] -> Store -> Store
bindMany ids values store = foldr (\(identifier, v) s -> M.insert identifier v s)
                                  store
                                  (zip ids values)

handlePrimOp :: Value -> Value -> Value -> Either Fail Value
handlePrimOp (PrimOpV Plus)  (NumberV left) (NumberV right) = Right $ NumberV $ left + right
handlePrimOp (PrimOpV Minus) (NumberV left) (NumberV right) = Right $ NumberV $ left - right
handlePrimOp (PrimOpV Star)  (NumberV left) (NumberV right) = Right $ NumberV $ left * right
handlePrimOp (PrimOpV Slash) (NumberV left) (NumberV right) = Right $ NumberV $ left / right
handlePrimOp op _ _ = Left $ Fail $ "unknown primop" ++ show op
