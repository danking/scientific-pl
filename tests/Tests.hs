module Tests (tests) where
------------------------------------------------------------------------------
import qualified Distribution.TestSuite as TS
import Test.HUnit as H
import qualified Distribution.TestSuite.HUnit as HU
import SPLData
import SPLEval

import qualified Data.Map.Strict as M

------------------------------------------------------------------------------
tests :: IO [TS.Test]
tests = return
    [ TS.testGroup "eval"
      [ checkEqual "int" (emptyEvalE (NumberE 1)) (Right (NumberV 1))
      , checkEqual "pair"
                   (emptyEvalE (PairE (NumberE 1) (NumberE 2)))
                   (Right (PairV (NumberV 1) (NumberV 2)))
      , checkEqual "unit"
                   (emptyEvalE UnitE)
                   (Right UnitV)
      , checkEqual "fail"
                   (emptyEvalE (FailE "die"))
                   (Left (Fail "die"))
      , checkEqual "defined var"
                   (evalE (VarE "x") (M.singleton "x" UnitV))
                   (Right UnitV)
      , checkEqual "undefined var"
                   (evalE (VarE "x") (M.singleton "y" UnitV))
                   (Left (Fail "evalE: Variable \"x\" is unbound."))
      , checkEqual "undefined var"
                   (evalE (VarE "x") M.empty)
                   (Left (Fail "evalE: Variable \"x\" is unbound."))
      , checkEqual "primop plus"
                   (emptyEvalE (PrimOpAppE (PrimOpE Plus) (NumberE 1) (NumberE 2)))
                   (Right (NumberV 3))
      , checkEqual "primop minus"
                   (emptyEvalE (PrimOpAppE (PrimOpE Minus) (NumberE 1) (NumberE 2)))
                   (Right (NumberV $ negate 1))
      , checkEqual "primop star"
                   (emptyEvalE (PrimOpAppE (PrimOpE Star) (NumberE 1) (NumberE 2)))
                   (Right (NumberV 2))
      , checkEqual "primop slash"
                   (emptyEvalE (PrimOpAppE (PrimOpE Slash) (NumberE 2) (NumberE 2)))
                   (Right (NumberV 1))
      , checkEqual "app (id 3)"
                   (evalE (AppE (VarE "id") [NumberT] [] [(NumberE 3)]) envWithId)
                   (Right (NumberV 3))
      , checkEqual "app (show 3)"
                   (evalE (AppE (MethodRef "show" (VarE "intShow")) [NumberT] [] [(NumberE 3)])
                          envWithIntShow)
                   (Right (UnitV))
      , checkEqual "app (id 1)"
                   (evalE (AppE (VarE "id") [NumberT] [] [(NumberE 1)])
                          envWithId)
                   (Right (NumberV 1))
      , checkEqual "app with evil shadowed id"
                   (evalE (AppE (VarE "id") [NumberT] [] [(NumberE 1)])
                          envShadowingId)
                   (Right UnitV)
      ]
    ]

emptyEvalE :: Expr -> Either Fail Value
emptyEvalE s = evalE s M.empty

checkEqual :: (Eq a, Show a) => String -> a -> a -> TS.Test
checkEqual name expected actual =
  HU.test name $ H.TestCase $ H.assertEqual name actual expected

intShowInstance = (InstanceV "intShow"
                             "Show"
                             NumberT
                             [("show", (FunctionV []
                                                  ["x"]
                                                  (QuantifiedConstraintsTS
                                                   []
                                                   []
                                                   (FunctionT [NumberT]
                                                              UnitT))
                                                  (UnitE)))])

envWithIntShow :: Store
envWithIntShow = M.singleton "intShow" intShowInstance

envWithId :: Store
envWithId = M.singleton "id"
                        (FunctionV []
                                   ["x"]
                                   (QuantifiedConstraintsTS
                                    ["a"]
                                    []
                                    (FunctionT [(TypeVar "a")]
                                               (TypeVar "a")))
                                   (VarE "x"))

envShadowingId :: Store
envShadowingId = M.insert "id"
                          (FunctionV []
                                     ["x"]
                                     (QuantifiedConstraintsTS []
                                                              []
                                                              (FunctionT [NumberT] UnitT))
                                     (UnitE))
                          envWithId