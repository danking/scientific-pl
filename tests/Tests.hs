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
      [ checkEqual "int"
                   (testEmptyEvalE (NumberE 1))
                   (Right (NumberV 1))
      , checkEqual "pair"
                   (testEmptyEvalE (PairE (NumberE 1) (NumberE 2)))
                   (Right (PairV (NumberV 1) (NumberV 2)))
      , checkEqual "unit"
                   (testEmptyEvalE UnitE)
                   (Right UnitV)
      , checkEqual "fail"
                   (testEmptyEvalE (FailE "die"))
                   (Left (Fail "die"))
      , checkEqual "defined var"
                   (testEvalE (VarE "x") (M.singleton "x" UnitV))
                   (Right UnitV)
      , checkEqual "undefined var"
                   (testEvalE (VarE "x") (M.singleton "y" UnitV))
                   (Left (Fail "evalE: Variable \"x\" is unbound."))
      , checkEqual "undefined var"
                   (testEvalE (VarE "x") M.empty)
                   (Left (Fail "evalE: Variable \"x\" is unbound."))
      , checkEqual "primop plus"
                   (testEmptyEvalE (PrimOpAppE (PrimOpE Plus) (NumberE 1) (NumberE 2)))
                   (Right (NumberV 3))
      , checkEqual "primop minus"
                   (testEmptyEvalE (PrimOpAppE (PrimOpE Minus) (NumberE 1) (NumberE 2)))
                   (Right (NumberV $ negate 1))
      , checkEqual "primop star"
                   (testEmptyEvalE (PrimOpAppE (PrimOpE Star) (NumberE 1) (NumberE 2)))
                   (Right (NumberV 2))
      , checkEqual "primop slash"
                   (testEmptyEvalE (PrimOpAppE (PrimOpE Slash) (NumberE 2) (NumberE 2)))
                   (Right (NumberV 1))
      , checkEqual "app (id 3)"
                   (testEvalE (AppE (VarE "id") [NumberT] [] [(NumberE 3)]) envWithId)
                   (Right (NumberV 3))
      , checkEqual "app (show 3)"
                   (testEvalE (AppE (MethodRef "show" (VarE "intShow")) [NumberT] [] [(NumberE 3)])
                          envWithIntShow)
                   (Right (UnitV))
      , checkEqual "app (id 1)"
                   (testEvalE (AppE (VarE "id") [NumberT] [] [(NumberE 1)])
                          envWithId)
                   (Right (NumberV 1))
      , checkEqual "app with evil shadowed id"
                   (testEvalE (AppE (VarE "id") [NumberT] [] [(NumberE 1)])
                          envShadowingId)
                   (Right UnitV)
      , checkEqual "array comprehension"
                   (map (\i -> (testEmptyEvalE (SubscriptE natArrayUpToFive (NumberE i)))) [0,1,2,3,4])
                   (map (\i -> (Right (NumberV i))) [0,1,2,3,4])
      , checkEqual "bad array comprehension"
                   (testEmptyEvalE (ArrayCompE "i" UnitE (VarE "i")))
                   (Left (Fail "evalE: Expected an integer for the array size but instead received UnitV."))
      , checkEqual "subscript"
                   (testEmptyEvalE (SubscriptE natArrayUpToFive (NumberE 1)))
                   (Right (NumberV 1))
      , checkEqual "bad subscript too small"
                   (testEmptyEvalE (SubscriptE natArrayUpToFive (NumberE $ negate 1)))
                   (Left (Fail "evalE: array index, -1, out of bounds, [0,4]"))
      , checkEqual "bad subscript too large"
                   (testEmptyEvalE (SubscriptE natArrayUpToFive (NumberE 5)))
                   (Left (Fail "evalE: array index, 5, out of bounds, [0,4]"))
      -- FIXME: this should actually mutate the environment
      -- , checkEqual "subscript update"
      --              (testEmptyEvalE (SubscriptUpdateE natArrayUpToFive (NumberE 1) (NumberE 10)))
      --              (Right (ArrayV $ fmap NumberV $ listArray (0,4) [0,10,2,3,4]))
      ]
    ]

natArrayUpToFive :: Expr
natArrayUpToFive = (ArrayCompE "i" (NumberE 5) (VarE "i"))

testEmptyEvalE :: Expr -> Either Fail Value
testEmptyEvalE s = testEvalE s M.empty

testEvalE :: Expr -> Store -> Either Fail Value
testEvalE e s = fmap fst $ runMonae (evalE e) emptyHeap M.empty s

checkEqual :: (Eq a, Show a) => String -> a -> a -> TS.Test
checkEqual name actual expected =
  HU.test name $ H.TestCase $ H.assertEqual name expected actual

intShowInstance = (InstanceV
                   (Instance { className = "Show"
                             , instantiatedType = (QuantifiedConstraintsTS [] [] NumberT)
                             , methods =
                               [("show", (FunctionV
                                          (Function { instanceParams = []
                                                    , valueParams = ["x"]
                                                    , functionType = (QuantifiedConstraintsTS [] []
                                                                      (FunctionT [NumberT] UnitT))
                                                    , body = (UnitE)})))]}))

envWithIntShow :: Store
envWithIntShow = M.singleton "intShow" intShowInstance

envWithId :: Store
envWithId = M.singleton "id"
            (FunctionV
             (Function { instanceParams = []
                       , valueParams = ["x"]
                       , functionType = (QuantifiedConstraintsTS
                                         ["a"]
                                         []
                                         (FunctionT [(TypeVar "a")]
                                          (TypeVar "a")))
                       , body = (VarE "x")}))

envShadowingId :: Store
envShadowingId = M.insert "id"
                 (FunctionV
                  (Function { instanceParams = []
                            , valueParams = ["x"]
                            , functionType = (QuantifiedConstraintsTS []
                                              []
                                              (FunctionT [NumberT] UnitT))
                            , body = (UnitE)}))
                 envWithId