module Tests (tests) where
------------------------------------------------------------------------------
import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck
import SPLData
import SPLEval

import qualified Data.Map.Strict as M

------------------------------------------------------------------------------
tests :: IO [Test]
tests = return
    [ testProperty "Succeeds" (\s -> length (take 5 s) <= 5)
    , checkEqual "Is 1 really 1?" 1 1
    , checkEqual "eval int" emptyEvalE (IntegerE 1) Right [(IntegerV 1)]
    , checkEqual "eval pair" emptyEvalE (PairE (IntegerE 1) (IntegerE 2)) Right [(PairV (IntegerV 1) (IntegerV 2))]
    , checkEqual "eval unit" emptyEvalE UnitE Right [UnitV]
    , checkEqual "eval fail" emptyEvalE (FailE "die") Left (Fail "die")
    , checkEqual "eval defined var"
                 evalE (Var "x") (M.singleton "x" UnitV)
                 (Right UnitV)
    , checkEqual "eval undefined var"
                  evalE (Var "x") (M.singleton "y" UnitV)
                  (Left (Fail "undefined variable x"))
    , checkEqual "eval undefined var"
                 evalE (Var "x") M.empty
                 (Left (Fail "undefined variable x"))
    , checkEqual "eval primop plus"
                 emptyEvalE (PrimOpAppE (PrimOpE Plus) (IntegerE 1) (IntegerE 2))
                 (IntegerV 3)
    , checkEqual "eval primop minus"
                 emptyEvalE (PrimOpAppE (PrimOpE Minus) (IntegerE 1) (IntegerE 2))
                 (IntegerV -1)
    , checkEqual "eval primop star"
                 emptyEvalE (PrimOpAppE (PrimOpE Star) (IntegerE 1) (IntegerE 2))
                 (IntegerV 2)
    , checkEqual "eval primop slash"
                 emptyEvalE (PrimOpAppE (PrimOpE Slash) (IntegerE 2) (IntegerE 2))
                 (IntegerV 1)
    , checkEqual "eval app" emptyEvalE (PrimOpE Slash (IntegerE 2) (IntegerE 2)) (IntegerV 1)
    ]

emptyEvalE :: Expr -> Value
emptyEvalE s = eval s M.empty

checkEqual :: (Eq a) => String -> a -> a -> Test
checkEqual name actual expected = Test t
  where
    t = TestInstance {
        run = case actual == expected of
           True -> return $ Finished Pass
           False -> return $ Finished $ Fail $ "Test " ++ name ++ " failed."
      , name = name
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t
      }
