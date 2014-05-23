module Tests (tests) where
------------------------------------------------------------------------------
import Distribution.TestSuite.QuickCheck


------------------------------------------------------------------------------
tests :: IO [Test]
tests = return
    [ testProperty "Succeeds" True
    ]
