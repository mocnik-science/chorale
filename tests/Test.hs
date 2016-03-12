import Chorale.Test.Common as ChoraleTestCommon

import Test.Framework

main :: IO ()
main = defaultMainWithArgs testsToRun ["--maximum-generated-tests=1000"]

testsToRun :: [Test]
testsToRun = ChoraleTestCommon.tests
