import Coloring
import Fractals
import Data.Complex
import Test.HUnit

--Authors: Petter Eckerbom, Axel Gällstedt, Thomas Mathiassen

test1 = TestCase $ assertEqual "No diverge" Nothing (iterationCheck 0 127 mandelSet 2)
test2 = TestCase $ assertEqual "Diverge after 3" (Just 3) (iterationCheck ((-0.56) :+ 0.89) 51 mandelSet 2)
test3 = TestCase $ assertEqual "coordToComp top-right corner" (2.0 :+ 2.0) (coordToComp (25, 25) ((50, 50), (0 :+ 0), 0.5))
test4 = TestCase $ assertEqual "coordToComp bottom-left corner" ((-2.0) :+ (-2.0)) (coordToComp ((-25), (-25)) ((50, 50), (0 :+ 0), 0.5))
test5 = TestCase $ assertEqual "coordToComp center" (0.0 :+ 0.0) (coordToComp (0, 0) ((50, 50), (0 :+ 0), 0.5))
test6 = TestCase $ assertEqual "coordToComp displacement" (1.5 :+ 2.0) (coordToComp (5, 5) ((20, 20), (1 :+ 1.5), 1))
test7 = TestCase $ assertEqual "stepTo positive" 4 (stepTo 1 7 3)
test8 = TestCase $ assertEqual "stepTo negative" 3 (stepTo 7 2 4)
test9 = TestCase $ assertEqual "stepTo no overshoot" 2 (stepTo 1 2 30)
test10 = TestCase $ assertEqual "twoCGradient (0,3,0,255) (0,0,3,255) 2" [(0,3,0,255), (0,1,2,255), (0,0,3,255)] (twoCGradient (0,3,0,255) (0,0,3,255) 2)
test11 = TestCase $ assertEqual "gradient of single element list" [(1,2,3,4)] (gradient [(1,2,3,4)] 2)
test12 = TestCase $ assertEqual "gradient [(0,0,0,255),(1,2,3,255),(6,5,4,255)] 2" [(0,0,0,255),(1,2,2,255),(1,2,3,255),(3,4,4,255),(5,5,4,255),(6,5,4,255)] (gradient [(0,0,0,255),(1,2,3,255),(6,5,4,255)] 2)

runTests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12]
