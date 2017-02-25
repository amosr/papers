

module Test.Fuse.DiamondMerge where
import Machine.Transform.Fuse
import Machine.Transform.StripLabels
import Machine.Combinator
import Machine.Execute
import Machine.Pretty
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-------------------------------------------------------------------------------
-- | Test evaluation of complex network.
--
--
--    cIn1      cIn2       cIn3
--     \      /      \     /
--      (merge)     (merge)
--          |         |
--         cM1       cM2
--          |         |
--           \       /  
--            (merge)
--               |
--              cOut
--
testFuseDiamondMerge
 = let
        cIn1    = Channel "cIn1"  TInt
        cIn2    = Channel "cIn2"  TInt
        cIn3    = Channel "cIn3"  TInt
        cM1     = Channel "cM1"   TInt
        cM2     = Channel "cM2"   TInt
        cOut    = Channel "cOut" TInt

        pFused
         = evalNew 
         $ do   
                p1_merge1       <- mkMerge cM1  cM2  cOut
                p2_merge2       <- mkMerge cIn1 cIn2 cM1
                let Right p12_merge12    = fusePair p1_merge1   p2_merge2

                p3_merge3       <- mkMerge cIn2 cIn3 cM2
                let Right p123_merge123  = fusePair  p12_merge12 p3_merge3

                pFused'         <- fmap fst $ stripLabels "F" p123_merge123

                return pFused'

        cvsInput
         =  Map.fromList
                [ (cIn1, [VInt 1, VInt 2, VInt 3, VInt 4, VInt 5])
                , (cIn2, [VInt 1, VInt 2, VInt 3, VInt 4, VInt 5]) 
                , (cIn3, [VInt 1, VInt 2, VInt 3, VInt 4, VInt 5]) ]

        cvsOutput
         = Map.fromList
                [ (cM1, [])
                , (cM2, []) ]

   in do
        putStrLn $ show $ pretty pFused

        executeTraceIO 100 cvsInput cvsOutput [pFused] []

