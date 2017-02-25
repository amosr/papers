
module Test.Fuse.DiamondAlt where
import Machine.Transform.Fuse
import Machine.Transform.StripLabels
import Machine.Combinator
import Machine.Execute
import Machine.Pretty
import qualified Data.Map       as Map
import qualified Data.Set       as Set


testFuseDiamondAltOk
 = putStrLn $ show $ pretty
 $ let  
        cIn1     = Channel "cIn1" TInt
        cIn2     = Channel "cIn2" TInt
        cIn3     = Channel "cIn3" TInt

        cA1      = Channel "cA1"  TInt
        cA2      = Channel "cA2"  TInt
        cOut     = Channel "cOut"  TInt

        pFused
         = evalNew 
         $ do   
                pZip3   <- mkZip  cA1  cA2  cOut
                pAlt1   <- mkAlt2_blk cIn1 cIn2 cA1
                let pAlt13  = fusePair' [pZip3, pAlt1] pZip3 pAlt1

                pAlt2   <- mkAlt2_blk  cIn2 cIn3 cA2
                let pZipped = fusePair' [pZip3, pAlt1, pAlt2] pAlt13 pAlt2

                pFused'         <- fmap fst $ stripLabels "F" pZipped
                return pFused'
   in   pFused


testFuseDiamondAltFail
 = putStrLn $ show $ pretty
 $ let  
        cIn1     = Channel "cIn1" TInt
        cIn2     = Channel "cIn2" TInt
        cIn3     = Channel "cIn3" TInt

        cA1      = Channel "cA1"  TInt
        cA2      = Channel "cA2"  TInt
        cOut     = Channel "cOut"  TInt

        pFused
         = evalNew 
         $ do   
                pAlt1             <- mkAlt2_blk cIn1 cIn2 cA1
                pAlt2             <- mkAlt2_blk cIn2 cIn3 cA2
                let pAlt12      = fusePair' [pAlt1, pAlt2] pAlt1 pAlt2

                pZip              <- mkZip  cA1 cA2  cOut
                let pZipped     = fusePair' [pAlt1, pAlt2, pZip] pAlt12 pZip

                pFused'         <- fmap fst $ stripLabels "F" pZipped
                return pFused'
   in   pFused


fusePair' psErr p1 p2
 = case fusePair p1 p2 of
        Right pp        -> pp
        Left  ll        -> fuseError psErr p1 p2 ll

fuseError psErr p1 p2 (LabelJoint (l1, cs1) (l2, cs2))
 = error $ unlines
 [ "Fusion Failure"
 , "  Process A state."
 , "    labelA = " ++ (show $ pretty l1)
 , "    insrA  = " ++ (show $ pretty $ lookup l1 (processBlocks p1))
 , "    csInA  = " ++ (show $ pretty $ cs1)
 , ""
 , "  Process B state."
 , "    labelB = " ++ (show $ pretty l2)
 , "    instrB = " ++ (show $ pretty $ lookup l2 (processBlocks p2))
 , "    csInB  = " ++ (show $ pretty $ cs2)
 , ""
 ]
 ++ unlines (map (show . pretty) psErr)

