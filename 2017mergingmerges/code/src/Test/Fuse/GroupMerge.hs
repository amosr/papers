
module Test.Fuse.GroupMerge where
import Machine.Transform.Fuse
import Machine.Combinator
import Machine.Execute
import Text.Show.Pretty
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-------------------------------------------------------------------------------
testFuseSplitGroupMerge
 = putStr $ ppShow 
 $ let  
        cIn1    = Channel "In2"    TInt
        cIn2    = Channel "In1"    TInt
        cUniq   = Channel "Uniq1"  TInt
        cMerged = Channel "Merged" TInt

        (pGroup, pMerge)
         = evalNew 
         $ do   pGroup  <- mkGroup cIn1 cUniq
                pMerge  <- mkMerge cIn1 cIn2 cMerged
                return (pGroup, pMerge)

        result -- Right pFused
         = fusePair pGroup pMerge

   in   (pGroup, pMerge, result)

{-   
        cvsInput
         =  Map.fromList
                [ (cIn1, map VInt [1, 3, 3, 6, 7, 7, 8])
                , (cIn2, map VInt [1, 2, 3, 4, 5, 6, 7, 8]) ]

        cvsOutput
         = Map.fromList
                [ (cUniq,  [])
                , (cMerged, []) ]

        (_inputs, _processes, actions)
         = execute cvsInput cvsOutput [pFused] []

   in   (pGroup, pMerge, pFused, actions)
-}
