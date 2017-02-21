
module Test.Fuse.GroupMerge where
import Machine.Transform.Fuse
import Machine.Transform.StripLabels
import Machine.Combinator
import Machine.Execute
import Machine.Pretty
import Text.Show.Pretty
import Text.PrettyPrint.Leijen
import qualified Data.Map       as Map
import qualified Data.Set       as Set
import System.IO

-------------------------------------------------------------------------------
testFuseSplitGroupMerge
 = do   let (_pGroup, _pMerge, lsDef, pResult')
                = evalNew $ testFuseSplitGroupMerge'

        displayIO stdout
         $ renderPretty 0 100
         $ vcat [ pretty lsDef,         empty
                , pretty pResult',      empty]


testFuseSplitGroupMerge'
 = do 
        let cIn1    = Channel "In2"    TInt
        let cIn2    = Channel "In1"    TInt
        let cUniq   = Channel "Uniq1"  TInt
        let cMerged = Channel "Merged" TInt

        pGroup  <- mkGroup cIn1 cUniq
        pMerge  <- mkMerge cIn1 cIn2 cMerged

        let Right pResult
                = fusePair pGroup pMerge

        let (pResult', lsDef)
                = evalNew 
                $ stripLabels "F" pResult

        return $ (pGroup, pMerge, lsDef, pResult')

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
