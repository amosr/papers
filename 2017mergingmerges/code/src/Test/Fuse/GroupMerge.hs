
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
 = do   let (lsDef, pFused, lsActions)
                = evalNew $ testFuseSplitGroupMerge'

        displayIO stdout
         $ renderPretty 0 100
         $ vcat [ pretty lsDef
                , empty
                , pretty pFused
                , empty
                , pretty (vcat $ map pretty lsActions)
                , empty ]


testFuseSplitGroupMerge'
 = do 
        let cIn1    = Channel "cIn2"    TInt
        let cIn2    = Channel "cIn1"    TInt
        let cUniq   = Channel "cUniq1"  TInt
        let cMerged = Channel "cMerged" TInt

        pGroup  <- mkGroup cIn1 cUniq
        pMerge  <- mkMerge cIn1 cIn2 cMerged

        let Right pResult
                = fusePair pGroup pMerge

        let (pFused, lsDef)
                = evalNew 
                $ stripLabels "F" pResult

        let cvsInput
                =  Map.fromList
                [ (cIn1, map VInt [1, 3, 3, 6, 7, 7, 8])
                , (cIn2, map VInt [1, 2, 3, 4, 5, 6, 7, 8]) ]

        let cvsOutput
                = Map.fromList
                [ (cUniq,   [])
                , (cMerged, []) ]

        let (_inputs, _processes, actions)
                = execute cvsInput cvsOutput [pFused] []

        return (lsDef, pFused, actions)
