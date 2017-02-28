
module Test.Fuse.Map where
import Machine.Transform.Fuse
import Machine.Transform.StripLabels
import Machine.Combinator
import Machine.Execute
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-------------------------------------------------------------------------------
-- | Test evaluation of pipelined map map.
--   Note that with a finite input stream the last value is still
--   in the channel buffer when the process ends.
testFusePipeMapMap
 = putStr $ show 
 $ let  
        cAs     = Channel "as" TInt
        cBs     = Channel "bs" TInt
        cCs     = Channel "cs" TInt
        xSucc x = XApp (XApp XAdd (XInt 1)) x

        (pMap1, pMap2)
         = evalNew 
         $ do   pMap1   <- mkMap xSucc cAs cBs
                pMap2   <- mkMap xSucc cBs cCs
                return (pMap1, pMap2)

        modes           = processChannelModes   pMap1 pMap2 
        bConn           = processesAreConnected pMap1 pMap2
        Right pOut      = fusePair pMap1 pMap2

        cvsInput
         =  Map.fromList
                [ (cAs, [VInt 1, VInt 2, VInt 3, VInt 4, VInt 5]) ]

        cvsOutput
         = Map.fromList
                [ (cCs, []) ]

        (inputs', processes', actions')
         = execute cvsInput cvsOutput [pOut] []

   in   (pMap1, pMap2, modes, bConn, pOut, actions')


-------------------------------------------------------------------------------
-- | Test evaluation of split map map.
testFuseSplitMapMap
 = putStr $ show 
 $ let  
        cAs     = Channel "as" TInt
        cBs     = Channel "bs" TInt
        cCs     = Channel "cs" TInt
        xSucc x = XApp (XApp XAdd (XInt 1)) x

        (pMap1, pMap2)
         = evalNew 
         $ do   pMap1   <- mkMap xSucc cAs cBs
                pMap2   <- mkMap xSucc cAs cCs
                return (pMap1, pMap2)

        modes           = processChannelModes   pMap1 pMap2 
        bConn           = processesAreConnected pMap1 pMap2
        Right pOut      = fusePair pMap1 pMap2

        cvsInput
         =  Map.fromList
                [ (cAs, [VInt 1, VInt 2, VInt 3, VInt 4, VInt 5]) ]

        cvsOutput
         = Map.fromList
                [ (cBs, [])
                , (cCs, []) ]

        (inputs', processes', actions')
         = execute cvsInput cvsOutput [pOut] []

   in   (pMap1, pMap2, modes, bConn, pOut, actions')



-------------------------------------------------------------------------------
-- | Test evaluation of fused unrelated map map.
testFuseUnrelatedMapMap
 = putStr $ show 
 $ let  
        cAs     = Channel "as" TInt
        cBs     = Channel "bs" TInt
        cCs     = Channel "cs" TInt
        cDs     = Channel "ds" TInt
        xSucc x = XApp (XApp XAdd (XInt 1)) x

        (pMap1, pMap2, pFused_stripped)
         = evalNew 
         $ do   pMap1   <- mkMap xSucc cAs cBs
                pMap2   <- mkMap xSucc cCs cDs

                let Right pFused = fusePair pMap1 pMap2
                pFused' <- stripLabels "F" pFused

                return (pMap1, pMap2, pFused')

        modes           = processChannelModes   pMap1 pMap2 
        bConn           = processesAreConnected pMap1 pMap2

        cvsInput
         =  Map.fromList
                [ (cAs, [VInt 1, VInt 2, VInt 3, VInt 4, VInt 5])
                , (cBs, [VInt 1, VInt 2, VInt 3, VInt 4, VInt 5]) ]

        cvsOutput
         = Map.fromList
                [ (cCs, [])
                , (cDs, []) ]

   in   (pMap1, pMap2, modes, bConn, pFused_stripped)


