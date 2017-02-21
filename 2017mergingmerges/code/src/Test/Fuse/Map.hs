
module Test.Fuse.Map where
import Machine.Transform.Fuse
import Machine.Combinator
import Machine.Execute
import Text.Show.Pretty
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-------------------------------------------------------------------------------
-- | Test evaluation of pipelined map map.
--   Note that with a finite input stream the last value is still
--   in the channel buffer when the process ends.
testFusePipeMapMap
 = putStr $ ppShow 
 $ let  
        cAs     = Channel "as" TInt
        cBs     = Channel "bs" TInt
        cCs     = Channel "cs" TInt
        xSucc x = XApp (XApp XAdd (XInt 1)) x

        (pMap1, pMap2)
         = evalNew 
         $ do   pMap1   <- mkMap cAs cBs xSucc 
                pMap2   <- mkMap cBs cCs xSucc
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
 = putStr $ ppShow 
 $ let  
        cAs     = Channel "as" TInt
        cBs     = Channel "bs" TInt
        cCs     = Channel "cs" TInt
        xSucc x = XApp (XApp XAdd (XInt 1)) x

        (pMap1, pMap2)
         = evalNew 
         $ do   pMap1   <- mkMap cAs cBs xSucc
                pMap2   <- mkMap cAs cCs xSucc
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

