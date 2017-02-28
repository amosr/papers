
module Test.Eval.Map where
import Machine.Transform.Fuse
import Machine.Combinator
import Machine.Execute
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-------------------------------------------------------------------------------
-- | Test evaluation of single map process.
testEvalMap
 = putStr $ show 
 $ let  
        cAs     = Channel "as" TInt
        cBs     = Channel "bs" TInt
        xSucc x = XApp (XApp XAdd (XInt 1)) x

        pMap1   = evalNew $ mkMap xSucc cAs cBs

        cvsInput
         = Map.fromList
                [ (cAs, map VInt [1, 2, 3])]

        cvsOutput
         = Map.fromList
                [ (cBs, []) ]

        (inputs', processes', actions')
         = execute cvsInput cvsOutput [pMap1] []

   in   (inputs', processes', actions')


-------------------------------------------------------------------------------
-- | Test evaluation of pipelined map map.
testEvalPipeMapMap
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

        cvsInput
         =  Map.fromList
                [ (cAs, map VInt [1, 2, 3])]

        cvsOutput
         = Map.fromList
                [ (cCs, [])]

        (inputs', processes', actions')
         = execute cvsInput cvsOutput [pMap1, pMap2] []

   in   (inputs', processes', actions')


-------------------------------------------------------------------------------
-- | Test evaluation of split map-map
testEvalSplitMapMap
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

        cvsInput
         =  Map.fromList
                [ (cAs, map VInt [1, 2, 3])]

        cvsOutput
         = Map.fromList
                [ (cBs, [])
                , (cCs, []) ]

        (inputs', processes', actions')
         = execute cvsInput cvsOutput [pMap1, pMap2] []

   in   (inputs', processes', actions')

