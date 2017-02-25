

module Test.Fuse.Merge where
import Machine.Transform.Fuse
import Machine.Transform.StripLabels
import Machine.Combinator
import Machine.Execute
import Machine.Pretty
import Text.Show.Pretty
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-------------------------------------------------------------------------------
-- | Test evaluation of split map map.
--
--
--    as      bs     cs
--     \     /  \    /
--      merge  merge
--        |      |
--        ds     es
--
testFuseSplitMergeMerge
 = let
        cAs     = Channel "as" TInt
        cBs     = Channel "bs" TInt
        cCs     = Channel "cs" TInt
        cDs     = Channel "ds" TInt
        cEs     = Channel "es" TInt

        (modes, bConn, pFused)
         = evalNew 
         $ do   pMerge1           <- mkMerge cAs cBs cDs
                pMerge2           <- mkMerge cBs cCs cEs
                let Right pResult   = fusePair pMerge1 pMerge2
                pFused_raw        <- fmap fst $ stripLabels "F" pResult


                let modes'         = processChannelModes   pMerge1 pMerge2
                let bConn'         = processesAreConnected pMerge1 pMerge2


                return (modes', bConn', pFused_raw)

        cvsInput
         =  Map.fromList
                [ (cAs, [VInt 1, VInt 2, VInt 3, VInt 4, VInt 5])
                , (cBs, [VInt 1, VInt 2, VInt 3, VInt 4, VInt 5]) 
                , (cCs, [VInt 1, VInt 2, VInt 3, VInt 4, VInt 5]) ]

        cvsOutput
         = Map.fromList
                [ (cDs, [])
                , (cEs, []) ]

   in do
        putStrLn $ show modes
        putStrLn $ show bConn
        putStrLn $ show $ pretty pFused

        executeTraceIO 100 cvsInput cvsOutput [pFused] []

{-
        (inputs', processes', actions')
         = execute cvsInput cvsOutput [pFused] []

   in   (pFused, actions')
-}