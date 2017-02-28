
module Test.Process.Map where
import Machine.Execute
import Machine.Combinator
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | Test construction of map process.
testProcessMap :: IO ()
testProcessMap
 = putStr $ show 
 $ let  cAs     = Channel "as" TInt
        cBs     = Channel "bs" TInt
        xSucc x = XApp (XApp XAdd (XInt 1)) x

   in   evalNew $ mkMap xSucc cAs cBs



-- | Test contruction of pipelined map map process.
testProcessMapMap :: IO ()
testProcessMapMap
 = putStr $ show 
 $ let
        cAs     = Channel "as" TInt
        cBs     = Channel "bs" TInt
        cCs     = Channel "cs" TInt
        xSucc x = XApp (XApp XAdd (XInt 1)) x

   in   evalNew
         $ do   pMap1   <- mkMap xSucc cAs cBs
                pMap2   <- mkMap xSucc cBs cCs
                return (pMap1, pMap2)

