
module Test.Process.Map where
import Machine.Execute
import Machine.Combinator
import Text.Show.Pretty
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | Test construction of map process.
testProcessMap :: IO ()
testProcessMap
 = putStr $ ppShow 
 $ let  cAs     = Channel "as" TInt
        cBs     = Channel "bs" TInt
        xSucc x = XApp (XApp XAdd (XInt 1)) x

   in   evalNew $ mkMap cAs cBs xSucc



-- | Test contruction of pipelined map map process.
testProcessMapMap :: IO ()
testProcessMapMap
 = putStr $ ppShow 
 $ let
        cAs     = Channel "as" TInt
        cBs     = Channel "bs" TInt
        cCs     = Channel "cs" TInt
        xSucc x = XApp (XApp XAdd (XInt 1)) x

   in   evalNew
         $ do   pMap1   <- mkMap cAs cBs xSucc
                pMap2   <- mkMap cBs cCs xSucc
                return (pMap1, pMap2)

