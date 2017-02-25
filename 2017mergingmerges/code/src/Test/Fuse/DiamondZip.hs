
module Test.Fuse.DiamondZip where
import Machine.Transform.Fuse
import Machine.Transform.StripLabels
import Machine.Combinator
import Machine.Execute
import Machine.Pretty
import qualified Data.Map       as Map
import qualified Data.Set       as Set


testFuseDiamondZip
 = putStrLn $ show $ pretty
 $ let  
        cIn1     = Channel "cIn1" TInt
        cIn2     = Channel "cIn2" TInt
        cIn3     = Channel "cIn3" TInt

        cZ1      = Channel "cZ1"  TInt
        cZ2      = Channel "cZ2"  TInt
        cOut     = Channel "cOut"  TInt

        pFused
         = evalNew 
         $ do   pZip3             <- mkZip cZ1  cZ2  cOut
                pZip1             <- mkZip cIn1 cIn2 cZ1
                let Right pZip13  =  fusePair pZip3 pZip1

                pZip2             <- mkZip cIn2 cIn3 cZ2
                let Right pZip123 = fusePair pZip13 pZip2

                pFused'         <- fmap fst $ stripLabels "F" pZip123 
                return pFused'
   in   pFused

