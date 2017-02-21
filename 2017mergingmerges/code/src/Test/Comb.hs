
module Test.Comb where
import Machine.Combinator.Group
import Machine.Combinator.Map
import Machine.Combinator.Merge
import Machine.Transform.Fuse
import Machine.Transform.StripLabels
import Machine.New
import Machine.Base

import Data.Maybe


-- | Wrapper for a combinator.
data Comb
        = Comb
        { combInputs    :: [Type]
        , combOutputs   :: [Type]    
        , combMk        :: [Channel] -> [Channel] -> New Process }


-- | Produce a concrete process from a combinator by generating
--   names for its input and output channels.
mkComb :: Comb -> New Process
mkComb comb
 = do   csIn    <- mapM (newChannel "cIn")  $ combInputs  comb
        csOut   <- mapM (newChannel "cOut") $ combOutputs comb
        proc    <- combMk comb csIn csOut
        fmap fst $ stripLabels "F" proc


-- | Fuse first and second combinators into a pipeline, if possible.
--    Requires the first combinator to have a single output,
--    and the second to have a single input.
fusePipeAB :: Comb -> Comb -> [Comb]
fusePipeAB    comb1 comb2
 | [tOut1]  <- combOutputs comb1
 , [tIn2]   <- combInputs  comb2
 , tOut1 == tIn2
 = [ Comb (combInputs  comb1) 
          (combOutputs comb2)
        $  \csIns1 csOuts2 
        -> do   cInternal <- newChannel "Internal" tOut1
                proc1     <- combMk comb1 csIns1     [cInternal]
                proc2     <- combMk comb2 [cInternal] csOuts2
                case fusePair proc1 proc2 of
                 Left err       -> error err
                 Right proc'    -> return proc'
   ]

 | otherwise
 = []


combMerge
 = Comb [TInt, TInt] [TInt]
 $ \[cIn1, cIn2] [cOut] -> mkMerge cIn1 cIn2 cOut


combMapSucc
 = Comb [TInt] [TInt]
 $ \[cIn1] [cOut]       -> mkMap   xSucc cIn1 cOut
 where  xSucc x = XApp (XApp XAdd (XInt 1)) x


combGroup
 = Comb [TInt] [TInt]
 $ \[cIn1] [cOut]       -> mkGroup cIn1 cOut



