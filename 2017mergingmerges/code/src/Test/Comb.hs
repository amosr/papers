
module Test.Comb where
import Machine.Combinator
import Machine.Transform.Fuse
import Machine.Transform.StripLabels
import Machine.New
import Machine.Base
import Data.Maybe


-------------------------------------------------------------------------------
-- | Generic wrapper for a combinator.
--    We use this to abstract over how many input and output
--    channels a particular combinator has.
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


-------------------------------------------------------------------------------
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
                 Right proc'    
                  -> return $ tagHow "pipe" proc'
   ]

 | otherwise
 = []


-- | Given a set of base combinators,
--   create combinators using all the possible ways of pipelining
--   the given number of instances.
manyPipeAB :: Int -> [Comb] -> [Comb]
manyPipeAB 0 cs = []
manyPipeAB 1 cs = cs
manyPipeAB 2 cs 
 =  concatMap (\c -> concatMap (fusePipeAB c) cs) cs

manyPipeAB n cs
 =  concatMap (\c -> concatMap (fusePipeAB c) (manyPipeAB (n - 1) cs)) cs
 ++ concatMap (\c -> concatMap (fusePipeAB c) cs) (manyPipeAB (n - 1) cs)


-------------------------------------------------------------------------------
-- Fuse first and second combinators in parallel, if possible.
--   This creates a split in the data-flow network.
--   Requires both combinators to have a single input stream,
--   but can have an arbitrary number of output.
fuseSplitAB :: Comb -> Comb -> [Comb]
fuseSplitAB    comb1 comb2
 | [tIn1]   <- combInputs comb1
 , [tIn2]   <- combInputs comb2
 , tIn1 == tIn2
 = [ Comb (combInputs  comb1) 
          (combOutputs comb1 ++ combOutputs comb2)
        $  \[cIn] csOuts 
        -> do   let csOuts1 =  take (length $ combOutputs comb1) csOuts
                let csOuts2 =  take (length $ combOutputs comb2) csOuts
                proc1       <- combMk comb1 [cIn] csOuts1
                proc2       <- combMk comb2 [cIn] csOuts2
                case fusePair proc1 proc2 of
                 Left  err      -> error err
                 Right proc'    
                  -> return $ tagHow "split" proc'
   ]

 | otherwise
 = []


manySplitAB :: Int -> [Comb] -> [Comb]
manySplitAB 0 cs = []
manySplitAB 1 cs = cs
manySplitAB 2 cs 
 =  concatMap (\c -> concatMap (fuseSplitAB c) cs) cs

manySplitAB n cs
 =  concatMap (\c -> concatMap (fuseSplitAB c) (manySplitAB (n - 1) cs)) cs
 ++ concatMap (\c -> concatMap (fuseSplitAB c) cs) (manySplitAB (n - 1) cs)

tagHow :: String -> Process -> Process
tagHow str process
 = process { processName = str ++ " " ++ processName process }

-------------------------------------------------------------------------------
-- Wrappers for builtin combinators,
-- at specific types as we're just using these for testing.

combMapSucc
 = Comb [TInt] [TInt]
 $ \[cIn1] [cOut]       
 -> mkMap   xSucc cIn1 cOut
 where  xSucc x = XApp (XApp XAdd (XInt 1)) x


combFilterPos
 = Comb [TInt] [TInt]
 $ \[cIn1] [cOut]       -> mkFilter xPos cIn1 cOut
 where  xPos  x = XApp (XApp XGe  (XInt 0)) x


combScanAdd
 = Comb [TInt] [TInt]
 $ \[cIn1] [cOut]       -> mkScan xAdd vZero cIn1 cOut
 where  xAdd  x1 x2     = XApp (XApp XAdd x1) x2
        vZero           = VInt 0

combMerge
 = Comb [TInt, TInt] [TInt]
 $ \[cIn1, cIn2] [cOut] -> mkMerge cIn1 cIn2 cOut

combGroup
 = Comb [TInt] [TInt]
 $ \[cIn1] [cOut]       -> mkGroup cIn1 cOut




