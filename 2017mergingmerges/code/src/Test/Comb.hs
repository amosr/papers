
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
                 Left err       -> error (show err)
                 Right proc'    
                  -> return $ tagHow "pipe" proc'
   ]

 | otherwise
 = []

-------------------------------------------------------------------------------
-- | Fuse first and second combinators into a semi-pipeline, if possible.
--    Requires the first combinator to have a single output,
--    and allows the second combinator to have many inputs.
--    The first input of the second combinator is connected, but other inputs are left.
fusePipeMultipleInputs :: Comb -> Comb -> [Comb]
fusePipeMultipleInputs    comb1 comb2
 | [tOut1]      <- combOutputs comb1
 , tIn2 : tIns2 <- combInputs  comb2
 , tOut1 == tIn2
 = [ Comb (combInputs  comb1 ++ tIns2) 
          (combOutputs comb2)
        $  \csIns csOuts2 
        -> do   let csIns1 = take (length $ combInputs comb1) csIns
                let csIns2 = drop (length $ combInputs comb1) csIns
                cInternal <- newChannel "Internal" tOut1
                proc1     <- combMk comb1 csIns1     [cInternal]
                proc2     <- combMk comb2 (cInternal : csIns2) csOuts2
                case fusePair proc1 proc2 of
                 Left err       -> error (show err)
                 Right proc'    
                  -> return $ tagHow "pipe" proc'
   ]

 | otherwise
 = []


-- | Given a set of base combinators and a combining function,
--   create combinators using all the possible ways of combining
--   the given number of instances.
manyCombineWith :: (Comb -> Comb -> [Comb]) -> Int -> [Comb] -> [Comb]
manyCombineWith f 0 cs = []
manyCombineWith f 1 cs = cs
manyCombineWith f 2 cs 
 =  concatMap (\c -> concatMap (f c) cs) cs

manyCombineWith f n cs
 =  concatMap (\c -> concatMap (f c) (manyCombineWith f (n - 1) cs)) cs
 ++ concatMap (\c -> concatMap (f c) cs) (manyCombineWith f (n - 1) cs)

-- | Given a set of base combinators,
--   create combinators using all the possible ways of pipelining
--   the given number of instances.
manyPipeAB :: Int -> [Comb] -> [Comb]
manyPipeAB = manyCombineWith fusePipeAB


-------------------------------------------------------------------------------
-- Fuse first and second combinators in parallel, if possible.
--   This creates a split in the data-flow network.
--   Requires both combinators to have a single input stream,
--   but can have an arbitrary number of output.
fuseSplitAB :: Comb -> Comb -> [Comb]
fuseSplitAB    comb1 comb2
 | [tIn1] <- combInputs comb1
 , [tIn2] <- combInputs comb2
 , tIn1 == tIn2
 = [ Comb [tIn1]
          (combOutputs comb1 ++ combOutputs comb2)
        $  \[cIn] csOuts 
        -> do   let csOuts1 =  take (length $ combOutputs comb1) csOuts
                let csOuts2 =  drop (length $ combOutputs comb1) csOuts
                proc1       <- combMk comb1 [cIn] csOuts1
                proc2       <- combMk comb2 [cIn] csOuts2
                case fusePair proc1 proc2 of
                 Left  err      -> error (show err)
                 Right proc'    
                  -> return $ tagHow "split" proc'
   ]

 | otherwise
 = []


manySplitAB :: Int -> [Comb] -> [Comb]
manySplitAB = manyCombineWith fuseSplitAB

-------------------------------------------------------------------------------
-- Fuse first and second combinators in parallel, if possible.
--   This joins the first input of both combinators, leaving any other inputs free.
fuseSplitManyInputs :: Comb -> Comb -> [Comb]
fuseSplitManyInputs    comb1 comb2
 | tIn1 : tIns1   <- combInputs comb1
 , tIn2 : tIns2   <- combInputs comb2
 , tIn1 == tIn2
 = [ Comb (tIn1 : tIns1 ++ tIns2) 
          (combOutputs comb1 ++ combOutputs comb2)
        $  \(cIn : csIns) csOuts 
        -> do   let csIns1  =  take (length tIns1)               csIns
                let csIns2  =  drop (length tIns1)               csIns
                let csOuts1 =  take (length $ combOutputs comb1) csOuts
                let csOuts2 =  drop (length $ combOutputs comb1) csOuts
                proc1       <- combMk comb1 (cIn : csIns1) csOuts1
                proc2       <- combMk comb2 (cIn : csIns2) csOuts2
                case fusePair proc1 proc2 of
                 Left  err      -> error (show err)
                 Right proc'    
                  -> return $ tagHow "split" proc'
   ]

 | otherwise
 = []

-------------------------------------------------------------------------------
-- Fuse first and second combinators in parallel, if possible.
--   This joins the first input of both combinators, leaving any other inputs free.
--   Unlike the above, this only allows the first combinator to have multiple inputs.
fuseSplitFirstManyInputs :: Comb -> Comb -> [Comb]
fuseSplitFirstManyInputs    comb1 comb2
 | tIn1 : tIns1   <- combInputs comb1
 , tIn2 : tIns2   <- combInputs comb2
 , [] <- tIns2
 , tIn1 == tIn2
 = [ Comb (tIn1 : tIns1 ++ tIns2) 
          (combOutputs comb1 ++ combOutputs comb2)
        $  \(cIn : csIns) csOuts 
        -> do   let csIns1  =  take (length tIns1)               csIns
                let csIns2  =  drop (length tIns1)               csIns
                let csOuts1 =  take (length $ combOutputs comb1) csOuts
                let csOuts2 =  drop (length $ combOutputs comb1) csOuts
                proc1       <- combMk comb1 (cIn : csIns1) csOuts1
                proc2       <- combMk comb2 (cIn : csIns2) csOuts2
                case fusePair proc1 proc2 of
                 Left  err      -> error (show err)
                 Right proc'    
                  -> return $ tagHow "split" proc'
   ]

 | otherwise
 = []


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




