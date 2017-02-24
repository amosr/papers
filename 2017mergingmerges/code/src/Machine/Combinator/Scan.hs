
module Machine.Combinator.Scan where
import Machine.New
import Machine.Base
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | Construct a new `scan` process.
--
mkScan  :: (Expr -> Expr -> Expr)       -- ^ Combining function.
        -> Value                        -- ^ Initial value.
        -> Channel                      -- ^ Input  Channel.
        -> Channel                      -- ^ Output Channel.
        -> New Process

mkScan mkF valZero cIn cOut
 = do
        [l0, l1, l2]
                <- replicateM 3 (newLabel "L")
        vVal    <- newVar "Val"
        vAcc    <- newVar "Acc"

        return  
         $ Process
         { processName  = "scan"
         , processIns   = Map.fromList [(cIn, None)]
         , processOuts  = Set.fromList [cOut]

         , processHeap          
            = Heap $ Map.fromList 
                [ (vVal,        defaultValueOfChannel cIn) 
                , (vAcc,        valZero) ]

         , processLabel  = l0

         , processBlocks =
                [ (l0,  Pull    cIn  vVal (next l1))
                , (l1,  Push    cOut (XVar vAcc)  
                                (next' l2 [(vAcc, mkF (XVar vAcc) (XVar vVal))]))
                , (l2,  Drop    cIn (next l0))
                ]
         }


