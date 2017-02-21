
module Machine.Combinator.Merge where
import Machine.New
import Machine.Base
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | Construct a new `merge` process.
--
--   Merge takes two sorted inputs streams and proces a sorted output stream.
--
mkMerge :: Channel      -- ^ First  input channel.
        -> Channel      -- ^ Second input channel.
        -> Channel      -- ^ Output channel.
        -> New Process

mkMerge cInA cInB cOut
 = do
        [lI0, lI1]      <- replicateM 2 (newLabel "I")
        lC0             <- newLabel "C"
        [lA0, lA1, lA2] <- replicateM 3 (newLabel "A")
        [lB0, lB1, lB2] <- replicateM 3 (newLabel "B")

        vA              <- newVar "A"
        vB              <- newVar "B"

        return
         $ Process
         { processName  = "merge"
         , processIns   = Map.fromList [(cInA, None), (cInB, None)]
         , processOuts  = Set.fromList [cOut]

         , processHeap
            = Heap $ Map.fromList
                [ (vA,  defaultValueOfChannel cInA)
                , (vB,  defaultValueOfChannel cInB) ]

        , processLabel  = lI0

        , processBlocks =
                [ (lI0, Pull    cInA vA         (next lI1))
                , (lI1, Pull    cInB vB         (next lC0))

                , (lC0, Case    (XLe @@ (XVar vA) @@ (XVar vB))
                                (next lA0) (next lB0))

                , (lA0, Push    cOut (XVar vA)  (next lA1))
                , (lA1, Drop    cInA            (next lA2))
                , (lA2, Pull    cInA vA         (next lC0))

                , (lB0, Push    cOut (XVar vB)  (next lB1))
                , (lB1, Drop    cInB            (next lB2))
                , (lB2, Pull    cInB vB         (next lC0))]
         }

