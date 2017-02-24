
module Machine.Combinator.Filter where
import Machine.New
import Machine.Base
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | Construct a new `filter` process.
--
mkFilter 
        :: (Expr -> Expr)       -- ^ Filter predicate.
        -> Channel              -- ^ Input  Channel.
        -> Channel              -- ^ Output Channel.
        -> New Process

mkFilter mkF cIn cOut
 = do
        [l0, l1, l2, l3]
                <- replicateM 4 (newLabel "L")
        vVal    <- newVar "Val"

        return  
         $ Process
         { processName  = "filter"
         , processIns   = Map.fromList [(cIn, None)]
         , processOuts  = Set.fromList [cOut]

         , processHeap          
            = Heap $ Map.fromList 
                [ (vVal,        defaultValueOfChannel cIn) ]

         , processLabel  = l0

         , processBlocks =
                [ (l0,  Pull    cIn vVal (next l1))
                , (l1,  Case    (mkF (XVar vVal)) (next l2) (next l3))
                , (l2,  Push    cOut (XVar vVal)  (next l3))
                , (l3,  Drop    cIn (next l0))
                ]
         }


