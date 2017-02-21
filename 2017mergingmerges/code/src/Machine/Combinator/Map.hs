
module Machine.Combinator.Map where
import Machine.New
import Machine.Base
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | Construct a new `map` process.
mkMap   :: (Expr -> Expr)       -- ^ Worker function.
        -> Channel              -- ^ Input  Channel.
        -> Channel              -- ^ Output Channel.
        -> New Process

mkMap f cIn cOut
 = do
        [l0, l1, l2]    <- replicateM 3 (newLabel "M")
        v0              <- newVar "V"

        return  
         $ Process
         { processName          = "map"
         , processIns           = Map.fromList [(cIn, None)]
         , processOuts          = Set.fromList [cOut]
         , processHeap          = Heap (Map.fromList [(v0, VInt 0)])
         , processLabel         = l0
         , processBlocks        =
                [ ( l0
                  , Pull cIn v0 (Next l1 Map.empty))

                , ( l1
                  , Push cOut   (f (XVar v0))
                                (Next l2 Map.empty))

                , ( l2
                  , Drop cIn    (Next l0 Map.empty))
                ]
         }
