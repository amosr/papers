
module Machine.Combinator.Zip where
import Machine.New
import Machine.Base
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | Construct a new `zip` process.
mkZip   :: Channel              -- ^ Input  Channel A.
        -> Channel              -- ^ Input  Channel B.
        -> Channel              -- ^ Output Channel.
        -> New Process

mkZip cInA cInB cOut
 = do
        [l0, l1, l2, l3, l4]    
                <- replicateM 5 (newLabel "Z")
        vA      <- newVar "A"
        vB      <- newVar "B"

        return  
         $ Process
         { processName          = "map"
         , processIns           = Map.fromList [(cInA, None), (cInB, None)]
         , processOuts          = Set.fromList [cOut]
         , processHeap          
                = Heap $ Map.fromList 
                        [ (vA, defaultValueOfChannel cInA)
                        , (vB, defaultValueOfChannel cInB)] 

         , processLabel         = l0

         , processBlocks        =
                [ (l0, Pull cInA vA (next l1))
                , (l1, Pull cInB vB (next l2))
                , (l2, Push cOut (XTuple 2 @@ XVar vA @@ XVar vB) (next l3))
                , (l3, Drop cInA    (next l3))
                , (l4, Drop cInB    (next l0)) ]
         }
