
module Machine.Combinator.Alt where
import Machine.New
import Machine.Base
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | Construct a new `alt2` process.
mkAlt2  :: Channel              -- ^ Input  Channel A.
        -> Channel              -- ^ Input  Channel B.
        -> Channel              -- ^ Output Channel.
        -> New Process

mkAlt2 cInA cInB cOut
 = do
        [l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11]   
                <- replicateM 12 (newLabel "L")
        vA1     <- newVar "A1"
        vA2     <- newVar "A2"
        vB1     <- newVar "B1"
        vB2     <- newVar "B2"

        return  
         $ Process
         { processName          = "alt"
         , processIns           = Map.fromList [(cInA, None), (cInB, None)]
         , processOuts          = Set.fromList [cOut]
         , processHeap          
                = Heap $ Map.fromList 
                        [ (vA1, defaultValueOfChannel cInA)
                        , (vA2, defaultValueOfChannel cInA)
                        , (vB1, defaultValueOfChannel cInB)
                        , (vB2, defaultValueOfChannel cInB)] 

         , processLabel         = l0

         , processBlocks        =
                [ (l0,  Pull cInA vA1 (next l1))
                , (l1,  Pull cInB vB1 (next l2))

                , (l2,  Drop cInA     (next l3))
                , (l3,  Pull cInA vA2 (next l4))

                , (l4,  Drop cInB     (next l5))
                , (l5,  Pull cInB vB2 (next l6))

                , (l6,  Push cOut     (XVar vA1) (next l7))
                , (l7,  Push cOut     (XVar vA2) (next l8))
                , (l8,  Push cOut     (XVar vB1) (next l9))
                , (l9,  Push cOut     (XVar vB2) (next l10))

                , (l10, Drop cInA (next l11))
                , (l11, Drop cInB (next l0)) ]

         }
