
module Machine.Combinator.Group where
import Machine.New
import Machine.Base
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | Construct a new `group` process.
--
--   Group filters out consecutive duplicate values from a stream.
--   If the stream is sorted then the output will contain only
--   the unique entries in the input stream.
--
mkGroup :: Channel      -- ^ Input  Channel.
        -> Channel      -- ^ Output Channel.
        -> New Process

mkGroup cIn cOut
 = do
        [l0, l1, l2, l3]
                <- replicateM 4 (newLabel "G")
        vFirst  <- newVar "First"
        vLast   <- newVar "Last"
        vVal    <- newVar "Val"

        return  
         $ Process
         { processName  = "group"
         , processIns   = Map.fromList [(cIn, None)]
         , processOuts  = Set.fromList [cOut]

         , processHeap          
            = Heap $ Map.fromList 
                [ (vFirst,      VBool True)
                , (vLast,       defaultValueOfChannel cIn)
                , (vVal,        defaultValueOfChannel cIn) ]

         , processLabel = l0

         , processBlocks =
                [ (l0,  Pull    cIn vVal (next l1))

                , (l1,  Case    (XOr @@ (XVar vFirst) 
                                     @@ (XNeq @@ (XVar vLast) @@ (XVar vVal)))
                                (next l2) (next l3))

                , (l2,  Push    cOut (XVar vVal)
                                (next' l3 [ (vLast,  XVar  vVal)
                                          , (vFirst, XBool False)]))

                , (l3,  Drop    cIn (next l0))
                ]
         }


