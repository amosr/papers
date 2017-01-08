
module Machine.Eval where
import Machine.Base
import Data.Map                         (Map)
import qualified Data.Map               as Map


-- | Evaluate an expression in the current heap.
eval   :: Heap -> Expr -> Value
eval heap@(Heap hsHeap) xx
 = case xx of
        XVal value
         -> value

        XVar var
         -> case Map.lookup var hsHeap of
                Just value      -> value
                Nothing         -> error $ "eval: unbound variable " ++ show var

        XAdd x1 x2
         |  VInt i1             <- eval heap x1
         ,  VInt i2             <- eval heap x2
         -> VInt (i1 + i2)

        XApp x1 x2
         |  VAbs var1 xBody     <- eval heap x1
         ,  v2                  <- eval heap x2
         -> error "eval: finish me"


---------------------------------------------------------------------------------------------------
-- | Inject a value into an input channel of a process.
inject :: Channel -> Value -> Process -> Maybe Process
inject c v p
 = case Map.lookup c (processIns p) of
        -- Process is ready to receive input on this channel.
        Just None  -> Just p { processIns = Map.insert c (Pending v) (processIns p) }

        -- Process was not ready to receive input on this channel.
        Just _     -> Nothing

        -- Process does not have an input of this name,
        -- so just return the original process.
        Nothing    -> Just p


---------------------------------------------------------------------------------------------------
-- | An output action from a Shake evaluation.
data Action
        = ActionPush Channel Value


-- | Shake an instruction of a process.
shake   :: Instruction                          -- ^ Instruction to shake.
        -> Map Channel InputState               -- ^ States of input channels.
        -> Heap                                 -- ^ Current heap.
        -> Maybe ( Label                        --   New label to jump to.
                 , Map Channel InputState       --   New channel input states.
                 , Heap                         --   New heap.
                 , Maybe Action)                --   Output action.

shake instr sInput heap@(Heap hvHeap)
 = case instr of
        -- Pull a value from a channel.
        Pull channel var (Next label' hxUpdate)
         -- If there is a pending value then write it to the heap,
         -- and set the channel state to 'Have' to indicate we're currently using it.
         |  Just (Pending value) <- Map.lookup channel sInput
         -> Just ( label'
                 , Map.insert channel Have sInput
                 , Heap (Map.unions 
                          [ hvHeap
                          , Map.map (eval heap) hxUpdate
                          , Map.singleton var value ])
                 , Nothing)

         | otherwise
         -> Nothing


        -- Drop an element that we currently have.
        Drop channel (Next label' hxUpdate) 
         -- If the channel is in the 'Have' state then we've already loaded its
         -- element into the heap. Set the state to None so we can load the next element.
         |  Just Have   <- Map.lookup channel sInput
         -> Just ( label'
                 , Map.insert channel None sInput
                 , Heap (Map.unions
                          [ hvHeap
                          , Map.map (eval heap) hxUpdate ])
                 , Nothing)


        -- Push an element to an output stream.
        Push channel expr (Next label' hxUpdate)
         -> Just ( label'
                 , sInput
                 , Heap (Map.unions
                          [ hvHeap
                          , Map.map (eval heap) hxUpdate ])
                 , Just (ActionPush channel (eval heap expr)))


        -- Jump to a new process label.
        Jump (Next label' hxUpdate)
         -> Just ( label'
                 , sInput
                 , Heap (Map.unions
                          [ hvHeap
                          , Map.map (eval heap) hxUpdate ])
                 , Nothing)

        _ -> Nothing






