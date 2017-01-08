
module Machine.Eval where
import Machine.Base
import Data.Map                         (Map)
import qualified Data.Map.Strict        as Map
import Data.List

---------------------------------------------------------------------------------------------------
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

         |  VSucc               <- eval heap x1
         ,  VInt i2             <- eval heap x2
         -> VInt (i2 + 1)


---------------------------------------------------------------------------------------------------
-- | Inject a value into an input channel of a process.
inject :: Channel -> Value -> Process -> Process
inject c v p
 = case Map.lookup c (processIns p) of
        -- Process is ready to receive input on this channel.
        Just None  -> p { processIns = Map.insert c (Pending v) (processIns p) }

        -- Process was not ready to receive input on this channel.
        Just _     -> error "inject: process not ready"

        -- Process does not have an input of this name,
        -- so just return the original process.
        Nothing    -> p


---------------------------------------------------------------------------------------------------
-- | An output action from a Shake evaluation.
data Action
        = ActionPush Channel Value
        deriving Show


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
         |   Just (Pending value) <- Map.lookup channel sInput
         ->  Just ( label'
                 , Map.insert channel Have sInput
                 , Heap (Map.unions 
                          [ Map.singleton var value
                          , Map.map (eval heap) hxUpdate
                          , hvHeap ])
                 , Nothing)

         |  otherwise
         -> Nothing


        -- Drop an element that we currently have.
        Drop channel (Next label' hxUpdate) 
         -- If the channel is in the 'Have' state then we've already loaded its
         -- element into the heap. Set the state to None so we can load the next element.
         |  Just Have   <- Map.lookup channel sInput
         -> Just ( label'
                 , Map.insert channel None sInput
                 , Heap (Map.unions
                          [ Map.map (eval heap) hxUpdate
                          , hvHeap ])
                 , Nothing)


        -- Push an element to an output stream.
        Push channel expr (Next label' hxUpdate)
         -> Just ( label'
                 , sInput
                 , Heap (Map.unions
                          [ Map.map (eval heap) hxUpdate
                          , hvHeap ])
                 , Just (ActionPush channel (eval heap expr)))


        -- Jump to a new process label.
        Jump (Next label' hxUpdate)
         -> Just ( label'
                 , sInput
                 , Heap (Map.unions
                          [ Map.map (eval heap) hxUpdate
                          , hvHeap ])
                 , Nothing)


        -- Case branching.
        Case xScrut (Next labelThen hxThen) (Next labelElse hxElse)
         -> case eval heap xScrut of
             VBool True    
              -> Just   ( labelThen
                        , sInput
                        , Heap (Map.unions
                                 [ Map.map (eval heap) hxThen
                                 , hvHeap ])
                        , Nothing )

             VBool False
              -> Just   ( labelElse
                        , sInput
                        , Heap (Map.unions
                                 [ Map.map (eval heap) hxElse
                                 , hvHeap ])
                        , Nothing )

             _ -> error "shake case: type error"

        _ -> Nothing


---------------------------------------------------------------------------------------------------
step :: Process -> Maybe (Process, Maybe Action)
step p
 |  Just instr  <- lookup (processLabel p) (processBlocks p)
 =  case shake instr (processIns p) (processHeap p) of
        Nothing 
         -> Nothing

        Just (label', sInput', heap', mAction)
         -> Just ( p    { processLabel  = label'
                        , processIns    = sInput'
                        , processHeap   = heap' }
                 , mAction)


---------------------------------------------------------------------------------------------------
shakes :: [Process] -> [Process] -> [Action] -> ([Process], [Action])

shakes stalled [] acc
 = (stalled, acc)

shakes stalled (p : psRest) acc
 = case step p of
        Just (p', mAction)
         -> case mAction of
                Nothing
                 -> shakes [] (stalled ++ (p' : psRest)) acc

                Just action@(ActionPush channel value)
                 -> let stalled' = map (inject channel value) stalled
                        psRest'  = map (inject channel value) psRest
                    in  shakes [] (stalled' ++ (p' : psRest')) (acc ++ [action])

        Nothing
         -> shakes (stalled ++ [p]) psRest acc


---------------------------------------------------------------------------------------------------
type ChannelValueS a    
        =   Map Channel [Value] -> a
        -> (Map Channel [Value], a)


-- | Feed input into a channel, if it needs it.
feedInputState ::  ChannelValueS (Channel, InputState)
feedInputState cvs (c, state)
 = case state of
        None    
         -> case Map.lookup c cvs of
                Nothing         -> (cvs, (c, state))
                Just (v:vs)     -> (Map.insert c vs cvs, (c, Pending v))

        _ -> (cvs, (c, state))


-- | Feed input into the given process.
feedProcess :: ChannelValueS Process
feedProcess cvs p
 = let  (cvs', cis')    = mapAccumL feedInputState cvs 
                        $ Map.toList $ processIns p

        p'              = p { processIns = Map.fromList cis' }
   in   (cvs', p')


-- | Feed input into the given proceses.
feedProcesses :: ChannelValueS [Process]
feedProcesses cvs ps
        = mapAccumL feedProcess cvs ps


execute 
        :: Map Channel [Value]          -- Input  channels values.
        -> Map Channel [Value]          -- Output channel values.
        -> [Process]                    -- Processes.
        -> [Action]                     -- Actions
        -> (Map Channel [Value], [Process], [Action])     
                                        -- Processes after execution.

execute cvsIn cvsOut ps acc
 | all null $ map snd $ Map.toList cvsIn
 = (cvsIn, ps, acc)

 | otherwise
 = let  (cvsIn', ps')   = feedProcesses cvsIn ps
        (ps'',   as')   = shakes [] ps' []
   in   execute cvsIn' cvsOut ps'' (acc ++ as')


