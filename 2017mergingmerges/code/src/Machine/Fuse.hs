{-# LANGUAGE TupleSections #-}
module Machine.Fuse where
import Machine.Base
import Data.Map                 (Map)
import Data.Set                 (Set)
import qualified Data.Map       as Map
import qualified Data.Set       as Set


---------------------------------------------------------------------------------------------------
-- | How a channel in a process group is used.
data ChannelMode        
        -- | Channel is the exclusive input of a single process.
        = ModeInputExclusive

        -- | Channel is a shared input of both processes.
        | ModeInputShared

        -- | Channel is the output of one process and the input of the other.
        | ModeConnected

        -- | Channel is the (exclusive) output of a single process.
        | ModeOutput
        deriving (Show, Eq, Ord)


---------------------------------------------------------------------------------------------------
(?) m x = Map.lookup x m


---------------------------------------------------------------------------------------------------
-- | Yield the set of channel usage modes relative to the given pair of processes.
processChannelModes :: Process -> Process -> Map Channel ChannelMode
processChannelModes process1 process2
 = let  ins1    = processIns  process1
        ins2    = processIns  process2

        cins1   = Set.fromList $ Map.keys $ processIns  process1
        cins2   = Set.fromList $ Map.keys $ processIns  process2
        
        outs1   = processOuts process1
        outs2   = processOuts process2
   in  Map.fromList
        $ Set.toList
        $ Set.unions
        [ Set.map (,ModeInputShared)    
                $ Set.intersection cins1 cins2

        , Set.map (,ModeInputExclusive)
                $ Set.filter (\c -> not $ Set.member c (Set.union outs1 outs2))
                $ Set.union cins1 cins2

        , Set.map (,ModeConnected)
                $ Set.intersection
                        (Set.union cins1 cins2)
                        (Set.union outs1 outs2)

        , Set.map (,ModeOutput)
                $ Set.filter (\c -> not $ Set.member c (Set.union cins1 cins2))
                $ Set.union outs1 outs2
        ]


-- | Check whether two processes are directly connected via any channel.
processesAreConnected :: Process -> Process -> Bool
processesAreConnected process1 process2
 = let  ins1    = processIns  process1
        ins2    = processIns  process2
        outs1   = processOuts process1
        outs2   = processOuts process2
   in not $ Set.null
          $ Set.intersection
               (Set.union (Set.fromList $ Map.keys ins1)
                          (Set.fromList $ Map.keys ins2))
               (Set.union (Set.fromList $ Map.keys ins1)
                          outs2)

---------------------------------------------------------------------------------------------------
-- | Fuse to processes.
fusePair :: Process -> Process -> Maybe Process
fusePair process1 process2
 = let  csModes = processChannelModes process1 process2

        lStart  = LabelJoint 
                        ( processLabel process1
                        , Map.map inputModeOfState $ processIns process1)
                        ( processLabel process2
                        , Map.map inputModeOfState $ processIns process2)

   in   Just $ Process
        { processName  = "(" ++ processName process1 ++ "/" ++ processName process2 ++ ")"

        , processIns   = Map.fromList
                       $ [(c, None) | (c, m) <- Map.toList csModes
                                    ,    m == ModeInputExclusive
                                      || m == ModeInputShared ]

        , processOuts  = Set.fromList
                       $ [ c        | (c, m) <- Map.toList csModes
                                    ,    m == ModeOutput ]

        , processHeap  = let Heap h1   = processHeap process1
                             Heap h2   = processHeap process2
                         in  Heap (Map.union h1 h2)

        , processLabel = lStart

        , processBlocks = [] 
        }


---------------------------------------------------------------------------------------------------
tryStep 
        :: Map Channel ChannelMode         -- ^ Structural mode of each channel.
        -> Map Channel Var                 -- ^ Buffer variable for each channel.
        -> (Label, Map Channel InputMode)  -- ^ Label and input channel states for first process.
        -> (Label, Map Channel InputMode)  -- ^ Label and input channel states for second process.
        -> Instruction                     -- ^ Instruction from first process.
        -> Maybe Instruction               -- ^ Result instruction for first process.

tryStep csMode xsBuffer
        (label1, csState1)
        (label2, csState2)
        instr

 = case instr of

        Pull c x (Next label1' xvsUpdate)
         -- First process has exclusive use of the input channel.
         |  Just ModeInputExclusive <- csMode ? c
         -> Just $ Pull c x 
                 $ Next (LabelJoint (label1', csState1) 
                                    (label2,  csState2))
                        xvsUpdate

         -- Input channel is used by both processes,
         -- and there is a pending value on the channel.
         |   (Just ModeInputShared == csMode   ? c)
          || (Just ModeConnected   == csMode   ? c)
         ,  Just ModePending       <- csState1 ? c
         ,  Just xBuffer           <- Map.lookup c xsBuffer
         -> Just $ Jump
                 $ Next (LabelJoint (label1', Map.insert c ModeHave csState1)
                                    (label2,  csState2))
                        (Map.insert x (XVar xBuffer) xvsUpdate)

         -- Input channel is used by both processes,
         -- and neither has pulled a value yet.
         |  Just ModeInputShared   <- csMode   ? c
         ,  Just ModeNone          <- csState1 ? c
         ,  Just ModeNone          <- csState2 ? c
         ,  Just xBuffer           <- Map.lookup c xsBuffer
         -> Just $ Pull c xBuffer
                 $ Next (LabelJoint (label1, Map.insert c ModePending csState1)
                                    (label2, Map.insert c ModePending csState2))
                         xvsUpdate


        _ -> Nothing


