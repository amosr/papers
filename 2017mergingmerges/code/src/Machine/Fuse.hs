{-# LANGUAGE TupleSections #-}
module Machine.Fuse where
import Machine.Base
import Data.Map                 (Map)
import Data.Set                 (Set)
import qualified Data.Map       as Map
import qualified Data.Set       as Set
import Control.Monad


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
-- | Fuse two processes.
fusePair :: Process -> Process -> Either String Process
fusePair process1 process2
 = let  
        -- Compute how shared channels are used.
        csModes
         = processChannelModes process1 process2

        -- Create the starting label for the output process.
        lStart  
         = LabelJoint 
                ( processLabel process1
                , Map.map inputModeOfState $ processIns process1)
                ( processLabel process2
                , Map.map inputModeOfState $ processIns process2)

        -- Main fusion loop.
        go instrs ll
         -- We already have an instruction with this set, 
         -- so we don't need to add any more.
         | Just  _      <- lookup ll instrs 
         = Right instrs

         -- Try to step the given instruction.
        go instrs ll@(LabelJoint   (l1, ims1) (l2, ims2))
         | Just instr1  <- lookup l1 (processBlocks process1)
         , Just instr2  <- lookup l2 (processBlocks process2)
         = case tryStepPair csModes (l1, ims1) instr1 (l2, ims2) instr2 of
                Just instr' 
                 -> let lsOut   = outLabelsOfInstruction instr'
                    in  foldM go (instrs ++ [(ll, instr')]) lsOut

                Nothing     
                 -> Left ("cannot step " ++ show ll)

                -- instrs

         | otherwise
         = error "missing instruction"

   in do 
       -- Run the fusion loop.
       instrs' <- go [] lStart

       -- Construct the result process.
       Right $ Process
        { processName   = "(" ++ processName process1 ++ "/" ++ processName process2 ++ ")"

        , processIns    = Map.fromList
                        $ [(c, None) | (c, m) <- Map.toList csModes
                                     ,    m == ModeInputExclusive
                                       || m == ModeInputShared ]

        , processOuts   = Set.fromList
                        $ [ c        | (c, m) <- Map.toList csModes
                                     ,    m == ModeOutput ]

        , processHeap   = let Heap h1   = processHeap process1
                              Heap h2   = processHeap process2
                          in  Heap (Map.union h1 h2)

        , processLabel  = lStart

        , processBlocks = instrs'
        }


---------------------------------------------------------------------------------------------------
tryStepPair 
        :: Map Channel ChannelMode          -- ^ Structural mode of each channel.
        -> (Label, Map Channel InputMode)   -- ^ Label and input channel states for first process.
        -> Instruction                      -- ^ Current instruction of first process.
        -> (Label, Map Channel InputMode)   -- ^ Label and input channel states for second process.
        -> Instruction                      -- ^ Current instruction of second process.
        -> Maybe Instruction

tryStepPair 
        csMode
        (label1, csState1) instr1
        (label2, csState2) instr2

 -- Try to advance the first instruction.
 | Just instr'  <- tryStep csMode 
                        (label1, csState1) instr1
                        (label2, csState2)
 = Just instr'

 -- We can't advance the first instruction, so try the second.
 --  As we call 'tryStep' with swapped arguments we then have to flip
 --  the labels in the result instruction.
 | Just instr'  <- tryStep csMode
                        (label2, csState2) instr2
                        (label1, csState1)
 = Just $ swapLabelsOfInstruction instr'

 | otherwise
 = Nothing


---------------------------------------------------------------------------------------------------
tryStep 
        :: Map Channel ChannelMode          -- ^ Structural mode of each channel.
        -> (Label, Map Channel InputMode)   -- ^ Label and input channel states for first process.
        -> Instruction                      -- ^ Instruction from first process.
        -> (Label, Map Channel InputMode)   -- ^ Label and input channel states for second process.
        -> Maybe Instruction                -- ^ Result instruction for first process.

tryStep csMode
        (label1, csState1) instr1
        (label2, csState2)

 = case instr1 of

        -- Jump ---------------------------------------------------------------
        Jump (Next label1' xvsUpdate)
         -> Just $ Jump 
                 $ Next (LabelJoint (label1', csState1)
                                    (label2,  csState2))
                        xvsUpdate


        -- Case ---------------------------------------------------------------
        Case xScrut (Next labelA xvsUpdateA) 
                    (Next labelB xvsUpdateB)
         -> Just
         $  Case xScrut
                  (Next (LabelJoint (labelA, csState1) (label2, csState2)) xvsUpdateA)
                  (Next (LabelJoint (labelB, csState1) (label2, csState2)) xvsUpdateB)


        -- Push ---------------------------------------------------------------
        Push c xx (Next label1' xvsUpdate)
         -- (LocalPush)
         --   Exclusive output, so no other coordination is required.
         |  Just ModeOutput       <- csMode   ? c
         -> Just $ Push c xx
                 $ Next (LabelJoint (label1', csState1)
                                    (label2,  csState2))
                        xvsUpdate

         -- (SharedPush)
         --   Connected output, and the downstream process is ready for the value.
         |  Just ModeConnected    <- csMode   ? c
         ,  Just ModeNone         <- csState2 ? c
         -> Just $ Jump 
                 $ Next (LabelJoint (label1', csState1)
                                    (label2,  Map.insert c ModePending csState2))
                        (Map.insert (VarBuf c) xx xvsUpdate)


        -- Pull ---------------------------------------------------------------
        Pull c x (Next label1' xvsUpdate)
         -- (LocalPull)
         --   First process has exclusive use of the input channel.
         |  Just ModeInputExclusive <- csMode ? c
         -> Just $ Pull c x 
                 $ Next (LabelJoint (label1', csState1) 
                                    (label2,  csState2))
                        xvsUpdate

         -- (SharedPull)
         --   Input channel is used by both processes,
         --   and there is a pending value on the channel.
         |   (Just ModeInputShared == csMode   ? c)
          || (Just ModeConnected   == csMode   ? c)
         ,  Just ModePending       <- csState1 ? c
         -> Just $ Jump
                 $ Next (LabelJoint (label1', Map.insert c ModeHave csState1)
                                    (label2,  csState2))
                        (Map.insert x (XVar (VarBuf c)) xvsUpdate)

         -- (SharedPullInject)
         --   Input channel is used by both processes,
         --   and neither has pulled a value yet.
         |  Just ModeInputShared   <- csMode   ? c
         ,  Just ModeNone          <- csState1 ? c
         ,  Just ModeNone          <- csState2 ? c
         -> Just $ Pull c (VarBuf c)
                 $ Next (LabelJoint (label1, Map.insert c ModePending csState1)
                                    (label2, Map.insert c ModePending csState2))
                         xvsUpdate


        -- Drop ---------------------------------------------------------------
        Drop c (Next label1' xvsUpdate)
         -- (LocalDrop)
         -- Exclusive input, so no other coordination is required. 
         |  Just ModeInputExclusive <- csMode ? c
         -> Just $ Drop c
                 $ Next (LabelJoint (label1', csState1)
                                    (label2,  csState2))
                        xvsUpdate

         -- (ConnectedDrop)
         -- This is a down-stream process and we got the value from upstream.
         |  Just ModeConnected      <- csMode ? c
         -> Just $ Jump 
                 $ Next (LabelJoint (label1', Map.insert c ModeNone csState1)
                                    (label2,  csState2))
                        xvsUpdate

         -- (SharedDropOne)
         -- This is a shared input, but the other process is still using the value.
         |  Just ModeInputShared    <- csMode ? c
         ,    (csState2 ? c == Just ModeHave)
           || (csState2 ? c == Just ModePending)
         -> Just $ Jump
                 $ Next (LabelJoint (label1', Map.insert c ModeNone csState1)
                                    (label2,  csState2))
                        xvsUpdate

         -- (SharedDropBoth)
         -- This is a shared input, and the other process is no longer using the value.
         |  Just ModeInputShared     <- csMode ? c
         ,     (csState2 ? c == Just ModeNone)
         -> Just $ Drop c 
                 $ Next (LabelJoint (label1', Map.insert c ModeNone csState1)
                                    (label2, csState2))
                        xvsUpdate

        _ -> Nothing

