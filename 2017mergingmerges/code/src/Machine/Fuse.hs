
module Machine.Fuse where
import Machine.Base
import Data.Map                         (Map)
import qualified Data.Map               as Map


data ChannelMode        
        = ModeInputExclusive
        | ModeInputShared
        | ModeOutput
        | ModeConnected
        deriving (Show, Eq)

(?) m x = Map.lookup x m

---------------------------------------------------------------------------------------------------
channelModes
        :: Process
        -> Process
        -> 



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


