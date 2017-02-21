
module Machine.Transform.StripLabels
        (stripLabels)
where
import Machine.Base
import Machine.New
import Control.Monad.State.Strict
import qualified Data.Map       as Map
import Data.Map                 (Map)


-------------------------------------------------------------------------------
-- | Strip out joint instruction labels from the given process,
--   replacing them by freshly allocated simple labels.
stripLabels 
        :: String               -- ^ Prefix for new labels.
        -> Process              -- ^ Process to strip.
        -> New (Process, Map Label Label)
                                -- ^ Map of old joint labels to new ones.

stripLabels str process
 = do
        ix      <- get

        let (process', state')
                = runState (stripProcess process)
                $ StateS 
                { statePrefix   = str
                , stateGen      = ix
                , stateMap      = Map.empty }

        put $ stateGen state'

        let mp  = stateMap state'
        let mp' = Map.fromList [(l', l) | (l, l') <- Map.toList mp]

        return (process', mp')


-------------------------------------------------------------------------------
type S a
        = State StateS a

data StateS
        = StateS
        { statePrefix   :: String
        , stateGen      :: Int
        , stateMap      :: Map Label Label }


-- | Strip joint labels from a process.
stripProcess :: Process -> S Process
stripProcess process
 = do   
        l'      <- stripLabel 
                $  processLabel  process

        is'     <- stripInstructions
                $  processBlocks process

        return  $ process
                { processLabel  = l'
                , processBlocks = is' }


-- | Strip joint labels from labeled instructions.
stripInstructions
        ::   [(Label, Instruction)]
        -> S [(Label, Instruction)]

stripInstructions lis
 = do
        let (ls, is)    = unzip lis
        
        -- Strip the labels first so they're named in order.
        ls'             <- mapM stripLabel ls

        -- Now strip the instructions,
        -- which replaces joint labels by ones we've just allocated.
        is'             <- mapM stripInstruction is

        return $ zip ls' is'


-- | Strip joint labels from an instruction.
stripInstruction :: Instruction -> S Instruction
stripInstruction instr
 = case instr of
        Pull c v nx     -> Pull <$> pure c <*> pure v       <*> stripNext nx
        Drop c nx       -> Drop <$> pure c                  <*> stripNext nx
        Push c x nx     -> Push <$> pure c <*> pure x       <*> stripNext nx
        Case x n1 n2    -> Case <$> pure x <*> stripNext n1 <*> stripNext n2
        Jump nx         -> Jump <$> stripNext nx


-- | Strip a next indicator.
stripNext :: Next -> S Next
stripNext (Next l us)
 = Next <$> stripLabel l <*> pure us


-- | Strip a joint label.
stripLabel :: Label -> S Label
stripLabel ll
 = case ll of
        Label _
         -> return ll

        LabelJoint{}
         -> do  mp      <- gets stateMap
                case Map.lookup ll mp of
                 Just ll'       -> return ll'
                 Nothing
                  -> do s       <- get 
                        let px  = statePrefix s
                        let ix  = stateGen s
                        let mp  = stateMap s

                        let l'  = Label (px ++ replicate (3 - (length (show ix))) '0'
                                            ++ show ix)
                        let ix' = ix + 1
                        let mp' = Map.insert ll l' mp
                
                        put     $ s { stateGen  = ix'
                                    , stateMap  = mp' }
                        return l'

