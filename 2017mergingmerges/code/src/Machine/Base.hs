
module Machine.Base where
import Data.Map                 (Map)
import Data.Set                 (Set)
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-------------------------------------------------------------------------------
-- | Generic name type.
type Name
        = String


-- | Channel name.
data Channel
        = Channel Name
        deriving (Show, Eq, Ord)


-- | Variable name.
data Var        
        = Var    Name
        | VarBuf Channel
        deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
-- | Expressions.
data Expr
        = XVal Value
        | XVar Var
        | XAdd Expr Expr
        | XApp Expr Expr
        deriving (Show, Eq)


-- | Values.
data Value
        = VInt  Int
        | VBool Bool
        | VSucc 
        | VAbs  Var Expr
        deriving (Show, Eq)


-------------------------------------------------------------------------------
-- | Like InputState, but does not carry the value in the 'Pending' state.
data InputMode
        = ModeNone
        | ModePending
        | ModeHave
        deriving (Show, Eq, Ord)


-- | Describes the state of the input buffer for each channel.
data InputState
        -- | No element is buffered.
        = None

        -- | A single value has been added to the buffer,
        --   but has not been read from the buffer yet.
        | Pending Value

        -- | A single value has been added to the buffer,
        --   and is currently being used.
        | Have
        deriving (Show, Eq)


inputModeOfState :: InputState -> InputMode
inputModeOfState ss
 = case ss of
        None            -> ModeNone
        Pending _       -> ModePending
        Have            -> ModeHave


-- | Code label.
data Label      
        = Label         Name
        | LabelJoint    (Label, Map Channel InputMode)
                        (Label, Map Channel InputMode)
        deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
-- | A single instruction in the process.
data Instruction
        = Pull  Channel Var     Next
        | Drop  Channel         Next
        | Push  Channel Expr    Next
        | Case  Expr            Next Next
        | Jump                  Next
        deriving (Show, Eq)


-- | Move to next label and set variables to the given expressions.
data Next
        = Next Label (Map Var Expr)
        deriving (Show, Eq)

-- | Swap the components of joint labels in the given instruction.
swapLabelsOfInstruction :: Instruction -> Instruction
swapLabelsOfInstruction ii
 = case ii of
        Pull c v n      -> Pull c v $ swapLabelsOfNext n
        Drop c   n      -> Drop c   $ swapLabelsOfNext n
        Push c x n      -> Push c x $ swapLabelsOfNext n
        Case x   n1 n2  -> Case x (swapLabelsOfNext n1) (swapLabelsOfNext n2)
        Jump     n      -> Jump     $ swapLabelsOfNext n


-- | Swap the components of joint labels in the given next instruction indicator.
swapLabelsOfNext :: Next -> Next
swapLabelsOfNext nn
 = case nn of
        Next (LabelJoint ls1 ls2) us    -> Next (LabelJoint ls2 ls1) us
        _                               -> nn


-- | Get the set of outgoing labels in the given instruction.
outLabelsOfInstruction  :: Instruction -> Set Label
outLabelsOfInstruction instr
 = case instr of
        Pull _ _ (Next l _)              -> Set.singleton l
        Drop _   (Next l _)              -> Set.singleton l
        Push _ _ (Next l _)              -> Set.singleton l
        Case _   (Next l1 _) (Next l2 _) -> Set.fromList  [l1, l2]
        Jump     (Next l _)              -> Set.singleton l


-------------------------------------------------------------------------------
-- | Value heap?
data Heap
        = Heap (Map Var Value)
        deriving Show


-- | A stream process.
data Process
        = Process
        { -- | Name of process.
          processName   :: String

          -- | Map of input channel name to what state it's in.
        , processIns    :: Map Channel InputState

          -- | Set of output channel names.
        , processOuts   :: Set Channel

          -- | Value heap?
        , processHeap   :: Heap

          -- | Process label?
        , processLabel  :: Label

          -- | Labeled instructions.
        , processBlocks :: [(Label, Instruction)]
        }
        deriving Show


-- | A nest of processes.
data Nest
        = Nest [Process]
        deriving (Show)

