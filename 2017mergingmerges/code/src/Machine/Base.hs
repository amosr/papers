
module Machine.Base where
import Data.Map                 (Map)
import Data.Set                 (Set)
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | Generic name type.
type Name
        = String


-- | Channel name.
data Channel
        = Channel Name
        deriving (Show, Eq, Ord)


-- | Variable name.
data Var        
        = Var Name
        deriving (Show, Eq, Ord)

-- | Code label.
data Label      
        = Label         Name
        | LabelJoint    (Label, Map Channel InputMode)
                        (Label, Map Channel InputMode)
        deriving (Show, Eq)
        

-- | Value heap?
data Heap
        = Heap (Map Var Value)
        deriving Show


-- | Like InputState, but does not carry the value in the 'Pending' state.
data InputMode
        = ModeNone
        | ModePending
        | ModeHave
        deriving (Show, Eq)


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


-- | A nest of processes.
data Nest
        = Nest [Process]
        deriving (Show)


-- | A stream process.
data Process
        = Process
        { -- | Name of process.
          processName           :: String

          -- | Set of input channel names.
        , processIns            :: Set Channel

          -- | Map of input channel name to what state it's in.
        , processInStates       :: Map Channel InputState

          -- | Set of output channel names.
        , processOuts           :: Set Channel

          -- | Value heap?
        , processHeap           :: Heap

          -- | Process label?
        , processLabel          :: Label

          -- | Labeled instructions.
        , processBlocks         :: [(Label, Instruction)]
        }
        deriving Show


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


