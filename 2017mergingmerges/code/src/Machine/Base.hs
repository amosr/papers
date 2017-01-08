
module Machine.Base where
import qualified Data.Map       as Map
import Data.Map                 (Map)


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
        = Label Name
        deriving (Show, Eq)
        

-- | Value heap?
data Heap
        = Heap (Map Var Value)
        deriving Show



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
          processName   :: String

          -- | Map of input channels to process to what state they're in.
        , processIns    :: Map Channel InputState

          -- | Map of output channel names.
        , processOuts   :: [Channel]

          -- | Value heap?
        , processHeap   :: Heap

          -- | Process label?
        , processLabel  :: Label

          -- | Labeled instructions.
        , processBlocks :: [(Label, Instruction)]
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


data Expr
        = XVal Value
        | XVar Var
        | XAdd Expr Expr
        | XApp Expr Expr
        deriving (Show, Eq)

data Value
        = VInt  Int
        | VBool Bool
        | VSucc 
        | VAbs  Var Expr
        deriving (Show, Eq)

data Component
        = ComponentProcess Process
        | ComponentInput   Channel [Value]
        | ComponentOuptut  Channel [Value]     

