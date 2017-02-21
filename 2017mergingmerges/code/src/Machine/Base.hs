{-# LANGUAGE PatternSynonyms #-}
module Machine.Base where
import Data.Map                 (Map)
import Data.Set                 (Set)
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-------------------------------------------------------------------------------
-- | Generic name.
type Name
        = String


-- | Channel name,
--   along with the type of the values that flow through the channel.
data Channel
        = Channel Name Type
        deriving (Show, Eq, Ord)


-- | Variable name.
data Var        
        -- | Variable with the given name.
        = Var    Name

        -- | Variable used as the buffer for some channel.
        | VarBuf Channel
        deriving (Show, Eq, Ord)


-- | Get the type associated with a channel.
typeOfChannel :: Channel -> Type
typeOfChannel (Channel _ t) = t



-------------------------------------------------------------------------------
-- | Value types.
data Type
        = TBool                 -- ^ Type of booleans.
        | TInt                  -- ^ Type of integers.
        deriving (Show, Eq, Ord)


-- | Expressions.
data Expr
        = XVal Value            -- ^ Values.
        | XVar Var              -- ^ Variables.
        | XApp Expr Expr        -- ^ Generic application.
        deriving (Show, Eq)

infixl 5 @@
(@@) = XApp

-- | Values.
data Value
        = VAbs  Var  Expr       -- ^ Function abstraction.
        | VLit  Prim            -- ^ Literal primitive value.
        | VPAP  Prim [Value]    -- ^ Partially applied primitive.
        deriving (Show, Eq)


-- | Primitives.
data Prim
        = PBool Bool            -- ^ Boolean literal.
        | POr                   -- ^ Boolean or.
        | PAnd                  -- ^ Boolean and.

        | PInt  Int             -- ^ Integer literal.
        | PAdd                  -- ^ Integer addition.

        | PEq                   -- ^ Primitive equality   check.
        | PNeq                  -- ^ Primitive inequality check.
        deriving (Show, Eq)


pattern XBool b = XVal (VLit (PBool b))
pattern XOr     = XVal (VLit  POr)
pattern XAnd    = XVal (VLit  PAnd)
pattern XInt  i = XVal (VLit (PInt i))
pattern XAdd    = XVal (VLit  PAdd)
pattern XEq     = XVal (VLit  PEq)
pattern XNeq    = XVal (VLit  PNeq)

pattern VBool b = VLit (PBool b)
pattern VOr     = VLit  POr
pattern VAnd    = VLit  PAnd
pattern VInt  i = VLit (PInt  i)
pattern VAdd    = VLit  PAdd
pattern VEq     = VLit  PEq
pattern VNeq    = VLit  PNeq


-- | Get a default value of the given type.
--   These values can be used to initialize heap variables that will not
--   be used until they are updated with a real value read from some stream.
defaultValueOfType :: Type -> Value
defaultValueOfType tt
 = case tt of
        TBool   -> VBool False
        TInt    -> VInt  0


-- | Get the default value associated with the type of a channel.
defaultValueOfChannel :: Channel -> Value
defaultValueOfChannel cc
 = defaultValueOfType $ typeOfChannel cc


-------------------------------------------------------------------------------
-- | Describes the state of the current value available from a channel.
data InputState
        -- | The value has not yet been read from the channel.
        = None

        -- | The value has been read from the channel 
        --   and stored in the associated single-element buffer,
        --   but has not yet been stored in a process-specific variable.
        | Pending Value

        -- | The value has been stored in a process specific variable,
        --   and is currently being used.
        | Have
        deriving (Show, Eq)


-- | Like InputState, but does not carry the value in the 'Pending' state.
data InputMode
        = ModeNone
        | ModePending
        | ModeHave
        deriving (Show, Eq, Ord)


-- | Yield the `InputMode` associated with an `InputState`.
inputModeOfState :: InputState -> InputMode
inputModeOfState ss
 = case ss of
        None            -> ModeNone
        Pending _       -> ModePending
        Have            -> ModeHave


-------------------------------------------------------------------------------
-- | Code label.
data Label      
        = Label         Name
        | LabelJoint    (Label, Map Channel InputMode)
                        (Label, Map Channel InputMode)
        deriving (Show, Eq, Ord)


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


-- | Construct a next instruction indicator with an empty set of updates.
next :: Label -> Next
next l  = Next l Map.empty


-- | Llike `next` but take a list of updates.
next' :: Label -> [(Var, Expr)] -> Next
next' l us  = Next l (Map.fromList us)


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

