

module Test.Stat where
import Machine.Base


-- | Collect statistics about a process.
data Stat       
        = Stat
        { statName      :: String       -- ^ Name of the process.
        , statInstrs    :: Int  }       -- ^ Number of instructions in the process
        deriving Show

-- | Collect statistics about a process.
statOfProcess :: Process -> Stat
statOfProcess process
        = Stat
        { statName      = processName process
        , statInstrs    = length $ processBlocks process }