
module Machine.New
        ( module Control.Monad.State.Strict
        , New (..)
        , evalNew
        , newIx
        , newLabel, newChannel
        , newVar)
where
import Machine.Base
import Control.Monad.State.Strict


-- | Type of computations that can allocate new names.
type New a 
 = State Int a

-- | Produce a new index.
newIx :: New Int
newIx 
 = do   i   <- get
        put (i + 1)
        return i


-- | Produce a new label name.
newLabel  :: String -> New Label
newLabel prefix
 = do   i   <- newIx
        return $ Label $ "l" ++ prefix ++ show i


-- | Produce a new channel name of the given type.
newChannel  :: String -> Type -> New Channel
newChannel prefix t
 = do   i   <- newIx
        return $ Channel ("c" ++ prefix ++ show i) t


-- | Produce a new variable.
newVar  :: String -> New Var
newVar prefix
 = do   i   <- newIx
        return $ Var   $ "x" ++ prefix ++ show i


-- | Evaluate a `New` computation.
evalNew  :: New a -> a
evalNew n
 = evalState n 0

