
module Machine.Execute.Feed where
import Machine.Execute.Inject
import Machine.Base
import Data.Map                         (Map)
import Data.List
import qualified Data.Map.Strict        as Map


-- | Feed input into the given proceses.
--
--   If a particular channel has values then all processes that 
--   have that channel as an input must accept it.

feedProcesses 
 ::  Map Channel [Value]         -- ^ Values on input channels.
 -> [Process]                    -- ^ Current processes.
 ->  Maybe ( Map Channel [Value] -- New processes and remaining values.
           , [Process])

feedProcesses cvs ps
  = do  (cvs', ps') <- feedProcessList (Map.toList cvs) ps
        return (Map.fromList cvs', ps')


-- | Try to feed a value from one of the given channels into
--   all of the processes that have that channel as an input.
feedProcessList 
 :: [(Channel, [Value])]
 -> [Process]
 -> Maybe ( [(Channel, [Value])]
          , [Process])

-- No more channels to handle.
feedProcessList []  ps
 =      Nothing

-- If the current channel is already empty then skip to the next one
feedProcessList (c@(_, []) : cvs) ps
 = do   
        (cvs', ps')     <- feedProcessList cvs ps
        return (c : cvs', ps')

-- Try to feed input from a single channel to the given processes.
feedProcessList (cvs : cvss) ps 
 = case feedProcess1 cvs ps of
        -- All the processes with the current channel as the input
        -- accepted the current value.
        Just (cvs', ps') 
         -> Just (cvs' : cvss, ps')

        -- One of the processes did not accept the current value,
        -- so try to feed a value from the next channel.
        Nothing 
         -> case feedProcessList cvss ps of
                -- We managed to feed a value from one of the channels.
                Just (cvss', ps')
                 -> Just (cvs : cvss', ps')

                -- We can't feed values from any channel into any process.
                Nothing -> Nothing


-- | Try to feed the next value from the given channel
--   to any the processes in the given list.
feedProcess1 
 :: (Channel, [Value]) 
 -> [Process] 
 -> Maybe ( (Channel, [Value])
          , [Process])

feedProcess1 (c, []) ps 
 = Just ((c, []), ps)

feedProcess1 (c, (v: vs)) ps 
 | Just  ps'    <- injects ps c v
 = Just ((c, vs), ps')

 | otherwise
 = Nothing







