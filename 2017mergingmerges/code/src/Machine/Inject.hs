
module Machine.Inject where
import Machine.Base
import Data.Map                 (Map)
import qualified Data.Map       as Map


-- | Inject a value into a possibly shared input channel of the given
--   processeses. All processes which have the named channel as one 
--   of their input channels must accept the value, otherwise `Nothing`.
injects :: [Process] -> Channel -> Value -> Maybe [Process]
injects ps c v
 = mapM (\p -> inject p c v) ps


-- | Inject a value into an input channel of a process.
inject  :: Process   -> Channel -> Value -> Maybe Process
inject p c v 

 -- InjectValue
 | Just None    <- Map.lookup c (processInStates p)
 = Just $ p { processInStates
               = Map.insert c (Pending v) (processInStates p) }

 -- InjectIgnore
 | Nothing      <- Map.lookup c (processInStates p)
 = Just $ p

 -- No rule matches.
 -- The process was not ready to receive input on this channel.
 | otherwise
 = Nothing 


