
module Machine.Feed where
import Machine.Base
import Data.Map                         (Map)
import Data.List
import qualified Data.Map.Strict        as Map


type ChannelValueS a    
        =   Map Channel [Value] -> a
        -> (Map Channel [Value], a)


-- | Feed input into a channel, if it needs it.
feedInputState ::  ChannelValueS (Channel, InputState)
feedInputState cvs (c, state)
 = case state of
        None    
         -> case Map.lookup c cvs of
                Nothing         -> (cvs, (c, state))
                Just (v:vs)     -> (Map.insert c vs cvs, (c, Pending v))

        _ -> (cvs, (c, state))


-- | Feed input into the given process.
feedProcess :: ChannelValueS Process
feedProcess cvs p
 = let  (cvs', cis')    = mapAccumL feedInputState cvs 
                        $ Map.toList $ processIns p

        p'              = p { processIns = Map.fromList cis' }
   in   (cvs', p')


-- | Feed input into the given proceses.
feedProcesses :: ChannelValueS [Process]
feedProcesses cvs ps
        = mapAccumL feedProcess cvs ps