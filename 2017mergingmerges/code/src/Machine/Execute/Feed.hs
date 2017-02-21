
module Machine.Execute.Feed where
import Machine.Execute.Inject
import Machine.Base
import Data.Map                         (Map)
import Data.List
import qualified Data.Map.Strict        as Map


-- | Feed input into the given proceses.
feedProcesses 
 ::  Map Channel [Value]         -- ^ Values on input channels.
 -> [Process]                    -- ^ Current processes.
 ->  Maybe ( Map Channel [Value] -- New processes and remaining values.
           , [Process])

feedProcesses cvs ps
  = do  (cvs', ps') <- feedProcessList (Map.toList cvs) ps
        return (Map.fromList cvs', ps')


feedProcessList 
 :: [(Channel, [Value])]
 -> [Process]
 -> Maybe ( [(Channel, [Value])]
          , [Process])

feedProcessList []  ps
 =      return ([], ps)

feedProcessList (cvs : cvss) ps 
 = do   (cvs',  ps')    <- feedProcess1    cvs  ps  
        (cvss', ps'')   <- feedProcessList cvss ps' 
        return (cvs' : cvss', ps'')


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

