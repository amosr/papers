
module Machine.Execute where
import Machine.Execute.Shake
import Machine.Execute.Feed
import Machine.Base
import Data.Map                         (Map)
import Data.List
import qualified Data.Map.Strict        as Map
import Data.Maybe
import qualified Text.Show.Pretty       as P

execute 
        :: Map Channel [Value]          -- Input  channels values.
        -> Map Channel [Value]          -- Output channel values.
        -> [Process]                    -- Processes.
        -> [Action]                     -- Actions
        -> (Map Channel [Value], [Process], [Action])     
                                        -- Processes after execution.

execute cvsIn cvsOut ps acc
 | all null $ map snd $ Map.toList cvsIn
 = (cvsIn, ps, acc)

 | otherwise
 = let  (cvsIn', ps')   
         = fromMaybe (error $ "execute: feed failed" ++ P.ppShow ps)
                     (feedProcesses cvsIn ps)

        (ps'',   as')
         = shakeSteps [] ps' []

   in   execute cvsIn' cvsOut ps'' (acc ++ as')


