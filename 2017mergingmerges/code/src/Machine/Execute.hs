
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
         = fromMaybe (error $ unlines
                            [ "execute: feed failed"
                            , P.ppShow cvsIn
                            , P.ppShow ps ])
                     (feedProcesses cvsIn ps)

        (ps'',   as')
         = shakeSteps [] ps' []

   in   execute cvsIn' cvsOut ps'' (acc ++ as')




executeTraceIO
        :: Int 
        -> Map Channel [Value]          -- Input  channels values.
        -> Map Channel [Value]          -- Output channel values.
        -> [Process]                    -- Processes.
        -> [Action]                     -- Actions
        -> IO ()   



executeTraceIO n cvsIn cvsOut ps acc
 | n <= 0
 = return ()

 | all null $ map snd $ Map.toList cvsIn
 = return () --(cvsIn, ps, acc)

 | otherwise
 = do   
        putStrLn "* step"
        dumpChannels cvsIn
        dumpProcesses ps

        -- Feed input into the processes that are ready for it.
        case feedProcesses cvsIn ps of
         Nothing        
          -> return ()

         Just (cvsIn_fed, ps_fed)
          -> do -- Try to advance each process.
                let lsBefore             = map processLabel ps_fed
                let (ps_shaken, actions) = shakeSteps [] ps_fed []
                let lsAfter              = map processLabel ps_shaken
        
                dumpProcesses ps_shaken

                mapM (putStrLn . show) actions

                let cvsOut'  = foldl' collectOutput cvsOut 
                             $ actions

                dumpChannels cvsOut'

                executeTraceIO (n - 1) cvsIn_fed cvsOut' ps_shaken (acc ++ actions)

collectOutput 
        :: Map Channel [Value] 
        -> Action
        -> Map Channel [Value]

collectOutput cs (ActionPush c v) 
 = Map.alter (\x -> case x of
                Nothing -> Just []
                Just vs -> Just (vs ++ [v]))
        c cs



dumpChannels :: Map Channel [Value] -> IO ()
dumpChannels cvs
 =      putStrLn 
         $  (unlines 
                [ "  - channel " ++ show (c, vs)
                        | (c, vs)       <- Map.toList cvs ])

dumpProcesses :: [Process] -> IO ()
dumpProcesses ps
 =      putStrLn
         $ unlines $ concat 
                $ [ [ "  - process   " ++ processName p
                    , "      ins   = " ++ show (Map.toList $ processIns   p)
                    , "      label = " ++ show (processLabel p) ]
                        | p <- ps ] 




