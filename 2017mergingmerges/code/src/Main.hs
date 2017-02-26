
import Machine.Base
import Machine.New
import Machine.Pretty

import Test.Process.Map
import Test.Eval.Map
import Test.Fuse.GroupMerge
import Test.Fuse.Map
import Test.Comb
import Test.Stat

import Machine.Combinator.Group
import Machine.Combinator.Map
import Machine.Combinator.Merge
import Machine.Transform.Fuse
import Machine.Transform.StripLabels

import Data.Maybe
import Text.PrettyPrint.Leijen  hiding ((<$>))

main = countBothUpTo 8

countBothUpTo n = do
  putStrLn "Splits"
  countUpTo n manySplitAB
  putStrLn ""
  putStrLn "Pipes"
  countUpTo n manyPipeAB

countUpTo n f
 = mapM_ countUpTo' [1..n]
 where
  countUpTo' num = do
    putStr ("Combinators: " ++ show num ++ "\t\tStates: ")
    print $ countsMax $ f num combs1

combs1 
 =      [ combMapSucc
        , combFilterPos
        , combScanAdd
        , combGroup 
        , combMerge ]

counts cs
        = putStrLn
        $ unlines
        $ map (show    . statOfProcess) 
        $ map (evalNew . mkComb) 
        $ cs


countsMax cs
        = maximum
        $ map (statInstrs . statOfProcess)
        $ map (evalNew . mkComb) 
        $ cs

countPipes n
        = counts 
        $ manyPipeAB n combs1

countSplits n 
        = counts 
        $ manySplitAB n combs1

countSplitPipe n
        = counts
        $ manySplitAB n
        $ concat [manyPipeAB i combs1 | i <- [0..n]]
