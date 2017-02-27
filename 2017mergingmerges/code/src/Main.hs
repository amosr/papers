
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

main = countUpTo 7

countUpTo n
 = mapM_ countUpTo' [1..n]
 where
  countUpTo' num = do
    let countWith str f combs = do
          putStr ("\t" ++ str)
          print $ countsMax $ f num combs
    putStrLn ("Combinators: " ++ show num)
    countWith "Splits all:       " (manyCombineWith fuseSplitFirstManyInputs) combsAll
    countWith "Splits no merge:  " (manyCombineWith fuseSplitFirstManyInputs) combsNoMerge
    countWith "SplitMany merge:  " (manyCombineWith fuseSplitManyInputs) combsOnlyMerge
    countWith "Pipes all:        " manyPipeAB combsAll
    countWith "Pipes no merge:   " manyPipeAB combsNoMerge
    countWith "PipeMany merge:   " (manyCombineWith fusePipeMultipleInputs) combsOnlyMerge


combsAll
 =      [ combMapSucc
        , combFilterPos
        , combScanAdd
        , combGroup
        , combMerge ]

combsNoMerge
 =      [ combMapSucc
        , combFilterPos
        , combScanAdd
        , combGroup ]

combsOnlyMerge
 =      [ combMerge ]



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
        $ manyPipeAB n combsAll

countSplits n
        = counts
        $ manySplitAB n combsAll

countSplitPipe n
        = counts
        $ manySplitAB n
        $ concat [manyPipeAB i combsAll | i <- [0..n]]
