
module MapMap where
import Machine.Base
import Machine.Execute
import Machine.Fuse
import Control.Monad.State.Strict
import qualified Data.Map       as Map
import qualified Data.Set       as Set
import qualified Text.Show.Pretty       as P


inputs
 = Map.fromList
 [ (Channel "as", [VInt 1, VInt 2, VInt 3]) ]

pMap1 :: Process
pMap1
 = Process
 { processName          = "map1"
 , processIns           = Set.fromList [Channel "as"]
 , processInStates      = Map.fromList [(Channel "as", None)]
 , processOuts          = Set.fromList [Channel "bs"]
 , processHeap          = Heap (Map.fromList [(Var "a1", VInt 0)])
 , processLabel         = Label "map10"
 , processBlocks        =
        [ ( Label "map10"
          , Pull (Channel "as") (Var "a1")
                 (Next (Label "map11") Map.empty))

        , ( Label "map11"
          , Push (Channel "bs") (XApp (XVal VSucc) (XVar (Var "a1")))
                 (Next (Label "map12") Map.empty))

        , ( Label "map12"
          , Drop (Channel "as")
                 (Next (Label "map10") Map.empty))
        ]
 }


pMap2 :: Process
pMap2   
 = Process
 { processName          = "map2"
 , processIns           = Set.fromList [Channel "bs"]
 , processInStates      = Map.fromList [(Channel "bs", None)]
 , processOuts          = Set.fromList [Channel "cs"]
 , processHeap          = Heap (Map.fromList [(Var "b1", VInt 0)])
 , processLabel         = Label "map20"
 , processBlocks        =
        [ ( Label "map20"
          , Pull (Channel "bs") (Var "b1")
                 (Next (Label "map21") Map.empty))

        , ( Label "map21"
          , Push (Channel "cs") (XApp (XVal VSucc) (XVar (Var "b1")))     
                 (Next (Label "map22") Map.empty))

        , ( Label "map22"
          , Drop (Channel "bs")
                 (Next (Label "map20") Map.empty))
        ]
 }

