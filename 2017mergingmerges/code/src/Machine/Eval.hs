
module Machine.Eval where
import Machine.Base
import Data.Map                         (Map)
import qualified Data.Map.Strict        as Map
import Data.List


-- | Evaluate an expression in the current heap.
eval   :: Heap -> Expr -> Value
eval heap@(Heap hsHeap) xx
 = case xx of
        XVal value
         -> value

        XVar var
         -> case Map.lookup var hsHeap of
                Just value      -> value
                Nothing         -> error $ "eval: unbound variable " ++ show var

        XApp x1 x2
         -> case eval heap x1 of
                VAbs var2 xBody -> error "eval: finish me"

                VAdd
                 |  v2          <- eval heap x2
                 -> VPAP PAdd [v2]

                VPAP PAdd [VInt i1]
                 |  VInt i2     <- eval heap x2
                 -> VInt (i1 + i2)

