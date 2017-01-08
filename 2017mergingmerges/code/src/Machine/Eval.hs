
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

        XAdd x1 x2
         |  VInt i1             <- eval heap x1
         ,  VInt i2             <- eval heap x2
         -> VInt (i1 + i2)

        XApp x1 x2
         |  VAbs var1 xBody     <- eval heap x1
         ,  v2                  <- eval heap x2
         -> error "eval: finish me"

         |  VSucc               <- eval heap x1
         ,  VInt i2             <- eval heap x2
         -> VInt (i2 + 1)


