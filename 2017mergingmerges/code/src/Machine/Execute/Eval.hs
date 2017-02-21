
module Machine.Execute.Eval where
import Machine.Base
import Data.Map                         (Map)
import qualified Data.Map.Strict        as Map
import Data.List


-- | Evaluate an expression in the current heap.
eval   :: Heap -> Expr -> Value
eval heap@(Heap hsHeap) xx
 = case xx of
        XVal value
         ->  value

        XVar var
         -> case Map.lookup var hsHeap of
                Just value      -> value
                Nothing         -> error $ "eval: unbound variable " ++ show var

        XApp x1 x2
         -> case eval heap x1 of
                VLit p
                 -> evalPrimValue p [eval heap x2]

                VPAP p vs
                 -> evalPrimValue p (vs ++ [eval heap x2])


-- | Evaluate a primitive applied to some values.
evalPrimValue :: Prim -> [Value] -> Value
evalPrimValue p vs
 | length vs == arityOfPrim p
 = case (do ps <- sequence $ map takePrimOfValue vs
            evalPrimPrim p ps) of
        Nothing -> error $ "evalValue: prim evaluation failure " ++ show (p, vs)
        Just p' -> VLit p'


 | length vs < arityOfPrim p
 = VPAP p vs

 | otherwise
 = error $ "evalValue: over application of primitive " ++ show (p, vs)


-- | Evaluate a primitive operator applied to some primitive arguments.
evalPrimPrim  :: Prim -> [Prim]  -> Maybe Prim
evalPrimPrim pp ps
 = case (pp, ps) of
        (POr,  [PBool b1, PBool b2])    -> Just $ PBool (b1 || b2)
        (PAnd, [PBool b1, PBool b2])    -> Just $ PBool (b1 && b2)
        (PAdd, [PInt  i1, PInt  i2])    -> Just $ PInt  (i1 +  i2)
        (PEq,  [PInt  i1, PInt  i2])    -> Just $ PBool (i1 == i2)
        (PNeq, [PInt  i1, PInt  i2])    -> Just $ PBool (i1 /= i2)
        (PLt,  [PInt  i1, PInt  i2])    -> Just $ PBool (i1 <  i2)
        (PLe,  [PInt  i1, PInt  i2])    -> Just $ PBool (i1 <= i2)
        (PGt,  [PInt  i1, PInt  i2])    -> Just $ PBool (i1 >  i2)
        (PGe,  [PInt  i1, PInt  i2])    -> Just $ PBool (i1 >= i2)
        _                               -> Nothing


