{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Machine.Pretty where
import Machine.Base
import Text.PrettyPrint.Leijen
import Data.Map                 (Map)
import Data.Set                 (Set)
import qualified Data.Map       as Map
import qualified Data.Set       as Set


instance (Pretty a, Pretty b) 
       => Pretty (Map a b) where
 pretty mp
        =   parens
        $   string "map" <+> indent 0
        (   vsep [parens $ string "def" <+> pretty x <+> pretty y
                 | (x, y) <- Map.toList mp])


instance Pretty a 
      => Pretty (Set a) where
 pretty st
        =   parens
        $   string "set" <+> indent 0
        (   vsep $ map pretty $ Set.toList st)



instance Pretty Name where
 pretty str     
  = string $ show str


instance Pretty Channel where
 pretty (Channel n t)
  = parens 
        $   string "channel" 
        <+> pretty n
        <+> pretty t


instance Pretty Var where
 pretty vv
  = case vv of
        Var n           -> pretty n
        VarBuf c        -> parens $ string "buf" <+> pretty c


instance Pretty Type where
 pretty tt
  = case tt of
        TBool           -> string "bool"
        TInt            -> string "int"


instance Pretty Expr where
 pretty xx
  = case xx of
        XVal v          -> pretty v
        XVar n          -> parens $ string "var" <+> pretty n
        XApp x1 x2      -> parens $ string "app" <+> pretty x1 <+> pretty x2


instance Pretty Value where
 pretty vv 
  = case vv of
        VAbs v x        -> parens $ string "abs" <+> pretty v  <+> pretty x
        VLit l          -> pretty l
        VPAP p vs       -> parens $ string "pap" <+> pretty p  <+> pretty vs


instance Pretty Prim where
 pretty pp
  = case pp of
        PBool True      -> parens $ string "bool true" 
        PBool False     -> parens $ string "bool false"
        POr             -> string "or"
        PAnd            -> string "and"
        PInt i          -> parens $ string "int" <+> int i
        PAdd            -> string "add"
        PEq             -> string "eq"
        PNeq            -> string "neq"
        PLt             -> string "lt"
        PLe             -> string "le"
        PGt             -> string "gt"
        PGe             -> string "ge" 


instance Pretty InputState where
 pretty is
  = case is of
        None            -> string "none"
        Pending v       -> parens $ string "pending" <+> pretty v
        Have            -> string "have"


instance Pretty InputMode where
 pretty im
  = case im of
        ModeNone        -> string "none"
        ModePending     -> string "pending"
        ModeHave        -> string "have"


instance Pretty Label where
 pretty ll
  = case ll of
        Label n         
         -> pretty n

        LabelJoint (l1, mcs1) (l2, mcs2)
         -> parens
         $  string "joint" <+> indent 0
         (  vcat
                [ padR 6 (pretty l1) <+> (pretty mcs1)
                , padR 6 (pretty l2) <+> (pretty mcs2)])

padR n x
 = x <+> string (replicate (n - length (show x)) ' ')


instance Pretty Next where
 pretty (Next l mp)
  | Map.null mp
  = parens
        $   string "next"
        <+> pretty l

 pretty (Next l us)
  = parens 
        $   string "next" 
        <+> pretty l
        <+> (indent 0 $ vcat $ map prettySetVar $ Map.toList us)

prettySetVar (v, x)
 = parens
        $   pretty "set"
        <+> pretty v
        <+> pretty x

 
instance Pretty Instruction where
 pretty ii
  = case ii of
        Pull c v n      
         ->  parens $ string "pull" 
         <+> indent 0
                (vcat   [ pretty c <+> pretty v
                        , pretty n])

        Drop c n        
         ->  parens $ string "drop" 
         <+> indent 0
                (vcat   [ pretty c 
                        , pretty n])

        Push c x n
         ->  parens $ string "push" 
         <+> indent 0
                (vcat   [ pretty c <+> pretty x
                        , pretty n])

        Case x n1 n2
         ->  parens $ string "case" 
         <+> indent 0
                (vcat   [ pretty x
                        , pretty n1
                        , pretty n2])

        Jump n 
         ->  parens $ string "jump" 
         <+> indent 0
                (pretty n)


instance Pretty Heap where
 pretty (Heap hh)
  = pretty hh


instance Pretty Process where
 pretty pp
  = parens   $ indent 0
  $ vcat  
  [ parens $ text "name  "     <+> (pretty $ processName   pp)
  , parens $ text "ins   "     <+> (pretty $ processIns    pp)
  , parens $ text "outs  "     <+> (pretty $ processOuts   pp)
  , parens $ text "heap  "     <+> (pretty $ processHeap   pp)
  , parens $ text "label "     <+> (pretty $ processLabel  pp)

  , parens 
        $    text "blocks"    
        <+>  indent 0 (vcat $ map prettyLabeledInstruction $ processBlocks pp) ]


prettyLabeledInstruction (l, i)
 = parens 
        $   text   "instr" 
        <+> pretty l
        <+> pretty i

