--# -path=.:../common:../abstract

concrete ExtendSom of Extend = CatSom
 --  ** ExtendFunctor -- Add this back when all relevant functions are implemented
 --  with (Grammar=GrammarSom)
  ** open Prelude, ResSom in {

lin
   -- : NP  -> SSlash  -> Utt ;   -- her I love -- Sayeed p. 189-
  FocusObj np sslash =

  -- FocusAdv : Adv -> S       -> Utt ;   -- today I will sleep
  -- FocusAdV : AdV -> S       -> Utt ;   -- never will I sleep
  -- FocusAP  : AP  -> NP      -> Utt ;   -- green was the tree
} ;
