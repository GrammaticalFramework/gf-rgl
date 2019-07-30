--# -path=.:../common:../abstract

concrete ExtendSom of Extend = CatSom
 --  ** ExtendFunctor -- Add this back when all relevant functions are implemented
 --  with (Grammar=GrammarSom)
  ** open Prelude, ResSom in {

lin
   -- : NP  -> SSlash  -> Utt ;   -- her I love -- Sayeed p. 189-
  FocusObj np sslash = -- FIXME: preposition disappears in negative sentences
    let ss = sslash.s ! False ;
        ssSub  = sslash.s ! True ; -- the negative particle is the same as subordinate, but verb forms come from main clause
        obj = objpron np ! Abs ;
     in {s = ssSub.beforeSTM ++ "waxa" ++ ssSub.stm ++ ss.afterSTM ++ obj} ;

  -- FocusAdv : Adv -> S       -> Utt ;   -- today I will sleep
  -- FocusAdV : AdV -> S       -> Utt ;   -- never will I sleep
  -- FocusAP  : AP  -> NP      -> Utt ;   -- green was the tree
} ;
