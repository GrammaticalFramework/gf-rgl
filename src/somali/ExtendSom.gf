--# -path=.:../common:../abstract

concrete ExtendSom of Extend = CatSom
  ** ExtendFunctor - [GenModNP, FocusObj, ComplDirectVS, ComplDirectVQ]
  with (Grammar=GrammarSom)
  ** open Prelude, ResSom, NounSom in {

lin

  -- : Num -> NP -> CN -> NP ; -- this man's car(s)
  GenModNP num np cn = DetCN (DetQuant IndefArt num) (genModCN cn np) ;

   -- : NP  -> SSlash  -> Utt ;   -- her I love -- Saeed p. 189-
  FocusObj np sslash = {s = sslash.s ! False ++ objpron np ! Abs} ;

  -- FocusAdv : Adv -> S       -> Utt ;   -- today I will sleep
  -- FocusAdV : AdV -> S       -> Utt ;   -- never will I sleep
  -- FocusAP  : AP  -> NP      -> Utt ;   -- green was the tree
} ;
