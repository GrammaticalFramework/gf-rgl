concrete AdverbSom of Adverb = CatSom ** open ResSom, ParamSom, ParadigmsSom, Prelude in {

lin

  -- : A -> Adv ;
  PositAdvAdj adj = mkAdv ("si" ++ adj.s ! AF Sg Abs) ;

  -- : CAdv -> A -> NP -> Adv ; -- more warmly than John
  ComparAdvAdj cadv a np =
    mkAdv (cadv.s ++ "si" ++ a.s ! AF Sg Abs ++ cadv.p ++ np.s ! Abs) ;

--    ComparAdvAdjS : CAdv -> A -> S  -> Adv ; -- more warmly than he runs
  ComparAdvAdjS cadv a s =
    mkAdv (cadv.s ++ "si" ++ a.s ! AF Sg Abs ++ cadv.p ++ s.s ! False) ;

  -- : Prep -> NP -> Adv ;
  PrepNP = prepNP ;

-- Adverbs can be modified by 'adadjectives', just like adjectives.

  AdAdv ada adv = adv ** {berri = ada.s ++ adv.berri} ;
-- Like adverbs, adadjectives can be produced by adjectives.

  -- : A -> AdA ;                 -- extremely
  PositAdAAdj a = mkAdA (a.s ! AF Sg Abs) ;
-- Subordinate clauses can function as adverbs.

    -- : Subj -> S -> Adv ;
  SubjS subj s = mkAdv (s.s ! True) ;

-- Comparison adverbs also work as numeral adverbs.

  AdnCAdv cadv = {s = cadv.s ++ cadv.p} ;
} ;
