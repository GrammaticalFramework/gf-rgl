concrete AdverbKor of Adverb = CatKor ** open ResKor, ParamKor, ParadigmsKor, Prelude in {

lin

  -- : A -> Adv ;
  PositAdvAdj adj = {s = glue (adj.s ! VStem Pos) "게"} ;

  -- : CAdv -> A -> NP -> Adv ; -- more warmly than John
  -- ComparAdvAdj cadv a np = { } ;

--    ComparAdvAdjS : CAdv -> A -> S  -> Adv ; -- more warmly than he runs

  -- : Prep -> NP -> Adv ;
  PrepNP = prepNP Bare ; 

-- Adverbs can be modified by 'adadjectives', just like adjectives.

    --AdAdv  : AdA -> Adv -> Adv ;             -- very quickly
  AdAdv ada adv = {s = ada.s ++ adv.s} ;
-- Like adverbs, adadjectives can be produced by adjectives.

  -- : A -> AdA ;                 -- extremely
  PositAdAAdj a = {s = glue (a.s ! VStem Pos) "게"} ;
-- Subordinate clauses can function as adverbs.

  -- : Subj -> S -> Adv ;
  SubjS subj s = {s = s.s ! Subord ++ subj.s} ;

-- Comparison adverbs also work as numeral adverbs.

    --AdnCAdv : CAdv -> AdN ;                  -- less (than five)
    --AdnCAdv cadv = {s = } ;
} ;
