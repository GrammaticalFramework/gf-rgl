concrete AdverbMay of Adverb = CatMay ** open ResMay, ParamMay, ParadigmsMay, Prelude in {

lin

  -- : A -> Adv ;
  --PositAdvAdj adj = { } ;

  -- : CAdv -> A -> NP -> Adv ; -- more warmly than John
  -- ComparAdvAdj cadv a np = { } ;

--    ComparAdvAdjS : CAdv -> A -> S  -> Adv ; -- more warmly than he runs

  -- : Prep -> NP -> Adv ;
  PrepNP prep np = {s = applyPrep prep np} ;

  -- PrepNP to_Prep (UsePron youSg_Pron)

-- Adverbs can be modified by 'adadjectives', just like adjectives.

    --AdAdv  : AdA -> Adv -> Adv ;             -- very quickly
  -- AdAdv ada adv = adv **
-- Like adverbs, adadjectives can be produced by adjectives.
  AdAdv ada adv = adv ** {
    s = ada.s ++ adv.s ;
  } ;

  -- : A -> AdA ;                 -- extremely
  --  PositAdAAdj a = { } ;
  -- PositAdAAdj a = {s = a.s} ;

  -- Subordinate clauses can function as adverbs.

  -- : Subj -> S -> Adv ;
  SubjS subj s = {s = subj.s ++ s.s} ;

-- Comparison adverbs also work as numeral adverbs.

    --AdnCAdv : CAdv -> AdN ;                  -- less (than five)
    --AdnCAdv cadv = {s = } ;
} ;
