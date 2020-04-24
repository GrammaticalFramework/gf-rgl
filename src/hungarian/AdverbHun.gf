concrete AdverbHun of Adverb = CatHun ** open ResHun, ParamHun, ParadigmsHun, Prelude in {

lin

  -- : A -> Adv ;
  --PositAdvAdj adj = { } ;

  -- : CAdv -> A -> NP -> Adv ; -- more warmly than John
  -- ComparAdvAdj cadv a np = { } ;

--    ComparAdvAdjS : CAdv -> A -> S  -> Adv ; -- more warmly than he runs

  -- : Prep -> NP -> Adv ;
  PrepNP prep np = {
    s = applyAdp prep np ;
    isPre = False ;
    } ;

-- Adverbs can be modified by 'adadjectives', just like adjectives.

    --AdAdv  : AdA -> Adv -> Adv ;             -- very quickly
  -- AdAdv ada adv = adv **
-- Like adverbs, adadjectives can be produced by adjectives.

  -- : A -> AdA ;                 -- extremely
--  PositAdAAdj a = { } ;
-- Subordinate clauses can function as adverbs.

  -- : Subj -> S -> Adv ;
  -- SubjS subj s =

-- Comparison adverbs also work as numeral adverbs.

    --AdnCAdv : CAdv -> AdN ;                  -- less (than five)
    --AdnCAdv cadv = {s = } ;
} ;
