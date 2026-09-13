concrete AdverbHun of Adverb = CatHun ** open ResHun, ParamHun, ParadigmsHun, Prelude in {

lin

  -- : A -> Adv ;
  PositAdvAdj adj = {
    s = let a : Str = adj.s ! Posit ! SgNom
         in a ++ BIND ++ "an" ;
    isPre = False ;
    } ;

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
  AdAdv ada adv = adv ** {s = ada.s ++ adv.s} ;
-- Like adverbs, adadjectives can be produced by adjectives.

  -- : A -> AdA ;                 -- extremely
  PositAdAAdj a = {s = a.s ! Posit ! SgNom} ;
-- Subordinate clauses can function as adverbs.

  -- : Subj -> S -> Adv ;
  SubjS subj s = {
    s = subj.s ++ s.s ;
    isPre = False ;
    } ;

-- Comparison adverbs also work as numeral adverbs.

    --AdnCAdv : CAdv -> AdN ;                  -- less (than five)
    --AdnCAdv cadv = {s = } ;
} ;
