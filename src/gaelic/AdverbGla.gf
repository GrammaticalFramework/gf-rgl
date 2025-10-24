concrete AdverbGla of Adverb = CatGla ** open ResGla, ParadigmsGla, Prelude in {
lin
{-

  -- : A -> Adv ;
  PositAdvAdj adj =

  -- : CAdv -> A -> NP -> Adv ; -- more warmly than John
  ComparAdvAdj cadv a np =

  -- : CAdv -> A -> S  -> Adv ; -- more warmly than he runs
  ComparAdvAdjS cadv a s =
-}
  -- : Prep -> NP -> Adv ;
  PrepNP prep np = {
    s = prepAndArt ++ noun
    } where {
      defaultCase : CoreCase = prep.c2 ! getDefi np.a ;
      complCase : Case = case <defaultCase, np.a> of {
        <Dat NoMutation, NotPron (DPoss _ (Sg1|Sg2|Sg3 Masc))>
          => CC (Dat Lenited) ; -- force lenition if possessive triggers it
        <Nom NoMutation, NotPron (DPoss _ (Sg1|Sg2|Sg3 Masc))>
          => CC (Nom Lenited) ; -- force lenition if possessive triggers it
        _ => CC defaultCase } ;
      prepStr : Str = prep.s ! agr2pagr np.a ; -- can be Prep or Prep+Pron merged
      artStr : Str = np.art ! complCase ;
      prepAndArt : Str = case np.a of {
        NotPron (DDef _ Indefinite) => prepStr ++ artStr ;
        _                           => prepStr } ;
      noun : Str = case <prep.replacesObjPron, np.a> of {
        <_, NotPron _> | <False, _>
          => np.s ! complCase ;
        _ => np.empty -- empty string to avoid metavariables
      }
    };
{-
-- Adverbs can be modified by 'adadjectives', just like adjectives.

  -- : AdA -> Adv -> Adv ;             -- very quickly
  AdAdv ada adv = adv **

-- Like adverbs, adadjectives can be produced by adjectives.

  -- : A -> AdA ;                 -- extremely
  PositAdAAdj a =

  -- Subordinate clauses can function as adverbs.

  -- : Subj -> S -> Adv ;
  SubjS subj s = {s = subj.s ++ s.s} ;

-- Comparison adverbs also work as numeral adverbs.

  -- : CAdv -> AdN ;                  -- less (than five)
  AdnCAdv cadv =  ;

-}

}
