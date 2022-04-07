concrete AdverbLat of Adverb = CatLat ** open ResLat, Prelude, ParadigmsLat in
  {

  lin

--  PositAdvAdj : A -> Adv ;                 -- warmly
    PositAdvAdj a = a.adv ;

--  PrepNP : Prep -> NP -> Adv ;        -- in the house
    PrepNP prep np =
      mkAdv (prep.s ++ (combineNounPhrase np) ! PronNonDrop ! APostN ! DPreN ! prep.c ) ;


--  ComparAdvAdj  : CAdv -> A -> NP -> Adv ; -- more warmly than John
    ComparAdvAdj cadv a np =
      mkAdv (cadv.s ++ a.adv.s ! Compar ++ cadv.p ++ (combineNounPhrase np) ! PronNonDrop ! APostN ! DPreN ! Nom) ;

--  ComparAdvAdjS : CAdv -> A -> S  -> Adv ; -- more warmly than he runs
    ComparAdvAdjS cadv a s =
      mkAdv (cadv.s ++ a.adv.s ! Compar ++ cadv.p ++ defaultSentence s ! SOV ) ;

--  AdAdv  : AdA -> Adv -> Adv ;             -- very quickly
    AdAdv ada adv = mkAdv (ada.s ++ (adv.s ! Posit) ) ;

--  PositAdAAdj : A -> AdA ;                 -- extremely
    PositAdAAdj a =
      { s = a.adv.s ! Posit } ;
    
-- Subordinate clauses can function as adverbs.

--  SubjS  : Subj -> S -> Adv ;              -- when she sleeps
    SubjS subj s = mkAdv (subj.s ++ defaultSentence s ! SOV ) ;
    
--  AdnCAdv : CAdv -> AdN ;                  -- less (than five)
    AdnCAdv cadv = {s = cadv.s ++ cadv.p} ;
--
}
