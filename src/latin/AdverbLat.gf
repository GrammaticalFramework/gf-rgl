concrete AdverbLat of Adverb = CatLat ** open ResLat, Prelude, ParadigmsLat in
  {

  lin

--  PositAdvAdj : A -> Adv ;                 -- warmly
    PositAdvAdj a = a.adv ;

--  PrepNP : Prep -> NP -> Adv ;        -- in the house
    PrepNP prep np =
      mkAdv (prep.s ++ np.adv ++ (combineNounPhrase np) ! PronNonDrop ! prep.c ) ;


--  ComparAdvAdj  : CAdv -> A -> NP -> Adv ; -- more warmly than John
    ComparAdvAdj cadv a np =
      mkAdv (cadv.s ++ a.adv.s ! Compar ++ cadv.p ++ (combineNounPhrase np) ! PronNonDrop ! Nom) ;

--  ComparAdvAdjS : CAdv -> A -> S  -> Adv ; -- more warmly than he runs
    ComparAdvAdjS cadv a s =
      mkAdv (cadv.s ++ a.adv.s ! Compar ++ cadv.p ++ combineSentence s ! SPreS ! PreV ! CPreV ! SOV ) ;

--  AdAdv  : AdA -> Adv -> Adv ;             -- very quickly
    AdAdv ada adv = mkAdv (ada.s ++ (adv.s ! Posit) ) ;

--  PositAdAAdj : A -> AdA ;                 -- extremely
    PositAdAAdj a =
      { s = a.adv.s ! Posit } ;
    
-- Subordinate clauses can function as adverbs.

--  SubjS  : Subj -> S -> Adv ;              -- when she sleeps
    SubjS subj s = mkAdv (subj.s ++ combineSentence s ! SPreS ! PreV ! CPreV ! SOV ) ;
    
--  AdnCAdv : CAdv -> AdN ;                  -- less (than five)
    AdnCAdv cadv = {s = cadv.s ++ cadv.p} ;
--
}
