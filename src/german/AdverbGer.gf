concrete AdverbGer of Adverb = CatGer ** open ResGer, Prelude in {

  lin
    PositAdvAdj a = {s = a.s ! Posit ! APred} ;

    ComparAdvAdj cadv a np = {
      s = cadv.s ++ a.s ! Posit ! APred ++ cadv.p ++ np.s ! False ! Nom ++ bigNP np
      } ;
    ComparAdvAdjS cadv a s = {
      s = cadv.s ++ a.s ! Posit ! APred ++ cadv.p ++ s.s ! Sub
      } ;

    PrepNP prep np = {s = appPrepNP prep np} ;

    AdAdv = cc2 ;

    PositAdAAdj a = {s = a.s ! Posit ! APred} ;

    SubjS subj s = {s = {- Predef.BIND ++ "," ++ -} subj.s ++ s.s ! Sub} ; --- comma needed in some uses

    AdnCAdv cadv = {s = cadv.s ++ conjThan} ;

}

---b    AdvSC s = s ;
