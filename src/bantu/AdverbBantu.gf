incomplete concrete AdverbBantu of Adverb = 
  CatBantu ** open CommonBantu, ResBantu, Prelude in {

  lin
   PositAdvAdj a =  {s = a.s !AAdj G1 Sg} ;

    ComparAdvAdj cadv a np =let agr = complAgr np.a
    in {
      s = cadv.s ++ a.s !AComp agr.g agr.n ++ cadv.p ++ np.s ! npNom
      } ;
    ComparAdvAdjS cadv a s = {
      s = cadv.s ++ a.s !AComp G1 Sg ++ cadv.p ++ s.s
      } ;

   PrepNP prep np = let agr = complAgr np.a

    in {s = prep.s!agr.n!agr.g ++ (np.s ! NCase Loc ) } ; 
       AdAdv = cc2 ;

    PositAdAAdj a = {s = a.s  !AAdj G1 Sg } ;
   

    SubjS = cc2 ;

    AdnCAdv cadv = {s = cadv.s ++ cadv.p} ;


}
