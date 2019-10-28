concrete AdverbKam of Adverb = CatKam **AdverbBantu -[ComparAdvAdj,ComparAdvAdjS] with
 (ResBantu = ResKam) ** open DiffKam in {

 flags coding=utf8;
  lin
    ComparAdvAdj cadv a np =let agr = complAgr np.a
    in { s = table {Ag g n p=> cadv.s ++ a.s !AComp g n ++ cadv.p ++ np.s ! npNom } } ;
    ComparAdvAdjS cadv a s = {  s = table{Ag g n p =>  cadv.s ++ a.s! AComp g n  ++ cadv.p ++ s.s} 
      } ;
    }

 