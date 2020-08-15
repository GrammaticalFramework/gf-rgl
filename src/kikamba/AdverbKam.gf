concrete AdverbKam of Adverb = CatKam **AdverbBantu -[ComparAdvAdj,ComparAdvAdjS] with
 (ResBantu = ResKam) ** open DiffKam in {

 flags coding=utf8;
  lin
   -- ComparAdvAdj cadv a np =let agr = complAgr np.a in { s = table {Ag g n p=> cadv.s ++ a.s !AComp g n ++ cadv.p ++ np.s ! npNom } } ;
   -- ComparAdvAdjS cadv a s = {  s = table{Ag g n p =>  cadv.s ++ a.s! AComp g n  ++ cadv.p ++ s.s}     } ;

  ComparAdvAdj cadv a np = let agr = complAgr np.a  in{
    s = table {AgP1  n =>  a.s !AComp G1 n ++ cadv.s ++ np.s ! npNom;
               AgP2  n =>  a.s !AComp G1 n ++ cadv.s ++ np.s ! npNom;
               AgP3 g n =>  a.s !AComp g n ++ cadv.s ++ np.s ! npNom }
           } ;
  ComparAdvAdjS cadv a s = {   s = table{
       AgP1 n  =>   a.s! AComp G1 n  ++ cadv.s ++ s.s;
       AgP2 n =>   a.s! AComp G1 n  ++ cadv.s ++ s.s;
       AgP3 g n  =>   a.s! AComp g n  ++ cadv.s ++ s.s} 
      } ;
    }

 