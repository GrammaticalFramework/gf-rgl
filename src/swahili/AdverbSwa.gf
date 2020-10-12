concrete AdverbSwa of Adverb = CatSwa **AdverbBantu -[ComparAdvAdj,ComparAdvAdjS] with
 (ResBantu = ResSwa)** open DiffSwa in
{ 
flags coding=utf8;
  lin
    ComparAdvAdj cadv a np =let agr = complAgr np.a
    in {
      s = cadv.s ++ a.s !AAdj agr.g agr.n ++ cadv.p ++ np.s ! npNom
      } ;
    ComparAdvAdjS cadv a s = {
      s = cadv.s ++ a.s !AAdj G1 Sg ++ cadv.p ++ s.s
      } ;
    }
