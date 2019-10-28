concrete AdjectiveKam of Adjective = CatKam **AdjectiveBantu -[ComparA,UseComparA]  with
  (ResBantu = ResKam)** open DiffKam in
{ 
flags coding=utf8;
  lin
    
     ComparA a np = {
      s = \\g,n => a.s  !AComp g n  ++ conjThan  ++ np.s ! npNom ; 
      isPre = False} ;

    UseComparA a = {s = \\g,n=> a.s !AComp g n;isPre = True};
    
    
    } 
