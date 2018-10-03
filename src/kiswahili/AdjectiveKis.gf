concrete AdjectiveKis of Adjective = CatKis **AdjectiveBantu-[ComparA,UseComparA,ComplA2] 
with  (ResBantu = ResKis)** open DiffKis in
{ 
flags coding=utf8;
  lin
    ComparA a np = {
      s = \\g,n => a.s  !AAdj g n  ++ conjThan  ++ np.s ! npNom ; 
      isPre = False} ;

    UseComparA a = {s = \\g,n=> a.s !AAdj g n;isPre = True};

     ComplA2 a np = {
      s = \\g,n => a.s  !AAdj g n  ++ a.c2 ++ np.s ! NPAcc; 
      isPre = False
      } ;
    }