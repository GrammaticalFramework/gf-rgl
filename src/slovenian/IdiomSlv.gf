----AR BEGIN the whole IdiomSlv
concrete IdiomSlv of Idiom = CatSlv ** 
  open ParadigmsSlv, ResSlv, (P=ParamX), Prelude in {

  lin
    ExistNP np = 
      mkClause [] np.a False {
        s  = \\p,vform => ne ! p ++ (mkV "obstajati" "obstaja").s ! vform ;
        s2 = \\a => np.s ! Nom ;
        isCop = False ;
        refl = []
      } ;

    ExistNPAdv np adv = mkClause [] np.a False {
      s=copula; s2=\\_ => np.s ! Nom ++ adv.s; isCop=True; refl=[]
    } ;
    ImpersCl vp = mkClause [] {g=Neut;n=Sg;p=P3} False vp ;
    ProgrVP vp = vp ;
    ImpPl1 vp = {s = vp.s ! P.Pos ! VImper2 Pl ++ vp.s2 ! {g=Masc;n=Pl;p=P1}} ;

    

}
----AR END
