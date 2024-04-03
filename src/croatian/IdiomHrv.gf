concrete IdiomHrv of Idiom = CatHrv ** open Prelude, ResHrv in {

lin
  ExistNP np = { ---- TODO verify this
    subj = np.s ! Nom ;
    verb = copula_VerbForms ;
    clit, compl = [] ;
    a = np.a 
    } ;
    
  ExistNPAdv np adv = ExistNP np ** {compl = adv.s} ;

  ImpersCl vp =
    let npa = Ag Neutr Sg P3 in {
      subj  = "" ;
      verb  = vp.verb ;
      clit  = vp.clit ! npa ;
      compl = vp.compl ! npa ;
      a = npa ;
      } ;

}
