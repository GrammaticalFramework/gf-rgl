concrete IdiomHrv of Idiom = CatHrv ** open Prelude, ResHrv in {

lin
  ExistNP np = { ---- TODO verify this
    subj = np.s ! Nom ;
    verb = biti_VerbForms ; ---- TODO: jesam
    clit, compl = [] ;
    a = np.a 
    } ;
    
  ExistNPAdv np adv = ExistNP np ** {compl = adv.s} ;

}
