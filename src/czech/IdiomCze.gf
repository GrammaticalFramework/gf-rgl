concrete IdiomCze of Idiom = CatCze ** open Prelude, ResCze in {

lin
    ImpP3 np vp = {
      s = "nechť" ++ np.s ! Nom ++ vp.clit ! np.a ++
          verbAgr vp.verb np.a True ++ vp.compl ! np.a
      } ;

    ImpersCl vp = let agr = Ag Neutr Sg P3 in {
      subj  = [] ;
      clit  = vp.clit ! agr ;
      compl = vp.compl ! agr ;
      verb  = vp.verb ;
      a     = agr
      } ;

    GenericCl vp = let agr = Ag (Masc Anim) Pl P3 in {
      subj  = [] ;
      clit  = vp.clit ! agr ;
      compl = vp.compl ! agr ;
      verb  = vp.verb ;
      a     = agr
      } ;

    ExistNP np = {
      subj, clit = [] ;
      compl = np.s ! Nom ;
      verb = iii_kupovatVerbForms "existovat" ;
      a = np.a
      } ;

}
