concrete AdverbCze of Adverb = CatCze ** 
  open ResCze, Prelude in {

lin
    PrepNP prep np = {
      s = prep.s ++ np.prep ! prep.c
      } ;

    SubjS subj s = {
      s = subj.s ++ s.s
      } ;

}
