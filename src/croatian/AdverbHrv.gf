concrete AdverbHrv of Adverb = CatHrv ** 
  open ResHrv, Prelude in {

lin
    PrepNP prep np = {
      s = prep.s ++ np.prep ! prep.c
      } ;

}
