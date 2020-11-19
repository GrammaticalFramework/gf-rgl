concrete AdverbSlo of Adverb = CatSlo ** 
  open ResSlo, Prelude in {

lin
    PrepNP prep np = {
      s = prep.s ++ np.prep ! prep.c
      } ;

}
