concrete AdverbCze of Adverb = CatCze ** 
  open ResCze, Prelude in {

lin
    PrepNP prep np = {
      s = fullComplement prep np.s np.prep
      } ;

    SubjS subj s = {
      s = (frontSentence subj.s s).s
      } ;

}
