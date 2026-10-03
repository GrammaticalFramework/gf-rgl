concrete AdverbCze of Adverb = CatCze ** 
  open ResCze, Prelude in {

lin
    PositAdvAdj a = {s = a.adv} ;
    PositAdAAdj a = {s = a.adv} ;
    AdAdv ada adv = {s = ada.s ++ adv.s} ;
    AdnCAdv cadv = {s = cadv.s} ;
    ComparAdvAdj cadv a np = {
      s = cadv.s ++ a.adv ++ "než" ++ np.s ! Nom
      } ;
    ComparAdvAdjS cadv a sent = {
      s = cadv.s ++ a.adv ++ "než" ++ sent.s
      } ;

    PrepNP prep np = {
      s = fullComplement prep np.s np.prep
      } ;

    SubjS subj s = {
      s = (frontSentence subj.s s).s
      } ;

}
