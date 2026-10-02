concrete AdverbCze of Adverb = CatCze ** 
  open ResCze, Prelude in {

lin
    PositAdvAdj a = {s = a.nsnom} ;
    PositAdAAdj a = {s = a.nsnom} ;
    AdAdv ada adv = {s = ada.s ++ adv.s} ;
    AdnCAdv cadv = {s = cadv.s} ;
    ComparAdvAdj cadv a np = {
      s = cadv.s ++ a.nsnom ++ "než" ++ np.s ! Nom
      } ;
    ComparAdvAdjS cadv a sent = {
      s = cadv.s ++ a.nsnom ++ "než" ++ sent.s
      } ;

    PrepNP prep np = {
      s = fullComplement prep np.s np.prep
      } ;

    SubjS subj s = {
      s = (frontSentence subj.s s).s
      } ;

}
