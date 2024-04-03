concrete AdverbHrv of Adverb = CatHrv ** 
  open ResHrv, Prelude in {

lin
    PrepNP prep np = {
      s = prep.s ++ np.prep ! prep.c
      } ;
    AdnCAdv a = a ;

    SubjS subj s = {s = subj.s ++ s.s} ;
}
