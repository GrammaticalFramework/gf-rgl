concrete AdverbSlv of Adverb = CatSlv ** open ResSlv in {

  lin
    PrepNP prep np = {s = prep.s ++ np.s ! prep.c} ;
    PositAdvAdj a = {s = a.s ! APosit Neut Sg Nom} ;
    PositAdAAdj a = {s = a.s ! APosit Neut Sg Nom} ;
    AdAdv ada adv = {s = ada.s ++ adv.s} ;
    SubjS subj s = {s = subj.s ++ s.s} ;

}
