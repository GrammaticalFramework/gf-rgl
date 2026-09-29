concrete AdjectiveSlv of Adjective = CatSlv ** open ResSlv in {

  lin
    PositA a = {
      s = \\spec,g,c,n => 
        case <spec,g,n,c> of {
          <Def,AMasc _,    Sg,Nom> => a.s ! APositDefNom ;
          <Def,AMasc _,    Sg,Acc> => a.s ! APositDefNom ;
          <_,AMasc Animate,Sg,Acc> => a.s ! APosit Masc Sg Gen ;
          _                        => a.s ! APosit (agender2gender g) n c
        }
      } ;
    UseComparA a = {
      s = \\spec,g,c,n => 
        case <spec,g,n,c> of {
          <Def,AMasc _,Sg,Acc> => a.s ! AComparDefAcc ;
          _                    => a.s ! ACompar (agender2gender g) n c
        }
      } ;

    AdAP ada ap = {
      s = \\spec,g,c,n => ada.s ++ ap.s ! spec ! g ! c ! n
      } ;

    AdvAP ap adv = {s = \\spec,g,c,n => ap.s ! spec ! g ! c ! n ++ adv.s} ;
    ComplA2 a np = {
      s = \\spec,g,c,n => a.s ! APosit (agender2gender g) n c ++ a.c.s ++ np.s ! a.c.c
    } ;
    UseA2 a = {s = \\_,g,c,n => a.s ! APosit (agender2gender g) n c} ;
    SentAP ap sc = ap ** {s = \\sp,g,c,n => ap.s ! sp ! g ! c ! n ++ sc.s} ;
    ComparA a np = {
      s = \\_,g,c,n => a.s ! ACompar (agender2gender g) n c ++ "kot" ++ np.s ! Nom
    } ;
}
