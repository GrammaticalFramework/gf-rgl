concrete AdjectiveHye of Adjective = CatHye ** open ResHye, Prelude in {
lin
  AdAP ada ap = {
    s = \\sp,c,num => ada.s ++ ap.s ! sp ! c ! num;
    isPre = ap.isPre
  } ;
  PositA a = {
    s = \\sp,c,num =>
            case <sp,c> of {
              <Def,Nom> => a.def_nom ! num ;
              <Def,Dat> => a.def_dat ! num ;
              <Poss P1,_> => a.poss1 ! c ! num ;
              <Poss P2,_> => a.poss2 ! c ! num ;
              _         => a.s ! c ! num
            } ;
    isPre = True
  } ;
  UseA2 a = PositA a ;
  ComplA2 a np = {
    s = \\sp,c,n => (case a.c2.isPre of {
      True => a.c2.s ++ np.s ! a.c2.c;
      False => np.s ! a.c2.c ++ a.c2.s
    }) ++ a.s ! c ! n;
    isPre = False
  } ;
  AdvAP ap adv = {s = \\sp,c,n => ap.s ! sp ! c ! n ++ adv.s; isPre = ap.isPre} ;
  SentAP ap sc = {s = \\sp,c,n => ap.s ! sp ! c ! n ++ sc.s; isPre = ap.isPre} ;
  ComparA a np = {
    s = \\sp,c,n => a.s ! c ! n ++ "քան" ++ np.s ! Nom; isPre = False
  } ;
  UseComparA a = {
    s = \\sp,c,n => "ավելի" ++ (a.s ! c ! n); isPre = True
  } ;

}
