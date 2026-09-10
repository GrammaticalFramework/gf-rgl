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

}
