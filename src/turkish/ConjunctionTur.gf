concrete ConjunctionTur of Conjunction =
  CatTur ** open ResTur, Coordination, Prelude in {

  lin
    ConjNP _ _ = variants {} ;
    ConsNP _ _ = variants {} ;
    BaseNP _ _ = variants {} ;

    ConsAP _ _ = variants {} ;
 
    -- TODO: ap2.s seems to irrelevant; investigate why.
    BaseAP ap1 ap2 = {
      s = ap1.s ! Sg ! Nom
    } ;

    ConjAP _ _ = variants {} ;

    BaseAdV adv1 adv2 = {
      s = adv1.s
    } ;

    ConsAdv _ _ = variants {} ;

    BaseAdv adv1 adv2 = {
      s = adv1.s
    } ;

    ConjAdv _ _ = variants {} ;

    ConjRS _ _ = variants {} ;
    ConsRS _ _ = variants {} ;
    BaseRS _ _ = variants {} ;

    ConjS _ _ = variants {} ;
    ConsS _ _ = variants {} ;
    BaseS _ _ = variants {} ;

}
