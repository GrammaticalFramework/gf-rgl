concrete AdverbPes of Adverb = CatPes ** open ResPes, Prelude in {

  flags coding = utf8;
  lin
 --   PositAdvAdj a = {s = a.s ! Bare  } ;
     PositAdvAdj a = {s = a.adv  } ;
    ComparAdvAdj cadv a np = {
      s = a.adv ++ cadv.p ++ cadv.s ++ np.s !  Bare  ;
      } ;
    ComparAdvAdjS cadv a s = {
      s =  a.adv  ++ cadv.p ++ cadv.s  ++  s.s ! Indic;
      } ;

    PrepNP prep np = {s = appComp prep np.s} ;

    AdAdv ada adv = { s =  ada.s ++ adv.s} ;

--    SubjS = cc2 ;
    SubjS sub snt = {
       s = case sub.relpron of {
            Ke   => sub.s ++ conjThat ++ snt.s ! sub.compl ;
            Ance => "آنچه" ++ snt.s ! sub.compl }
    } ;

    AdnCAdv cadv = {s =  cadv.s ++ "از"} ;

}
