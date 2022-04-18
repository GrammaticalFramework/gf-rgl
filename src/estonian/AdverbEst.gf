concrete AdverbEst of Adverb = CatEst ** open ResEst, Prelude in {

  flags coding=utf8;

  lin
    PositAdvAdj a = {s = a.s ! Posit ! AAdv} ;
    ComparAdvAdj cadv a np = {
      s = cadv.s ++ a.s ! Posit ! AAdv ++ cadv.p ++ linNP (NPCase Nom) np
      } ;
    ComparAdvAdjS cadv a s = {
      s = cadv.s ++ a.s ! Posit ! AAdv ++ cadv.p ++ s.s
      } ;

    PrepNP prep np = {s = appCompl True Pos prep np} ;

    AdAdv = cc2 ;

    PositAdAAdj a = {s = a.s ! Posit ! AN (NCase Sg Gen)} ; -- älyttömän

    SubjS = cc2 ;
----b    AdvSC s = s ;

    AdnCAdv cadv = {s = cadv.s ++ "kui"} ;

}
