concrete IdiomTur of Idiom = CatTur ** open Prelude, ResTur, SuffixTur in {

lin
  ImpersCl _ = variants {} ;
  GenericCl _ = variants {} ;
  ExistNP _ = variants {} ;
  ExistIP _ = variants {} ;
  CleftNP _ _ = variants {} ;
  CleftAdv _ _ = variants {} ;
  ImpPl1 _ = variants {} ;

  ProgrVP vp = vp ** {
    s = \\asp,vform => vp.s ! Imperf ! vform
  } ;

}

