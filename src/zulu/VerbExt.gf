abstract VerbExt = Cat ** {

  fun

    CopAP : AP -> VP ;
    CopNP : NP -> VP ;
    CopNPAssoc : NP -> VP ;
    CopLoc : Loc -> VP ;

    ComplV2Nonspec : V2 -> NP -> VP ;

}
