abstract VerbExt = Cat,ExtraCatZulAbs ** {

  fun

    CopAP : AP -> VP ;
    CopNP : NP -> VP ;
    CopNPAssoc : NP -> VP ;
    CopLoc : Loc -> VP ;

    BecomeAP : AP -> VP ;
    BecomeNP : NP -> VP ;
    BecomeNPAssoc : NP -> VP ;
    BecomeLoc : Loc -> VP ;

    ComplV2Nonspec : V2 -> NP -> VP ;

}
