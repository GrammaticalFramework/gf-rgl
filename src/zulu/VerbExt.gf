abstract VerbExt = Cat ** {

  fun

    CopAP : AP -> VP ;
    CopNP : NP -> VP ;
    CopNPAssoc : NP -> VP ;
    -- CopAdv : Adv -> VP ;

    ComplV2Nonspec : V2 -> NP -> VP ;

}
