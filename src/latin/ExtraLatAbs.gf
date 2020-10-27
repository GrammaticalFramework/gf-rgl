abstract ExtraLatAbs =
  Conjunction
  ** {
  cat CS ;
      TestRS ;
  fun
    useS : S -> CS ;
    -- do not drop pronouns
    UsePronNonDrop : Pron -> NP ;

    -- add adjective before the noun
    AdjCNPre : AP -> CN -> CN ;

    -- handle conjunction with suffix
    ConjNPque : Conj -> ListNP -> NP ;

    -- Alternative form for female noun phrase
    everybodyFem_NP : NP ;
    somebodyFem_NP : NP ;
    nobodyFem_NP : NP ;
    
    
    -- Some empty prepositions to enforce cases
    Nom_Prep : Prep ;
    Gen_Prep : Prep ;
    Acc_Prep : Prep ;
    Dat_Prep : Prep ;
    Abl_Prep : Prep ;

    -- Preposition with alternate case
    inAbl_Prep : Prep ;
    onAbl_Prep : Prep ;

    -- Add other word orders
    UttS_SVO : S -> Utt ;
    UttS_VInS : S -> Utt ;
    TestRCl : Temp -> Pol -> RCl -> TestRS ;
    UseRCl_OSV : Temp -> Pol -> RCl -> RS ;
    UseRCl_OVS : Temp -> Pol -> RCl -> RS ;
    UseRCl_SOV : Temp -> Pol -> RCl -> RS ;
    UseRCl_SVO : Temp -> Pol -> RCl -> RS ;

    PrepNP_DPostN : Prep -> NP -> Adv ;
    ApposCN_DPostN : CN -> NP -> CN ;

    -- More genders
    DetNP_Fem   : Det -> NP ;
    AdjAsNP_Fem : AP -> NP ;

    -- Ellipsis
    PredVP_VP_Ellipsis : NP -> Cl ;
    SlashVP_VP_Ellipsis  : NP -> ClSlash ;
    FunRP_RP_Ellipsis : Prep -> NP -> RP ;
    RelNP_NP_Ellipsis : RS -> NP ;
    comma_Conj : Conj ;
}
