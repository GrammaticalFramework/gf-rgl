abstract ExtraExt =
  Cat [NP,VP,CN,V,Temp,S,Cl,Adv,Pron,QCl,QS,A,RS,IAdv,IComp,Pol,Det,Quant,N,PN,Conj],
  CatExt ** {

  fun
    ProDrop : Pron -> Pron ;  -- unstressed subject pronoun becomes empty: "am tired"

  fun
    ExistNP : NP -> S ;
    GreetSg : Utt ;
    GreetPl : Utt ;
    -- PotQS : Pol -> QCl -> QS ;
    -- SubjunctS : S -> Utt ;

    -- AssocCop : NP -> VP ;
    -- EqCop : NP -> VP ;

    -- UsePNPl : PN -> NP ;
    -- PNAsCN : PN -> CN ;

    -- DemPron : Quant -> Pron -> NP ;

    -- EmphCN : CN -> CN ; --
    -- ContrastCN : CN -> CN ;
    -- ApposNPN : NP -> N -> NP ;
    -- ContrastNP : NP -> NP ;

    -- DescrNP : CN -> NP -> CN ;
    -- PossLocNP : CN -> NP -> CN ;
    -- PossPronZul : CN -> Pron -> CN ; -- of you (yours)
    -- RelV : CN -> V -> Temp -> Pol -> CN ; -- lights that flash

    -- PossLocNP : LocN -> NP -> CN ;
    PossNPLoc : CN -> NP -> CN ; -- zasepulazini

    InstrNPAdv : NP -> Adv ; -- ngokuhlinzwa
    InstrAdvNPAdv : Adv -> NP -> Adv ; -- cishe ngehora

    LocAdvNPAdv : Adv -> NP -> Adv ;   -- cishe emahoreni amabili
    KwaNPAdv : NP -> Adv ; -- kwa-Laurette
    -- KwaAdvNPAdv : LocAdv -> NP -> Adv ; -- ngaphezu kwamahora amabili adlule
    KuNPAdv : NP -> Adv ; -- kwixesha [elingangeyure enye egqithileyo]
    KuAdvNPAdv : Adv -> NP -> Adv ; -- ngaphezu kwamahora amabili adlule
    NaNPAdv : NP -> Adv ;

    -- LocNAdv : LocN -> Adv ; -- phansi kwetafula

    LocAdvAdv : LocAdv -> Adv ;
    LocAdvNP : LocAdv -> NP -> LocAdv ; -- ngaphezu kwamahora amabili adlule
    LocNAdv : LocN -> LocAdv ;
    LocNNgaAdv : LocN -> LocAdv ;
    LocNPAdv : NP -> LocAdv ;   -- emahoreni

    NPAdv : NP -> Adv ; -- sonke lesi sikhathi

    -- InstrAdvNP   : NP -> NP -> NP ; -- questions about your pregnancy

    -- RelAdv : Adv -> RS ;

    -- ProgVP : VP -> VP ;

    -- QuantRS : QuantStem -> RS ; -- elilodwa
    -- RelRS : RelStem -> RS ; -- elibuhlungu

    -- QuantCN : QuantStem -> CN -> CN ; -- izindlu zonke

    -- NumAdjCN : CN -> A -> CN ;

    only_QuantStem : QuantStem ;
    all_QuantStem : QuantStem ;
    -- all_pre_QuantStem : QuantStem ;
    -- all_pre_Predet : Predet ;

    -- painful_RelStem : RelStem ;
    -- sharp_RelStem : RelStem ;

    -- PredNP : NP -> Cl ;
    -- IAdvQS : NP -> INAdv -> QS ; -- where is the wine? iwayini liphi?
    -- IAdvQCl : NP -> INAdv -> QCl ; -- where is the wine? iwayini liphi?

    -- AdvQCl : Adv -> QCl -> QCl ;

    -- ComplVAux : VAux -> VP -> VP ;

    ConjNAdv : ConjN -> S -> Adv ; -- lapho kunobuhlungu khona

    where_ConjN : ConjN ;

    IAdvVP : VP -> IAdv -> VP ; -- kushisa kangakanani eGoli

    it3_Pron : Pron ;
    they4_Pron : Pron ;
    it5_Pron : Pron ;
    they6_Pron : Pron ;
    it7_Pron : Pron ;
    they8_Pron : Pron ;
    it9_Pron : Pron ;
    they10_Pron : Pron ;
    it11_Pron : Pron ;
    it14_Pron : Pron ;
    it15_Pron : Pron ;
    it17_Pron : Pron ;

    yonder_Quant : Quant ;

    at_which_IAdv : NP -> IAdv ;
    what_IAdv : IAdv ;

    -- about_NP_Adv : NP -> Adv ;

    how_many_IAdj : IAdj ;

    -- IAdjIAdv : NP -> IAdj -> IAdv ;

    how_IComp : IComp ; -- -njani
    where_IComp : IComp ; -- -phi
    how_much_IComp : IComp ; -- -ngakanani

    how2_IAdv : IAdv ;
    how8much2_IAdv : IAdv ;

    phakathi_LocN : LocN ;
    phansi_LocN : LocN ;
    phesheya_LocN : LocN ;
    phandle_LocN : LocN ;
    phambili_LocN : LocN ;
    phambi_LocN : LocN ;
    phakade_LocN : LocN ;
    phezulu_LocN : LocN ;

    ngemuva_LocAdv : LocAdv ;
    emuva_LocAdv : LocAdv ;
    ecaleni_LocAdv : LocAdv ;
    ngaphezu_LocAdv : LocAdv ;

    lapha_Loc : Loc ;
    khona_Loc : Loc ;

    kakhulu_Adv : Adv ;

    ExtConjNP : NP -> Conj -> NP -> NP ;

    with_Conj : Conj ;

    want_VV : VV ;
    prepare_to_VV : VV ;

    -- Deverb15 : V -> N ;

    -- AdvQS     : IAdv -> S  -> QS ;            -- maybe it is a pain that burns 2020-01-15: consider that this should rather be dealt with using QuestIAdv)

}
