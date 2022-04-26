abstract ExtraExt =
  Cat [NP,VP,CN,V,Temp,S,Cl,Adv,Pron,QCl,QS,A,RS,IAdv,IComp,Pol,Det,Quant,N,PN],
  CatExt ** {

  fun
    ProDrop : Pron -> Pron ;  -- unstressed subject pronoun becomes empty: "am tired"

  fun
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

    PossLocNP : LocN -> NP -> CN ;

    InstrNPAdv : NP -> Adv ; -- ngokuhlinzwa
    InstrAdvNPAdv : Adv -> NP -> Adv ; -- cishe ngehora
    LocNPAdv : NP -> Adv ;   -- emahoreni
    LocAdvNPAdv : Adv -> NP -> Adv ;   -- cishe emahoreni
    KwaNPAdv : NP -> Adv ; -- kwa-Laurette
    -- KwaAdvNPAdv : Adv -> NP -> Adv ; -- ngaphezu kwamahora amabili adlule
    KuNPAdv : NP -> Adv ; -- kwixesha [elingangeyure enye egqithileyo]
    KuAdvNPAdv : Adv -> NP -> Adv ; -- ngaphezu kwamahora amabili adlule
    NaNPAdv : NP -> Adv ;

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

    -- TPerfPast : Temp ;
    -- TPastPast : Temp ;
    -- TPresPres : Temp ;
    -- TPastPres : Temp ;
    -- TPastPerf : Temp ;
    -- TPerfPerf : Temp ;

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

    lapha_Loc : Loc ;
    khona_Loc : Loc ;

    kakhulu_Adv : Adv ;

    -- Deverb15 : V -> N ;

    -- AdvQS     : IAdv -> S  -> QS ;            -- maybe it is a pain that burns 2020-01-15: consider that this should rather be dealt with using QuestIAdv)

}
