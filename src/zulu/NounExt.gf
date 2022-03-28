abstract NounExt = Cat,CatExt ** {

  fun

    -- Quant is used for demonstratives, and QuantStem for all/only

    PronPostdetNP : Pron -> Postdet -> NP ;

    QuantPostdet : QuantStem -> Postdet ;

    DemPostdet : Quant -> Postdet ;

    QuantDemPostdet : QuantStem -> Quant -> Postdet ;

    DemQuantPostdet : Quant -> QuantStem -> Postdet ;

    DetNum : Num -> Det ;

    PostdetCN : CN -> Postdet -> Det -> NP ;

    RelN : RS -> N -> CN ;

    ApposCN : CN -> N -> CN ; -- (takes agr of CN)

    ApposN : CN -> N -> CN ; -- (takes agr of N)

    PredetN : Predet -> N -> CN ;

    QuantPredet : QuantStem -> Predet ;

    DemPredet : Quant -> Predet ;

    QuantDemPredet : QuantStem -> Quant -> Predet ;

    EmphCN : CN -> CN ; -- prepends emphatic pronoun

    ContrastCN : CN -> CN ; -- inserts contrastive pronoun

    UsePNPl : PN -> NP ;

    Deverb15 : V -> N ;

    LocNP : NP -> Loc ;

    LocNLoc : LocN -> Loc ;

}
