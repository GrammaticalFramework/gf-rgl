resource MissingAra = open GrammarAra, Prelude in {

-- temporary definitions to enable the compilation of RGL API
oper AdAdv : AdA -> Adv -> Adv = notYet "AdAdv" ;
oper AdVVP : AdV -> VP -> VP = notYet "AdVVP" ;
oper AdnCAdv : CAdv -> AdN = notYet "AdnCAdv" ;
oper BaseRS : RS -> RS -> ListRS = notYet "BaseRS" ;
oper CAdvAP : CAdv -> AP -> NP -> AP = notYet "CAdvAP" ;
oper CleftNP : NP -> RS -> Cl = notYet "CleftNP" ;
oper ComparAdvAdj : CAdv -> A -> NP -> Adv = notYet "ComparAdvAdj" ;
oper ComparAdvAdjS : CAdv -> A -> S -> Adv = notYet "ComparAdvAdjS" ;
oper ConjRS : Conj -> ListRS -> RS = notYet "ConjRS" ;
oper ConsRS : RS -> ListRS -> ListRS = notYet "ConsRS" ;
oper DetNP : Det -> NP = notYet "DetNP" ;
oper ExistIP : IP -> QCl = notYet "ExistIP" ;
oper FunRP : Prep -> NP -> RP -> RP = notYet "FunRP" ;
oper ImpPl1 : VP -> Utt = notYet "ImpPl1" ;
oper PConjConj : Conj -> PConj = notYet "PConjConj" ;
oper PPartNP : NP -> V2 -> NP = notYet "PPartNP" ;
oper PredSCVP : SC -> VP -> Cl = notYet "PredSCVP" ;
oper ProgrVP : VP -> VP = notYet "ProgrVP" ;
oper SentCN : CN -> SC -> CN = notYet "SentCN" ;
oper SlashPrep : Cl -> Prep -> ClSlash = notYet "SlashPrep" ;
oper SlashV2A : V2A -> AP -> VPSlash = notYet "SlashV2A" ;
oper SlashV2Q : V2Q -> QS -> VPSlash = notYet "SlashV2Q" ;
oper SlashV2S : V2S -> S -> VPSlash = notYet "SlashV2S" ;
oper SlashVS : NP -> VS -> SSlash -> ClSlash = notYet "SlashVS" ;
oper SubjS : Subj -> S -> Adv = notYet "SubjS" ;
oper VocNP : NP -> Voc = notYet "VocNP" ;
oper pot3plus : Sub1000 -> Sub1000 -> Sub1000000 = notYet "pot3plus" ;

}
