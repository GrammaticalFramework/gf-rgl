concrete CzeRoundTripCze of CzeRoundTrip = LangCze [
  Utt, S, Cl, NP, VP, VPSlash, Pron, V2, V3, A, AP, Comp, Imp,
  Adv, Prep, N, CN, Det, Quant, Num,
  PrepNP, UttAdv, UseN, DetCN, DetQuant, DefArt, NumSg, in_Prep, city_N,
  Temp, Tense, Ant, Pol,
  UttS, UseCl, PredVP, UsePron, ComplSlash, SlashV2a, Slash2V3, Slash3V3,
  UseComp, CompAP, PositA, ImpVP, UttImpSg, UttImpPl, UttImpPol,
  TTAnt, TPres, ASimul, PPos, PNeg,
  youSg_Pron, he_Pron, she_Pron, youPl_Pron, youPol_Pron,
  love_V2, read_V2, young_A
] ** open ParadigmsCze in {
  lin
    buy_V3 = mkV3 (kupovatV "kupovat") ;
    currency_N = zenaN "měna" ;
    vAcc_Prep = v_Prep accusative ;
    -- Select the short prepositional accusative without a duplicate Pron.
    i_Pron = StructuralCze.i_Pron ** {pacc = "mě"} ;
}
