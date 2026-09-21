-- A finite RGL fragment: all parses can be checked without truncation.
abstract CzeRoundTrip = Lang [
  Utt, S, Cl, NP, VP, VPSlash, Pron, V2, V3, A, AP, Comp, Imp,
  Adv, Prep, N, CN, Det, Quant, Num,
  PrepNP, UttAdv, UseN, DetCN, DetQuant, DefArt, NumSg, in_Prep, city_N,
  Temp, Tense, Ant, Pol,
  UttS, UseCl, PredVP, UsePron, ComplSlash, SlashV2a, Slash2V3, Slash3V3,
  UseComp, CompAP, PositA, ImpVP, UttImpSg, UttImpPl, UttImpPol,
  TTAnt, TPres, ASimul, PPos, PNeg,
  i_Pron, youSg_Pron, he_Pron, she_Pron, youPl_Pron, youPol_Pron,
  love_V2, read_V2, young_A
] ** {
  flags startcat = Utt ;
  fun buy_V3 : V3 ;
  fun currency_N : N ; vAcc_Prep : Prep ;
}
