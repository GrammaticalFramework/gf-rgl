-- A finite fragment of the Czech extensions. Subject and BareRNP limit
-- ProDrop and predeterminers to one application without recursive aliases.
abstract CzeExtensionRoundTrip = AllCzeAbs [
  Utt, S, Cl, NP, VP, VPSlash, Pron, V, V2, AP, QCl, QS, IP,
  N, CN, Det, Quant, Num, IDet, Predet, Adv, Prep, Subj, Temp, Tense, Ant, Pol,
  PrepNP, possess_Prep, by8agent_Prep, with_Prep, he_Pron, we_Pron,
  IAdv, UttIAdv, PrepIP, QuestVP, whoSg_IP,
  A, Comp, UseComp, CompAP, young_A,
  UttS, UttQS, UttAdv, UseCl, UseQCl, UsePron, UseV, SlashV2a, ComplSlash,
  SlashV2AP, DativeCopulaCl, DativeCopulaQCl, SubjS,
  UseN, DetCN, DetQuant, DefArt, NumSg, NumPl, IdetCN,
  TTAnt, TPres, ASimul, PPos, PNeg,
  i_Pron, she_Pron, youPol_Pron, have_V2, love_V2, wait_V2,
  year_N, child_N, how8many_IDet, only_Predet, all_Predet, if_Subj
] ** {
  flags startcat = Utt ;
  cat Subject ; BareRNP ; RNP ; ComparisonNP ; ModifiedNP ; NPModifier ;
  fun
    -- NPModifier excludes PrepNP, avoiding recursion through NP and Adv.
    AdvPron : Pron -> NPModifier -> ModifiedNP ;
    PredetNP : Predet -> ModifiedNP -> NP ;
    home_Adv : NPModifier ;
    ComparisonPron : Pron -> ComparisonNP ;
    Compare : A -> ComparisonNP -> AP ;
    ModifiedN : AP -> N -> CN ;
    FullSubject : NP -> Subject ;
    DroppedSubject : Pron -> Subject ;
    PredSubject : Subject -> VP -> Cl ;
    ReflPron : BareRNP ;
    ReflPoss : Num -> CN -> BareRNP ;
    UseRNP : BareRNP -> RNP ;
    PredetRNP : Predet -> BareRNP -> RNP ;
    ReflRNP : VPSlash -> RNP -> VP ;
    like_AP : AP ; son_N : N ; wash_V : V ;
    fast_A : A ;
    two_Num, five_Num : Num ;
    writeAbout_V2 : V2 ;
}
