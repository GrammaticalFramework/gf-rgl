concrete CzeExtensionRoundTripCze of CzeExtensionRoundTrip = AllCze [
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
] ** open SyntaxCze, ParadigmsCze, (E = ExtendCze) in {
  lincat Subject, ComparisonNP, ModifiedNP = NP ; BareRNP, RNP = E.RNP ;
    NPModifier = Adv ;
  lin
    AdvPron p adv = SyntaxCze.mkNP (SyntaxCze.mkNP p) adv ;
    PredetNP pred np = SyntaxCze.mkNP pred np ;
    home_Adv = ParadigmsCze.mkAdv "doma" ;
    ComparisonPron = UsePron ;
    Compare a np = mkAP a np ;
    ModifiedN ap n = mkCN ap n ;
    FullSubject np = np ;
    DroppedSubject p = UsePron (E.ProDrop p) ;
    PredSubject = PredVP ;
    ReflPron = E.ReflPron ;
    ReflPoss = E.ReflPoss ;
    UseRNP rnp = rnp ;
    PredetRNP = E.PredetRNP ;
    ReflRNP = E.ReflRNP ;
    like_AP = shortAP "rád" "ráda" "rádo" "rádi" "rády" "ráda" ;
    fast_A = mkA "rychlý" ;
    son_N = panN "syn" ;
    wash_V = seV (krytV "mýt") ;
    two_Num = mkNum "2" ;
    five_Num = mkNum "5" ;
    writeAbout_V2 = LexiconCze.write_V2 ** {c = mkPrep "o" locative} ;
}
