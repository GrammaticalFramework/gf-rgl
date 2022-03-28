-- All functions are explicitly inherited, in order to allow useful probabilities

concrete NguniGrammarZul of NguniGrammar =
  NounZul [DetCN, UsePron, DetQuant, NumSg, NumPl, IndefArt, DefArt, UseN, AdjCN, RelCN, PossNP,UsePN, RelNP, PredetNP, MassNP], -- MassNP, AdvCN,AdvNP,
  VerbZul [UseV, ComplVS, ComplVA, SlashV2a, UseComp, AdvVP, CompAP, CompNP, CompAdv, ComplSlash],
  AdjectiveZul [PositA, AdAP],
  AdverbZul [PositAdvAdj, SubjS],
  -- Numeral,
  SentenceZul [PredVP, ImpVP, UseCl, UseQCl, UseRCl],
  QuestionZul [QuestCl, QuestIAdv, QuestIComp],
  RelativeZul [RelVP, IdRP],
  -- Conjunction,
  PhraseZul [PhrUtt, UttS, UttQS, UttImpSg, NoPConj, NoVoc],
  TextX - [Temp,Adv,IAdv],
  StructuralZul [and_Conj,how_IAdv,how8much_IAdv,i_Pron,youSg_Pron,he_Pron,she_Pron,we_Pron,youPl_Pron,they_Pron,this_Quant,that_Quant,all_Predet,only_Predet,very_AdA],
  -- Idiom,
  TenseX [PPos,PNeg],
  -- Transfer ,
  TempExtZul -- [TPresTemp, TPerfTemp, TPastTemp, TFutTemp, TPastPresTemp, TFutPresTemp, TPerfPerfTemp, TFutPerfTemp, TPerfPresTemp]
  ** open ResZul, Prelude in {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
