-- All functions are explicitly inherited, in order to allow useful probabilities

concrete NguniGrammarZul of NguniGrammar =
  NounZul [DetCN, UsePron, AdvNP, DetQuant, NumSg, NumPl, IndefArt, MassNP, UseN, AdjCN, RelCN, AdvCN, PossNP],
  VerbZul [UseV, ComplVS, ComplVA, SlashV2a, UseComp, AdvVP, CompAP, CompNP, CompAdv],
  AdjectiveZul [PositA, AdAP],
  AdverbZul [PositAdvAdj, SubjS],
  -- Numeral,
  SentenceZul [PredVP, ImpVP, UseCl, UseQCl, UseRCl],
  QuestionZul [QuestCl, QuestIAdv, QuestIComp],
  RelativeZul [RelVP, IdRP],
  -- Conjunction,
  PhraseZul [PhrUtt, UttS, UttQS, UttImpSg, NoPConj, NoVoc],
  TextX - [Temp,Adv,IAdv],
  StructuralZul [and_Conj, how_IAdv, how8much_IAdv, i_Pron, youSg_Pron],
  -- Idiom,
  TenseX [PPos,PNeg]
  -- Transfer ,
  TempZul [TPresTemp, TPerfTemp, TPastTemp, TFutTemp, TPastPresTemp, TFutPresTemp, TPerfPerfTemp, TFutPerfTemp, TPerfPresTemp]
  ** open ResZul, Prelude in {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
