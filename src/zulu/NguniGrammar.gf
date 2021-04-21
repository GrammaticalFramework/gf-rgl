-- All functions are explicitly inherited, in order to allow useful probabilities

abstract NguniGrammar =
  Noun [DetCN, UsePron, AdvNP, DetQuant, NumSg, NumPl, IndefArt, MassNP, UseN, AdjCN, RelCN, AdvCN, PossNP],
  Verb [UseV, ComplVS, ComplVA, SlashV2a, UseComp, AdvVP, CompAP, CompNP, CompAdv],
  Adjective [PositA, AdAP],
  Adverb [PositAdvAdj, SubjS],
  -- Numeral,
  Sentence [PredVP, ImpVP, UseCl, UseQCl, UseRCl],
  Question [QuestCl, QuestIAdv, QuestIComp],
  Relative [RelVP, IdRP],
  -- Conjunction,
  Phrase [PhrUtt, UttS, UttQS, UttImpSg, NoPConj, NoVoc],
  Text - [Temp,Adv,IAdv],
  Structural [and_Conj,how_IAdv,how8much_IAdv,i_Pron,youSg_Pron,he_Pron,she_Pron,we_Pron,youPl_Pron],
  -- Idiom,
  Tense [PPos, PNeg],
  -- Transfer ,
  TempAbs [TPresTemp, TPerfTemp, TPastTemp, TFutTemp, TPastPresTemp, TFutPresTemp, TPerfPerfTemp, TFutPerfTemp, TPerfPresTemp]
  ;
