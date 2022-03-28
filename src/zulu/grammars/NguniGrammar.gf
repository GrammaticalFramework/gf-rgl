-- All functions are explicitly inherited, in order to allow useful probabilities

abstract NguniGrammar =
  Noun [DetCN, UsePron, DetQuant, NumSg, NumPl, IndefArt, DefArt, UseN, AdjCN, RelCN, PossNP,UsePN, RelNP, PredetNP, MassNP], -- MassNP,AdvCN, AdvNP,
  Verb [UseV, ComplVS, ComplVA, SlashV2a, UseComp, AdvVP, CompAP, CompNP, CompAdv, ComplSlash],
  Adjective [PositA, AdAP],
  Adverb [PositAdvAdj, SubjS],
  -- Numeral,
  Sentence [PredVP, ImpVP, UseCl, UseQCl, UseRCl],
  Question [QuestCl, QuestIAdv, QuestIComp],
  Relative [RelVP, IdRP],
  -- Conjunction,
  Phrase [PhrUtt, UttS, UttQS, UttImpSg, NoPConj, NoVoc],
  Text - [Temp,Adv,IAdv],
  Structural [and_Conj,how_IAdv,how8much_IAdv,i_Pron,youSg_Pron,he_Pron,she_Pron,we_Pron,youPl_Pron,they_Pron,this_Quant,that_Quant,all_Predet,only_Predet,very_AdA],
  -- Idiom,
  Tense [PPos, PNeg],
  -- Transfer ,
  TempExt -- [TPresTemp, TPerfTemp, TPastTemp, TFutTemp, TPastPresTemp, TFutPresTemp, TPerfPerfTemp, TFutPerfTemp, TPerfPresTemp]
  ;
