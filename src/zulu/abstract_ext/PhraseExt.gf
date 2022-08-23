abstract PhraseExt = Cat,CatExt ** {

  fun

    -- direct speech
    DirectSpeech: Phr -> Temp -> V -> NP -> Phr ;
    DirectSpeechNP : NP -> Temp -> V -> NP -> Phr ;
    DirectSpeechAdv : Adv -> Temp -> V -> NP -> Phr ;
    ExtPhrConj : Phr -> Phr -> Phr ;
}
