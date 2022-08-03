abstract PhraseExt = Cat,CatExt ** {

  fun

    -- direct speech
    DirectSpeech: Phr -> Temp -> V -> NP -> Phr ;
    ExtPhrConj : Phr -> Phr -> Phr ;
}
