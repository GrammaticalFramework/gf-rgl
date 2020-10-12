abstract TenseExtra = {
  cat
      TempExtra;
      TenseExtra;
      Asp;
  fun
    TTAsp : TenseExtra -> Asp -> TempExtra ;  -- [combination of tense and Apect, e.g. ExPres Performative]

    --PPos : Pol ;           -- I sleep  [positive polarity]
    --PNeg : Pol ;           -- I don't sleep [negative polarity]

    TExPres  : TenseExtra ;             -- I sleep/have slept [present]  
    
    TMPres  : TenseExtra ;             -- I slept [past, "imperfect"]      --# notpresent
    TNFut   : TenseExtra ;             -- I will sleep [future] --# notpresent
    TNPast  : TenseExtra ;             -- I would sleep [conditional] --# notpresent
    TRPast  : TenseExtra ;
    TRFut   : TenseExtra ;
    APerform : Asp ;               -- I sleep/slept [simultaneous, not compound]
    APerf : Asp ;               -- I have slept/had slept [anterior, "compound", "perfect"] --# notpresent
    AResult : Asp ;
    ARetrosp : Asp ;
    AHabitual : Asp ;
    AProg : Asp ;
    APersist : Asp ;
}