concrete TenseExtraCgg of TenseExtra = {
	param
		-- for Extra Tenses not implemented
  		-- would be better if I had alliases
  		Tenses = RemotePast|NearPast | MemorialPres |ExpPres|NearFut |RemoteFut;

  		-- for Extra Aspects not implemented
  		-- would be better if I had alliases
  		Aspect = Performative | Perfect | Resultative | Retrospective | Habitual | Progressive | Persistive;
	lincat
		TempExtra  = {s:Str; t:Tenses; a:Aspect};
	    TenseExtra = {s:Str; t:Tenses};
	    Asp = {s:Str; a:Aspect};
	--lindef
	 --TempExtra = \str_0 -> {s = str_0; a = Habitual;t = ExPres} ;

    lin
    	--TTAsp : TenseExtra -> Asp -> TempExtra ;  -- [combination of tense and Apect, e.g. ExPres Performative]
    	TTAsp tense aspect = {s=[]; t=tense.t; a=aspect.a};
	    --PPos : Pol ;           -- I sleep  [positive polarity]
	    --PNeg : Pol ;           -- I don't sleep [negative polarity]

	    TExPres  = {s=[]; t= ExpPres} ;             -- I sleep/have slept [present]  
	    
	    TMPres  = {s=[]; t=MemorialPres} ;             -- I slept [past, "imperfect"]      --# notpresent
	    TNFut   = {s=[]; t=NearFut} ;             -- I will sleep [future] --# notpresent
	    TNPast  = {s=[]; t= NearPast} ;             -- I would sleep [conditional] --# notpresent
	    TRPast  = {s=[]; t= RemotePast} ;
	    TRFut   = {s=[]; t = RemoteFut} ;
	    APerform = {s=[]; a = Performative} ;               -- I sleep/slept [simultaneous, not compound]
	    APerf = {s=[]; a= Perfect};               -- I have slept/had slept [anterior, "compound", "perfect"] --# notpresent
	    AResult = {s=[]; a= Resultative};
	    ARetrosp = {s=[]; a= Retrospective};
	    AHabitual = {s=[]; a= Habitual};
	    AProg = {s=[]; a=Progressive} ;
	    APersist = {s=[]; a=Persistive} ;
}