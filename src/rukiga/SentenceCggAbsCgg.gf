concrete SentenceCggAbsCgg of SentenceCggAbs = CatCgg 
 open (R=ResCgg) in {

 	lincat
 		ExtTense  = {s : Str ; t : R.TensesExtra } ;
 		TempExtra = {s : Str ; t : R.TensesExtra a : R.Aspects} ;
 		Aspect    = {s : Str ; a : R.AspectsExtra } ;
 	lin
 		--TAspect -> ExtTense ->Ant -> TempExtra ;
 		TAspect extT a ={s = extT.s ++ a.s; t = exT.t; a = a.a};
    	--TRPast   		: ExtTense ;         -- bakagyenda [Remote past]
    	TRPast = {s = [] ; t = R.Remotepast };
    	--TIPast  		: ExtTense ;         -- baagyenda  [Immediate Past or Memorial ]
    	TIPast = {s =[] ; t = R.ImmediatePast}; 
    	--TRFut    		: ExtTense ;         -- I sleep/slept [simultaneous, not compound]
    	TRFut = {s = [] ; t = R.RemoteFut};
    	
    	--APerformative  	: Aspect ;           -- I slept [past, "imperfect"]      --# notpresent
    	APerformative = {s = []; a = R.Performative };
    	APerfect   	  = {s = []; a = R.Perfect };          -- I will sleep [future] --# notpresent
    	ARes  		  = {s = []; a = R.Resultative };           -- I would sleep [conditional] --# notpresent
    	ARetr 		  = {s = []; a = R.Retrospective };          -- I have slept/had slept [anterior, "compound", "perfect"] --# notpresent
		AHab          = {s = []; a = R.Habitual };
		AProg 		  = {s = []; a = R.Progrssive };
		APer  		  = {s = []; a = R.Persitive };
 		UseClExtra    : TempExtra -> Pol -> Cl  -> S ;   -- she had not slept
    		

    	--UseQClExtra   : TempExtra -> Pol -> QCl -> QS ;  -- who had not slept
    	--UseRClExtra   : TempExtra -> Pol -> RCl -> RS ;  -- that had not slept
    	--UseSlashExtra : TempExtra -> Pol -> ClSlash -> SSlash ; -- (that) she had not seen

}