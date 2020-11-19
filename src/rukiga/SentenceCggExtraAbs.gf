abstract SentenceCggExtraAbs = Cat **{
	
	cat
		ExtTense;
		TempExtra;
		Aspect;
	fun
		TAspect -> ExtTense ->Ant -> TempExtra ;
    	TRPast   		: ExtTense ;         -- bakagyenda [Remote past]
    	TIPast  		: ExtTense ;         -- baagyenda  [Immediate Past or Memorial ]
    	TRFut    		: ExtTense ;               -- I sleep/slept [simultaneous, not compound]
    	APerformative  	: Aspect ;             -- I slept [past, "imperfect"]      --# notpresent
    	APerfect   		: Aspect ;             -- I will sleep [future] --# notpresent
    	ARes  			: Aspect ;             -- I would sleep [conditional] --# notpresent
    	ARetr 			: Aspect ;               -- I have slept/had slept [anterior, "compound", "perfect"] --# notpresent
		AHab 			: Aspect ;
		AProg 			: Aspect ;
		APer  			: Aspect ;
		

		UseClExtra    : TempExtra -> Pol -> Cl  -> S ;   -- she had not slept
    	UseQClExtra   : TempExtra -> Pol -> QCl -> QS ;  -- who had not slept
    	UseRClExtra   : TempExtra -> Pol -> RCl -> RS ;  -- that had not slept
    	UseSlashExtra : TempExtra -> Pol -> ClSlash -> SSlash ; -- (that) she had not seen
}