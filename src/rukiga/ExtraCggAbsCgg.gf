concrete ExtraCggAbsCgg of ExtraCggAbs = CatCgg 
 open (R=ResCgg), (P=ParamX) in {

 	lincat
 		AllTenses =  {s : Str ; t : P.Tense; tExtra : R.TensesExtra } ;
 		TempExtra = {s : Str ; t : R.TensesExtra } ;
 		--TempExtraWithAspects  = {s : Str ; t : P.Tense ; a : R.AspectsExtra } ;
 	fun
 		UseClExtra    : TempTempExtra -> Pol -> Cl  -> S ;   -- she had not slept
    	UseQClExtra   : TempTempExtra -> Pol -> QCl -> QS ;  -- who had not slept
    	UseRClExtra   : TempTempExtra -> Pol -> RCl -> RS ;  -- that had not slept
    	UseSlashExtra : TempTempExtra -> Pol -> ClSlash -> SSlash ; -- (that) she had not seen

}