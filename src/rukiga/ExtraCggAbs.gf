abstract ExtraCggAbs = Cat **{

-- there is a default linearization for abstract 
-- categories Tense and Temp
-- these in TenseX
--
	cat
		AllTenses;
		--TempExtra;
	fun
		UseClExtra    : TempExtra -> Pol -> Cl  -> S ;   -- she had not slept
    	UseQClExtra   : TempExtra -> Pol -> QCl -> QS ;  -- who had not slept
    	UseRClExtra   : TempExtra -> Pol -> RCl -> RS ;  -- that had not slept
    	UseSlashExtra : TempExtra -> Pol -> ClSlash -> SSlash ; -- (that) she had not seen

}