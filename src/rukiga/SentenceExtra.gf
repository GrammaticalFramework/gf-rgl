abstract SentenceExtra = Cat, TenseExtra  ** {
	
	fun
	  UseClExtra    : TempExtra -> Pol -> Cl ->S;  -- Maria naagura
	  
	  UseQClExtra   : TempExtra -> Pol -> QCl -> QS ;  -- Maria naagura?
	  UseRClExtra   : TempExtra -> Pol -> RCl -> RS ;  -- ngu Maria naagura
      UseSlashExtra : TempExtra -> Pol -> ClSlash -> SSlash ; -- (ngu) Maria tiaagura = tyaagura
}