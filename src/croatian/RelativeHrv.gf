concrete RelativeHrv of Relative = CatHrv ** open
  ParadigmsHrv,
  ResHrv,
  Prelude in {

lin
    RelVP rp vp = vp ** {
      subj  =
        let rel = (adjFormsAdjective rp).s
	in \\a => case a of {
	  Ag g n _ => rel ! g ! n ! Nom
	  } 
      } ;
      
    IdRP = mkA "který" ; 


}
