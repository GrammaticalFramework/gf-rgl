concrete RelativeSlo of Relative = CatSlo ** open
  ParadigmsSlo,
  ResSlo,
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
