concrete RelativeCze of Relative = CatCze ** open
  ParadigmsCze,
  ResCze,
  Prelude in {

lin
    RelVP rp vp = vp ** {
      subj  =
        let rel = (adjFormsAdjective rp).s
	in \\a => case a of {
	  Ag g n _ => rel ! g ! n ! Nom ; AgPol g => rel ! g ! Sg ! Nom ; AgQuant g => rel ! g ! Pl ! Nom
	  }
      } ;

    IdRP = guessAdjForms "který" ;

    RelCl cl = {
      subj = \\_ => cl.subj ; clit = \\_ => cl.clit ;
      compl = \\_ => cl.compl ; verb = cl.verb
      } ;

    RelSlash rp cls = {
      subj = \\_ => cls.subj ; clit = \\_ => cls.clit ; verb = cls.verb ;
      compl = \\a => case a of {
        Ag g n _ => fullComplement cls.c
          (\\c => (adjFormsAdjective rp).s ! g ! n ! c)
          (\\c => (adjFormsAdjective rp).s ! g ! n ! c) ;
        AgPol g => fullComplement cls.c
          (\\c => (adjFormsAdjective rp).s ! g ! Sg ! c)
          (\\c => (adjFormsAdjective rp).s ! g ! Sg ! c) ;
        AgQuant g => fullComplement cls.c
          (\\c => (adjFormsAdjective rp).s ! g ! Pl ! c)
          (\\c => (adjFormsAdjective rp).s ! g ! Pl ! c)
        } ++ cls.compl ++ cls.ind
      } ;


}
