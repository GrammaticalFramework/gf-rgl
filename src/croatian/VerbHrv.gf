concrete VerbHrv of Verb = CatHrv ** open ResHrv, Prelude in {

lin
    UseV v = {
      verb = v.s ;
      clit,compl = \\_ => []
      } ;
    
    ComplSlash vps np = case <np.hasClit, vps.c.hasPrep> of {
      <True,False> => vps ** {
        clit = \\a => vps.clit ! a ++ np.clit ! vps.c.c
        } ;
      _  => vps ** {
        compl = \\a => vps.compl ! a ++ vps.c.s ++ np.s ! vps.c.c
        }
      } ;

    SlashV2a v = {
      verb = v.s ;
      clit,compl = \\_ => [] ;
      c = v.c
      } ;

    UseComp comp = {
      verb  = copula_VerbForms ;
      clit = \\_ => [] ;
      compl = comp.s
      } ;
      
    CompAP ap = {
      s = \\a => case a of {
        Ag g n p_ => ap.s ! g ! n ! Nom
        }
      } ;
      
    CompNP np = {
      s = \\a_ => np.s ! Nom ;
      } ;
      
    CompCN cn = {
      s = \\a => case a of {
        Ag _ n _ => cn.s ! n ! Nom
	} ;
      } ;
      
    CompAdv adv = {
      s = \\a_ => adv.s
      } ;

    AdvVP vp adv = vp ** {
      compl = \\a => vp.compl ! a ++ adv.s
      } ;

}
