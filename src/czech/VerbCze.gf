concrete VerbCze of Verb = CatCze ** open ResCze, Prelude in {

lin
    UseV v = {
      verb = v ;
      clit,compl = \\_ => []
      } ;
    
    ComplSlash vps np = case <np.hasClit, vps.c.hasPrep> of {
      <True,False> => vps ** {
        clit = \\a => vps.clit ! a ++ np.clit ! vps.c.c ;
        compl = \\a => vps.compl ! a ++ vps.ind ! a
        } ;
      _  => vps ** {
        compl = \\a => vps.compl ! a ++ vps.c.s ++ np.s ! vps.c.c ++ vps.ind ! a
        }
      } ;

    SlashV2a v = {
      verb = v ;
      clit,compl = \\_ => [] ;
      c = v.c ;
      ind = \\_ => []
      } ;

    -- three-place verbs: c = direct object case, c2 = indirect object case
    Slash2V3 v np = {          -- fill the direct object, leave the indirect open
      verb = v ;
      clit = \\_ => [] ;
      compl = \\_ => v.c.s ++ np.s ! v.c.c ;
      c = v.c2 ;
      ind = \\_ => []
      } ;
    Slash3V3 v np = {          -- fill the indirect object (rendered after the object slot)
      verb = v ;
      clit = \\_ => [] ;
      compl = \\_ => [] ;
      c = v.c ;
      ind = \\_ => v.c2.s ++ np.s ! v.c2.c
      } ;

    UseComp comp = {
      verb  = copulaVerbForms ;
      clit = \\_ => [] ;
      compl = comp.s
      } ;
      
    CompAP ap = {
      s = \\a => case a of {
        Ag g n p_ => ap.s ! g ! n ! Nom
        }
      } ;
      
    CompNP np = {
      s = \\a_ => np.s ! Nom ; ---- InstrC in Pol
      } ;

    CompCN cn = {
      s = \\a => case a of {
        Ag _ n _ => cn.s ! n ! Nom ---- InstrC also possible
        }
      } ;
      
    CompAdv adv = {
      s = \\a_ => adv.s
      } ;

    AdvVP vp adv = vp ** {
      compl = \\a => vp.compl ! a ++ adv.s
      } ;

-- VerbForms has no passive participle yet, so the reflexive passive is used:
-- "číslo se dělí" = "the number is divided"
    PassV2 v = {
      verb  = v ;
      clit  = \\_ => "se" ;
      compl = \\_ => []
      } ;

    ComplVV vv vp = {
      verb  = vv ;
      clit  = vp.clit ;
      compl = \\a => vp.verb.inf ++ vp.compl ! a
      } ;

    ComplVS vs s = {
      verb  = vs ;
      clit  = \\_ => [] ;
      compl = \\_ => SOFT_BIND ++ "," ++ "že" ++ s.s
      } ;

}
