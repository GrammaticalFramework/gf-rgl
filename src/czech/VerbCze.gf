concrete VerbCze of Verb = CatCze ** open ResCze, Prelude in {

lin
    UseV v = {
      verb = v ; clitPresent = v.isRefl ;
      clit = \\_ => v.refl ; compl = \\_ => []
      } ;
    
    ComplSlash vps np = case hasCliticComplement vps.c np.hasClit of {
      True => vps ** {
        clitPresent = True ;
        clit = \\a => vps.clit ! a ++ np.clit ! vps.c.c ++ vps.clitAfter ! a ;
        compl = \\a => vps.compl ! a ++ vps.ind ! a
        } ;
      False => vps ** {
        clit = \\a => vps.clit ! a ++ vps.clitAfter ! a ;
        compl = \\a => vps.compl ! a ++ vps.c.s ++ (case vps.c.hasPrep of {True => np.prep ! vps.c.c ; False => np.s ! vps.c.c}) ++ vps.ind ! a
        }
      } ;

    SlashV2a v = {
      verb = v ; clitPresent = v.isRefl ;
      clit = \\_ => v.refl ; compl = \\_ => [] ;
      c = v.c ;
      ind = \\_ => [] ; clitAfter = \\_ => []
      } ;

    -- Full objects retain c-before-c2 order; weak objects follow case order.
    Slash2V3 v np = let
      isClit = hasCliticComplement v.c np.hasClit ;
      weak = case isClit of {True => np.clit ! v.c.c ; False => []} ;
      before = cliticBefore v.c.c v.c2.c
      in {
      verb = v ; clitPresent = orB v.isRefl isClit ;
      clit = \\_ => v.refl ++ case before of {True => weak ; False => []} ;
      clitAfter = \\_ => case before of {True => [] ; False => weak} ;
      compl = \\_ => case isClit of {True => [] ; False => v.c.s ++ np.s ! v.c.c} ;
      c = v.c2 ;
      ind = \\_ => []
      } ;
    Slash3V3 v np = let
      isClit = hasCliticComplement v.c2 np.hasClit ;
      weak = case isClit of {True => np.clit ! v.c2.c ; False => []} ;
      before = cliticBefore v.c.c v.c2.c
      in {
      verb = v ; clitPresent = orB v.isRefl isClit ;
      clit = \\_ => v.refl ++ case before of {True => [] ; False => weak} ;
      clitAfter = \\_ => case before of {True => weak ; False => []} ;
      compl = \\_ => [] ;
      c = v.c ;
      ind = \\_ => case isClit of {True => [] ; False => v.c2.s ++ np.s ! v.c2.c}
      } ;

    UseComp comp = {
      verb  = copulaVerbForms ; clitPresent = False ;
      clit = \\_ => [] ;
      compl = comp.s
      } ;
      
    CompAP ap = {s = ap.pred} ;
      
    CompNP np = {
      -- An identifying NP retains its own number; only its case is selected.
      s = \\a_ => np.s ! Nom ;
      } ;

    CompCN cn = {
      s = \\a => case a of {
        Ag _ n _ => cn.s ! n ! Nom ;
        AgQuant _ => cn.s ! Pl ! Ins ; -- selected formal predicative instrumental
        AgPol _ => cn.s ! Sg ! Nom
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
      verb  = v ; clitPresent = True ;
      clit  = \\_ => "se" ;
      compl = \\_ => []
      } ;

    ComplVV vv vp = {
      verb  = vv ; clitPresent = orB vv.isRefl (andB vv.isAux vp.clitPresent) ;
      clit = \\a => vv.refl ++ case vv.isAux of {True => vp.clit ! a ; False => []} ;
      compl = \\a => vp.verb.inf ++ case vv.isAux of {True => [] ; False => vp.clit ! a} ++ vp.compl ! a
      } ;

    ComplVS vs s = {
      verb  = vs ; clitPresent = vs.isRefl ;
      clit  = \\_ => vs.refl ;
      compl = \\_ => SOFT_BIND ++ "," ++ (frontSentence "že" s).s
      } ;

    ComplVQ v q = {
      verb = v ; clitPresent = v.isRefl ; clit = \\_ => v.refl ;
      compl = \\_ => SOFT_BIND ++ "," ++ q.ind
      } ;
}
