concrete VerbCze of Verb = CatCze ** open ResCze, Prelude in {

lin
    UseV v = {
      verb = v ; clitPresent = False ;
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
        compl = \\a => vps.compl ! a ++ fullComplement vps.c np.s np.prep ++ vps.ind ! a
        }
      } ;

    SlashV2a v = {
      verb = v ; clitPresent = False ;
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
      verb = v ; clitPresent = isClit ;
      clit = \\_ => v.refl ++ case before of {True => weak ; False => []} ;
      clitAfter = \\_ => case before of {True => [] ; False => weak} ;
      compl = \\_ => case isClit of {True => [] ; False => fullComplement v.c np.s np.prep} ;
      c = v.c2 ;
      ind = \\_ => []
      } ;
    Slash3V3 v np = let
      isClit = hasCliticComplement v.c2 np.hasClit ;
      weak = case isClit of {True => np.clit ! v.c2.c ; False => []} ;
      before = cliticBefore v.c.c v.c2.c
      in {
      verb = v ; clitPresent = isClit ;
      clit = \\_ => v.refl ++ case before of {True => [] ; False => weak} ;
      clitAfter = \\_ => case before of {True => weak ; False => []} ;
      compl = \\_ => [] ;
      c = v.c ;
      ind = \\_ => case isClit of {True => [] ; False => fullComplement v.c2 np.s np.prep}
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

    ExtAdvVP vp adv = AdvVP vp adv ;
    AdVVP adv vp = vp ** {compl = \\a => adv.s ++ vp.compl ! a} ;

    AdvVPSlash vp adv = vp ** {compl = \\a => vp.compl ! a ++ adv.s} ;
    AdVVPSlash adv vp = vp ** {compl = \\a => adv.s ++ vp.compl ! a} ;

    UseCopula = {
      verb = copulaVerbForms ; clitPresent = False ;
      clit,compl = \\_ => []
      } ;

    ComplVA va ap = {
      verb = va ; clitPresent = False ; clit = \\_ => va.refl ;
      compl = ap.pred
      } ;

    ReflVP vp = {
      verb = vp.verb ; clitPresent = True ;
      clit = \\a => vp.clit ! a ++ "se" ++ vp.clitAfter ! a ;
      compl = \\a => vp.compl ! a ++ vp.ind ! a
      } ;

    VPSlashPrep vp prep = vp ** {
      c = prep ; ind = \\_ => [] ; clitAfter = \\_ => []
      } ;

    SlashV2A v ap = {
      verb = v ; clitPresent = False ; clit = \\_ => v.refl ;
      clitAfter = \\_ => [] ; c = v.c ; ind = \\a => ap.pred ! a ;
      compl = \\_ => []
      } ;

    SlashV2S v sent = {
      verb = v ; clitPresent = False ; clit = \\_ => v.refl ;
      clitAfter = \\_ => [] ; c = v.c ; ind = \\_ => SOFT_BIND ++ "," ++ sent.s ;
      compl = \\_ => []
      } ;

    SlashV2Q v sent = {
      verb = v ; clitPresent = False ; clit = \\_ => v.refl ;
      clitAfter = \\_ => [] ; c = v.c ; ind = \\_ => SOFT_BIND ++ "," ++ sent.ind ;
      compl = \\_ => []
      } ;

    SlashV2V v vp = {
      verb = v ; clitPresent = False ; clit = \\_ => v.refl ;
      clitAfter = \\_ => [] ; c = v.c ;
      compl = \\_ => [] ;
      ind = \\a => vp.verb.inf ++ vp.clit ! a ++ vp.compl ! a
      } ;

    SlashVV vv vp = {
      verb = vv ; clitPresent = andB vv.isAux vp.clitPresent ;
      clit = \\a => vv.refl ++ case vv.isAux of {True => vp.clit ! a ; False => []} ;
      clitAfter = vp.clitAfter ; c = vp.c ;
      compl = \\a => vp.verb.inf ++ case vv.isAux of {True => [] ; False => vp.clit ! a} ++ vp.compl ! a ;
      ind = vp.ind
      } ;

    SlashV2VNP v np vp = {
      verb = v ; clitPresent = np.hasClit ;
      clit = \\a => v.refl ++ case hasCliticComplement v.c np.hasClit of {
        True => np.clit ! v.c.c ; False => []} ;
      clitAfter = vp.clitAfter ; c = vp.c ;
      compl = \\a => case hasCliticComplement v.c np.hasClit of {
        True => [] ; False => fullComplement v.c np.s np.prep} ++
        vp.verb.inf ++ vp.clit ! a ++ vp.compl ! a ;
      ind = vp.ind
      } ;

-- VerbForms has no passive participle yet, so the reflexive passive is used:
-- "číslo se dělí" = "the number is divided"
    PassV2 v = {
      verb  = v ; clitPresent = True ;
      clit  = \\_ => "se" ;
      compl = \\_ => []
      } ;

    ComplVV vv vp = {
      verb  = vv ; clitPresent = andB vv.isAux vp.clitPresent ;
      clit = \\a => vv.refl ++ case vv.isAux of {True => vp.clit ! a ; False => []} ;
      compl = \\a => vp.verb.inf ++ case vv.isAux of {True => [] ; False => vp.clit ! a} ++ vp.compl ! a
      } ;

    ComplVS vs s = {
      verb  = vs ; clitPresent = False ;
      clit  = \\_ => vs.refl ;
      compl = \\_ => SOFT_BIND ++ "," ++ (frontSentence "že" s).s
      } ;

    ComplVQ v q = {
      verb = v ; clitPresent = False ; clit = \\_ => v.refl ;
      compl = \\_ => SOFT_BIND ++ "," ++ q.ind
      } ;
}
