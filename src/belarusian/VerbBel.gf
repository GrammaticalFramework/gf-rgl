concrete VerbBel of Verb = CatBel ** open ResBel, (R = ParamX), Prelude in {

lin
  UseV v = mkVPhrase v ;

  ComplVV vv vp = {
    s = \\t,p,a => finiteVerb vv t p a ++ vp.inf ! a ;
    inf = \\a => vv.infinitive ++ vp.inf ! a ;
    imp = \\p,n => neg p ++ vv.imperative ! n ++ vp.inf ! defaultAgr
  } ;
  ComplVS vs s = {
    s = \\t,p,a => finiteVerb vs t p a ++ "што" ++ s.s ;
    inf = \\_ => vs.infinitive ++ "што" ++ s.s ;
    imp = \\p,n => neg p ++ vs.imperative ! n ++ "што" ++ s.s
  } ;
  ComplVQ vq qs = {
    s = \\t,p,a => finiteVerb vq t p a ++ qs.s ;
    inf = \\_ => vq.infinitive ++ qs.s ;
    imp = \\p,n => neg p ++ vq.imperative ! n ++ qs.s
  } ;
  ComplVA va ap = {
    s = \\t,p,a => finiteVerb va t p a ++ ap.s ! Nom ! genNum a.g a.n ;
    inf = \\a => va.infinitive ++ ap.s ! Nom ! genNum a.g a.n ;
    imp = \\p,n => neg p ++ va.imperative ! n ++ ap.s ! Nom ! GPl
  } ;

  SlashV2a v = mkVSlash v v.c2 v.pa ;
  Slash2V3 v np = {
    s = \\t,p,a => finiteVerb v t p a ++ prepNP v.c2 np ;
    inf = v.infinitive ++ prepNP v.c2 np ;
    c = v.c3 ;
    imp = \\p,n => neg p ++ v.imperative ! n ++ prepNP v.c2 np ;
    pass = v.pa ;
    post = [] ;
    objPost = \\_ => []
  } ;
  Slash3V3 v np = {
    s = \\t,p,a => finiteVerb v t p a ;
    inf = v.infinitive ++ prepNP v.c3 np ;
    c = v.c2 ;
    imp = \\p,n => neg p ++ v.imperative ! n ;
    pass = v.pa ;
    post = prepNP v.c3 np ;
    objPost = \\_ => prepNP v.c3 np
  } ;
  SlashV2V v vp = {
    s = \\t,p,a => finiteVerb v t p a ;
    inf = v.infinitive ++ vp.inf ! defaultAgr ;
    c = v.c2 ;
    imp = \\p,n => neg p ++ v.imperative ! n ;
    pass = v.pa ;
    post = vp.inf ! defaultAgr ;
    objPost = vp.inf
  } ;
  SlashV2S v s = {
    s = \\t,p,a => finiteVerb v t p a ;
    inf = v.infinitive ++ "што" ++ s.s ;
    c = v.c2 ;
    imp = \\p,n => neg p ++ v.imperative ! n ;
    pass = v.pa ;
    post = "што" ++ s.s ;
    objPost = \\_ => "што" ++ s.s
  } ;
  SlashV2Q v qs = {
    s = \\t,p,a => finiteVerb v t p a ;
    inf = v.infinitive ++ qs.s ;
    c = v.c2 ;
    imp = \\p,n => neg p ++ v.imperative ! n ;
    pass = v.pa ;
    post = qs.s ;
    objPost = \\_ => qs.s
  } ;
  SlashV2A v ap = {
    s = \\t,p,a => finiteVerb v t p a ;
    inf = v.infinitive ++ ap.s ! Nom ! GSg Masc ;
    c = v.c2 ;
    imp = \\p,n => neg p ++ v.imperative ! n ;
    pass = v.pa ;
    post = ap.s ! Instr ! GSg Masc ;
    objPost = \\a => ap.s ! Instr ! genNum a.g a.n
  } ;

  ComplSlash vp np = {
    s = \\t,p,a => vp.s ! t ! p ! a ++ prepNP vp.c np ++ vp.objPost ! np.a ;
    inf = \\_ => vp.inf ++ prepNP vp.c np ;
    imp = \\p,n => vp.imp ! p ! n ++ prepNP vp.c np ++ vp.objPost ! np.a
  } ;

  SlashVV vv vp = {
    s = \\t,p,a => finiteVerb vv t p a ++ vp.inf ;
    inf = vv.infinitive ++ vp.inf ;
    c = vp.c ;
    imp = \\p,n => neg p ++ vv.imperative ! n ++ vp.imp ! R.Pos ! n ;
    pass = vp.pass ;
    post = vp.post ;
    objPost = vp.objPost
  } ;
  SlashV2VNP v np vp = {
    s = \\t,p,a => finiteVerb v t p a ++ prepNP v.c2 np ++ vp.s ! R.Pres ! R.Pos ! a ;
    inf = v.infinitive ++ prepNP v.c2 np ++ vp.inf ;
    c = vp.c ;
    imp = \\p,n => neg p ++ v.imperative ! n ++ prepNP v.c2 np ++ vp.imp ! R.Pos ! n ;
    pass = vp.pass ;
    post = vp.post ;
    objPost = vp.objPost
  } ;

  ReflVP vp = {
    s = \\t,p,a => vp.s ! t ! p ! a ++ "сябе" ++ vp.objPost ! a ;
    inf = \\a => vp.inf ++ "сябе" ++ vp.objPost ! a ;
    imp = \\p,n => vp.imp ! p ! n ++ "сябе" ++ vp.post
  } ;
  UseComp comp = {
    s = \\t,p,a => copula t p a ++ comp.s ! a ;
    inf = \\a => "быць" ++ comp.s ! a ;
    imp = \\p,_ => neg p ++ "будзь" ++ comp.s ! defaultAgr
  } ;

  PassV2 v = {
    s = \\t,p,a => copula t p a ++ v.participle ! a.g ! a.n ;
    inf = \\a => "быць" ++ v.participle ! a.g ! a.n ;
    imp = \\p,_ => neg p ++ "будзь" ++ v.participle ! Masc ! Sg
  } ;

  AdvVP vp adv = addAdvVP vp adv.s ;
  ExtAdvVP vp adv = addAdvVP vp adv.s ;
  AdVVP adv vp = addAdVVP adv.s vp ;
  AdvVPSlash vp adv = addAdvSlash vp adv.s ;
  AdVVPSlash adv vp = {
    s = \\t,p,a => adv.s ++ vp.s ! t ! p ! a ;
    inf = adv.s ++ vp.inf ;
    c = vp.c ;
    imp = \\p,n => adv.s ++ vp.imp ! p ! n ;
    pass = vp.pass ;
    post = vp.post ;
    objPost = vp.objPost
  } ;
  VPSlashPrep vp prep = {
    s = \\t,p,a => vp.s ! t ! p ! a ++ prep.s ;
    inf = vp.inf ! defaultAgr ++ prep.s ;
    c = prep ;
    imp = \\p,n => vp.imp ! p ! n ++ prep.s ;
    pass = adjFromStr "выкананы" ;
    post = [] ;
    objPost = \\_ => []
  } ;

  CompAP ap = {s = \\a => ap.s ! Nom ! genNum a.g a.n} ;
  CompNP np = {s = \\_ => np.s ! Nom} ;
  CompAdv adv = {s = \\_ => adv.s} ;
  CompCN cn = {s = \\a => cn.s ! Nom ! a.n} ;
  UseCopula = {
    s = \\t,p,a => copula t p a ;
    inf = \\_ => "быць" ;
    imp = \\p,_ => neg p ++ "будзь"
  } ;

}
