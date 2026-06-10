concrete VerbUkr of Verb = CatUkr ** open ResUkr, (R = ParamX) in {

oper
  mkVP : V -> VP = \v -> lin VP {
    s = \\t,pol,g,n,p => finiteVerb v t pol g n p ;
    inf = v.infinitive ;
    imp = \\pol,n => neg pol ++ v.imperative2 ! n
  } ;

  mkSlash : V -> Compl -> VPSlash = \v,c -> lin VPSlash {
    s = \\t,pol,g,n,p => finiteVerb v t pol g n p ;
    inf = v.infinitive ;
    imp = \\pol,n => neg pol ++ v.imperative2 ! n ;
    c = c ;
    post = []
  } ;

lin
  UseV v = mkVP v ;

  ComplVV vv vp = {
    s = \\t,pol,g,n,p => finiteVerb vv t pol g n p ++ vp.inf ;
    inf = vv.infinitive ++ vp.inf ;
    imp = \\pol,n => neg pol ++ vv.imperative2 ! n ++ vp.inf
  } ;
  ComplVS vs s = {
    s = \\t,pol,g,n,p => finiteVerb vs t pol g n p ++ s.s ;
    inf = vs.infinitive ++ s.s ;
    imp = \\pol,n => neg pol ++ vs.imperative2 ! n ++ s.s
  } ;
  ComplVQ vq qs = {
    s = \\t,pol,g,n,p => finiteVerb vq t pol g n p ++ qs.s ;
    inf = vq.infinitive ++ qs.s ;
    imp = \\pol,n => neg pol ++ vq.imperative2 ! n ++ qs.s
  } ;
  ComplVA va ap = {
    s = \\t,pol,g,n,p => finiteVerb va t pol g n p ++ ap.s ! Nom ! genNum g n ;
    inf = va.infinitive ++ ap.s ! Nom ! GSg Masc ;
    imp = \\pol,n => neg pol ++ va.imperative2 ! n ++ ap.s ! Nom ! genNum Masc n
  } ;

  SlashV2a v = mkSlash v v.c2 ;
  Slash2V3 v np = (mkSlash v v.c3) ** {
    s = \\t,pol,g,n,p => finiteVerb v t pol g n p ++ prepNP v.c2 np ;
    inf = v.infinitive ++ prepNP v.c2 np ;
    imp = \\pol,n => neg pol ++ v.imperative2 ! n ++ prepNP v.c2 np
  } ;
  Slash3V3 v np = (mkSlash v v.c2) ** {
    s = \\t,pol,g,n,p => finiteVerb v t pol g n p ++ prepNP v.c3 np ;
    inf = v.infinitive ++ prepNP v.c3 np ;
    imp = \\pol,n => neg pol ++ v.imperative2 ! n ++ prepNP v.c3 np
  } ;
  SlashV2V v vp = (mkSlash v v.c2) ** {
    post = vp.inf
  } ;
  SlashV2S v s = (mkSlash v v.c2) ** {
    post = s.s
  } ;
  SlashV2Q v qs = (mkSlash v v.c2) ** {
    post = qs.s
  } ;
  SlashV2A v ap = (mkSlash v v.c2) ** {
    post = ap.s ! Nom ! GSg Masc
  } ;

  ComplSlash slash np = {
    s = \\t,pol,g,n,p => slash.s ! t ! pol ! g ! n ! p ++ prepNP slash.c np ++ slash.post ;
    inf = slash.inf ++ prepNP slash.c np ++ slash.post ;
    imp = \\pol,n => slash.imp ! pol ! n ++ prepNP slash.c np ++ slash.post
  } ;
  SlashVV vv slash = slash ** {
    s = \\t,pol,g,n,p => finiteVerb vv t pol g n p ++ slash.inf ;
    inf = vv.infinitive ++ slash.inf ;
    imp = \\pol,n => neg pol ++ vv.imperative2 ! n ++ slash.inf
  } ;
  SlashV2VNP v np slash = slash ** {
    s = \\t,pol,g,n,p => finiteVerb v t pol g n p ++ prepNP v.c2 np ++ slash.inf ;
    inf = v.infinitive ++ prepNP v.c2 np ++ slash.inf ;
    imp = \\pol,n => neg pol ++ v.imperative2 ! n ++ prepNP v.c2 np ++ slash.inf
  } ;

  ReflVP slash = {
    s = \\t,pol,g,n,p => slash.s ! t ! pol ! g ! n ! p ++ "себе" ++ slash.post ;
    inf = slash.inf ++ "себе" ++ slash.post ;
    imp = \\pol,n => slash.imp ! pol ! n ++ "себе" ++ slash.post
  } ;
  UseComp comp = {
    s = \\t,pol,g,n,p => copula t pol g n p ++ comp.s ! g ! n ;
    inf = "бути" ++ comp.s ! Masc ! Sg ;
    imp = \\pol,n => neg pol ++ "будь" ++ comp.s ! Masc ! n
  } ;

  AdvVP vp adv = vp ** {
    s = \\t,pol,g,n,p => vp.s ! t ! pol ! g ! n ! p ++ adv.s ;
    inf = vp.inf ++ adv.s ;
    imp = \\pol,n => vp.imp ! pol ! n ++ adv.s
  } ;
  ExtAdvVP vp adv = AdvVP vp adv ;
  AdVVP adv vp = vp ** {
    s = \\t,pol,g,n,p => adv.s ++ vp.s ! t ! pol ! g ! n ! p ;
    inf = adv.s ++ vp.inf ;
    imp = \\pol,n => adv.s ++ vp.imp ! pol ! n
  } ;
  AdvVPSlash slash adv = slash ** {
    s = \\t,pol,g,n,p => slash.s ! t ! pol ! g ! n ! p ++ adv.s ;
    inf = slash.inf ++ adv.s ;
    imp = \\pol,n => slash.imp ! pol ! n ++ adv.s
  } ;
  AdVVPSlash adv slash = slash ** {
    s = \\t,pol,g,n,p => adv.s ++ slash.s ! t ! pol ! g ! n ! p ;
    inf = adv.s ++ slash.inf ;
    imp = \\pol,n => adv.s ++ slash.imp ! pol ! n
  } ;
  VPSlashPrep vp prep = {
    s = vp.s ;
    inf = vp.inf ;
    imp = vp.imp ;
    c = prep ;
    post = []
  } ;

  CompAP ap = {s = \\g,n => ap.s ! Nom ! genNum g n} ;
  CompNP np = {s = \\_,_ => np.s ! Nom} ;
  CompAdv adv = {s = \\_,_ => adv.s} ;
  CompCN cn = {s = \\_,n => cn.s ! Nom ! n} ;
  UseCopula = {
    s = \\t,pol,g,n,p => copula t pol g n p ;
    inf = "бути" ;
    imp = \\pol,n => neg pol ++ "будь"
  } ;
}
