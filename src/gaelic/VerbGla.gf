concrete VerbGla of Verb = CatGla ** open ResGla, AdverbGla, Prelude in {


lin

-----
-- VP
  -- : V -> VP
  -- NB. assumes that lincat V = lincat VP
  -- This will most likely change when you start working with VPs
  UseV v = v ;

  --  : V2 -> VP ;
  PassV2 v2 = appendVP v2 ("air" ++ v2.participle) ;

  -- : VPSlash -> VP ;
  ReflVP vps = vps ;

  -- : VV  -> VP -> VP ;
  ComplVV vv vp = appendVP vv ("a" ++ vp.noun) ;

  -- : VS  -> S  -> VP ;
  ComplVS vs sent = appendVP vs ("gun" ++ sent.s) ;

  -- : VQ -> QS -> VP ;
  ComplVQ vq qs = appendVP vq qs.s ;

  -- : VA -> AP -> VP ;
  ComplVA va ap = appendVP va (ap.s ! ASg NOM Masc) ;

  -- : Comp -> VP ;
  UseComp comp = appendVP (copulaV "bi") comp.s ;
--------
-- Slash
  -- : V2 -> VPSlash
  SlashV2a v2 = v2 ;

  -- : V3 -> NP -> VPSlash ; -- give it (to her)
  Slash2V3 v3 dobj = appendSlash (v3 ** {c2 = v3.c3}) (linNP dobj) ;

  -- : V3 -> NP -> VPSlash ; -- give (it) to her
  Slash3V3 v3 iobj = appendSlash v3 (prepNP v3.c3 iobj) ;

  SlashV2A v2 adj = appendSlash v2 (adj.s ! ASg NOM Masc) ;

  -- : V2S -> S  -> VPSlash ;  -- answer (to him) that it is good
  SlashV2S v2s sent = appendSlash v2s ("gun" ++ sent.s) ;

  -- : V2V -> VP -> VPSlash ;  -- beg (her) to go
  SlashV2V v2v vp = appendSlash v2v ("a" ++ vp.noun) ;

  -- : V2Q -> QS -> VPSlash ;  -- ask (him) who came
  SlashV2Q v2q qs = appendSlash v2q qs.s ;

  -- : V2A -> AP -> VPSlash ;  -- paint (it) red
  -- : VPSlash -> NP -> VP
  -- Often VPSlash has a field called c2, which is used to pick right form of np complement
  ComplSlash vps np = appendVP vps (prepNP vps.c2 np) ;

  -- : VV  -> VPSlash -> VPSlash ;
  SlashVV vv vps = appendSlash (vv ** {c2 = vps.c2}) ("a" ++ vps.noun) ;

  -- : V2V -> NP -> VPSlash -> VPSlash ; -- beg me to buy
  SlashV2VNP v2v np vps = appendSlash v2v (linNP np ++ "a" ++ vps.s) ;

  -- : VP -> Adv -> VP ;  -- sleep here
  AdvVP vp adv = appendVP vp adv.s ;

  -- : AdV -> VP -> VP ;  -- always sleep
  AdVVP adv vp = prependVP adv.s vp ;

  -- : VPSlash -> Adv -> VPSlash ;  -- use (it) here
  AdvVPSlash vps adv = appendSlash vps adv.s ;

  -- : VP -> Adv -> VP ;  -- sleep , even though ...
  ExtAdvVP vp adv = appendVP vp ("," ++ adv.s) ;

  -- : AdV -> VPSlash -> VPSlash ;  -- always use (it)
  AdVVPSlash adv vps = prependSlash adv.s vps ;

  -- : VP -> Prep -> VPSlash ;  -- live in (it)
  VPSlashPrep vp prep = vp ** {c2 = prep} ;


--2 Complements to copula

-- Adjectival phrases, noun phrases, and adverbs can be used.

  -- : AP  -> Comp ;
  CompAP ap = {s = ap.s ! ASg NOM Masc} ;

  -- : CN  -> Comp ;
  CompCN cn = {s = cn.s ! NOM ! Indef ! Sg} ;

  --  NP  -> Comp ;
  CompNP np = {s = linNP np} ;

  -- : Adv  -> Comp ;
  CompAdv adv = adv ;

  -- : VP -- Copula alone;
  UseCopula = copulaV "bi" ;

oper
  copulaV : Str -> LinV = \_ -> {
    s = "bi" ;
    conditional = table {Sg => "bhiodh" ; Pl => "bhiodh"} ;
    imperative = table {
      P1 => table {Sg => "bitheam" ; Pl => "bitheamaid"} ;
      P2 => table {Sg => "bi" ; Pl => "bithibh"} ;
      P3 => table {Sg => "bitheadh" ; Pl => "bitheadh"}
      } ;
    future = table {Indep => "bidh" ; Dep => "bi"} ;
    past = table {Indep => "bha" ; Dep => "robh"} ;
    noun = "bhith" ; participle = "air a bhith" ;
    copular = True ; complement = []
    } ;

}
