concrete PhraseSwa of Phrase = CatSwa ** open Prelude, ResSwa in {
  flags optimize=all_subs ; coding=utf8 ;
  lin
    PhrUtt p u v = {s = p.s ++ u.s ++ v.s} ;
    UttS s = s ;
    UttQS q = {s = q.s ! QDir} ;
    UttImpSg p i = {s = p.s ++ i.s ! p.p ! Sg} ;
    UttImpPl p i = {s = p.s ++ i.s ! p.p ! Pl} ;
    UttImpPol p i = {s = p.s ++ i.s ! p.p ! Pl} ;
    UttIP ip = {s = ip.s ! npNom} ;
    UttIAdv a = a ;
    UttNP np = {s = np.s ! npNom} ;
    UttAdv a = a ;
    UttVP vp = {s = infVP Pos (agre G1 Sg P3) vp} ;
    UttCN cn = {s = cn.s ! Sg ! Nom ++ cn.s2 ! Sg} ;
    UttCard c = {s = c.s ! G1} ;
    UttAP ap = {s = ap.s ! G1 ! Sg} ;
    UttInterj i = i ;
    NoPConj = {s = []} ;
    PConjConj c = {s = c.s2} ;
    NoVoc = {s = []} ;
    VocNP np = {s = SOFT_BIND ++ "," ++ np.s ! npNom} ;
}
  
