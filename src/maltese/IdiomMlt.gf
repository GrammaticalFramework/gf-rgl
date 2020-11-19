-- IdiomMlt.gf: idiomatic expressions
--
-- Maltese GF Resource Grammar
-- John J. Camilleri 2011 -- 2013
-- Licensed under LGPL

concrete IdiomMlt of Idiom = CatMlt ** open Prelude, ResMlt, Maybe in {

  flags
    coding=utf8;

  lin
    -- VP -> Cl
    -- it is hot
    ImpersCl vp = mkClause "" (agrP3 Sg Masc) vp ;

    -- VP -> Cl
    -- one sleeps
    -- GenericCl vp = mkClause "wieħed" (agrP3 Sg Masc) vp ;
    GenericCl vp = mkClause "dak li jkun" (agrP3 Sg Masc) vp ;

    -- NP -> RS -> Cl
    -- it is I who did it
    CleftNP np rs = {
      s = \\t,a,p,o => case p of {
        Pos => np.s ! NPNom ++ kont ++ "li" ++ rs.s ! np.a ; -- jiena kont li qrajt il-ktieb
        Neg => "mhux" ++ np.s ! NPNom ++ kont ++ "li" ++ rs.s ! np.a -- mhux jiena kont li qrajt il-ktieb
        }
      where {
        kont : Str = case t of {
          Pres => copula_kien.s ! VImpf (toVAgr np.a) ! Pos ;
          Past => copula_kien.s ! VPerf (toVAgr np.a) ! Pos ;
          Fut  => "ser" ++ copula_kien.s ! VImpf (toVAgr np.a) ! Pos ;
          Cond => "kieku" ++ copula_kien.s ! VPerf (toVAgr np.a) ! Pos
          } ;
        }
      } ;

    -- Adv -> S -> Cl
    -- it is here she slept
    CleftAdv adv s = {
      s = \\t,a,p,o => adv.s ++ s.s ;
      } ;

    -- NP -> Cl
    -- there is a house
    ExistNP np = {
      s = \\t,a,p,o => auxHemm.s ! t ! p ++ np.s ! NPAcc ;
      } ;

    -- IP -> QCl
    -- which houses are there
    ExistIP ip = {
      s = \\t,a,p,o => ip.s ++ auxHemm.s ! t ! p ;
      } ;

    -- NP -> Adv -> Cl
    -- there is a house in Paris
    ExistNPAdv np adv = {
      s = \\t,a,p,o => auxHemm.s ! t ! p ++ np.s ! NPAcc ++ adv.s ;
      } ;

    -- IP -> Adv -> QCl
    -- which houses are there in Paris
    ExistIPAdv ip adv = {
      s = \\t,a,p,o => ip.s ++ auxHemm.s ! t ! p ++ adv.s ;
      } ;

    -- VP -> VP
    -- be sleeping
    ProgrVP vp = {
      v = CopulaVP.v ;
      s2 = \\agr => case exists Participle vp.v.presPart of {
        True  => (fromJust Participle vp.v.presPart) ! (toGenNum agr) ;
        False => (vp.v.s ! VImpf (toVAgr agr)).s1
        } ;
      dir = NullAgr ;
      ind = NullAgr ;
      } ;

    -- VP -> Utt
    -- let's go
    ImpPl1 vp = {
      s = "ejja" ++ infVP vp Simul Pos (mkAgr Pl P1 Masc)
      } ;

    -- NP -> VP -> Utt
    -- let John walk
    ImpP3 np vp = {
        s = halli ++ np.s ! NPAcc ++ infVP vp Simul Pos np.a
      } where {
        halli : Str = case np.a.n of {
          Sg => "ħalli" ;
          Pl => "ħallu"
          }
      } ;

    -- VP -> VP
    -- is at home himself
    SelfAdvVP vp = insertObj (\\agr => reflPron ! (toVAgr agr)) vp ;

    -- VP -> VP
    -- is himself at home
    SelfAdVVP vp = insertObjPre (\\agr => reflPron ! (toVAgr agr)) vp ;

    -- NP -> NP
    -- the president himself (is at home)
    SelfNP np = {
      s = \\c => np.s ! c ++ "stess" ;
      a = np.a ;
      isPron = np.isPron ;
      isDefn = np.isDefn ;
      } ;

}
