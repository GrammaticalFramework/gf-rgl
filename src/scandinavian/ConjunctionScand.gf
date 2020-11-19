incomplete concrete ConjunctionScand of Conjunction = 
  CatScand ** open CommonScand, ResScand, Coordination, Prelude in {

  flags optimize=all_subs ;

  lin

    ConjS conj ss = {
      s = \\p => let o : Order = case conj.isDiscont of {True => Inv ; _ => p}
                 in conj.s1 ++ ss.s1 ! o ++ conj.s2 ++ ss.s2 ! p ;      
      } ;

    ConjAdv conj ss = conjunctDistrSS conj ss ;

    ConjNP conj ss = conjunctDistrTable NPForm conj ss ** {
      a = {g = ss.a.g ; n = conjNumber conj.n ss.a.n ; p = ss.a.p} ;
      isPron = False
      } ;

    ConjAP conj ss = conjunctDistrTable AFormPos conj ss ** {
      isPre = ss.isPre
      } ;

    ConjRS conj ss = conjunctDistrTable2 Agr RCase conj ss ** {
      c = ss.c
      } ;

    ConjIAdv = conjunctDistrSS ;   

    ConjCN co ns = conjunctDistrTable3 Number DetSpecies Case co ns ** 
      {g = neutrum ; isMod = False} ; ----

    ConjDet conj ss = let css = (conjunctDistrTable2 Bool NGender conj ss).s in {
      s,sp = css ;
      n = ss.n ; 
      det = ss.det ;
      } ;


-- These fun's are generated from the list cat's.

    BaseS x y = { -- twoTable Order ;
      s1 = x.s ;
      s2 = table {Inv => y.s ! Main ; o => y.s ! o}
      } ;
    ConsS x xs = { -- consrTable Order comma ;
      s1 = \\o => x.s ! o ++ comma ++ xs.s1 ! case o of {Inv => Main ; _ => o} ;
      s2 = xs.s2
      } ;
    BaseAdv = twoSS ;
    ConsAdv = consrSS comma ;
    BaseNP x y = twoTable NPForm x y ** {a = conjAgr x.a y.a} ;
    ConsNP xs x = consrTable NPForm comma xs x ** {a = conjAgr xs.a x.a} ;
    BaseAP x y = twoTable AFormPos x y ** {isPre = andB x.isPre y.isPre} ;
    ConsAP xs x = consrTable AFormPos comma xs x ** {isPre = andB xs.isPre x.isPre} ;
    BaseRS x y = twoTable2 Agr RCase x y ** {c = y.c} ;
    ConsRS xs x = consrTable2 Agr RCase comma xs x ** {c = xs.c} ;
    BaseIAdv = twoSS ;
    ConsIAdv = consrSS comma ;
    BaseCN = twoTable3 Number DetSpecies Case ;
    ConsCN = consrTable3 Number DetSpecies Case comma ;

    BaseDAP x y = twoTable2 Bool NGender x y ** {n = y.n ; det = y.det} ;
    ConsDAP x xs = consrTable2 Bool NGender comma x xs ** {n = xs.n ; det = xs.det} ;

  lincat
    [S] = {s1,s2 : Order => Str} ;
    [Adv] = {s1,s2 : Str} ;
    [IAdv] = {s1,s2 : Str} ;
    [NP] = {s1,s2 : NPForm => Str ; a : Agr} ;
    [AP] = {s1,s2 : AFormPos => Str ; isPre : Bool} ;
    [RS] = {s1,s2 : Agr => RCase => Str ; c : NPForm} ;
    [CN] = {s1,s2 : Number => DetSpecies => Case => Str} ; --- g : NGender ; isMod : Bool} ;
    [DAP] = {s1,s2 : Bool => NGender => Str ; n : Number ; det : DetSpecies} ;


}
