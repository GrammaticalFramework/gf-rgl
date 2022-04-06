concrete ConjunctionFin of Conjunction =
  CatFin ** open ResFin, Coordination, Prelude, (N=NounFin) in {

  flags optimize=all_subs ;

  lin

    ConjS = conjunctDistrSS ;

    ConjAdv, ConjAdV = conjunctDistrSS ;

    ConjIAdv = conjunctDistrSS ;

    ConjNP conj ss = conjunctDistrTable NPForm conj ss ** {
      a = conjAgr (Ag conj.n P3) ss.a ; -- P3 is the maximum
      isPron = False ; isNeg = ss.isNeg
      } ;

    ConjAP conj ss = conjunctDistrTable2 Bool NForm conj ss ** { hasPrefix = False ; p = [] } ;

    ConjRS conj ss = conjunctDistrTable Agr conj ss ** {
      c = ss.c
      } ;

    ConjCN conj ss =
      let s = (conjunctDistrTable NForm conj ss).s
      in {s = s ; h = Back ; postmod = \\_ => []} ; ---- harmony?

-- These fun's are generated from the list cat's.

    BaseS = twoSS ;
    ConsS = consrSS comma ;
    BaseAdv, BaseAdV = twoSS ;
    ConsAdv, ConsAdV = consrSS comma ;
    BaseIAdv = twoSS ;
    ConsIAdv = consrSS comma ;
    BaseNP x y = twoTable NPForm x y ** {a = conjAgr x.a y.a ; isNeg = orB x.isNeg y.isNeg} ;
    ConsNP xs x = consrTable NPForm comma xs x ** {a = conjAgr xs.a x.a ; isNeg = orB xs.isNeg x.isNeg} ;
    BaseAP x y = twoTable2 Bool NForm x y ;
    ConsAP xs x = consrTable2 Bool NForm comma xs x ;
    BaseRS x y = twoTable Agr x y ** {c = y.c} ;
    ConsRS xs x = consrTable Agr comma xs x ** {c = xs.c} ;
    BaseCN x y = twoTable NForm (mergeCN x) (mergeCN y) ; -- put postmod in s field
    ConsCN x xs = consrTable NForm comma (mergeCN x) xs;
  oper
    -- RS, SC and Adv are in separate fields, to prevent "lasi viiniänsa" for "lasinsa viiniä".
    -- But for coordination, we just give up and attach the postmod.
    -- Really, if you want to have "lasinsa viiniä ja kuppinsa teetä", just use ConjNP instead.
    mergeCN : LinCN -> LinCN = \cn -> cn ** {
      s = \\nf => cn.s ! nf ++ cn.postmod ! N.numN nf } ;

  lincat
    [S] = {s1,s2 : Str} ;
    [Adv],[AdV] = {s1,s2 : Str} ;
    [IAdv] = {s1,s2 : Str} ;
    [NP] = {s1,s2 : NPForm => Str ; a : Agr ; isNeg : Bool} ;
    [AP] = {s1,s2 : Bool => NForm => Str} ;
    [RS] = {s1,s2 : Agr => Str ; c : NPForm} ;
    [CN] = {s1,s2 : NForm => Str} ;

}
