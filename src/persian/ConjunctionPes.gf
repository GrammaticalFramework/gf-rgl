
concrete ConjunctionPes of Conjunction =
  CatPes ** open ResPes, Coordination, Prelude in {


  flags optimize=all_subs ;

  lin

    ConjS  = conjunctDistrTable VVForm  ;

    ConjAdv = conjunctDistrSS ;
--    ConjAdv conj advs = conjunctDistrTable Gender conj advs ;

    ConjNP conj ss = ss ** conjunctDistrTable Mod conj ss ** {
      a = conjAgr (agrP3 conj.n) ss.a ;
      animacy = ss.animacy ;
      } ;

    ConjAP conj ss = ss ** conjunctDistrTable Mod conj ss ; -- Adv isn't changed
    ConjRS conj rs = rs ** conjunctDistrTable Agr conj rs ;

---- These fun's are generated from the list cat's.

    BaseS = twoTable VVForm ;
    ConsS = consrTable VVForm comma ;

    BaseAdv = twoSS ;
    ConsAdv = consrSS comma ;

    BaseNP x y = y ** twoTable Mod x y ** {a = conjAgr x.a y.a ; animacy = y.animacy } ; -- check animacy
    BaseRS x y = x ** twoTable Agr x y ;
    ConsNP xs x = xs ** consrTable Mod comma xs x ** {a = conjAgr xs.a x.a ; animacy = xs.animacy } ; --  InaandB xs.animacy x.animacy} ;
    ConsRS xs x = xs ** consrTable Agr comma xs x ;
    BaseAP x y = y ** twoTable Mod x y ;
    ConsAP xs x = xs ** consrTable Mod comma xs x ; -- Table3 Number Gender Case comma xs x ;-- ** {isPre = andB xs.isPre x.isPre} ;

    BaseCN cn1 cn2  = leanCN cn1 ** twoTable2 Number Mod (leanCN cn1) (leanCN cn2) ;
    ConsCN cn cns   = leanCN cn ** consrTable2 Number Mod comma (leanCN cn) cns ;
    ConjCN conj cns = cns ** conjunctDistrTable2 Number Mod conj cns ;

  lincat
    [S] = {s1,s2 : VVForm => Str} ;
    [Adv] = {s1,s2 : Str} ;
    [NP] = {s1,s2 : Mod => Str} ** BaseNP ;
    [CN] = {s1,s2 : Number => Mod => Str ;
            animacy : Animacy ;
            isCmpd : CmpdStatus;
            hasAdj : Bool ;
            compl : Number => Str} ;
    [AP] = {s1,s2 : Mod => Str ; adv : Str ; isPre,afterPrefix : Bool} ;
    [RS] = {s1,s2 : Agr => Str ; rp : RelPron => Str} ;

oper
  leanCN : CN -> CN = \cn -> cn ** {
    compl = \\n => [] ;
    s = \\n,m => cn.s ! n ! m ++ cn.compl ! n ;
    } ;
}
