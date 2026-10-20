concrete ConjunctionTha of Conjunction = CatTha ** open Prelude, Coordination in {

  lin

    ConjS = conjunctDistrSS ;
    ConjAdv = conjunctDistrSS ;
    ConjNP = conjunctDistrSS ;
    ConjAP = conjunctDistrSS ;
    ConjCN conj xs = {
      s = conj.s1 ++ xs.s1 ++ conj.s2 ++ xs.s2 ;
      c = xs.c
      } ;
    ConjRS = conjunctDistrSS ;

-- These fun's are generated from the list cat's.

    BaseS = twoSS ;
    ConsS = consrSS thcomma ;
    BaseAdv = twoSS ;
    ConsAdv = consrSS thcomma ;
    BaseNP = twoSS ;
    ConsNP = consrSS thcomma ;
    BaseAP = twoSS ;
    ConsAP = consrSS thcomma ;
    BaseCN x y = twoSS x y ** {c = y.c} ;
    ConsCN x xs = consrSS thcomma x xs ** {c = xs.c} ;
    BaseRS = twoSS ;
    ConsRS = consrSS thcomma ;

  lincat
    [S] = {s1,s2 : Str} ;
    [Adv] = {s1,s2 : Str} ;
    [NP] = {s1,s2 : Str} ;
    [AP] = {s1,s2 : Str} ;
    [CN] = {s1,s2,c : Str} ;
    [RS] = {s1,s2 : Str} ;

  oper
    thcomma : Str = [] ; ---- should be a space

}
