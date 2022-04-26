concrete ConjunctionChi of Conjunction = CatChi ** open ResChi, Prelude, Coordination in {

  lin

    ConjS c ss =
      let conj = c.s ! CSent
       in case c.conjType of {
        Jiu    => {preJiu = conj.s1 ++ ss.s1 ++ comma ++ ss.preJiu ;
                   postJiu = conj.s2 ++ ss.postJiu} ;
        NotJiu => {preJiu = conj.s1 ++ ss.s1 ++ conj.s2 ++ ss.preJiu ;
                   postJiu = ss.postJiu}
        } ;
    ConjAdv c as = conjunctDistrSS (c.s ! CSent) as ** {advType = as.advType ; hasDe = as.hasDe} ; ---- ??
    ConjNP c = conjunctDistrSS (c.s ! CPhr CNPhrase) ;
    ConjAP c as = conjunctDistrTable AdjPlace (c.s ! CPhr CAPhrase) as ** {monoSyl = notB as.monoSyl ; hasAdA = True} ; ---- add de iff as doesn't
    ConjRS c = conjunctDistrSS (c.s ! CSent) ;
    ConjCN c ns = conjunctDistrSS (c.s ! CPhr CNPhrase) ns ** {c = ns.c} ;

-- These fun's are generated from the list cat's.

    BaseS s t = t ** {
      s1 = linS s
    } ;

    ConsS s ss = -- here we do the same thing actually, the crucial split has happened in BaseS
     ss ** {s1 = linS s ++ comma ++ ss.s1};


    BaseAdv x y = twoSS x y ** {advType = x.advType ; hasDe = y.hasDe} ; ---- ??
    ConsAdv x xs = consrSS duncomma x xs ** {advType = x.advType ; hasDe = xs.hasDe} ; ---- ??
    BaseNP = twoSS ;
    ConsNP = consrSS duncomma ;
    BaseAP x y = twoTable AdjPlace x y ** {monoSyl = y.monoSyl} ;
    ConsAP x xs = consrTable AdjPlace duncomma x xs ** {monoSyl = xs.monoSyl} ;
    BaseRS = twoSS ;
    ConsRS = consrSS duncomma ;
    BaseCN x y = twoSS x y ** {c = x.c} ;  --- classified comes from first part ; should it rather be ge?
    ConsCN x xs = consrSS duncomma x xs ** {c = x.c} ;

  lincat
    --[S] = ConjType => {s1,s2 : Str} ;
    [S] = {s1,preJiu,postJiu : Str} ;
    [Adv] = {s1,s2 : Str ; advType : AdvType ; hasDe : Bool} ;
    [NP] = {s1,s2 : Str} ;
    [AP] = {s1,s2 : AdjPlace => Str ; monoSyl : Bool} ;
    [RS] = {s1,s2 : Str} ;
    [CN] = {s1,s2 : Str ; c : Str} ;


}
