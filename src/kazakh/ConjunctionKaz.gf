concrete ConjunctionKaz of Conjunction = CatKaz ** open ResKaz in {
  lincat
    [S], [Adv], [AdV], [IAdv], [AP], [RS] = {s1,s2 : Str} ;
    [NP] = {s1,s2 : Case => Str; a : Agr} ;
    [CN] = {s1,s2 : Noun} ;
    [DAP] = {s1,s2 : Str} ;

  lin
    BaseNP x y = {s1=\\c => x.s ! c;s2=\\c => y.s ! c;a={p=P3;n=Pl}} ;
    ConsNP x xs = {s1=\\c => x.s ! c ++ "," ++ xs.s1 ! c;s2=xs.s2;a={p=P3;n=Pl}} ;
    ConjNP c xs = {s=\\k => coord c.s (xs.s1 ! k) (xs.s2 ! k);a=xs.a} ;
    BaseAP x y = {s1=x.s;s2=y.s} ; ConsAP x xs = {s1=x.s ++ "," ++ xs.s1;s2=xs.s2} ;
    ConjAP c xs = {s=coord c.s xs.s1 xs.s2} ;
    BaseAdv x y = {s1=x.s;s2=y.s} ; ConsAdv x xs = {s1=x.s ++ "," ++ xs.s1;s2=xs.s2} ;
    ConjAdv c xs = {s=coord c.s xs.s1 xs.s2} ;
    BaseS x y = {s1=x.s;s2=y.s} ; ConsS x xs = {s1=x.s ++ "," ++ xs.s1;s2=xs.s2} ;
    ConjS c xs = {s=coord c.s xs.s1 xs.s2} ;
    BaseCN x y = {s1=x;s2=y} ; ConsCN x xs = {s1=x;s2=xs.s2} ;
    ConjCN c xs = {s=\\k,n => coord c.s (xs.s1.s ! k ! n) (xs.s2.s ! k ! n);
      poss=\\o,p,n => coord c.s (xs.s1.poss ! o ! p ! n) (xs.s2.poss ! o ! p ! n)} ;
}
