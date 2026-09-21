concrete ConjunctionHye of Conjunction = CatHye ** open Prelude, ResHye in {
  lincat
    [S] = {s1,s2 : Str} ; [RS] = {s1,s2 : Str} ; [Adv] = {s1,s2 : Str} ;
    [AdV] = {s1,s2 : Str} ; [NP] = {s1,s2 : Str} ; [AP] = {s1,s2 : Str} ;
    [IAdv] = {s1,s2 : Str} ; [CN] = {s1,s2 : Str} ; [DAP] = {s1,s2 : Str} ;
  lin
    BaseS x y = {s1=x.s;s2=y.s}; ConsS x xs = {s1=x.s;s2=xs.s1 ++ "," ++ xs.s2}; ConjS c xs = {s=xs.s1 ++ c.s ++ xs.s2};
    BaseRS x y = {s1=x.s;s2=y.s}; ConsRS x xs = {s1=x.s;s2=xs.s1 ++ "," ++ xs.s2}; ConjRS c xs = {s=xs.s1 ++ c.s ++ xs.s2};
    BaseAdv x y = {s1=x.s;s2=y.s}; ConsAdv x xs = {s1=x.s;s2=xs.s1 ++ "," ++ xs.s2}; ConjAdv c xs = {s=xs.s1 ++ c.s ++ xs.s2};
    BaseAdV x y = {s1=x.s;s2=y.s}; ConsAdV x xs = {s1=x.s;s2=xs.s1 ++ "," ++ xs.s2}; ConjAdV c xs = {s=xs.s1 ++ c.s ++ xs.s2};
    BaseIAdv x y = {s1=x.s;s2=y.s}; ConsIAdv x xs = {s1=x.s;s2=xs.s1 ++ "," ++ xs.s2}; ConjIAdv c xs = {s=xs.s1 ++ c.s ++ xs.s2};
    BaseNP x y = {s1=x.s ! Nom;s2=y.s ! Nom}; ConsNP x xs = {s1=x.s ! Nom;s2=xs.s1 ++ "," ++ xs.s2};
    ConjNP c xs = {s=\\_ => xs.s1 ++ c.s ++ xs.s2; a={n=Pl;p=P3}};
    BaseAP x y = {s1=x.s ! Indef ! Nom ! Sg;s2=y.s ! Indef ! Nom ! Sg};
    ConsAP x xs = {s1=x.s ! Indef ! Nom ! Sg;s2=xs.s1 ++ "," ++ xs.s2};
    ConjAP c xs = {s=\\_,_,_ => xs.s1 ++ c.s ++ xs.s2;isPre=True};
    BaseCN x y = {s1=x.s ! Indef ! Nom ! Sg;s2=y.s ! Indef ! Nom ! Sg};
    ConsCN x xs = {s1=x.s ! Indef ! Nom ! Sg;s2=xs.s1 ++ "," ++ xs.s2};
    ConjCN c xs = {s=\\_,_,_ => xs.s1 ++ c.s ++ xs.s2};
    BaseDAP x y = {s1=x.s;s2=y.s}; ConsDAP x xs = {s1=x.s;s2=xs.s1 ++ "," ++ xs.s2};
    ConjDet c xs = {s=xs.s1 ++ c.s ++ xs.s2;n=Pl;sp=Indef};
}
