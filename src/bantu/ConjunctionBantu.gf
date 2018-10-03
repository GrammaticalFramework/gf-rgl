incomplete concrete ConjunctionBantu of Conjunction = 
  CatBantu ** open CommonBantu, ResBantu, Coordination, Prelude in {

  flags optimize=all_subs ;

  lin
   
    ConjS = conjunctDistrSS ;

    ConjAdv = conjunctDistrSS ;
    ConjAdV = conjunctDistrSS ;

    ConjNP conj ss = conjunctDistrTable NPCase conj ss ** {
     a = Ag  (agrFeatures ss.a).g  (conjNumber (agrFeatures ss.a).n conj.n) (agrFeatures ss.a).p ; -- a = conjAgr (agrP3 conj.n) ss.a
      } ;

    ConjAP conj ss = conjunctDistrTable2 Gender  Number conj ss;-- ** { isPre = ss.isPre    } ;

    ConjRS conj ss = conjunctDistrTable Agr conj ss ** {
      c = ss.c
      } ;

    ConjIAdv = conjunctDistrSS ;   

  ConjCN conj cn = {
      s = \\num,c => conj.s1 ++ cn.n1.s ! num!c ++ conj.s2 ++ cn.n2.s ! num!c ;
      g = conjGender cn.n1.g cn.n2.g ;
      s2 = \\num => [];
           } ; 
     ConjDet c xs = { s = table {
       Sub  => [] ;
       Obj  g => xs.s1!Sub  ++ c.s2 ++ xs.s2 !Obj g} ;
       n = xs.n};
  
 -- These fun's are generated from the list cat's.

    BaseS = twoSS ;
    ConsS = consrSS comma ;
    BaseAdv = twoSS ;
    ConsAdv = consrSS comma ;
    BaseAdV = twoSS ;
    ConsAdV = consrSS comma ;
    BaseNP x y = twoTable NPCase x y ** {a = conjAgr Sg x.a y.a} ;
   ConsNP xs x = consrTable NPCase comma xs x ** {a = conjAgr Sg xs.a x.a} ;

    
    BaseAP x y = twoTable2 Gender Number x y ;
    ConsAP xs x = consrTable2  Gender Number comma xs x ;
    BaseRS x y = twoTable Agr x y ** {c = y.c} ;
    ConsRS xs x = consrTable Agr comma xs x ** {c = xs.c} ;
    BaseIAdv = twoSS ;
    ConsIAdv = consrSS comma ;
   BaseCN x y = { n1 = x ; n2 = y } ;
    ConsCN xs x = {
      n1 = {
       s = \\num,c => x.n1.s ! num!c ++ comma ++ x.n2.s ! num !c ;
       g = x.n2.g ;
        s2 = \\num => [] ;      
                } ;
      n2 = xs ;
      } ; 
   BaseDAP x y = twoTable DetForm x y ** {n = y.n} ; 
   ConsDAP x xs = consrTable DetForm comma x xs ** {n = xs.n} ;
   --BaseDet x y = twoTable DetForm x y ** {n = y.n} ;   
   --ConsDet xs x = consrTable  DetForm comma xs x ** {n = xs.n} ;
  lincat
    [S] = {s1,s2 : Str} ;
    [Adv] = {s1,s2 : Str} ;
    [AdV] = {s1,s2 : Str} ;
    [IAdv] = {s1,s2 : Str} ;
    [NP] = {s1,s2 : NPCase => Str ; a : Agr} ;
    [AP] = {s1,s2 : Gender => Number =>  Str} ; --isPre : Bool} ;
    [RS] = {s1,s2 : Agr => Str ; c : NPCase} ;
    [CN] = {n1,n2 : CNoun } ;
   [DAP] = {s1,s2 : DetForm =>  Str ; n : Number} ;

}
