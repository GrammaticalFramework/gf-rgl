concrete ConjunctionCze of Conjunction = CatCze **
  open ResCze, Coordination, Prelude in {

  lincat
    [Adv] = {s1,s2 : Str} ;
    [AP]  = {s1,s2 : Gender => Number => Case => Str ; isPost : Bool} ;
    [NP]  = {s1,s2,prep1,prep2 : Case => Str ; a : Agr} ;
    [S] = {s1,s2 : Str} ;
    [RS] = {s1,s2 : Agr => Str} ;

  lin
    BaseAdv = twoSS ;
    ConsAdv = consrSS comma ;

    BaseAP x y = twoTable3 Gender Number Case x y
                  ** {isPost = orB x.isPost y.isPost} ; ---- should be so in Pol too
    ConsAP x xs = consrTable3 Gender Number Case comma x xs
                  ** {isPost = orB x.isPost xs.isPost} ;

    BaseNP x y = {
      s1 = x.s ;
      s2 = y.s ;
      prep1 = x.prep ;
      prep2 = y.prep ;
      a = y.a
      } ; -- clitics disappear ---- Agr TODO
    ConsNP x xs = {
      s1 = \\c => x.s ! c ++ comma ++ xs.s1 ! c ;
      s2 = xs.s2 ; 
      prep1 = \\c => x.prep ! c ++ comma ++ xs.prep1 ! c ;
      prep2 = xs.prep2 ;
      a = xs.a ----
      } ; 

    BaseS = twoSS ;
    ConsS = consrSS comma ;

    BaseRS = twoTable Agr ;
    ConsRS = consrTable Agr comma ;

    ConjAdv = conjunctDistrSS ;
    
    ConjAP conj xs = conjunctDistrTable3 Gender Number Case conj xs
                       ** {isPost = xs.isPost} ;
    
    ConjNP conj xs = {
      s,clit = \\c => conj.s1 ++ xs.s1 ! c ++ conj.s2 ++ xs.s2 ! c ;
      prep   = \\c => conj.s1 ++ xs.prep1 ! c ++ conj.s2 ++ xs.prep2 ! c ;
      a = xs.a ; ---- dep. on conj as well
      hasClit = False ;
      } ;

    ConjS = conjunctDistrSS ;
    ConjRS = conjunctDistrTable Agr ;

}
