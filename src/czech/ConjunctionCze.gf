concrete ConjunctionCze of Conjunction = CatCze **
  open ResCze, Coordination, Prelude in {

  lincat
    [Adv] = {s1,s2 : Str} ;
    [AP]  = {s1,s2 : Gender => Number => Case => Str ; pred1,pred2 : Agr => Str ; isPost : Bool} ;
    [NP]  = {s1,s2,prep1,prep2 : Case => Str ; a : Agr} ;
    [S] = {s1 : Sentence ; s2 : Str} ;
    [RS] = {s1,s2 : Agr => Str} ;

  lin
    BaseAdv = twoSS ;
    ConsAdv = consrSS comma ;

    BaseAP x y = twoTable3 Gender Number Case x y
                  ** {pred1 = x.pred ; pred2 = y.pred ; isPost = orB x.isPost y.isPost} ;
    ConsAP x xs = consrTable3 Gender Number Case comma x xs
                  ** {pred1 = \\a => x.pred ! a ++ comma ++ xs.pred1 ! a ; pred2 = xs.pred2 ; isPost = orB x.isPost xs.isPost} ;

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

    BaseS x y = {s1 = x ; s2 = y.s} ;
    ConsS x xs = {s1 = appendSentence x (comma ++ xs.s1.s) ; s2 = xs.s2} ;

    BaseRS = twoTable Agr ;
    ConsRS = consrTable Agr comma ;

    ConjAdv = conjunctDistrSS ;
    
    ConjAP conj xs = conjunctDistrTable3 Gender Number Case conj xs
                       ** {pred = \\a => conj.s1 ++ xs.pred1 ! a ++ conj.s2 ++ xs.pred2 ! a ; isPost = xs.isPost} ;
    
    ConjNP conj xs = {
      s,clit = \\c => conj.s1 ++ xs.s1 ! c ++ conj.s2 ++ xs.s2 ! c ;
      prep   = \\c => conj.s1 ++ xs.prep1 ! c ++ conj.s2 ++ xs.prep2 ! c ;
      a = xs.a ; ---- dep. on conj as well
      hasClit = False ; isDrop = False ;
      } ;

    ConjS conj xs = prefixSentence conj.s1 (appendSentence xs.s1 (conj.s2 ++ xs.s2)) ;
    ConjRS = conjunctDistrTable Agr ;

}
