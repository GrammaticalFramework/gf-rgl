concrete ConjunctionEst of Conjunction =
  CatEst ** open ResEst, Coordination, Prelude in {

  flags optimize=all_subs ; coding=utf8;

  lin

    ConjS = conjunctDistrSS ;

    ConjAdv = conjunctDistrSS ;

    ConjCN conj ss = conjunctDistrTable NForm conj ss ** ss ;

    ConjNP conj ss = conjunctDistrTable NPForm conj ss ** {
      a = conjAgr (Ag conj.n P3) ss.a ; -- P3 is the maximum
      isPron = False ;
      postmod = ss.postmod
      } ;

    ConjAP conj ss = conjunctDistrTableAdj conj ss ;

    ConjRS conj ss = conjunctDistrTable Agr conj ss ** {
      c = ss.c
      } ;

-- These fun's are generated from the list cat's.

    BaseS = twoSS ;
    ConsS = consrSS comma ;
    BaseAdv = twoSS ;
    ConsAdv = consrSS comma ;
    BaseCN x y = twoTable NForm (mergeCN x) y ** {postmod = y.postmod} ;
    ConsCN x xs = consrTable NForm comma (mergeCN x) xs ** xs ;
    BaseNP x y = twoTable NPForm (mergeNP x) y ** {a = conjAgr x.a y.a ; postmod = y.postmod} ;
    ConsNP x xs = consrTable NPForm comma (mergeNP x) xs ** {a = conjAgr xs.a x.a ; postmod = xs.postmod} ;
    BaseAP x y = twoTableAdj x y ;
    ConsAP x xs = consrTableAdj comma x xs ;
    BaseRS x y = twoTable Agr x y ** {c = y.c} ;
    ConsRS x xs = consrTable Agr comma x xs ** {c = xs.c} ;

  lincat
    [S] = {s1,s2 : Str} ;
    [Adv] = {s1,s2 : Str} ;
    [CN] = {s1,s2 : NForm => Str ; postmod : Str} ;
    [NP] = {s1,s2 : NPForm => Str ; a : Agr ; postmod : Str} ;
    [AP] = {s1,s2 : {s : Bool => NForm => Str ; infl : Infl }} ;
    [RS] = {s1,s2 : Agr => Str ; c : NPForm} ;

  oper
    --Modified from prelude/Coordination.gf generic functions
    twoTableAdj : (_,_ : AP) -> [AP] = \x,y ->
    lin ListAP {
      s1 = x ;
      s2 = y
    } ;

    consrTableAdj : Str -> APhrase -> [AP] -> [AP] = \c,x,xs ->
      let
        ap1 = xs.s1 ;
        ap2 = xs.s2
      in
       lin ListAP {s1 =
             {s = \\isMod,nf =>
                case isMod of {
                  True => case <ap1.infl, ap2.infl> of {
                            <(Participle|Invariable),(Participle|Invariable)> =>
                                 ap1.s ! isMod ! (NCase Sg Nom) ++ c ++ ap2.s ! isMod ! (NCase Sg Nom) ; --valmis ja täis kassid
                            <(Participle|Invariable),Regular>  =>
                                 ap1.s ! isMod ! (NCase Sg Nom) ++ c++ ap2.s ! isMod ! nf ;    --valmis ja suured kassid
                            <Regular,(Participle|Invariable)>  =>
                                 ap1.s ! isMod ! nf ++ c ++ ap2.s ! isMod ! (NCase Sg Nom) ;   --suured ja valmis kassid
                            _ => ap1.s ! isMod ! nf ++ c ++ ap2.s ! isMod ! nf --suured ja mustad kassid
                            } ;
                  False => ap1.s ! isMod ! nf ++ c ++ ap2.s ! isMod ! nf --kassid on valmid ja suured
                  } ;
              infl = Regular } ;
       s2 = x ;
      } ;


    conjunctDistrTableAdj : ConjunctionDistr -> [AP] -> AP =  \or,xs ->
      let
        ap1 = xs.s1 ;
        ap2 = xs.s2 ;
      in
      lin AP {s = \\isMod,nf =>
                case isMod of {
                  True => case <ap1.infl, ap2.infl> of {
                            <(Participle|Invariable),(Participle|Invariable)> =>
                                 or.s1 ++ ap1.s ! isMod ! (NCase Sg Nom) ++
                                 or.s2 ++ ap2.s ! isMod ! (NCase Sg Nom) ;
                            <(Participle|Invariable),Regular> =>
                                 or.s1 ++ ap1.s ! isMod ! (NCase Sg Nom) ++
                                 or.s2 ++ ap2.s ! isMod ! nf ;
                            <Regular,(Participle|Invariable)> =>
                                 or.s1 ++ ap1.s ! isMod ! nf ++
                                 or.s2 ++ ap2.s ! isMod ! (NCase Sg Nom) ;
                            _ => or.s1 ++ ap1.s ! isMod ! nf ++ or.s2 ++ ap2.s ! isMod ! nf
                               } ;
                    False => or.s1 ++ ap1.s ! isMod ! nf ++ or.s2 ++ ap2.s ! isMod ! nf
                    } ;
              infl = Regular
             } ;

    -- for CN and NP with discontinuous fields, put all stuff in s field
    mergeNP : NPhrase -> NPhrase = \np -> np ** {s = \\c => linNP c np} ;
    mergeCN : CNoun -> CNoun = \cn -> cn ** {s = \\nf => linCN nf cn} ;
}
