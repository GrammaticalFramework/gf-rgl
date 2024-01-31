incomplete concrete CatRomance of Cat = CommonX - [SC,Pol,MU]
  ** open Prelude, CommonRomance, ResRomance, (R = ParamX) in {

  flags optimize=all_subs ;
    coding=utf8 ;

  lincat

-- exception to CommonX, due to the distinction ne/ne-pas

    Pol = {s : Str ; p : RPolarity} ;

-- Tensed/Untensed

    S  = {s : Mood => Str} ;
    QS = {s : QForm => Str} ;
    RS = {s : Mood => Agr => Str ; c : Case} ;
    SSlash = {
      s  : AAgr => Mood => Str ;
      c2 : Compl
      } ;

    SC = {s : Case => Str} ;  -- de dormir / à dormir

-- Sentence

    Cl    = {s : Direct => RTense => Anteriority => RPolarity => Mood => Str} ;
    ClSlash = {
      s  : AAgr => Direct => RTense => Anteriority => RPolarity => Mood => Str ;
      c2 : Compl
      } ;
    Imp   = {s : RPolarity => ImpForm => Gender => Str} ;

-- Question

    QCl    = {s : RTense => Anteriority => RPolarity => QForm => Str} ;
    IP     = {s : Case => Str ; a : AAgr} ;
    IComp  = {s : AAgr => Str ; cop : CopulaType} ;
    IDet   = {s : Gender => Case => Str ; n : Number} ;
    IQuant = {s : Number => Gender => Case => Str} ;

-- Relative

    RCl  = {
      s : Agr => RTense => Anteriority => RPolarity => Mood => Str ;
      c : Case
      } ;
    RP   = {s : Bool => AAgr => Case => Str ; a : AAgr ; hasAgr : Bool} ;

-- Verb

    VP = ResRomance.VP ;
    VPSlash = ResRomance.VP ** {c2 : Compl} ;
    Comp = {s : Agr => Str ; cop : CopulaType} ;

-- Adjective

    AP = {s : AForm => Str ; isPre : Bool ; copTyp : CopulaType} ;

-- Noun

    CN      = {s : Number => Str ; g : Gender} ;
    Pron    = Pronoun ;
    NP      = NounPhrase ;
    Det,DAP = {
      s : Gender => Case => Str ;
      n : Number ;
      s2 : Gender => Str ;            -- -ci
      sp : Gender => Case => Str ;   -- substantival: mien, mienne
      spn: Case => Str ;
      isNeg : Bool -- negative element, e.g. aucun
      } ;
    Quant = {
      s  : Bool => Number => Gender => Case => Str ;
      s2 : Str ;
      sp : Number => Gender => Case => Str ;
      spn: Case => Str ;  -- neutral Spa: esto, eso, Por: isto, isso
      isNeg : Bool -- negative element, e.g. aucun
      } ;
    Predet  = {
      s : AAgr   => Case => Str ;
      c : Case ; -- c : la plupart de
      a : PAgr   -- if an agr is forced, e.g. chacun de nous
      } ;
    Num     = {s : Gender => Str ; isNum : Bool ; n : Number} ;
    Card    = {s : Gender => Str ; n : Number} ;
    Ord     = {s, s2 : AAgr => Str} ;

-- Numeral

    Numeral = {s : CardOrd => Str ; n : Number} ;
    Digits  = {s : CardOrd => Str ; n : Number ; tail : DTail} ;
    Decimal = {s : CardOrd => Str ; n : Number ; hasDot : Bool} ;

-- Structural

---b    Conj  = {s : Str ; n : Number} ;
---b    DConj = {s1,s2 : Str ; n : Number} ;
    Conj = {s1,s2 : Str ; n : Number} ;
    Subj = {s : Str ; m : Mood} ;
    Prep = {s : Str ; c : Case ; isDir : Bool} ;

-- Open lexical classes, e.g. Lexicon

    V, VQ, VA = Verb ;
    V2, VV, V2S, V2Q = Verb ** {c2 : Compl} ;
    V3, V2A, V2V = Verb ** {c2,c3 : Compl} ;
    VS = Verb ** {m : RPolarity => Mood} ;

    A  = {s : AForm => Str ; compar : ComparAgr => Str ; isPre : Bool ; copTyp : CopulaType ; isDeg : Bool} ;
    A2 = {s : AForm => Str ; compar : ComparAgr => Str ; c2 : Compl ; copTyp : CopulaType ; isDeg : Bool} ;

    N  = Noun  ** {relType : NRelType};
    N2 = Noun  ** {relType : NRelType; c2 : Compl} ;
    N3 = Noun  ** {relType : NRelType; c2,c3 : Compl} ;
    GN, PN = {s : Str ; g : Gender} ;
    SN = {s : Gender => Str ; pl : Str} ;
    LN = {s  : Str;
          onPrep : Bool;
          art : HasArt;
          g : Gender;
          num : Number;
         } ;

-- tense augmented with passé simple
  lincat
    Temp  = {s : Str ; t : RTense ; a : Anteriority} ;
    Tense = {s : Str ; t : RTense} ;

  linref
    SSlash = \ss -> ss.s ! aagr Masc Sg ! Indic ++ ss.c2.s ;
    ClSlash = \cls -> cls.s ! aagr Masc Sg ! DDir ! RPres ! Simul ! RPos ! Indic ++ cls.c2.s ;

    VP = \vp -> infVP vp RPos (agrP3 Masc Sg) ;
    VPSlash = \vps -> infVP vps RPos (agrP3 Masc Sg) ++ vps.c2.s ;

    V, VS, VQ, VA = \v -> infVP (predV v) RPos (agrP3 Masc Sg);
    V2, V2A, V2Q, V2S = \v -> infVP (predV v) RPos (agrP3 Masc Sg) ++ v.c2.s ;
    V3 = \v -> infVP (predV v) RPos (agrP3 Masc Sg) ++ v.c2.s ++ v.c3.s ;
    VV = \v -> infVP (predV v) RPos (agrP3 Masc Sg) ;
    V2V = \v -> infVP (predV v) RPos (agrP3 Masc Sg) ;

    NP = \np -> (np.s ! Nom).comp ;
    Conj = \c -> c.s2 ;

    A = \a -> a.s ! genNum2Aform Masc Sg ;
    A2 = \a -> a.s ! genNum2Aform Masc Sg ++ a.c2.s ;

    Det = \d -> d.s ! Masc ! Nom ++ d.s2 ! Masc ;
    Ord = \o -> o.s ! aagr Masc Sg ++ o.s2 ! aagr Masc Sg ;

    N = \n -> n.s ! Sg ;
    N2 = \n -> n.s ! Sg ++ n.c2.s ;
    N3 = \n -> n.s ! Sg ++ n.c2.s ++ n.c3.s ;

  lincat MU = {s : Str ; isPre : Bool ; hasArt : Bool} ;


}
