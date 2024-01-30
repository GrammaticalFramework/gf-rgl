--# -path=.:../abstract:../common:../prelude
concrete CatGer of Cat =
  CommonX - [Tense,Temp] ** 
  open ResGer, Prelude in {

  flags optimize=all_subs ;

  lincat

-- Tensed/Untensed

    S  = {s : Order => Str} ;
    QS = {s : QForm => Str} ;
    RS = {s : RelGenNum => Str ; c : Case} ;
    SSlash = {s : Order => Str} ** {c2 : Preposition} ;

-- Sentence

    Cl = {s : Mood => ResGer.Tense => Anteriority => Polarity => Order => Str} ;
    ClSlash = {
      s : Mood => ResGer.Tense => Anteriority => Polarity => Order => Str ; 
      c2 : Preposition
      } ;
    Imp = {s : Polarity => ImpForm => Str} ;

-- Question

    QCl = {s : Mood => ResGer.Tense => Anteriority => Polarity => QForm => Str} ;
    IP = {s : Case => Str ; n : Number} ;
    IComp = {s : Agr => Str ; ext : Str} ; 
    IDet = {s : Gender => Case => Str ; n : Number} ;
    IQuant = {s : GenNum => Case => Str} ;

-- Relative

    RCl = {s : Mood => ResGer.Tense => Anteriority => Polarity => RelGenNum => Str ; c : Case} ;
    RP = {s : RelGenNum => Case => Str ; a : RAgr} ;

-- Verb

    VP = ResGer.VP ;
    VPSlash = ResGer.VPSlash ;
    Comp = {s : Agr => Str ; ext : Str} ; 

-- Adjective  (HL 7/23: we need c : Agr => Str * Str to handle reflexive objects, cf ReflA2)

    AP = {
      s : AForm => Str ; -- (strong) adjective paradigm
      s2 : Case => Str ; -- comparison np, e.g. [s kleineres] (Tier) [s2 als den Hund] HL 1/34
      isPre : Bool ; -- pre-nominal as attribute, e.g. False with sentential complement
      c: Str * Str ; -- np,pp-complement, e.g. (ich bin) [c1 ihm] treu ; stolz [c2 auf dich]
      ext : Str      -- s,inf-complement, (du bist) so klug (gewesen) [ext ihn zu lesen]
      } ;

-- Noun

    CN = {
      s : Adjf => Number => Case => Str ;
      rc : Number => Str ; -- Frage , [rc die ich gestellt habe]
      ext : Str ;          -- Frage , [sc wo sie schläft]
      adv : Str ;          -- Haus    [adv auf dem Hügel]
      g : Gender
      } ;
    NP = ResGer.NP ;
    Pron = {s : NPForm => Str ; a : Agr ; sp : PossForm => Str} ;
    Det = {s,sp : Bool => Gender => Case => Str ; -- True if DefArt is dropped, HL 8/22
           n : Number ; a : Adjf ; isDef, hasDefArt : Bool} ;
    DAP = {s,sp : Gender => Case => Str ; n : Number ; a : Adjf ; isDef,hasDefArt : Bool} ;

    Quant = {
      s : Bool => GenNum => Case => Str ; -- True if leading DefArtSg is dropped
      sp : GenNum => Case => Str ;        --     and contracted with preposition
      a : Adjf ;
      isDefArt : Bool ;
      delCardOne : Bool -- delete following cardinal 1 (IndefArt and no_Quant)
      } ;
    Predet = {
      s : Number => Gender => Case => Str ;
      c : {p : Str ; k : PredetCase} ;
      a : PredetAgr -- if an agr is forced, e.g. jeder von uns ist ...
      } ;

    Num = {s,sp : AForm => Str ; n : Number ; isNum : Bool} ; -- Num,Card.s AForm HL 12/23
    Card = {s : AForm => Str ; n : Number} ; -- inflection mainly for: einer,eine,eines
    Ord = {s : AForm => Str} ;

-- Numeral

    Numeral = {s : CardOrd => Str ; n : Number } ;
    Digits = {s : CardOrd => Str ; n : Number ; isDig, tail1to19 : Bool} ;
    Decimal = {s : CardOrd => Str ; n : Number ; hasDot : Bool} ;

-- Structural

    Conj = {s1,s2 : Str ; n : Number} ;
    Subj = {s : Str} ;
    Prep = Preposition ;

-- Open lexical classes, e.g. Lexicon

    V, VA, VS, VQ = Verb ; -- = {s : VForm => Str} ;
    VV = Verb ** {isAux : Bool} ;
    V2, V2A, V2S, V2Q = Verb ** {c2 : Preposition} ;
    V2V = Verb ** {c2 : Preposition ; isAux : Bool ; objCtrl : Bool} ;
    V3 = Verb ** {c2, c3 : Preposition} ;

    A  = Adjective ; -- = {s : Degree => AForm => Str} ;
    A2 = Adjective ** {c2 : Preposition} ;

    N  = Noun ; -- {s : Number => Case => Str ; co : Str ;
                --  uncap : {s: Number => Case => Str ; co : Str} ; g : Gender}
    N2 = Noun ** {c2 : Preposition} ;
    N3 = Noun ** {c2,c3 : Preposition} ;
    GN = {s : Case => Str; g : Sex} ;
    SN = {s : Sex => Case => Str} ;
    PN = {s : Case => Str; g : Gender; n : Number} ;
    LN = {s : Adjf => Case => Str; hasArt : Bool; g : Gender; n : Number} ;

-- tense with possibility to choose conjunctive forms

    Temp = {s : Str ; t : ResGer.Tense ; a : Anteriority ; m : Mood} ;
    Tense = {s : Str ; t : ResGer.Tense ; m : Mood} ;

  linref
    NP = \np -> np.s ! False ! Nom ++ np.ext ++ np.rc ; -- HL 7/2022 Bool added
    CN = \cn -> cn.s ! Strong ! Sg ! Nom ++ cn.adv ++ cn.ext ++ cn.rc ! Sg ;

    SSlash = \ss -> ss.s ! Main ++ ss.c2.s ! GPl ;
    ClSlash = \cls -> cls.s ! MIndic ! Pres ! Simul ! Pos ! Main ++ cls.c2.s ! GPl ;

    VP = \vp -> useInfVP False vp ;
    VPSlash = \vps -> useInfVP False vps ++ vps.c2.s ! GPl ++ vps.ext;

    AP = \ap -> ap.c.p1 ++ ap.s ! APred ++ ap.c.p2 ++ ap.s2 ! Nom ++ ap.ext ;
    A2 = \a2 -> a2.s ! Posit ! APred ++ a2.c2.s ! GPl ;

    V, VS, VQ, VA = \v -> useInfVP False (predV v) ;
    V2, V2A, V2Q, V2S = \v -> useInfVP False (predV v) ++ v.c2.s ! GPl ;
    V3 = \v -> useInfVP False (predV v) ++ v.c2.s ! GPl ++ v.c3.s ! GPl;

    VV = \v -> useInfVP v.isAux (predVGen v.isAux v) ;
    V2V = \v -> useInfVP v.isAux (predVGen v.isAux v) ++ v.c2.s ! GPl ;

    Conj = \c -> c.s1 ++ c.s2 ;

    Det = \det -> det.s ! False ! Masc ! Nom ;
    Prep = \prep -> case prep.t of {isPrepDefArt => prep.s ! GSg Masc ;
                                    _ => prep.s ! GPl } ;

}
