concrete CatTur of Cat = CommonX - [CAdv,AdN] ** open ResTur, HarmonyTur, Prelude, Predef in {

  flags optimize=all_subs ;

  lincat

    -- Tensed/Untensed
    S  = {s : Str} ;
    RS = {s : Agr => Str} ;

    -- Sentence
    Cl = {s : Tense => Anteriority => Polarity => Str} ;
    Imp = {s : Polarity => Number => Str} ;

    -- Noun
    CN = {s : Number => Case => Str; gen : Number => Agr => Str; h : Harmony} ;
    NP = {s : Case => Str ; h : Harmony; a : Agr} ;

    -- Relative
    RCl = {s : Tense => Anteriority => Polarity => Agr => Str} ;
    RP = {s : Agr => Str} ;

    -- Verb
    VP = {s : Aspect => VForm => Str; compl : Str} ;
    VPSlash = Verb ** {compl : Str; c : Prep} ;
    Comp = {s : Aspect => VForm => Str; compl : Str} ;

    Pron = ResTur.Pron ;
    Det = {s : Str; n : Number; useGen : UseGen} ;
    Num  = {s : Number => Case => Str; n : Number} ;
    Card = {s : Number => Case => Str} ;
    Ord  = {s : Number => Case => Str} ;
    Quant = {s : Str; useGen : UseGen} ;
    PrepNP = {s : Str} ;
    DAP = {s : Number => Case => Str} ;
    CAdv = {s : Str; p : Str; c : Case} ;
    AdN = {s : Str; c : Case} ;

    Numeral = {s : CardOrd => Number => Case => Str ; n : Number} ;
    Digits  = {s : CardOrd => Number => Case => Str ; n : Number; tail : DTail} ;
    Decimal  = {s : CardOrd => Number => Case => Str ; n : Number; hasDot : Bool} ;

    -- Adjective
    AP = {s : Number => Case => Str; h : Harmony} ;

    -- Structural
    Conj = {s : Str; sep : Ints 4; n : Number} ;
    Prep = {s : Str; c : Case} ;

    -- Open lexical classes, e.g. Lexicon
    V, VS, VV, VQ, VA = Verb ;
    V2, V2S, V2V, V2Q, V2A = Verb ** {c : Prep} ;
    V3 = Verb ** {c1,c2 : Prep} ;

    A = Adjective ;
    A2 = Adjective ** {c : Prep} ;

    N  = Noun ;
    N2 = Noun ** {c : Prep} ;
    N3 = Noun ** {c1,c2 : Prep} ;
    GN, SN, LN, PN = {
      s   : Case => Str ;
      h   : Harmony ;
      n   : Number
    } ;


linref
    V2 = \v -> v.s ++ v.c.s ;
    VP = \vp -> vp.compl ++ vp.s ! Perf ! VInf Pos ;

}
