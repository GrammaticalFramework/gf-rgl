concrete CatTur of Cat = CommonX - [CAdv,AdN] ** open ResTur, Prelude in {

  flags optimize=all_subs ;

  lincat

    S  = {s, subord : Str} ;

    Cl = {s : Tense => Str; subord : Str} ;


    -- Noun
    CN = {s : Number => Case => Str; gen : Number => Agr => Str} ;
    NP = {s : Case => Str ; a : Agr} ;
    VP = Verb ;
    VPSlash = VP ** {c : Prep} ;
    Conj = {s : Str ; s1 : Str ; s2 : Str ; ct : ConjType} ;

    Pron = ResTur.Pron ;
    Det = {s : Str; n : Number; useGen : UseGen} ;
    Num  = {s : Number => Case => Str; n : Number} ;
    Card = {s : Number => Case => Str} ;
    Ord  = {s : Number => Case => Str} ;
    Quant = {s : Str; useGen : UseGen} ;
    Prep = {s : Str; c : Case} ;
    PrepNP = {s : Str} ;
    DAP = {s : Number => Case => Str} ;
    CAdv = {s : Str; p : Str; c : Case} ;
    AdN = {s : Str; c : Case} ;

    Numeral = {s : CardOrd => Number => Case => Str ; n : Number} ;
    Digits  = {s : CardOrd => Number => Case => Str ; n : Number; tail : DTail} ;

    -- Adjective
    AP = {s : Number => Case => Str} ;

    -- Open lexical classes, e.g. Lexicon
    V, VS, VV, VQ, VA = Verb ;
    V2, V2S, V2V, V2Q, V2A = Verb ** {c : Prep} ;
    V3 = Verb ** {c1,c2 : Prep} ;

    A = Adjective ;
    A2 = Adjective ** {c : Prep} ;

    N  = Noun ;
    N2 = Noun ** {c : Prep} ;
    N3 = Noun ** {c1,c2 : Prep} ;
    PN = Noun ;

linref
    V = \v -> v.s ! VInfinitive ;
    V2 = \v -> v.s ! VInfinitive ++ v.c.s ;

}
