concrete CatTel of Cat = CommonX ** open ResTel, Prelude in {

  flags optimize=all_subs ;

  lincat
--
---- Tensed/Untensed
--
    S  = {s : Str} ;
    QS = {s : Str} ;
    RS = {s : Str} ;
    SSlash = {s : Str} ;
--
---- Sentence
--
    Cl = ResTel.Clause ;
    ClSlash = {s : VPHTense => Polarity => Str ; c2 : Compl} ;
    Imp = {s : Str} ;
--
---- Question
--
    QCl = ResTel.Clause ;
    IP = {s : Case => Str ; n : Number} ;
    IComp = {s : Str} ;
    IDet = {s : Str ; n : Number} ;
    IQuant = {s : Number => Str} ;
--
---- Relative
--
    RCl = ResTel.Clause ;
    RP = {s : Str} ;
--
---- Verb
--
    VP = ResTel.VPH ;
    VPSlash = ResTel.VPHSlash ;
    Comp = {s : Agr => Str} ;
--
---- Adjective
--
    AP = ResTel.Adjective ;
--
---- Noun
--
    CN = ResTel.Noun ;
    NP = ResTel.NP ;
    Pron = {s : PronCase => Str ; a : Agr} ;
    Det = {s : Gender => Case => Str ; n : Number} ;
    Predet, Ord = {s : Str} ;
    Num  = {s : Str ; n : Number} ;
    Card, ACard = {s : Str; n : Number} ;
    DAP = {s : Gender => Case => Str ; n : Number} ;
    Quant = {s : Number => Gender => Case => Str} ;
    Art = {s : Str} ;
--
---- Numeral
--
    Numeral, Digits, Decimal = {s : Str ; n : Number} ;
    LN, GN, SN = {s : Str} ;
--
---- Structural
--
    Conj = {s1,s2 : Str ; n : Number} ;
-----b    Conj = {s : Str ; n : Number} ;
-----b    DConj = {s1,s2 : Str ; n : Number} ;
    Subj = {s : Str} ;
    Prep = {s : Str} ;
--
---- Open lexical classes, e.g. Lexicon
--
    V, VS, VQ, VA = Verb ; -- = {s : VForm => Str} ;
    V2, V2A, V2Q, V2S = Verb ** {c2 : Compl} ;
    V3 = Verb ** {c2, c3 : Compl} ;
    VV = Verb ** {isAux : Bool} ;
    V2V = Verb ** {c2 : Compl} ;
--
    A = ResTel.Adjective ; --- {s : Gender => Number => Case => Str} ;
    A2 = ResTel.Adjective ** {c2 : Str} ;
--
    N = ResTel.Noun ; --{s : Number => Case => Str ; g : Gender} ;
    N2 = ResTel.Noun ** {c2 : Str} ;
    N3 = ResTel.Noun ** {c2,c3 : Str} ;
    PN = {s : Case => Str ; g : Gender} ;
--
}
