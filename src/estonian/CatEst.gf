concrete CatEst of Cat = CommonX ** open HjkEst, ResEst, Prelude in {

  flags optimize=all_subs ; coding=utf8;

  lincat

-- Tensed/Untensed

    S  = {s : Str} ; --TODO {s : Order => Str}, like in German?
    QS = {s : Str} ;
    RS = {s : Agr => Str ; c : NPForm} ;
    SSlash = {s : Str ; c2 : Compl} ;

-- Sentence

    Cl    = {s : ResEst.Tense => Anteriority => Polarity => SType => Str} ;
    ClSlash = {s : ResEst.Tense => Anteriority => Polarity => Str ; c2 : Compl} ;
    Imp   = {s : Polarity => Agr => Str} ;

-- Question

    QCl    = {s : ResEst.Tense => Anteriority => Polarity => Str} ;
    IP     = ResEst.IPhrase ;
    IComp  = {s : Agr => Str} ;
    IDet   = {s : Case => Str ; n : Number ; isNum : Bool} ;
    IQuant = {s : Number => Case => Str} ;

-- Relative

    RCl   = {s : ResEst.Tense => Anteriority => Polarity => Agr => Str ; c : NPForm} ;
    RP    = ResEst.RelPron ;

-- Verb

    VP   = ResEst.VP ;
    VPSlash = ResEst.VP ** {c2 : Compl} ;
    Comp = {s : Agr => Str} ;

-- Adjective

-- The $Bool$ in s tells whether usage is modifying (as opposed to
-- predicative), e.g. "x on suurem kui y" vs. "y:st suurem arv".
-- The $Infl$ in infl tells whether the adjective inflects as a
-- modifier: e.g. "väsinud mehele" vs. "mees muutus väsinuks".

    AP = ResEst.APhrase ;

-- Noun

    CN   = ResEst.CNoun ;
    Pron = {s : NPForm => Str ; a : Agr} ;
    NP   = ResEst.NPhrase ;
    DAP, Det = ResEst.Determiner ;

----    QuantSg, QuantPl = {s : Case => Str ;  isDef : Bool} ;
    Ord    = {s : NForm => Str} ;
    Predet = {s : Number => NPForm => Str} ;
    Quant  = {s,sp : Number => Case => Str ; isDef : Bool} ;
    Card   = {s : Number => Case => Str ; n : Number} ;
    Num    = {s : Number => Case => Str ; isNum : Bool ; n : Number} ;

-- Numeral

    Numeral = {s : CardOrd => Str ; n : Number} ;
    Digits  = {s : CardOrd => Str ; n : Number} ;

-- Structural

    Conj = {s1,s2 : Str ; n : Number} ;
----b    DConj = {s1,s2 : Str ; n : Number} ;
    Subj = {s : Str} ;
    Prep = ResEst.Compl ;

-- Open lexical classes, e.g. Lexicon

    V, VS, VQ = ResEst.Verb1 ; -- = {s : VForm => Str ; sc : Case} ;
    V2, VA, V2Q, V2S = ResEst.Verb2 ;
    V2A, V3 = ResEst.Verb3 ;
    VV  = ResEst.Verb1 ** {vi : InfForms} ;
    V2V = ResEst.Verb2 ** {vi : InfForms} ;

    A  = ResEst.Adjective ** {infl : Infl} ;
    A2 = ResEst.Adjective ** {infl : Infl ; c2 : Compl} ;

    N  = ResEst.Noun  ;
    N2 = ResEst.Noun ** {
            postmod : Str ; -- postmod, because N2 can come from N3+complement via ComplN3
            c2 : Compl ;
            isPre : Bool} ;
    N3 = ResEst.Noun ** {   -- no postmod, because N3 can only come from lexical funs
            c2,c3 : Compl ;
            isPre,isPre2 : Bool
            } ;
    PN = {s : Case  => Str} ;

  linref
    VP = \vp -> linV vp.v ;
    NP = linNP (NPCase Nom) ;
    CN = linCN (NCase Sg Nom) ;
    V,VS,VQ = linV ;
    V2,VA,V2S,V2Q = linV2 ;



}
