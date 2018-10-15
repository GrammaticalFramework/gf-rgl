concrete CatAra of Cat = CommonX - [Utt]  ** open ResAra, Prelude, ParamX in {

  flags optimize=all_subs ;

  lincat

-- Phrase

    Utt = {s : Gender => Str};

-- Tensed/Untensed

    S  = {s : Str} ;
    QS = {s : QForm => Str} ;
--    RS = {s : Agr => Str} ;

-- Sentence

    Cl = ResAra.Cl ; -- {s : ResAra.Tense => Polarity => Order => Str} ;
    ClSlash = ResAra.ClSlash ;
    Imp = {s : Polarity => Gender => ResAra.Number => Str} ;

-- Question

    QCl = ResAra.QCl ; -- {s : ResAra.Tense => Polarity => QForm => Str} ;
    IP,
    IDet,
    IComp = ResAra.IP ; -- {s : Gender => State => Case => Str ; n : ResAra.Number} ;
    --    IAdv = {s : Str} ;
    IQuant = {s : State => Case => Str} ;
--
---- Relative
--
--    RCl = {s : Tense => Anteriority => Polarity => Agr => Str} ;
--    RP = {s : Case => Str ; a : RAgr} ;
--
-- Verb

    VP = ResAra.VP ;
    VPSlash = ResAra.VPSlash ; -- VP ** {c2:Str}
    Comp = ResAra.Comp ; --{s : AAgr => Case => Str} ;
--    SC = {s : Str} ;
--
-- Adjective

    AP = ResAra.AP ;

-- Noun

    CN = ResAra.Noun ** {adj : NTable};
    NP, Pron = ResAra.NP; --{s : Case => Str ; a : Agr } ;
    Num,
    Ord,
    Card = ResAra.NumOrdCard ;
    Predet = ResAra.Predet ;

    Det = ResAra.Det ;
--  {s : Species => Gender => Case => Str ;
--   d : State; n : Size; isNum : Bool } ;
    Quant = {s : ResAra.Number => Species => Gender => Case => Str;
             d : State;
             isNum : Bool;
             isPron: Bool} ;
    Art = {s : ResAra.Number => Species => Gender => Case => Str;
             d : State} ;

-- Numeral

    Numeral = {s : CardOrd => Gender => State => Case => Str ;
               n : Size } ;
    Digits = {s : Str;
              n : Size};

-- Structural

    Conj = {s : Str ; n : ResAra.Number} ;
--    DConj = {s1,s2 : Str ; n : ResAra.Number} ;
--    Subj = {s : Str} ;
    Prep = {s : Str} ;

-- Open lexical classes, e.g. Lexicon

    V, VS, VQ, VA = ResAra.Verb ; -- = {s : VForm => Str} ;
    V2, V2A = ResAra.Verb ** {c2 : Str} ;
    VV, V2V, V2S, V2Q = ResAra.Verb ** {c2 : Str} ; --- AR
    V3 = ResAra.Verb ** {c2, c3 : Str} ;

    A = ResAra.Adj ;
    A2 = ResAra.Adj ** {c2 : Str} ;

    N = ResAra.Noun ;
    N2 = ResAra.Noun ** {c2 : Str} ;
    N3 = ResAra.Noun ** {c2, c3 : Str} ;
    PN = {s : Case => Str; g : Gender; h : Species} ;

}
