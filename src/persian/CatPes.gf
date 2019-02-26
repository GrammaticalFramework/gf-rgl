concrete CatPes of Cat = CommonX - [Adv] ** open ResPes, Prelude in {

  flags optimize=all_subs ;

  lincat
------ Tensed/Untensed

    S  = {s : Str} ;
    QS = {s : QForm => Str} ;
    RS = {s : Agr => Str } ; -- c for it clefts
    SSlash = {s : Str ; c2 : ResPes.Compl} ;

---- Sentence

    Cl = ResPes.Clause ;
    ClSlash = {
      subj : Str ;
      vp : ResPes.VPHTense => Polarity => Order => Str ;
      c2 : ResPes.Compl
      } ;
    Imp = {s : Polarity => ImpForm => Str} ;

---- Question
    QCl = {s : ResPes.VPHTense => Polarity => QForm => Str} ;

    IP = {s: Str ; n : Number};

--    IDet = {s :Number => Str } ;
      IDet = {s : Str ; n : Number ; isNum : Bool} ;
    IQuant = {s : Str ; n : Number } ;

---- Relative

    RCl = {
      s : ResPes.VPHTense => Polarity => Order => Agr => Str ;
    --  c : Case
      } ;
    RP = {s: Str ; a:RAgr};

---- Verb

    VP = ResPes.VPH ;

    VPSlash = ResPes.VPHSlash ;
    Comp = {s : Agr => Str} ;

---- Adv
    Adv = {s : Str} ;

---- Adjective

    AP = ResPes.Adjective ;

---- Noun

    CN = ResPes.CN ;

    NP = ResPes.NP ;
    Pron = {s : Str ; ps : Str ; a : Agr};
    Det = ResPes.Determiner ;
    Predet = {s : Str} ;
    Num  = {s : Str ; n : Number ; isNum : Bool} ;
    Card = {s : Str; n : Number} ;
    Ord = {s : Str; n : Number ; isNum : Bool} ;
    Quant = {s: Number => Str ; a:Agr ; mod : Mod};
    Art = {s : Str} ;

---- Numeral

    Numeral = {s : CardOrd => Str ; n : Number} ;
    Digits  = {s : CardOrd => Str ; n : Number } ;

---- Structural

    Conj = {s1,s2 : Str ; n : Number} ;
-----b    Conj = {s : Str ; n : Number} ;
-----b    DConj = {s1,s2 : Str ; n : Number} ;
    Subj = {s : Str} ;
    Prep = {s : Str};
---- Open lexical classes, e.g. Lexicon
    V, VS, VQ, VA = ResPes.Verb ;

    V2, V2A, V2Q, V2S = ResPes.Verb ** {c2 : Compl} ;
    V3 = ResPes.Verb ** {c2, c3 : Compl} ;
    VV = ResPes.VV ;
    V2V = ResPes.VV ** {c1 : Str ; c2 : Str} ;
    A = ResPes.Adjective ;
    A2 = ResPes.Adjective ** {c2 : Str} ;

    N = ResPes.Noun ;

    N2 = ResPes.Noun ** {c : Str ; compl : Str}; -- when N3 is made to N2, need to retain compl
    N3 = ResPes.Noun ** {c2 : Str ; c3 : Str} ;
    PN = {s : Str ; animacy : Animacy} ;

}
