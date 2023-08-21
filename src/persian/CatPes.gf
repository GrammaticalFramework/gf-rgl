concrete CatPes of Cat = CommonX ** open ResPes, Prelude in {

  flags optimize=all_subs ;

  lincat
------ Tensed/Untensed

    S  = {s : VVForm => Str} ; -- as a complement to Subj
    QS = {s : Str} ;
    RS = {s : Agr => Str ; rp : RelPron => Str} ;
    SSlash = {s : VVForm => Str ; c2 : ResPes.Compl} ;

---- Sentence

    Cl = ResPes.Clause ;
    ClSlash = {
      subj : Str ;
      vp : ResPes.TAnt => Polarity => Order => Str ;
      c2 : ResPes.Compl
      } ;
    Imp = {s : Polarity => Number => Str} ;

---- Question
    QCl = {s : ResPes.TAnt => Polarity => Str} ;

    IP = {s: Str ; n : Number};

--    IDet = {s :Number => Str } ;
      IDet = {s : Str ; n : Number ; isNum : Bool} ;
    IQuant = {s : Str ; n : Number } ;

---- Relative

    RCl = {
      s : ResPes.TAnt => Polarity => Agr => Str ;
      rp : RelPron => Str
      } ;
    RP = {s : RelPron => Str ; a : RAgr};

---- Verb

    VP = ResPes.VPH ;

    VPSlash = ResPes.VPHSlash ;
    Comp = {s : Agr => Str} ;

---- Adjective

    AP = ResPes.AP ;

---- Noun

    CN = ResPes.CN ;

    NP = ResPes.NP ;
    Pron = ResPes.Pron ;
    Det = ResPes.Determiner ;
    Predet = {s : Str} ;
    Num  = {s : Str ; n : Number ; isNum : Bool} ;
    Card = {s : Str; n : Number} ;
    Ord = {s : Str; n : Number ; isNum,isPre : Bool} ;
    Quant = ResPes.Quant ;

---- Numeral

    Numeral = {s : CardOrd => Str ; n : Number} ;
    Digits  = {s : CardOrd => Str ; n : Number} ;
    Decimal = {s : CardOrd => Str ; n : Number; hasDot : Bool} ;

---- Structural

    Conj = {s1,s2 : Str ; n : Number} ;
-----b    Conj = {s : Str ; n : Number} ;
-----b    DConj = {s1,s2 : Str ; n : Number} ;
    Subj = {
      s : Str ;
      compl : VVForm ;  -- subjunctive or indicative
      relpron : RelPron -- choose between که and آنچه
      } ;
    VS = ResPes.Verb ** {compl : VVForm} ; -- subjunctive or indicative
    V2S = ResPes.Verb ** {c2 : Compl ; compl : VVForm} ;
    Prep = Compl ;
---- Open lexical classes, e.g. Lexicon
    V, VQ = ResPes.Verb ;

    V2, VA, V2A, V2Q = ResPes.Verb ** {c2 : Compl} ;
    V3 = ResPes.Verb ** {c2, c3 : Compl} ;
    VV = ResPes.VV ;
    V2V = ResPes.VV ** {c2 : Compl} ;
    A = ResPes.Adjective ;
    A2 = ResPes.Adjective ** {c2 : Str} ;

    N = ResPes.Noun ;

    N2 = ResPes.Noun ** {c2 : Compl ; compl : Str}; -- when N3 is made to N2, need to retain compl
    N3 = ResPes.Noun ** {c2 : Compl ; c3 : Compl} ;
    PN = {s : Str ; animacy : Animacy} ;

}
