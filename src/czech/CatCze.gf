concrete CatCze of Cat =
---  CommonX **

  open ResCze, Prelude in {

  lincat
    Text = {s : Str} ;
    Phr = {s : Str} ;
    Utt = {s : Str} ;
    
    S   = {s : Str} ;
    Cl  = {subj,clit,compl : Str ; verb : VerbForms ; a : Agr} ;
    Comp = {s : Agr => Str} ;

    QS  = {s : Str} ; ----
    QCl = {subj,clit,compl : Str ; verb : VerbForms ; a : Agr} ; ----
    IAdv = {s : Str} ;

    RS  = {s : Agr => Str} ;
    RCl = {subj,clit,compl : Agr => Str ; verb : VerbForms} ; ---- RAgr
    RP  = AdjForms ;

    VP = {verb : VerbForms ; clit,compl : Agr => Str} ; ----
    VPSlash = {verb : VerbForms ; clit,compl : Agr => Str ; c : ComplementCase} ; ----
    V  = ResCze.VerbForms ;
    V2 = ResCze.VerbForms ** {c : ComplementCase} ;

    A  = ResCze.AdjForms ;
    AP = ResCze.Adjective ** {isPost : Bool} ; -- {s : Gender => Number => Case => Str}
    A2 = ResCze.AdjForms ** {c : ComplementCase} ;
    
    AdA = {s : Str} ;

    N  = ResCze.NounForms ;
    CN = ResCze.Noun ;      -- {s : Number => Case => Str ; g : Gender}
    NP = {s,clit,prep : Case => Str ; a : Agr ; hasClit : Bool} ; ----
    PN = {s : Case => Str ; g : Gender} ; ----
    Det = {s : Gender => Case => Str ; n : NumSize} ;
    Quant = {s : Gender => Number => Case => Str} ; -- same as AP
    Num = {s : Gender => Case => Str ; n : NumSize} ; 
    Card = {s : Gender => Case => Str ; n : NumSize} ; 
    Pron = PronForms ;

    Adv  = {s : Str} ;
    Prep = ResCze.ComplementCase ; -- {s : Str ; c : Case ; hasPrep : Bool} ;
    Conj = {s1,s2 : Str} ; ----

    Pol = {s : Str ; p : Bool} ;
    Temp = {s : Str ; t : CTense} ; ----
    Tense = {s : Str ; t : CTense} ; ----
    Ant = {s : Str ; t : CTense} ; ----

    PConj = {s : Str} ;
    Voc = {s : Str} ;

    AdN = {s : Str} ;
    AdV = {s : Str} ;
    CAdv = {s : Str} ;
    SC = {s : Str} ;

  linref
    N = \s -> s.snom ;
    A = \s -> s.msnom ;


  lincat Numeral = {s : NumeralForms ; size : NumSize} ;
  lincat Digits = {s:Str ; n : NumSize} ;


}
