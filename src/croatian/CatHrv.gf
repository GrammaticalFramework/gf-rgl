concrete CatHrv of Cat =
---  CommonX **

  open ResHrv, Prelude in {

  lincat
    Text = {s : Str} ;
    Phr = {s : Str} ;
    Utt = {s : Str} ;
    
    S   = {s : Str} ;
    Cl  = {subj,clit,compl : Str ; verb : VerbForms ; a : Agr} ;
    Comp = {s : Agr => Str} ;

    QS  = {s : Str} ; ---- TODO: indirect questions
    QCl = {subj,clit,compl : Str ; verb : VerbForms ; a : Agr} ; -- = Cl ---- check if enough
    IAdv = {s : Str} ;

    RS  = {s : Agr => Str} ;
    RCl = {subj,clit,compl : Agr => Str ; verb : VerbForms} ; ---- RAgr with composite RP
    RP  = {s : Gender => Number => Case => Str} ; -- same as AP 

    VP = {verb : VerbForms ; clit,compl : Agr => Str} ; ---- more fields probably needed
    VPSlash = {verb : VerbForms ; clit,compl : Agr => Str ; c : ComplementCase} ; ----
    V  = {s : VerbForms} ;
    V2 = {s : VerbForms ; c : ComplementCase} ;
    VS,VQ  = {s : VerbForms} ;

    A  = {posit, compar, superl : AdjForms} ;
    AP = ResHrv.Adjective ** {isPost : Bool} ; -- {s : Gender => Number => Case => Str}
    A2 = ResHrv.AdjForms ** {c : ComplementCase} ;
    
    AdA = {s : Str} ;

    N  = ResHrv.NounForms ** {g : Gender} ;
    N2  = ResHrv.NounForms ** {g : Gender ; c : ComplementCase} ;
    CN = ResHrv.Noun ;      -- {s : Number => Case => Str ; g : Gender}
    NP = {s,clit,prep : Case => Str ; a : Agr ; hasClit : Bool} ; -- clit,prep differ for pronouns
    PN = {s : Case => Str ; g : Gender} ; 
    Det = Determiner ; -- {s : Gender => Case => Str ; size : NumSize} ; -- can contain a numeral, therefore NumSize
    Predet = {s : Gender => Number => Case => Str} ; -- same as AP
    Quant = {s : Gender => Number => Case => Str} ; -- same as AP
    Num = Determiner ;
    Card = Determiner ; -- {s : Gender => Case => Str ; size : NumSize} ;
    Ord = AdjForms ;
    Pron = PronForms ** {poss : AdjForms} ;

    Adv  = {s : Str} ;
    Prep = ResHrv.ComplementCase ; -- {s : Str ; c : Case ; hasPrep : Bool} ;
    Conj = {s1,s2 : Str} ; ---- may need a number

    Pol = {s : Str ; p : Bool} ;
    Temp = {s : Str ; t : CTense} ; 
    Tense = {s : Str ; t : CTense} ;
    Ant = {s : Str ; t : CTense} ; 

    PConj = {s : Str} ;
    Voc = {s : Str} ;

    AdN = {s : Str} ;
    AdV = {s : Str} ;
    CAdv = {s : Str} ;
    SC = {s : Str} ;

  linref
    N = \s -> s.snom ;
    A = \s -> s.posit.msnom ;
    V = \v -> v.s ! VInf ;


  lincat Numeral = {s : AdjForms ; size : NumSize} ;
  lincat Digits = {s : Str ; size : NumSize} ;
  lincat Decimal = {s : Str ; size : NumSize ; hasDot : Bool} ;

}
