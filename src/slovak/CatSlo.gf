concrete CatSlo of Cat =
---  CommonX **

  open ResSlo, Prelude in {

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
    RP  = AdjForms ;

    VP = {verb : VerbForms ; clit,compl : Agr => Str} ; ---- more fields probably needed
    VPSlash = {verb : VerbForms ; clit,compl : Agr => Str ; c : ComplementCase} ; ----
    V  = ResSlo.VerbForms ;
    V2 = ResSlo.VerbForms ** {c : ComplementCase} ;

    A  = ResSlo.AdjForms ;
    AP = ResSlo.Adjective ** {isPost : Bool} ; -- {s : Gender => Number => Case => Str}
    A2 = ResSlo.AdjForms ** {c : ComplementCase} ;
    
    AdA = {s : Str} ;

    N  = ResSlo.NounForms ;
    CN = ResSlo.Noun ;      -- {s : Number => Case => Str ; g : Gender}
    NP = {s,clit,prep : Case => Str ; a : Agr ; hasClit : Bool} ; -- clit,prep differ for pronouns
    PN = {s : Case => Str ; g : Gender} ; 
    Det = Determiner ; -- {s : Gender => Case => Str ; size : NumSize} ; -- can contain a numeral, therefore NumSize
    Quant = {s : Gender => Number => Case => Str} ; -- same as AP
    Num = Determiner ;
    Card = Determiner ; -- {s : Gender => Case => Str ; size : NumSize} ; 
    Pron = PronForms ;

    Adv  = {s : Str} ;
    Prep = ResSlo.ComplementCase ; -- {s : Str ; c : Case ; hasPrep : Bool} ;
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
    A = \s -> s.msnom ;


  lincat Numeral = Determiner ; ---- TODO: should contain Ord as well
  lincat Digits = {s:Str ; size : NumSize} ;


}
