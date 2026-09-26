concrete CatCze of Cat =
---  CommonX **

  open ResCze, Prelude in {

  lincat
    Text = {s : Str} ;
    Phr = {s : Str} ;
    Utt = {s : Str} ;

    S   = ResCze.Sentence ;
    Cl  = {subj,clit,compl : Str ; verb : VerbForms ; a : Agr ; isDrop,clitPresent : Bool} ;
    Comp = {s : Agr => Str} ;

    QS  = {s,ind : Str} ;
    QCl = {q,subj,clit,compl : Str ; verb : VerbForms ; a : Agr ; yesNo : Bool} ;
    IAdv, IComp = {s : Str} ;
    IP = {s : Case => Str ; a : Agr} ;
    IDet = Determiner ;
    IQuant = Adjective ;
    Imp = {s : Bool => Agr => Str} ;

    RS  = {s : Agr => Str} ;
    RCl = {subj,clit,compl : Agr => Str ; verb : VerbForms} ; ---- RAgr with composite RP
    RP  = AdjForms ;

    -- clitPresent records an overt clitic in this domain, not NP eligibility.
    VP = {verb : VerbForms ; clit,compl : Agr => Str ; clitPresent : Bool} ; ---- more fields probably needed
    -- clit/clitAfter surround the open slot in the eventual clitic cluster.
    -- Its presence flag covers both sides of the slot.
    VPSlash = {verb : VerbForms ; clit,clitAfter,compl : Agr => Str ; clitPresent : Bool ; c : ComplementCase ; ind : Agr => Str} ; -- ind : incorporated second object, rendered after the object slot
    V  = ResCze.VerbForms ;
    V2 = ResCze.VerbForms ** {c : ComplementCase} ;
    V3 = ResCze.VerbForms ** {c,c2 : ComplementCase} ; -- c : direct object, c2 : indirect object
    VS,VQ = ResCze.VerbForms ;
    VV = ResCze.VerbForms ** {isAux : Bool} ;

    A  = ResCze.DegreeForms ;
    AP = ResCze.Adjective ** {pred : Agr => Str ; isPost : Bool} ;
    A2 = ResCze.DegreeForms ** {c : ComplementCase} ;

    AdA = {s : Str} ;

    N  = ResCze.NounForms ;
    CN = ResCze.Noun ;      -- {s : Number => Case => Str ; g : Gender}
    -- Object-clitic eligibility and subject omission are independent.
    -- Extend.ProDrop selects isDrop; clit ! Nom retains its empty constituent.
    -- Modifiers restore full forms. s and prep are always available for strong use.
    -- m controls NP modifiers; a controls the clause. Scale nouns can differ.
    -- A modified pronoun can keep its pronominal head without a weak form.
    NP = NPForms ** {clit : Case => Str ; a : Agr ; m : ModifierAgr ; hasClit,isDrop,isPron : Bool} ;
    PN = {s : Case => Str ; g : Gender} ;
    Ord = Adjective ;
    Det = Determiner ; -- {s : Gender => Case => Str ; size : NumSize} ; -- can contain a numeral, therefore NumSize
    Quant = {s : Gender => Number => Case => Str} ; -- same as AP
    Predet = Adjective ** {postPron : Bool} ;
    Num = Determiner ;
    Card = Determiner ; -- {s : Gender => Case => Str ; size : NumSize} ;
    Pron = PronForms ** {poss : DemPronForms} ;

    Adv  = {s : Str} ;
    Prep = ResCze.ComplementCase ; -- {s : Str ; c : Case ; hasPrep : Bool} ;
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
  lincat Decimal = {s:Str ; size : NumSize ; hasDot : Bool} ;


}
