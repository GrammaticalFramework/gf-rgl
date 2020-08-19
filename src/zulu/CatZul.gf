concrete CatZul of Cat = CommonX - [Temp,Adv] **
  open ResZul, Prelude,ParamX in {

  flags optimize=all_subs ;

  lincat

    -- for now, no anteriority
    Temp = { s : Str ; t : ZTense } ;

-- Tensed/Untensed
    S = {
      s : DMood => Str ;
      subjs : Str ;
      pots : DMood => Str
    } ;
    QS = { s : Str ; qword : Str } ;
    RS = { s : Agr => Str } ;
--     SSlash = {s : Str ; c2 : Str} ;

-- Sentence

    Cl = {
      s : Polarity => ZTense => DMood => Str ;
      subjcl : Polarity => ZTense => Str ;
      potcl : Polarity => DMood => Str
    } ;
--     ClSlash = {
--       s : ResZul.Tense => Anteriority => CPolarity => Order => Str ;
--       c2 : Str
--       } ;
    Imp = { s : Polarity => Str } ;

-- Question

    QCl = {
      s : Polarity => ZTense => DMood => Str ;
      potqcl : Polarity => DMood => Str ;
      qword : Str
    } ;
--     IP = {s : NPCase => Str ; n : Number} ;
--     IComp = {s : Str} ;
--     IDet = {s : Str ; n : Number} ;
--     IQuant = {s : Number => Str} ;

-- Relative

    RCl = { s : Polarity => ZTense => Agr => Str } ;
    RP = { s : Str } ;

-- Verb

    VP = {
      s : Str ;
      perfSuff : Str ;
      oc : Str ;
      comp : Str ;
      hasComp : Bool ;
      r : RInit ;
      syl : Syl ;
      asp : Aspect ;
      asp_pref : VForm => Str ;
      vptype : VPType ;
      comp_agr : Agr ;
      ap_comp : AForm => Str ;
      ap_bool : Bool ;
      aux_root : Str ;
      hasAux : Bool
     } ;
--     VPSlash = ResZul.SlashVP ;

    Comp = {
      s : AForm => Str ;
      r : RInit ;
      agr : Agr ;
      asp : Aspect ;
      asp_pref : VForm => Str ;
      comptype : VPType ;
      ap_bool : Bool ;
    } ;

-- Adjective

    AP = { s : AForm => Str ; b : Bool } ;

-- Noun

    CN = {
      nom : Number => NForm => Str ;
      loc : Number => Str ;
      desc : Number => Str ;
      c : ClassGender
    } ;
    NP = {
      empty : Str ;
      nom : NForm => Str ;
      loc : Str ;
      desc : Str ;
      agr : Agr ;
      isPron : Bool ;
      reqLocS : Bool
    } ;

    -- Pronoun
    Pron = { s : Str ; agr : Agr ; empty : Str } ;
--     DAP
    Det = { s : Str ; n : Number } ;
--     Predet = {s : Str} ;
--     Ord = { s : Case => Str } ;
--     Num  = {s,sp : Bool => Case => Str ; n : Number ; hasCard : Bool} ;
--     Card = {s,sp : Bool => Case => Str ; n : Number} ;
--     ACard = {s : Case => Str ; n : Number} ;
--     Quant = {s : Bool => Number => Str ; sp : Gender => Bool => Number => NPCase => Str; isDef : Bool} ;

-- Numeral

--     Numeral = {s : Bool => CardOrd => Case => Str ; n : Number} ;
--     Digits  = {s : CardOrd => Case => Str ; n : Number ; tail : DTail} ;

-- Structural

    Conj = { s : RInit => Str ; fix : Bool } ;
    Subj = {s : Str} ;
    -- Adv = { s : Str ; asp : Aspect ; reqLocS : Bool } ;
--     Prep = {s : Str; isPre : Bool} ;
--     CAdv = {s : Polarity => Str; p : Str} ;

-- Open lexical classes, e.g. Lexicon
    V,V2,VA = { s : Str ; r : RInit ; syl : Syl ; voice : Voice ; perfSuff : Str } ;
    VS, V3 = { s : Str ; r : RInit ; syl : Syl ; voice : Voice ; perfSuff : Str } ;
    -- VQ = Verb ;
    -- V2Q, V2S = Verb ** {c2 : Str} ;
    -- V2A,V3 = Verb ** {c2, c3 : Str} ;
    -- V = {s : VVForm => Str ; p : Str ; typ : VVType} ;
    -- V2V = Verb ** {c2,c3 : Str ; typ : VVType} ;

    A = { s : AForm => Str ; b : Bool } ;
--     A2 = {s : AForm => Str ; c2 : Str ; isPre : Bool} ;

    N = { nom : Number => NForm => Str ; loc : Number => Str ; c : ClassGender } ;
--     N2 = {s : Number => Case => Str ; g : Gender} ** {c2 : Str} ;
--     N3 = {s : Number => Case => Str ; g : Gender} ** {c2,c3 : Str} ;
--     PN = {s : Case => Str ; g : Gender} ;

    Adv = { s : Str ; asp : Aspect ; reqLocS : Bool } ;

}
