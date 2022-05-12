concrete CatZul of Cat = CommonX - [Temp,Tense,Adv,IAdv] **
  open ResZul, Prelude, ParamX in {

  flags optimize=all_subs ;

  lincat

    -- for now, no anteriority
    Temp = { s : Str ; t : BasicTense } ;

-- Tensed/Untensed
    S = { s : Str } ;
    QS = { s : Str ; qword_pre : Str ; qword_post : Str } ;
    RS = { s : Agr => Str } ;
--     SSlash = {s : Str ; c2 : Str} ;

-- Sentence

    Cl = {
      s : Polarity => BasicTense => Str
    } ;
--     ClSlash = {
--       s : ResZul.Tense => Anteriority => CPolarity => Order => Str ;
--       c2 : Str
--       } ;
    Imp = { s : Polarity => Str } ;

-- Question

    QCl = {
      s : Polarity => BasicTense => Str ;
      -- potqcl : Polarity => Str ;
      qword_pre : Str ;
      qword_post : Str
    } ;
--     IP = {s : NPCase => Str ; n : Number} ;
    IComp = { s : Str ; postIComp : Bool } ; -- possibly needs parameter for comp type
--     IDet = {s : Str ; n : Number} ;
--     IQuant = {s : Number => Str} ;

-- Relative

    RCl = { s : Agr => Polarity => BasicTense => Str } ;
    RP = { s : Str } ;

-- Verb

    VP = {
      s : CType => Agr => Polarity => BasicTense => Bool => Str ; -- TODO: mood
      -- oc : Str ;
      comp : Str ;
      iadv : Str ;
      advs : Str ;
      hasComp : Bool ;
      r : RInit ;
      syl : Syl ;
      -- asp : Aspect ;
      -- asp_pref : VForm => Str ;
      vptype : VPType ;
      -- comp_agr : Agr ;
      -- ap_comp : AForm => Str -- ;
      -- aux_root : Str ;
      -- hasAux : Bool
    } ;

    -- VPSlash = {
    --   s : RForm => Str ;
    --   oc : Str ;
    --   comp : Str ;
    --   hasComp : Bool ;
    --   r : RInit ;
    --   syl : Syl ;
    --   asp : Aspect ;
    --   asp_pref : VForm => Str ;
    --   vptype : VPType ;
    --   comp_agr : Agr ;
    --   ap_comp : AForm => Str ;
    --   aux_root : Str ;
    --   hasAux : Bool
    -- } ;

    -- Comp = {
    --   s : AForm => Str ;
    --   r : RInit ;
    --   agr : Agr ;
    --   asp : Aspect ;
    --   asp_pref : VForm => Str ;
    --   comptype : VPType
    -- } ;

-- Adjective

    AP = { s : AForm => Str ; empty : Str ; t : AType } ;

-- Noun

    CN = {
      s : Number => NForm => Str ;
      -- mod : Number => Str ;
      c : ClassGender ;
      empty : Str ;
      emph : Bool
    } ;

    NP = {
      empty : Str ;
      s : NForm => Str ;
      agr : Agr ;
      i : RInit ;
      proDrop : Bool ; -- used when NP is object
      isPron : Bool ; -- used to choose correct between Full/Reduced
      heavy : Bool -- has object/adv/qword
    } ;

    -- Pronoun
    Pron = { s : NForm => Str ; agr : Agr ; empty : Str ; proDrop : Bool } ;
--     DAP
    Det = { s : Str ; n : Number } ;
    Predet = { s : Agr => Str ; hasDem : Bool } ;
--     Ord = { s : Case => Str } ;
    Num  = { s: Str ; n : Number } ;
--     Card = {s,sp : Bool => Case => Str ; n : Number} ;
--     ACard = {s : Case => Str ; n : Number} ;
    Quant = { s : Str ; dist : Distance } ;

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
    V,V2,VA,VS,V3 = {
      s : RForm => Str ;
      r : RInit ;
      syl : Syl ;
      voice : Voice
    } ;
    -- VQ = Verb ;
    -- V2Q, V2S = Verb ** {c2 : Str} ;
    -- V2A,V3 = Verb ** {c2, c3 : Str} ;
    -- V = {s : VVForm => Str ; p : Str ; typ : VVType} ;
    -- V2V = Verb ** {c2,c3 : Str ; typ : VVType} ;

    A = { s : AForm => Str ; empty : Str ; t : AType } ;
--     A2 = {s : AForm => Str ; c2 : Str ; isPre : Bool} ;

    N, N2, N3, PN = {
      s : Number => NForm => Str ;
      c : ClassGender ;
      -- sem : SemanticType ;
      empty : Str
    } ;
--     N2 = {s : Number => Case => Str ; g : Gender} ** {c2 : Str} ;
--     N3 = {s : Number => Case => Str ; g : Gender} ** {c2,c3 : Str} ;

    Adv = { s : Str ; reqLocS : Bool } ;

    IAdv = { s : Str ; postIAdv : Bool } ;

}
