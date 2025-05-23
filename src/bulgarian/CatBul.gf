--# -coding=utf8
concrete CatBul of Cat = CommonX - [Temp,Tense,TTAnt,IAdv,AdV] ** open ResBul, Prelude, Predef, (R = ParamX) in {

  lincat
-- Tensed/Untensed

    S  = {s : Str} ;
    QS = {s : QForm => Str} ;
    RS = {s : Agr => Str} ;
    SSlash = {s : Agr => Str ; c2 : Preposition} ;

-- Sentence

    Temp  = {s : Str ; t : ResBul.Tense ; a : R.Anteriority} ;
    Tense = {s : Str ; t : ResBul.Tense} ;

    Cl = {s : ResBul.Tense => Anteriority => Polarity => Order => Str} ;
    ClSlash = {
      s : Agr => ResBul.Tense => Anteriority => Polarity => Order => Str ;
      c2 : Preposition
      } ;
    Imp = {s : Polarity => GenNum => Str} ;

-- Question

    QCl = {s : ResBul.Tense => Anteriority => Polarity => QForm => Str} ;
    IP = {s : Role => QForm => Str; gn : GenNum} ;
    IComp = {s : QForm => Str} ;
    IDet = {s : AGender => QForm => Str; n : Number ; nonEmpty : Bool} ;
    IQuant = {s : GenNum => QForm => Str} ;

-- Relative

    RCl = {s : ResBul.Tense => Anteriority => Polarity => Agr => Str} ;
    RP = {s : GenNum => Str} ;

-- Verb

    VP = ResBul.VP ;
    VPSlash = ResBul.VPSlash ;

    Comp = {s : Agr => Str; p : Polarity} ;

-- Adjective

    AP = {s : AForm => Person => Str; isPre : Bool} ;

-- Adverb

    IAdv = {s : QForm => Str} ;
    AdV  = {s : Str; p : Polarity} ;

-- Noun

    CN = {s : NForm => Str; g : AGender} ;
    NP = {s : Role => Str; gn : GenNum; p : PronPerson} ;
    Pron = {s : Role => Str; gen : AForm => Str; gn : GenNum; p : PronPerson} ;
    Det,DAP = {s : Bool => AGender => Role => Str; nn : NNumber; spec : Species; p : Polarity} ;
    Predet = {s : GenNum => Str} ;
    Ord = {s : AForm => Str} ;
    Num = {s : CardForm => Str; nn : NNumber; nonEmpty : Bool} ;
    Card = {s : CardForm => Str; nn : NNumber} ;
    ACard = {s : Species => Str; nn : NNumber} ;
    Quant = {s : Bool => AForm => Str; nonEmpty : Bool; spec : Species; p : Polarity} ;

-- Numeral

    Numeral = {s : CardOrd => Str; n : Number} ;
    Digits  = {s : CardOrd => Str; n : Number; tail : DTail} ;
    Decimal = {s : CardOrd => Str; n : Number; hasDot : Bool} ;

-- Structural

    Conj = {s : Str; sep : Ints 3; n : Number} ;
    Subj = {s : Str} ;
    Prep = {s : Str; c : Case} ;

-- Open lexical classes, e.g. Lexicon

    V, VS, VQ, VA = Verb ;
    V2 = Verb ** {c2 : Preposition} ;
    V2A, V2V = Verb ** {c2, c3 : Preposition; subjCtrl : Bool} ;
    V2S, V2Q = Verb ** {c2, c3 : Preposition} ;
    V3 = Verb ** {c2, c3 : Preposition} ;
    VV = Verb ** {typ : VVType};

    A = {s : AForm => Str; adv : Str; isPre : Bool} ;
    A2 = {s : AForm => Str; adv : Str; c2 : Preposition} ;

    N  = {s : NForm => Str; rel : AForm => Str; relType : NRelType; g : AGender} ;
    N2 = {s : NForm => Str; rel : AForm => Str; relType : NRelType; g : AGender} ** {c2 : Preposition} ;
    N3 = {s : NForm => Str; rel : AForm => Str; relType : NRelType; g : AGender} ** {c2,c3 : Preposition} ;
    GN = {s : Str; g : Sex} ;
    SN = {s : Sex => Str; pl : Str} ;
    LN = {s : Species => Str; defNom: Str; onPrep : Bool; hasArt : Bool; gn : GenNum} ;
    PN = {s : Str; gn : GenNum} ;

  lindef
    SSlash = \s -> {s = \\_ => s; c2 = {s=""; c=Acc}};
    ClSlash = \s -> {s = \\_,_,_,_,_ => s; c2 = {s=""; c=Acc}};
    
    VP = \s -> predV {s = \\_,_ => s; vtype = VNormal};
    VPSlash = \s -> slashV {s = \\_,_ => s; vtype = VNormal} {s=""; c=Acc} False ;
    
    V, VS, VQ, VA = \s -> {s = \\_,_ => s; vtype = VNormal};
    V2 = \s -> {s = \\_,_ => s; vtype = VNormal; c2 = {s=""; c=Acc}};
    V2A, V2V = \s -> {s = \\_,_ => s; vtype = VNormal; c2,c3 = {s=""; c=Acc}; subjCtrl = False};
    V2S, V2Q = \s -> {s = \\_,_ => s; vtype = VNormal; c2,c3 = {s=""; c=Acc}};
    V3 = \s -> {s = \\_,_ => s; vtype = VNormal; c2,c3 = {s=""; c=Acc}};
    VV = \s -> {s = \\_,_ => s; vtype = VNormal; typ = VVInf Perf};

	A = \s -> {s = \\_ => s; adv = s; isPre = True};
    A2 = \s -> {s = \\_ => s; adv = s; c2 = {s=""; c=Acc}};

    N  = \s -> {s = \\_ => s; rel = \\_ => s; relType = Pref; g = AMasc NonHuman};
    N2 = \s -> {s = \\_ => s; rel = \\_ => s; relType = Pref; g = AMasc NonHuman; c2 = {s=""; c=Acc}};
    N3 = \s -> {s = \\_ => s; rel = \\_ => s; relType = Pref; g = AMasc NonHuman; c2,c3 = {s=""; c=Acc}};
    
  linref
    SSlash = \ss -> ss.s ! agrP3 (GSg Masc) ++ ss.c2.s;
    ClSlash = \cl -> cl.s ! agrP3 (GSg Masc) ! VPresent ! Simul ! Pos ! Main ++ cl.c2.s;

    VP = \vp -> linrefVP vp;
    VPSlash = \vps -> let vp : ResBul.VP
                             = {s  = vps.s ;
                                ad = vps.ad ;
                                clitics = vps.clitics ;
                                compl = \\a => vps.compl1 ! a ++ vps.c2.s ++ vps.compl2 ! a ;
                                vtype = vps.vtype ;
                                p     = Pos ;
                                isSimple = vps.isSimple
                               }
                      in linrefVP vp;

    Conj = \conj -> conj.s ;

    V, VS, VQ, VA = \v -> linrefVP (predV v);
    V2 = \v -> linrefVP (predV v) ++ linPrep v.c2 ;
    V2V = \v -> linrefVP (predV v) ++ linPrep v.c2 ++ linPrep v.c3 ++ "да";
    V2A, V2S, V2Q = \v -> linrefVP (predV v) ++ linPrep v.c2 ++ linPrep v.c3;
    V3 = \v -> linrefVP (predV v) ++ linPrep v.c2 ++ linPrep v.c3;
    VV = \v -> linrefVP (predV v);

    Prep = linPrep ;

	A = \a -> a.s ! ASg Masc Indef;
    A2 = \a -> a.s ! ASg Masc Indef ++ linPrep a.c2;

    N  = \n -> n.s ! NF Sg Indef;
    N2 = \n -> n.s ! NF Sg Indef ++ linPrep n.c2;
    N3 = \n -> n.s ! NF Sg Indef ++ linPrep n.c2 ++ linPrep n.c3;

}
