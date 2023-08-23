--# -path=.:../abstract:../common:../prelude

-- Adam Slaski, 2009, 2010 <adam.slaski@gmail.com>

concrete CatLit of Cat = CommonX - [CAdv, Adv] ** open ResLit, Prelude, (R = ParamX) in {

  flags optimize=all_subs; coding=utf8;

  lincat

---- Tensed/Untensed

  S  = {s : Str};
  QS = {s : Str};
  RS = {s : GenNum => Str};
  SSlash = {s : Str; cpl : Complement};

---- Sentence
-- clause (variable tense) e.g. "John walks"/"John walked"
  Cl = {s : Polarity => Anteriority => R.Tense => Str};
  ClSlash = {s : Polarity => Anteriority => R.Tense => Str} 
    ** { cpl : Complement };
  Imp = { s : Polarity => Number => Str };

---- Question

   QCl = {s : Polarity => Anteriority => R.Tense => Str};

-- interrogative pronoun "who"
   IP = NounPhrase;

-- interrrogatove complement of copula "where"  
   IComp = {s : Str};    

-- interrogative determiner "which" : "który", "jaki"
   IDet = IDeterminer;
   IQuant = { s : AForm => Str };

---- Relative

   RCl = {s : GenNum => Polarity => Anteriority => R.Tense => Str};
   RP = {s : AForm => Str; mgn : MaybeGenNum};

-- Verb
-- s - verb
-- s2 - adverbials
-- s3 - object
    
---- Adjective

    AP = AdjPhrase;

---- Noun
  
    CN = { s : Number => Case => Str; g : NounAgrCat ; nomType : NomType};

    NP = NounPhrase;

    Pron = ResLit.Pron;
    
    Det, DAP = Determiner;
    Predet = {s : AForm => Str; np:NounPhrase; adj:Bool }; 
-- 'all', 'most' and 'only' belong in Polish to three completly different parts of speach
    Quant = {s : AForm => Str ; detType: DetType};


---- Numeral

    Num = { s: Case => NounAgrCat => Str; 
                numAgr:NumComb; nb:Number; hasCard:Bool };
    Numeral = { s:Case => NounAgrCat => Str; 
                o:AForm => Str;
                numAgr:NumComb; nb:Number };
    Card = { s:Case => NounAgrCat => Str; 
                numAgr:NumComb; nb:Number };
    Ord = {  s:AForm => Str };
    Digits = { s:Str; o:Str; numAgr:NumComb; nb:Number };
    Decimal = { s:Str; o:Str; numAgr:NumComb; nb:Number; hasDot : Bool };


---- Structural

    Conj = {s1,s2,sent1,sent2 : Str};  
    Subj = {s : Str};
    Prep = Complement; --{s : Str; cplcas: ComplCase };

---- Open lexical classes, e.g. Lexicon

    V  = Verb; -- defined in ResPol.gf
    V2 = Verb ** { cplCase : Complement };
    V3 = Verb ** { cplCase, cplCase2 : Complement };

    VV = Verb;    -- verb-phrase-complement verb         e.g. "want"
    VS = Verb;    -- sentence-complement verb            e.g. "claim"
    VQ = Verb;    -- question-complement verb            e.g. "wonder"
--     VA ;    -- adjective-complement verb           e.g. "look"
    V2V = Verb ** { cplCase : Complement };   -- verb with NP and V complement       e.g. "cause"
    V2S = Verb ** { cplCase : Complement };   -- verb with NP and S complement       e.g. "tell"
    V2Q = Verb ** { cplCase : Complement };   -- verb with NP and Q complement       e.g. "ask"
--     V2A ;   -- verb with NP and AP complement      e.g. "paint"
    
    VA = Verb  ** { cplCase:{ cas:Case; s:Str; adv:Bool } };
    V2A = Verb ** { cplCase:{ cas:Case; s:Str; adv:Bool }; cplCase2:Complement };
    
    VPSlash = VerbPhraseSlash;
    
    VP = VerbPhrase;
    Comp = { s: GenNum => Str };
    
    Ord =  { s : AForm => Str };

    A = Adj;
    A2 = Adj ** { cplCase : Complement };


-- Substantives moreover have an inherent gender. 
    N  = CommNoun;   

    N2 = CommNoun2;

    N3 = CommNoun3;-- ** { cpl, cpl2 : Complement } ;

    PN = NounPhrase;
    
    CAdv = {s,p,sn,pn : Str} ;

    Adv = {s : Str ; advType : AdvType} ;

  linref
    A = \a -> a.pos.msnom ;
    A2 = \a -> a.pos.msnom ++ a.cplCase.s ;

};

