concrete CatLat of Cat = CommonX-[Adv] ** open ResLat, ParamX, Prelude in {

  flags optimize=all_subs ;

  lincat

---- Tensed/Untensed
--
    S  = Sentence ;
    QS = {s : QForm => Str} ;
    RS = { s : Gender => Number => Str } ; -- Sentence ;
--    RS = {s : Agr => Str ; c : Case} ; -- c for it clefts
--    SSlash = {s : Str ; c2 : Str} ;
--
---- Sentence
--
    Cl = Clause ;
    ClSlash = Clause ;
    Imp = {s : Polarity => VImpForm => Str} ;
--
---- Question
    --
    -- TO FIX
    QCl = Clause ** { q : Str } ; 
    IP = {s : Case => Str ; n : Number} ;
    IComp = {s : Str} ;    
    IDet = Determiner ; --{s : Str ; n : Number} ;
    IQuant = {s : Agr => Str} ;
--
---- Relative
--
    RCl = { s : Gender => Number => Clause };
--      s : ResLat.Tense => Anteriority => CPolarity => Agr => Str ; 
--      c : Case
--      } ;
    RP = {s : Agr => Str } ;
--
---- Verb
--
    VP = ResLat.VerbPhrase ;
    VPSlash = ResLat.ObjectVerbPhrase ; -- VP ** {c2 : Preposition} ;
    Comp = {s : Agr => Str} ; 
--
---- Adjective
--
--    AP = Adjective ** {isPre : Bool} ; ---- {s : Agr => Str ; isPre : Bool} ; 
    AP = AdjectivePhrase ;
 
--
---- Noun
--
    CN = ResLat.CommonNoun ;
    NP = ResLat.NounPhrase ;
    Pron = ResLat.Pronoun ;
    Det = Determiner ;
    Predet = {s : Str} ;
    Ord = { s : Gender => Number => Case => Str } ;
    Num  = {s : Gender => Case => Str ; n : Number} ;
    Card = {s : Gender => Case => Str ; n : Number} ;
    Quant = Quantifier ;
--
---- Numeral
--
    Numeral = ResLat.TNumeral ;
    Digits  = {s : Str ; unit : Unit} ;
--
---- Structural
--
    Conj = ResLat.Conjunction; --{s1,s2 : Str ; n : Number} ;
    Subj = {s : Str} ;
    Prep = ResLat.Preposition ;
--
---- Open lexical classes, e.g. Lexicon

    V, VS, VQ, VA = ResLat.Verb ; -- = {s : VForm => Str} ;
    V2, V2A, V2Q, V2S = Verb2 ;
    V3 = Verb3 ;
    VV = ResLat.VV ;
    V2V = Verb ** {c2 : Str ; isAux : Bool} ;

    A = Adjective ;
    Adv = Adverb ;
    
    N = Noun ;
    N2 = Noun ** { c : Prep } ;
    N3 = Noun ** { c : Prep ; c2 : Prep } ;
    PN = { s : Case => Str ; n : Number ; g : Gender } ;
    A2 = Adjective ** { c : Prep} ;

  linref
    NP = \np -> combineNounPhrase np ! PronNonDrop ! APreN ! DPostN ! Nom ; 
    VP = \vp -> vp.adv ++ vp.inf !  VInfActPres ++ vp.obj ++ vp.compl ! Ag Masc Sg Nom ;
    S = \s -> defaultSentence s ! SOV ;
    V, VS, VQ, VA, VV = \v -> v.act ! (VAct VSim (VPres VInd) Sg P1) ;
    V2, V2A, V2Q, V2S = \v -> v.act ! (VAct VSim (VPres VInd) Sg P1) ;
    Pron = \p -> p.pers.s ! PronNonDrop ! PronNonRefl ! Nom ;
    Conj = \c -> c.s1 ++ c.s2 ++ c.s3 ;
}
