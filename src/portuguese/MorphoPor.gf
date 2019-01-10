--# -path=.:../romance:../common:../../prelude

--1 A Simple Portuguese Resource Morphology
--
--
-- This resource morphology contains definitions needed in the
-- resource syntax. To build a lexicon, it is better to use
-- $ParadigmsPor$, which gives a higher-level access to this module.

resource MorphoPor = CommonRomance, ResPor **
  open PhonoPor, Prelude, Predef, CatPor in {

  flags optimize=all ;
        coding=utf8 ;
--2 Nouns
--

oper
-- For example:
  nomVinho : Str -> Number => Str = \vinho ->
    numForms vinho (vinho + "s") ;

  nomAreia : Str -> Number => Str = \areia ->
    numForms areia areia ;

  nomAlemao : Str -> Number => Str = \alemao ->
    numForms alemao (init alemao + "es") ;

  nomFalcao : Str -> Number => Str = \falcao ->
    numForms falcao (tk 2 falcao + "ões") ;

  nomCidadao : Str -> Number => Str = -- for completeness
    nomVinho ;

  nomNuvem : Str -> Number => Str = \nuvem ->
    numForms nuvem (init nuvem + "ns") ;

  nomRapaz : Str -> Number => Str = \rapaz ->
    numForms rapaz (rapaz + "es") ;

  nomCanal : Str -> Number => Str = \canal ->
    numForms canal (init canal + "is") ;

  nomReptil : Str -> Number => Str = \reptil ->
    numForms reptil (tk 2 reptil + "eis") ;

  nomNounNoun : Str -> Str -> Number => Str = \couve,flor ->
    let couves = mkNomReg' couve ;
        flores = mkNomReg' flor
    in numForms (couve + "-" + flor) (couves.s ! Pl + "-" + flores.s ! Pl) ;

  nomVerbNoun : Str -> Str -> Number => Str = \guarda,chuva ->
    let chuvas = mkNomReg' chuva
    in numForms (guarda + "-" + chuva) (guarda + "-" + chuvas.s ! Pl) ;

  nomChapeudesol : Str -> Str -> Str -> Number => Str = \chapeu,de,sol ->
    let chapeus = mkNomReg' chapeu
    in numForms (chapeu + "-" + de + "-" + sol) (chapeus.s ! Pl + "-" + de + "-" + sol) ;

  vowelToAcute : Str -> Str = \v ->
    case v of {
      "a" => "á" ;
      "e" => "é" ;
      "i" => "í" ;
      "o" => "ó" ;
      "u" => "ú" ;
      _ => error ("input" ++ v ++ "must be vowel character.")
    } ;

  diacriticToVowel : Str -> Str = \v ->
    case v of {
      ("á"|"â"|"ã") => "a" ;
      ("é"|"ê") => "e" ;
      "í" => "i" ;
      ("ó"|"ô"|"õ") => "o" ;
      "ú" => "u" ;
      _ => error ("input" ++ v ++ "must be a vowel character with an accent.")
    } ;

-- Common nouns are inflected in number and have an inherent gender.

  mkNoun : (Number => Str) -> Gender -> Noun = \mecmecs,gen ->
    {s = mecmecs ; g = gen} ;

  mkNounIrreg : Str -> Str -> Gender -> Noun = \mec,mecs ->
    mkNoun (numForms mec mecs) ;

  mkNomReg : Str -> Noun = \vinho -> case vinho of {
    chapéu + "-" + de + "-" + sol => mkNoun (nomChapeudesol chapéu de sol) Masc ;

    -- use nomVerbNoun for compounds of verb+noun
    couve + "-" + flor => mkNoun (nomNounNoun couve flor) Masc ;

    _ => mkNomReg' vinho

    } ;

  mkNomReg' : Str -> Noun = \vinho -> case vinho of {

    -- casa, artesã, saudade, juventude, marquise, artrite
    cas + ("a"|"ã"|"dade"|"tude"|"ise"|"ite")  =>
      mkNoun (nomVinho vinho) Fem ;

    va + "gem" => mkNoun (nomNuvem vinho) Fem ;

    -- if syllable stress is not on -ão, orthographical rules say that
    -- it should be marked with an accented letter
    s + ("ó"|"â"|"á"|"ê"|"é"|"ô"|"í"|"ú") + t + "ão"
      => mkNoun (nomVinho vinho) Masc ; -- although gender is still not predictable, counterexample *bênção

    -- fails for e.g. *coração, but the productive morpheme -ção is
    -- feminine (https://en.wiktionary.org/wiki/-%C3%A7%C3%A3o#Suffix)
    revolu + "ção" => mkNoun (nomFalcao vinho) Fem ;

    certid + "ão" =>
      mkNoun (nomFalcao vinho) Masc ;

    -- problema, carro, maracujá
    proble + ("ma"|"o"|"á") =>
      mkNoun (nomVinho vinho) Masc ;

    ma + ("r"|"z"|"n") => mkNoun (nomRapaz vinho) Masc ;

    -- fóssil, míssil, móbil, portátil, útil
    f + ("ó"|"á"|"é"|"í"|"ú") + s + "il" => mkNoun (nomReptil vinho) Masc ;

    can  + ("a"|"e"|"o"|"u") + "l"  => mkNoun (nomCanal vinho) Masc ;

    can + "il" => mkNoun (numForms vinho (can + "is")) Masc ;

    home  + "m" => mkNoun (nomNuvem vinho) Masc ;

    g + v@("á"|"é"|"í"|"ó"|"ú"|"ê") + "s" => mkNoun (numForms vinho (g + (diacriticToVowel v) + "ses")) Masc ;

    ônibu + "s" => mkNoun (nomAreia vinho) Masc ;

    urub + "u" => mkNoun (nomVinho vinho) Masc ;

    -- tórax/Masc, xerox/Fem
    tóra + "x" => mkNoun (nomAreia vinho) Masc ;

    _           => mkNoun (nomVinho vinho) Masc

    } ;

--2 Adjectives
--
-- Adjectives are conveniently seen as gender-dependent nouns.  Here
-- are some patterns. First one that describes the worst case.

  mkAdj : (_,_,_,_,_ : Str) -> Adj =
    \burro,burra,burros,burras,burramente ->
    {s = table {
       ASg g _ => genForms burro burra ! g ;
       APl g   => genForms burros burras ! g ;
       AA      => burramente
       }
    } ;

  mkAdj4 : (_,_,_,_ : Str) -> Adj ;
  mkAdj4 ms fs mp fp =
    let adv : Str = case fs of {
          exeg + vo@("é"|"á"|"í"|"ó"|"ú"|"ê"|"ô") + tica
            => exeg + (diacriticToVowel vo) + tica ;

          comu + "m" => comu ; -- for Brazilian Portuguese

          _          => fs
          } + "mente" ;
    in {
      s = table {
        ASg g _ => genForms ms fs ! g ;
        APl g   => genForms mp fp ! g ;
        AA      => adv
        }
    } ;

  mkAdjFromNouns : Noun -> Noun -> Adj ;
  mkAdjFromNouns nm nf = mkAdj4 (nm.s ! Sg) (nf.s ! Sg) (nm.s ! Pl) (nf.s ! Pl) ;

  mkAdjReg2 : Str -> Str -> Adj ;
  mkAdjReg2 ms fs = mkAdjFromNouns (mkNomReg ms) (mkNomReg fs) ;

  -- smart paradigm for adjectives amounts to guessing the feminine
  -- form from the masculine form given, and then using the noun smart
  -- paradigm for the plural forms
  mkAdjReg : Str -> Adj = \a ->
    let mkAdj : Str -> Adj = mkAdjReg2 a ;
    in case a of {
      alem   + "ão" => mkAdj (alem + "ã") ; -- fails for patrão/patroa
      pret   + "o"  => mkAdj (pret + "a") ;
      ouvido + "r"  => mkAdj (ouvido + "ra") ;
      chin   + "ês" => mkAdj (chin + "esa") ;
      europ  + "eu" => mkAdj (europ + "eia") ;
      _             => mkAdj a
    } ;

--2 Personal pronouns
--
-- All the eight personal pronouns can be built by the following macro.
-- The use of "ne" as atonic genitive is debatable.
-- We follow the rule that the atonic nominative is empty.

  mkPronoun : (_,_,_,_,_,_,_,_ : Str) -> Gender -> Number -> Person
    -> Pronoun = \ele,o,lhe,Ele,seu,sua,seus,suas,g,n,p ->
    {poss = \\n,g => case <n,g> of {
       <Sg,Masc> => seu ;
       <Sg,Fem>  => sua ;
       <Pl,Masc> => seus ;
       <Pl,Fem>  => suas
       } ;
     a = Ag g n p ;
     hasClit = True ; isPol = False
    } ** pronLin ele o lhe Ele ;

  pronLin : (_,_,_,_ : Str) -> {s : Case =>  {c1,c2,comp,ton : Str}} ;
  -- change pronoun's linearizations without changing its agreement
  -- features (doesn't change possessive linearizations either). e.g.,
  -- he_Pron -> you_Pron
  pronLin = \você, o, lhe, Você ->
    let
      aVocê : Case -> Str = \x -> prepCase x ++ Você ;
    in
    {s = table {
       Nom        => {c1 = [] ; c2 = []  ; comp = você ; ton = Você} ;
       Acc        => {c1 = o ; c2 = []  ; comp = [] ; ton = Você} ;
       CPrep P_a  => {c1 = [] ; c2 = lhe ; comp = [] ; ton = aVocê (CPrep P_a)} ;
       c          => {c1 = [] ; c2 = []  ; comp, ton = aVocê c}
       }
    } ;

  pronAgr : Pronoun -> Gender -> Number -> Person -> Pronoun ;
  -- change a pronoun's agreement features without changing its
  -- linearization field (e.g., You_Pron -> YouFem_Pron)
  pronAgr = \pron, g, n, p -> pron ** {a = Ag g n p} ;

  mkPronFrom : Pronoun -> (_,_,_,_ : Str) -> Gender -> Number -> Person
    -> Pronoun ;
  -- change everything in a pronoun but its possessive linearizations
  mkPronFrom = \pron, você, o, lhe, Você, g, n, p ->
    (pronAgr pron g n p) ** pronLin você o lhe Você ;


--2 Determiners
--
-- Determiners, traditionally called indefinite pronouns, are
-- inflected in gender and number, like adjectives.

  pronForms : Adj -> Gender -> Number -> Str =
    \tale,g,n -> tale.s ! (genNum2Aform g n) ;

  mkOrdinal : A -> Ord = \adj ->
  lin Ord {
    s = \\ag => adj.s ! Posit ! (genNum2Aform ag.g ag.n) ;
    } ;

  mkQuantifier : (esse,essa,esses,essas : Str) -> Quant = \esse,essa,esses,essas->
    let
      attrforms : Number => Gender => Case => Str = table {
        Sg => \\g,c => prepCase c ++ genForms esse essa ! g ;
        Pl => \\g,c => prepCase c ++ genForms esses essas ! g
        } ;
    in lin Quant {
      s = \\_ => attrforms ;
      s2 = [] ;
      sp = attrforms  ; -- in spanish it was different
      isNeg = False
      } ;

  mkDeterminer : (muito,muita : Str) -> Number -> Bool -> Det = \muito,muita,number,neg ->
    lin Det {
      s,sp = \\g,c => prepCase c ++ genForms muito muita ! g ;
      n = number;
      s2 = [] ;
      isNeg = neg
      } ;

   mkIDet : (quantos, quantas : Str) -> Number -> IDet = \quantos,quantas,number ->
  lin IDet {
        s = \\g,c => prepCase c ++ genForms quantos quantas ! g ;
        n = number
        } ;

} ;
