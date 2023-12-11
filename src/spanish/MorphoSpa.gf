--# -path=.:../romance:../common:../../prelude

--1 A Simple Spanish Resource Morphology
--
-- Aarne Ranta 2002 -- 2005
--
-- This resource morphology contains definitions needed in the resource
-- syntax. To build a lexicon, it is better to use $ParadigmsSpa$, which
-- gives a higher-level access to this module.

resource MorphoSpa = CommonRomance, ResSpa **
  open PhonoSpa, Prelude, Predef,
  CatSpa in {

  flags optimize=all ;
    coding=utf8 ;


--2 Nouns

oper
  nomVino : Str -> Number => Str = \vino ->
    numForms vino (vino + "s") ;

  nomPilar : Str -> Number => Str = \pilar ->
    numForms pilar (pilar + "es") ;

  nomTram : Str -> Number => Str = \tram ->
    numForms tram tram ;

-- Common nouns are inflected in number and have an inherent gender.

  mkNoun : (Number => Str) -> Gender -> Noun = \mecmecs,gen ->
    {s = mecmecs ; g = gen} ;

  mkNounIrreg : Str -> Str -> Gender -> Noun = \mec,mecs ->
    mkNoun (numForms mec mecs) ;

  mkNomReg : Str -> Noun = \mec ->
    case mec of {
      _ + ("o" | "e" | "é" | "á") => mkNoun (nomVino mec) Masc ;  --bebé, papá; how about other accented vocal endings? champú champúes
      _ + "a" => mkNoun (nomVino mec) Fem ;
      _ + "z" => mkNounIrreg mec (init mec + "ces") Fem ;
      _ + "ión" => mkNounIrreg mec (tk 2 mec + "ones") Fem ;
      _ + "tud" => mkNoun (nomPilar mec) Fem ;
      _ + "dad" => mkNoun (nomPilar mec) Fem ;
      _ + "án" => mkNounIrreg mec (tk 2 mec + "anes") Masc ;
      _ + "én" => mkNounIrreg mec (tk 2 mec + "enes") Masc ;
      _ + "ín" => mkNounIrreg mec (tk 2 mec + "ines") Masc ;
      _ + "ón" => mkNounIrreg mec (tk 2 mec + "ones") Masc ;
      _ + "ún" => mkNounIrreg mec (tk 2 mec + "unes") Masc ;
      _   => mkNoun (nomPilar mec) Masc
      } ;


--2 Adjectives
--
-- Adjectives are conveniently seen as gender-dependent nouns.
-- Here are some patterns. First one that describes the worst case.

  mkAdj : (x1,_,_,_,_,_,x7 : Str) -> Adj = \buen,solo,gran,sola,solos,solas,solamente ->
    {s = table {
      AAttr g => genForms buen gran ! g ; -- un buen amigo, una gran idea
      AF g Sg => genForms solo sola ! g ;
      AF g Pl => genForms solos solas ! g ;
      AA         => solamente
      }
    } ;

-- Then the regular and invariant patterns.

  adjSolo : Str -> Adj = \solo ->
    let
      sol = Predef.tk 1 solo
    in
    mkAdj solo solo (sol + "a") (sol + "a")
         (sol + "os") (sol + "as") (sol + "amente") ;

  -- masculine and feminine are identical:
  -- adjectives ending with -e, -a and many but not all that end in a consonant
  adjUtil : Str -> Str -> Adj = \util,utiles ->
    mkAdj util util util util
          utiles utiles (util + "mente") ;

  -- adjectives that end in consonant but have different masc and fem forms
  -- español, hablador ...
  adjEspanol : Str -> Str -> Adj = \espanol,espanola ->
    mkAdj espanol espanol espanola espanola
          (espanol + "es") (espanol + "as") (espanola + "mente") ;

  adjBueno : Str -> Adj = \bueno ->
    let buen = init bueno in
    mkAdj buen bueno (buen + "a") (buen + "a")
          (buen + "os") (buen + "as") (buen + "amente") ;

  adjGrande : Str -> Str -> Adj = \gran,grande ->
    mkAdj gran grande gran grande
          (grande + "s") (grande + "s") (grande + "mente") ;

 -- francés francesa franceses francesas
  adjEs : Str -> Adj = \francEs ->
    let franc  : Str = Predef.tk 2 francEs ;
        frances : Str = franc + "es" ;
    in mkAdj francEs francEs (frances + "a") (frances + "a")
             (frances + "es") (frances + "as") (frances + "amente") ;


   -- alemán alemana alemanes alemanas
  adjVn : Str -> Adj = \alemAn ->
    let alemA : Str = init alemAn ;
        alem  : Str = init alemA ;
        A : Str = last alemA ;
        V : Str = case A of {
          "á" => "a" ;
          "é" => "e" ;
          "í" => "i" ;
          "ó" => "o" ;
          "ú" => "u"
        } ;
        alemVn : Str = alem + V + "n" ;
    in mkAdj alemAn alemAn (alemVn + "a") (alemVn + "a")
             (alemVn + "es") (alemVn + "as") (alemVn + "amente") ;

  mkAdjReg : Str -> Adj = \solo ->
    case solo of {
      "grande" => adjGrande "gran" "grande" ;
      "bueno" => adjBueno solo ;
      _ + "o" => adjSolo solo ;
      _ + ("e" | "a") => adjUtil solo (solo + "s") ;
      _ + "és" => adjEs solo ;
      _ + ("á" | "é" | "í" | "ó" | "ú")  + "n" => adjVn solo ;
      _   => adjUtil solo (solo + "es")
      } ;

--2 Personal pronouns
--
-- All the eight personal pronouns can be built by the following macro.
-- The use of "ne" as atonic genitive is debatable.
-- We follow the rule that the atonic nominative is empty.

  mkPronoun : (_,_,_,_,_,_,_,_ : Str) ->
              Gender -> Number -> Person -> Pronoun =
    \il,le,lui,Lui,son,sa,ses,see,g,n,p ->
    let
      alui : Case -> Str = \x -> prepCase x ++ Lui ;
    in {
    s = table {
      Nom        => {c1 = [] ; c2 = []  ; comp = il ; ton = Lui} ;
      Acc        => {c1 = le ; c2 = []  ; comp = [] ; ton = Lui} ;
      CPrep P_a  => {c1 = [] ; c2 = lui ; comp = [] ; ton = alui (CPrep P_a)} ;
      c          => {c1 = [] ; c2 = []  ; comp, ton = alui c}
      } ;
    poss = \\n,g => case <n,g> of {
       <Sg,Masc> => son ;
       <Sg,Fem>  => sa ;
       <Pl,Masc> => ses ;
       <Pl,Fem>  => see
       } ;

    a = Ag g n p ;
    hasClit = True ; isPol = False
    } ;

oper
  -- To retrieve a matching pronoun for an NP argument.
  -- Used in application grammars, please don't remove. /IL
  agr2pron : Agr => Pron = table {
    {g=Masc ; n=Sg ; p=P1}
      => mkPronoun
           "yo" "me" "me" "mí"
           "mi" "mi" "mis" "mis"
            Masc Sg P1 ;
    {g=Masc ; n=Sg ; p=P2}
      => mkPronoun
           "tú" "te" "te" "ti"
           "tu" "tu" "tus" "tus"
            Masc Sg P2 ;
    {g=Masc ; n=Sg ; p=P3}
      => mkPronoun
           "él" "lo" "le" "él"
           "su" "su" "sus" "sus"
            Masc Sg P3 ;
    {g=Masc ; n=Pl ; p=P1}
      => mkPronoun
           "nosotros" "nos" "nos" "nosotros"
           "nuestro" "nuestra" "nuestros" "nuestras"
            Masc Pl P1 ;
    {g=Masc ; n=Pl ; p=P2}
      => mkPronoun
           "vosotros" "os" "os" "vosotros"
           "vuestro" "vuestra" "vuestros" "vuestras"
            Masc Pl P2 ;
    {g=Masc ; n=Pl ; p=P3}
      => mkPronoun
           "ellos" "los" "les" "ellos"
           "su" "su" "sus" "sus"
            Masc Pl P3 ;
    {g=Fem ; n=Sg ; p=P1}
      => mkPronoun
           "yo" "me" "me" "mí"
           "mi" "mi" "mis" "mis"
            Fem Sg P1 ;
    {g=Fem ; n=Sg ; p=P2}
      => mkPronoun
           "tú" "te" "te" "ti"
           "tu" "tu" "tus" "tus"
            Fem Sg P2 ;
    {g=Fem ; n=Sg ; p=P3}
      => mkPronoun
           "ella" "la" "le" "ella"
           "su" "su" "sus" "sus"
            Fem Sg P3 ;
    {g=Fem ; n=Pl ; p=P1}
      => mkPronoun
           "nosotras" "nos" "nos" "nosotras"
           "nuestro" "nuestra" "nuestros" "nuestras"
            Fem Pl P1 ;
    {g=Fem ; n=Pl ; p=P2}
      => mkPronoun
           "vosotras" "os" "os" "vosotras"
           "vuestro" "vuestra" "vuestros" "vuestras"
            Fem Pl P2 ;
    {g=Fem ; n=Pl ; p=P3}
      => mkPronoun
           "ellas" "las" "les" "ellas"
           "su" "su" "sus" "sus"
            Fem Pl P3
    } ;

--2 Determiners
--
-- Determiners, traditionally called indefinite pronouns, are inflected
-- in gender and number, like adjectives.

  pronForms : Adj -> Gender -> Number -> Str = \tale,g,n -> tale.s ! genNum2Aform g n ;

  mkOrdinal : A -> Ord = \adj->
  lin Ord {
    s = \\ag => adj.s ! genNum2Aform ag.g ag.n ;
    s2 = \\_ => []
    } ;

  mkQuantifier : (ese,esa,esos,esas,eso : Str) -> Quant = \ese,esa,esos,esas,eso->
    let
      attrforms : Number => Gender => Case => Str = table {
        Sg => \\g,c => prepCase c ++ genForms ese esa ! g ;
        Pl => \\g,c => prepCase c ++ genForms esos esas ! g ----
        } ;
      npforms : Number => Gender => Case => Str = table {
        Sg => \\g,c => prepCase c ++ genForms ese  esa  ! g ;
        Pl => \\g,c => prepCase c ++ genForms esos esas ! g }
    in lin Quant {
      s = \\_ => attrforms ;
      s2 = [] ;
      sp = npforms ;
      spn= \\c => prepCase c ++ eso ;
      isNeg = False
      } ;

  mkDeterminer : (mucho,mucha : Str) -> Number -> Bool -> Det = \mucho,mucha,number,neg ->
    lin Det {
      s,sp = \\g,c => prepCase c ++ genForms mucho mucha ! g ;
      spn = \\c => prepCase c ++ mucho ;
      n = number;
      s2 = \\c => [] ;
      isNeg = neg
      } ;

   mkIDet : (cuantos, cuantas : Str) -> Number -> IDet = \cuantos,cuantas,number ->
     lin IDet {
       s = \\g,c => prepCase c ++ genForms cuantos cuantas ! g ;
       n = number
       } ;
}
