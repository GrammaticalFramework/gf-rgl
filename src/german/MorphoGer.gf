--# -path=.:../common:../../prelude
--
----1 A Simple German Resource Morphology
----
---- Aarne Ranta & Harald Hammarström 2002 -- 2006
----
---- This resource morphology contains definitions needed in the resource
---- syntax. To build a lexicon, it is better to use $ParadigmsGer$, which
---- gives a higher-level access to this module.
--
resource MorphoGer = ResGer ** open Prelude, (Predef=Predef) in {

  flags optimize=all ;
    coding=utf8 ;

oper

-- For $StructuralGer$.

  mkPrep : Str -> PCase -> Preposition = \s,c -> 
    {s = s ; s2 = [] ; c = c ; isPrep = True} ;

  nameNounPhrase : {s : Case => Str} ->  {s : PCase => Str ; a : Agr ; 
                                          -- isLight, isPron : Bool ; 
                                          w : Weight ;
                                          ext,rc : Str} = \name -> heavyNP {
      s = \\c => usePrepC c (\k -> name.s ! k) ;
      a = agrP3 Sg 
      } ;

  detLikeAdj : Bool -> Number -> Str -> 
    {s,sp : Gender => PCase => Str ; n : Number ; a : Adjf ; isDef : Bool} = \isDef,n,dies -> 
      {s,sp = appAdj (regA dies) ! n ; n = n ; a = Weak ; isDef = isDef} ;

  detUnlikeAdj : Bool -> Number -> Str -> 
    {s,sp : Gender => PCase => Str ; n : Number ; a : Adjf ; isDef : Bool} = \isDef,n,dies -> 
      {s,sp = appAdj (regDetA dies) ! n ; n = n ; a = Weak ; isDef = isDef} ;

  mkOrd : {s : Degree => AForm => Str} -> {s : AForm => Str} = \a ->
    {s = a.s ! Posit} ;

-- For $ParadigmsGer$.

  genitS : Bool -> Str -> Str = \flag,hund -> case hund of {
    _ + ("el" | "en" | "er") => hund + "s" ;
    _ + ("s" | "ß" | "sch" | "st" | "x" | "z") => hund + "es" ;
    _ => hund + case flag of {True => "s"; False => "es"}
    } ;
  pluralN : Str -> Str = \hund -> case hund of {
    _ + ("el" | "er" | "e") => hund + "n" ;
    _ + "en" => hund ;
    _ => hund + "en"
    } ;
  dativE : Bool -> Str -> Str = \flag,hund -> case hund of {
    _ + ("el" | "en" | "er" | "e") => hund ;
    _ => case flag of {True => hund; False => hund + "e"}
    } ;

-- Duden, p. 119

  verbT : Str -> Str = \v -> case v of {
    _ + ("t" | "d") => v + "et" ; -- gründen, reden, betten
    _ + ("ch" | "k" | "p" | "t" | "g" | "b" | "d" | "f" | "s") + 
        ("m" | "n") => v + "et" ; -- atmen, widmen, öffnen, rechnen
    _ => v + "t"                  -- lernen, lärmen, qualmen etc
    } ;

  verbST : Str -> Str = \v -> case v of {
    _ + ("s" | "ss" | "ß" | "sch" | "x" | "z") => v + "t" ;
    _ => v + "st"
    } ;

  stemVerb : Str -> Str = \v -> case v of {
    _ + ("rn" | "ln") => init v ;
    _ => Predef.tk 2 v
    } ;

-- For $Numeral$.

  LinDigit = {s : DForm => CardOrd => Str} ;

  cardOrd : Str -> Str -> CardOrd => Str = \drei,dritte ->
    table {
      NCard _ _ => drei ;
      NOrd a => (regA (init dritte)).s ! Posit ! a
      } ;

  cardReg : Str -> CardOrd => Str = \zehn ->
    cardOrd zehn (zehn + "te") ;

  mkDigit : (x1,_,_,x4 : Str) -> LinDigit = 
    \drei,dreizehn,dreissig,dritte ->
    {s = table {
           DUnit => cardOrd drei dritte ;
           DTeen => cardReg dreizehn ;
           DTen  => cardOrd dreissig (dreissig + "ste")
           }
     } ;

  regDigit : Str -> LinDigit = \vier -> 
    mkDigit vier (vier + "zehn") (vier + "zig") (vier + "te") ;

  invNum : CardOrd = NCard Masc Nom ;

} ;

