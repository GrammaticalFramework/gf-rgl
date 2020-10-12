--# -path=.:../abstract:../common:../../prelude

resource ResTur = ParamX ** open Prelude, Predef, HarmonyTur in {

--2 For $Noun$

  flags
    coding=utf8 ;

  param
    Case = Nom | Acc | Dat | Gen | Loc | Ablat | Abess Polarity ;
    Species = Indef | Def ;
    Contiguity = Con | Sep ; --Concatenate or Separate

  oper
    Agr = {n : Number ; p : Person} ;
    Noun = {
      s   : Number => Case => Str ;
      gen : Number => Agr => Str ;
      h   : Harmony
    } ;
    Pron = {s : Case => Str ;
            h : Harmony;
            a : Agr} ;

    agrP3 : Number -> Agr ;
    agrP3 n = {n = n; p = P3} ;
    -- For $Adjective$

    conjAgr : Agr -> Agr -> Agr = \a,b -> 
      {n=conjNumber a.n b.n; p=conjPerson a.p b.p} ;

  oper
    Adjective = Noun ** { adv : Str } ;

    -- For $Verb$.

  param
    VForm =
       VPres      Agr
     | VProg      Agr
     | VPast      Agr
     | VFuture    Agr
     | VImperative
     | VInfinitive
     | Gerund Number Case
     | VNoun Number Case
     ;

  param
    ConjType = Infix | Mixfix ;

    UseGen = NoGen | YesGen Agr | UseIndef ;

  oper
    Verb : Type = {
      s : VForm => Str
      } ;

--2 For $Numeral$
  param
    DForm = unit | ten ;
    CardOrd = NCard | NOrd ;

  oper
    mkPron : (ben,beni,bana,banin,bende,benden,benli,bensiz:Str) -> Number -> Person -> Pron =
     \ben,beni,bana,benim,bende,benden,benli,bensiz,n,p -> {
     s = table {
       Nom => ben ;
       Acc => beni ;
       Dat => bana ;
       Gen => benim ;
       Loc => bende ;
       Ablat => benden ;
       Abess Pos => benli ;
       Abess Neg => bensiz
       } ;
     h = getHarmony ben ;
     a = {n=n; p=p} ;
     } ;

    -- Prep
    noPrep = mkPrep [] Nom;

    mkPrep : Str -> Case -> {s : Str; c : Case; lock_Prep : {}} =
      \s, c -> lin Prep {s=s; c=c};

    mkNP : Noun -> Number -> Person -> {s : Case => Str; h : Harmony; a : Agr} =
      \noun, n, p -> {
        s = noun.s ! n;
        h = noun.h;
        a = {n = n; p = p}
      } ;

    mkClause : Str -> Agr -> Verb -> {s : Tense => Str; subord : Str} =
      \np, a, v -> {
        s = table {
              Pres => np ++ v.s ! VPres a ;
              Past => np ++ v.s ! VPast a ;
              Fut  => np ++ v.s ! VFuture a ;
              Cond => "TODO"
            } ;
        subord = np ++ v.s ! VNoun a.n Nom
      } ;

    mkDet : Str -> Number -> UseGen -> {s : Str; n : Number; useGen : UseGen} =
      \s, n, ug -> {s = s; n = n; useGen = ug} ;

    attachMe : Verb -> {s : Str} =
      \v ->
        let
          s : Str = v.s ! VImperative
        in
          case s of {
            (_ + #vowel + _ )* + (_ + #frontVowel + _) => ss (s ++ "me") ;
            (_ + #vowel + _)*  + (_ + #backVowel  + _) => ss (s ++ "ma")
          } ;

    linCoord : Str -> Ints 4 => Str ;
    linCoord comma = table {0 => "hem"; 1=>"ya"; 2=>"ne"; 3=>comma; 4=>[]} ;

}
