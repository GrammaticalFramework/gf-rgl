--# -path=.:abstract:common:prelude

-- Here goes manually made utils used for automaticaly exported Tēzaurs lexicon.

resource PortedMorphoUtilsLav = open Prelude, Predef, ResLav in {

flags coding = utf8 ;

oper

  changeNounForm : Number -> Case -> Str -> Noun -> Noun = 
      \N,C,form,noun -> noun ** {
    s = \\n,c => case <n,N,c,C> of {
      <Sg, Sg, Nom, Nom> |
      <Sg, Sg, Gen, Gen> |
      <Sg, Sg, Dat, Dat> |
      <Sg, Sg, Voc, Voc> |
      <Pl, Pl, Nom, Nom> |
      <Pl, Pl, Gen, Gen> |
      <Pl, Pl, Dat, Dat> |
      <Pl, Pl, Voc, Voc> => form ;
    _                    => noun.s ! n ! c }
  } ;

  addNounForm : Number -> Case -> Str -> Noun -> Noun = 
      \N,C,form,noun -> noun ** {
    s = \\n,c => case <n,N,c,C> of {
      <Sg, Sg, Nom, Nom> |
      <Sg, Sg, Gen, Gen> |
      <Sg, Sg, Dat, Dat> |
      <Sg, Sg, Voc, Voc> |
      <Pl, Pl, Nom, Nom> |
      <Pl, Pl, Gen, Gen> |
      <Pl, Pl, Dat, Dat> |
      <Pl, Pl, Voc, Voc> => variants {form ; noun.s ! n ! c} ;
    _                    => noun.s ! n ! c }
  } ;

  {- A note about GF restrictions as of 2025-08-27:
    Following thing does not work, compiler asumes that N an C are free variables.
     changeForm_ideal : Number -> Case -> Str -> Noun -> Noun ;
     changeForm_ideal N C str noun = noun ** {NF N C => str} ;
    Meanwhile this should work
     changeForm_fixed : Str -> Noun -> Noun ;
     changeForm_fixed str noun = noun ** {NF Sg Nom => str} ;
    Related: https://github.com/GrammaticalFramework/gf-core/issues/198
    Also Inari suggested to avoid using a function and write this thing inline:
     Noun : Type = {s : NForm => Str} ;
     myRegularNoun = {- … some noun …- } ;
     mySpecialNoun = {s = myRegularNoun.s ** {NF Sg Acc => "special form"}} ;
  -}

}
