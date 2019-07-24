--# -path=.:../prelude:../abstract:../common

concrete GrammarCgg of Grammar = 
  NounCgg,
  VerbCgg, 
  AdjectiveCgg,
  AdverbCgg,
  NumeralCgg,
  SentenceCgg, 
  QuestionCgg,
  RelativeCgg,
  ConjunctionCgg,
  PhraseCgg,
  TextX -[Adv, IAdv,AdA],
  StructuralCgg,
  IdiomCgg,
  TenseX -[Adv,IAdv,AdA]
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

{-
--1 Grammar: the Main Module of the Resource Grammar

-- This grammar is a collection of the different grammar modules,
-- To test the resource, import [``Lang`` Lang.html], which also contains
-- a lexicon.
-}

}
