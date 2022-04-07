--# -path=.:../prelude:../abstract:../common

concrete LangCgg of Lang = 
  GrammarCgg,
  LexiconCgg
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

{-
--1 Lang: a Test Module for the Resource Grammar

-- This grammar is for testing the resource as included in the
-- language-independent API, consisting of a grammar and a lexicon.
-- The grammar without a lexicon is [``Grammar`` Grammar.html],
-- which may be more suitable to open in applications.
-}

}
