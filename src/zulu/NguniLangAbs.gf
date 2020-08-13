--1 Lang: a Test Module for the Resource Grammar

abstract NguniLangAbs =
  Grammar,
  NguniLexiconAbs,
  -- GuesserAbs,
  TempAbs
  -- ,Construction  --- could be compiled here, but not in concretes, as they call Syntax and Grammar
  -- ,Documentation  --# notpresent
  -- ,Markup - [stringMark]
  ** {
  flags startcat=Phr ;
  } ;
