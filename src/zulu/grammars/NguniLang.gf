--1 Lang: a Test Module for the Resource Grammar

abstract NguniLang =
  NguniGrammar,
  Backward[ComplV2,ComplV3],
  ExtraExt,
  NounExt,
  VerbExt
  -- MonoLexAbs,
  -- Chunk
  -- Symbol - [Symb],
  -- ,Construction  --- could be compiled here, but not in concretes, as they call Syntax and Grammar
  -- ,Documentation  --# notpresent
  -- ,Markup - [stringMark]
  ** {
  flags startcat=Phr ;
  } ;
