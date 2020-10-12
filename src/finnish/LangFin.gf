--# -path=.:../abstract:../common:../api

concrete LangFin of Lang =
  GrammarFin,
  LexiconFin
  , ConstructionFin
  , DocumentationFin --# notpresent
  , MarkupFin - [stringMark]
** {

} ;
