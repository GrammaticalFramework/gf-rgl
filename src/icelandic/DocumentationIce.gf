--# -path=.:../abstract:../common

-- documentation of Icelandic in Icelandic: the default introduced in LangIce

concrete DocumentationIce of Documentation = CatIce ** 
  DocumentationIceFunctor with (Terminology = TerminologyIce) ;
