--# -path=.:../abstract:../common

-- documentation of Russian in Russian: the default introduced in LangRus

concrete DocumentationRus of Documentation = CatRus **
  DocumentationRusFunctor with (Terminology = TerminologyRus) ;
