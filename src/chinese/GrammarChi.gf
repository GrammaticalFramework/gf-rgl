--# -path=.:../abstract:../common:prelude

concrete GrammarChi of Grammar = 
  NounChi, 
  VerbChi, 
  AdjectiveChi,
  AdverbChi,
  NumeralChi,
  SentenceChi,
  QuestionChi,
  RelativeChi,
  ConjunctionChi,
  PhraseChi,
  TextChi,
  StructuralChi,
  IdiomChi,
  TenseChi,
  NamesChi
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
