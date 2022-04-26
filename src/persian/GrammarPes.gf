--# -path=.:../abstract:../common:../prelude

 concrete GrammarPes of Grammar =
  NounPes,
  VerbPes,
  AdjectivePes,
  AdverbPes,
  NumeralPes,
  SentencePes,
  QuestionPes,
  RelativePes,
  ConjunctionPes,

  PhrasePes,
  TextPes,
  StructuralPes,
  TenseX,
  IdiomPes
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

}
