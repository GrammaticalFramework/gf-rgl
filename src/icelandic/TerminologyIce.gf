--# -path=.:../abstract:../common

concrete TerminologyIce of Terminology = CatIce ** open 
  ResIce,
  ParadigmsIce,
  (G = GrammarIce),
  (S = SyntaxIce),
  (L = LexiconEng),
  Prelude
in {


lincat
  Category = G.N ;
  ParameterType = G.N ;
  Parameter = G.N ;
  Modifier = G.A ;

lin
  noun_Category = mkN "nafnorð" ;
  adjective_Category = mkN "lýsingarorð" ;
  verb_Category = mkN "sagnorð" ;
  adverb_Category = mkN "atviksorð" ;
  preposition_Category = mkN "preposition" ;
  numeral_Category = mkN "töluorð" ;
  pronoun_Category = mkN "persónufornafn" ;
  determiner_Category = mkN "determiner" ;
  article_Category = mkN "greinir" ;

  number_ParameterType = mkN "tala" ;
  gender_ParameterType = mkN "kyn" ;
  case_ParameterType   = mkN "fall" ;
  person_ParameterType = mkN "persóna" ;
  tense_ParameterType  = mkN "tíð" ;
  degree_ParameterType = mkN "" ;

  singular_Parameter = mkN "eintala" ;
  plural_Parameter = mkN "fleirtala" ;

  masculine_Parameter = mkN "karlkyn" ;
  feminine_Parameter = mkN "kvenkyn" ;
  neuter_Parameter = mkN "hvorugkyn" ;
--  uter_Parameter = mkN "uter" ;

  nominative_Parameter = mkN "nefnifall" ;
  genitive_Parameter = mkN "eignarfall" ;
  dative_Parameter = mkN "þágufall" ;
  accusative_Parameter = mkN "þolfall" ;
{-
  partitive_Parameter = mkN "partitive" ;
  translative_Parameter = mkN "translative" ;
  essive_Parameter = mkN "essive" ;
  inessive_Parameter = mkN "inessive" ;
  elative_Parameter = mkN "elative" ;
  illative_Parameter = mkN "illative" ;
  adessive_Parameter = mkN "adessive" ;
  ablative_Parameter = mkN "ablative" ;
  allative_Parameter = mkN "allative" ;
  abessive_Parameter = mkN "abessive" ;
  comitative_Parameter = mkN "comitative" ;
  instructive_Parameter = mkN "instructive" ;
-}

  active_Parameter = mkN "germynd" ;
  passive_Parameter = mkN "miðmynd" ;
  middle_Parameter = mkN "þolmynd" ;
  
  imperative_Parameter = mkN "boðháttur" ;
  indicative_Parameter = mkN "framsöguháttur" ;
  conjunctive_Parameter = mkN "viðtengingarháttur" ;
  infinitive_Parameter = mkN "nafnháttur" ;

  definite_Parameter = mkN "ákveðinn" ;
  indefinite_Parameter = mkN "óákveðinn" ;

  present_Parameter = mkN "nútíð" ;
  past_Parameter = mkN "þátíð" ;
--  future_Parameter = mkN "future" ;
--  conditional_Parameter = mkN "conditional" ;
--  potential_Parameter = mkN "potential" ;
--  perfect_Parameter = mkN "perfect" ;
--  imperfect_Parameter = mkN "imperfect" ;
  supine_Parameter = mkN "sagnbót" ;
--  agent_Parameter = mkN "agent" ;
--  simple_past_Parameter = mkN "simple past" ;

  participle_Parameter = mkN "lýsingarháttur" ;
 -- aux_verb_Parameter = mkN "auxiliary" ;
 -- gerund_Parameter = mkN "Gerund" ;

  positive_Parameter = mkN "frumstig" ;
  comparative_Parameter = mkN "miðstig" ;
  superlative_Parameter = mkN "efsta stig" ; ----
--  predicative_Parameter = mkN "predicative" ;
--  negative_Parameter = mkN "negative" ;

  short_Parameter = mkN "short" ;
  long_Parameter = mkN "long" ;

  strong_Parameter = mkN "sterk" ; --- sterk beyging
  weak_Parameter = mkN "veik" ;

  nounHeading n = ss (n.s ! Sg ! Free ! Nom) ;
  nounPluralHeading n = ss (n.s ! Pl ! Free ! Nom) ;

  exampleGr_N = mkN "example" ;
  formGr_N = mkN "form" ;


}