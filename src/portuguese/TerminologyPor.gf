--# -path=.:../abstract:../common:../prelude:../romance:../api

concrete TerminologyPor of Terminology = CatPor ** open
  ResPor,
  CommonRomance,
  ParadigmsPor,
  (G = GrammarPor),
  (S = SyntaxPor),
  (L = LexiconPor),
  Prelude

in {
flags coding=utf8 ;


lincat
  Category = G.N ;
  ParameterType = G.N ;
  Parameter = G.N ;
  Modifier = G.A ;

  Heading = {s : Str} ;


lin
  noun_Category = mkN "substantivo" ;
  adjective_Category = mkN "adjetivo" ;
  verb_Category = mkN "verbo" ;
  adverb_Category = mkN "advérbio" ;
  preposition_Category = mkN "preposição" masculine ;

  number_ParameterType = mkN "número" ;
  gender_ParameterType = mkN "gênero" ;
  case_ParameterType = mkN "caso" ;
  person_ParameterType = mkN "pessoa" ;
  tense_ParameterType = mkN "tempo" ;
  degree_ParameterType = mkN "grau" ;
  finite_form_ParameterType = compNN (mkN "forma") (mkN "finita") ;
  nominal_form_ParameterType = compNN (mkN "forma") (mkN "nominal") ;

  singular_Parameter = mkN "singular" ;
  plural_Parameter = mkN "plural" ;

  definite_Parameter = mkN "definido" ;
  indefinite_Parameter = mkN "indefinido" ;

  masculine_Parameter = mkN "masculino" ;
  feminine_Parameter = mkN "feminino" ;
  neuter_Parameter = mkN "neutro" ;
  uter_Parameter = mkN  "neutro" ;

  nominative_Parameter = mkN "nominativo" ;
  genitive_Parameter = mkN "genitivo" ;
  dative_Parameter = mkN "dativo" ;
  accusative_Parameter = mkN "acusativo" ;

  partitive_Parameter = mkN "partitivo" ;
  translative_Parameter = mkN "translativo" ;
  essive_Parameter = mkN "essivo" ;
  inessive_Parameter = mkN "inessivo" ;
  elative_Parameter = mkN "elativo" ;
  illative_Parameter = mkN "ilativo" ;
  adessive_Parameter = mkN "adessivo" ;
  ablative_Parameter = mkN "ablativo" ;
  allative_Parameter = mkN "alativo" ;
  abessive_Parameter = mkN "abessivo" ;
  comitative_Parameter = mkN "comitativo" ;
  instructive_Parameter = mkN "instrutivo" ;
  terminative_Parameter = mkN "terminativo" ;

  imperative_Parameter = mkN "imperativo" ;
  indicative_Parameter = mkN "indicativo" ;
  conjunctive_Parameter = mkN "subjuntivo" ;
  quotative_Parameter = mkN "quotativo" ;
  infinitive_Parameter = mkN "infinitivo" ;

  active_Parameter = mkN "ativa" ;
  passive_Parameter = mkN "passiva" ;

  present_Parameter = mkN "presente" ;
  past_Parameter = mkN "pretérito" ;
  future_Parameter = mkN "futuro" ;
  conditional_Parameter = mkN "condicional" ;
  perfect_Parameter = mkN "perfeito composto" ; ----
  imperfect_Parameter = mkN "imperfeito" ;
  potential_Parameter = mkN "potencial" ;
  supine_Parameter = mkN "supino" ;
  simple_past_Parameter = mkN "perfeito simples" ; ----

  participle_Parameter = mkN "particípio" ;
  aux_verb_Parameter = compNN (mkN "verbo") (mkN "auxiliar") ;
  agent_Parameter = mkN "agente" ;
  gerund_Parameter = mkN "gerúndio" ;

  positive_Parameter = mkN "positivo" ;
  comparative_Parameter = mkN "comparativo" ;
  superlative_Parameter = mkN "superlativo" ;
  predicative_Parameter = mkN "predicativo" ;
  attributive_Parameter = mkN "atributivo" ;
  negative_Parameter = mkN "negativo" ;
  positivePol_Parameter = mkN "positivo" ;

  subject_Parameter = mkN "sujeito" ;
  object_Parameter = mkN "objeto" ;

  person1_Parameter = compNN (mkN "primeira") (mkN "pessoa") ;
  person2_Parameter = compNN (mkN "segunda") (mkN "pessoa") ;
  person3_Parameter = compNN (mkN "terceira") (mkN "pessoa") ;

  short_Parameter = mkN "curto" ;
  long_Parameter = mkN "longo" ;

  finite_Modifier = mkA "finito" ;
  transitive_Modifier = mkA "transitivo" ;
  nominal_Modifier = mkA "nominal" ;

  nounHeading n = ss (n.s ! Sg) ;
  nounPluralHeading n = ss (n.s ! Pl) ;
  modNounHeading a n = ss (n.s ! Sg ++ a.s ! Posit ! genNumPos2Aform n.g Sg False) ;

  exampleGr_N = mkN "exemplo" ;
  formGr_N = mkN "forma" ;


} ;
