--# -path=.:../abstract:../common

concrete TerminologyRus of Terminology = CatRus ** open
  ResRus,
  ParadigmsRus,
  (G = GrammarRus),
  (S = SyntaxRus),
  (L = LexiconRus),
  Prelude,
  HTML
in {
flags coding=utf8 ;

lincat
  Category = G.N ;
  ParameterType = G.N ;
  Parameter = G.N ;
  Modifier = G.A ;
  Heading = {s : Str} ;

lin
  noun_Category = mkN (mkA "существительный") neuter inanimate ;
  adjective_Category = mkN (mkA "прилагательный") neuter inanimate ;
  verb_Category = mkN "глагол" ;
  adverb_Category = mkN "наречие" ;
  preposition_Category = mkN "предлог" ;

  finite_form_ParameterType = mkN (mkN (mkA "личный" "" "1*a") feminine inanimate) "" (mkN "форма") ;
  nominal_form_ParameterType = mkN (mkN (mkA "субстантивный" "" "1*a") feminine inanimate) "" (mkN "форма") ;

  singular_Parameter = mkN "ед.ч." ;
  plural_Parameter = mkN "мн.ч." ;

  masculine_Parameter = mkN "мужской" ;
  feminine_Parameter = mkN "женский" ;
  neuter_Parameter = mkN "средний" ;

  nominative_Parameter = mkN "именительный" ;
  genitive_Parameter = mkN "родительный" ;
  dative_Parameter = mkN "дательный" ;
  accusative_Parameter = mkN "винительный" ;

  partitive_Parameter = mkN "разделительный" ;
  translative_Parameter = mkN "становительный" ;
  essive_Parameter = mkN "эссив" ;
  inessive_Parameter = mkN "инессив" ;
  elative_Parameter = mkN "элатив" ;
  illative_Parameter = mkN "иллатив" ;
  adessive_Parameter = mkN "адессив" ;
  ablative_Parameter = mkN "аблатив" ;
  allative_Parameter = mkN "аллатив" ;
  abessive_Parameter = mkN "абессив" ;
  comitative_Parameter = mkN "комитатив" ;
  instructive_Parameter = mkN "инструктив" ;

  active_Parameter = mkN "автивный" ;
  passive_Parameter = mkN "пассивный" ;

  imperative_Parameter = mkN "повелительное" ;
  indicative_Parameter = mkN "изъявительное" ;
  conjunctive_Parameter = mkN "сослагательное" ;
  infinitive_Parameter = mkN "инфинитив" ;

  present_Parameter = mkN "настоящее" ;
  past_Parameter = mkN "прошлое" ;
  future_Parameter = mkN "будущее" ;
  conditional_Parameter = mkN "условное" ;
  perfect_Parameter = mkN "перфект" ;
  potential_Parameter = mkN "потенциал" ;

  participle_Parameter = mkN "причастие" ;
  aux_verb_Parameter = mkN "глагол" ;
  agent_Parameter = mkN "агенс" ;

  positive_Parameter = mkN "положительная" ;
  comparative_Parameter = mkN "сравнительная" ;
  superlative_Parameter = mkN "превосходная" ;
  predicative_Parameter = mkN "предикатив" ;
  negative_Parameter = mkN "отрицательное" ;
  positivePol_Parameter = mkN "утвердительное" ;

  gender_ParameterType = mkN "род" ;
  number_ParameterType = mkN "число" ;
  person_ParameterType = mkN "лицо" ;

  person1_Parameter = mkN "1 л." ;
  person2_Parameter = mkN "2 л." ;
  person3_Parameter = mkN "3 л." ;

  long_Parameter = mkN "длинная" ;
  short_Parameter = mkN "краткая" ;

  finite_Modifier = mkA "финитный" ;

  nounHeading n = ss n.snom ;
  nounPluralHeading n = ss n.pnom ;

  formGr_N = mkN "форма" ;
  exampleGr_N = mkN "пример" ;

}
