--# -path=.:alltenses:prelude:src/hungarian

resource CombinatorsHun = Combinators with
  (Cat = CatHun),
  (Structural = StructuralHun),
  (Noun = NounHun),
  (Constructors = ConstructorsHun) ** open MissingHun in {}  ;
