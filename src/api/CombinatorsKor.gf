--# -path=.:alltenses:prelude:../korean

resource CombinatorsKor = Combinators with
  (Cat = CatKor),
  (Structural = StructuralKor),
  (Noun = NounKor),
  (Constructors = ConstructorsKor) ** open MissingKor in {}  ;
