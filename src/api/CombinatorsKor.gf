--# -path=.:alltenses:prelude:src/korean

resource CombinatorsKor = Combinators with
  (Cat = CatKor),
  (Structural = StructuralKor),
  (Noun = NounKor),
  (Constructors = ConstructorsKor) ** open MissingKor in {}  ;
