--# -path=.:alltenses:prelude

resource CombinatorsHrv = Combinators with 
  (Cat = CatHrv),
  (Structural = StructuralHrv),
  (Constructors = ConstructorsHrv)
    ** open MissingHrv in {}
    

