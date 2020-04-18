--# -path=.:alltenses:prelude

resource CombinatorsSlo = Combinators with 
  (Cat = CatSlo),
  (Structural = StructuralSlo),
  (Constructors = ConstructorsSlo)
    ** open MissingSlo in {}
    

