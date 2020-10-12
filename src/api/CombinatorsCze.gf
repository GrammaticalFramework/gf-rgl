--# -path=.:alltenses:prelude

resource CombinatorsCze = Combinators with 
  (Cat = CatCze),
  (Structural = StructuralCze),
  (Constructors = ConstructorsCze)
    ** open MissingCze in {}
    

