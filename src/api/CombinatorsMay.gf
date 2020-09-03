--# -path=.:alltenses:prelude

resource CombinatorsMay = Combinators - [ appCN, appCNc ] with 
  (Cat = CatMay),
  (Structural = StructuralMay),
  (Noun = NounMay),
  (Constructors = ConstructorsMay) ** 
  {}
