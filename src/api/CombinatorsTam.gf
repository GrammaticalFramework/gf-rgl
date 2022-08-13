--# -path=.:alltenses:prelude

resource CombinatorsTam = Combinators - [ appCN, appCNc ] with 
  (Cat = CatTam),
  (Structural = StructuralTam),
  (Noun = NounTam),
  (Constructors = ConstructorsTam) ** 
  {}
}
