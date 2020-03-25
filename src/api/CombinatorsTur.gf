--# -path=.:alltenses:prelude

resource CombinatorsTur = Combinators with 
  (Cat = CatTur),
  (Structural = StructuralTur),
  (Noun = NounTur),
  (Constructors = ConstructorsTur) ** 
{
}
