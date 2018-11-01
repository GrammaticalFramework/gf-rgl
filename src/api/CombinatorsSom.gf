--# -path=.:alltenses:prelude:src/somali

resource CombinatorsSom = Combinators with
  (Cat = CatSom),
  (Structural = StructuralSom),
  (Noun = NounSom),
  (Constructors = ConstructorsSom) ** open MissingSom in {}  ;
