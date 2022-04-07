--# -path=.:api
concrete ConstructionLat of Construction = CatLat ** 
  open SyntaxLat, SymbolicLat, ParadigmsLat, 
       (L = LexiconLat), (E = ExtraLat), (G = GrammarLat), (I = IrregLat), (R = ResLat), (N = NounLat), Prelude in {
}