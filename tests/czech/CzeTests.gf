resource CzeTests = open Prelude, SyntaxCze, SymbolicCze, ParadigmsCze, (L = LexiconCze), (N = NumeralCze), (I = IdiomCze), (E = ExtendCze) in {
oper
  -- Typed consumers exercise every public verb-valency overload.
  drink_V2 : V2 = mkV2 (krytV "pít") ;
  drinkAcc_V2 : V2 = mkV2 (krytV "pít") accusative ;
  drinkFrom_V2 : V2 = mkV2 (krytV "pít") (ParadigmsCze.mkPrep "z" genitive) ;
  buy_V3 : V3 = mkV3 (kupovatV "kupovat") ;
  -- Buying from a source for a recipient: both complements are prepositional.
  buyFor_V3 : V3 = mkV3 (kupovatV "kupovat")
    (ParadigmsCze.mkPrep "od" genitive) (ParadigmsCze.mkPrep "pro" accusative) ;
  like_AP : AP = shortAP "rád" "ráda" "rádo" "rádi" "rády" "ráda" ;
  ready_AP : AP = shortAP "připraven" "připravena" "připraveno" "připraveni" "připraveny" "připravena" ;
}
