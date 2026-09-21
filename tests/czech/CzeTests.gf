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
  wash_V : V = seV (krytV "mýt") ;
  learn_V : V = seV (mkV {
    inf = "učit" ;
    pressg1 = "učím" ; pressg2 = "učíš" ; pressg3 = "učí" ;
    prespl1 = "učíme" ; prespl2 = "učíte" ; prespl3 = "učí" ;
    pastpartsg = "učil" ; pastpartpl = "učili" ;
    impsg2 = "uč" ; imppl1 = "učme" ; imppl2 = "učte"
    }) ;
  learn_VV : VV = mkVV learn_V ;
}
