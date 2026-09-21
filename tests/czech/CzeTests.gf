resource CzeTests = open Prelude, SyntaxCze, SymbolicCze, ParadigmsCze, (L = LexiconCze), (N = NumeralCze), (I = IdiomCze), (E = ExtendCze) in {
oper
  like_AP : AP = shortAP "rád" "ráda" "rádo" "rádi" "rády" "ráda" ;
  ready_AP : AP = shortAP "připraven" "připravena" "připraveno" "připraveni" "připraveny" "připravena" ;
}
