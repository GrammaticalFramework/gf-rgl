--# -path=.:../arabic:../common:../abstract:../prelude:../morphodict

resource TryAra = SyntaxAra, LexiconAra, MorphoDictAra, ParadigmsAra - [mkAdN, mkAdv,mkOrd,mkQuant] ** 
  open (P = ParadigmsAra) in {

}
