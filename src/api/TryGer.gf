--# -path=.:../german:../common:../abstract:../prelude

resource TryGer = SyntaxGer, ExtraGer, LexiconGer, ParadigmsGer - [mkAdv,mkIAdv], MakeStructuralGer ;
