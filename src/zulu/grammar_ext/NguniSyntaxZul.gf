--# -path=.:../zulu:../abstract

resource NguniSyntaxZul =
  GrammarZul,
  ExtraZul ** --- inheriting everything from Grammar, not just Cat and Structural
  NguniSyntax with
    (Grammar=GrammarZul),
    (ExtraZulAbs=ExtraZul),
    (Backward=BackwardZul),
    (TempAbs=TempZul) ;
