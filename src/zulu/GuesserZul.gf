concrete GuesserZul of GuesserAbs = CatZul ** open ResZul in {

  lin
    LiteralV1 s = {
      s = s.s ;
      r = RC ;
      syl = SylMult ;
      voice = Active ;
      perfSuff = "ile"
    } ;

    LiteralN1_2 s = string_literal_noun s.s C1_2 ;
    LiteralN3_4 s = string_literal_noun s.s C3_4 ;
    LiteralN5_6 s = string_literal_noun s.s C5_6 ;
    LiteralN7_8 s = string_literal_noun s.s C7_8 ;
    LiteralN9_10 s = string_literal_noun s.s C9_10 ;
    LiteralN11_10 s = string_literal_noun s.s C11_10 ;
    -- LiteralN14 s = string_literal_noun s.s C14 ;
    -- LiteralN15 s = string_literal_noun s.s C15 ;
    -- LiteralN17 s = string_literal_noun s.s C17 ;

  oper
    string_literal_noun : Str -> ClassGender -> N = \s,c -> lin N {
      nom = table {
        Sg => table {
          Full => s ;
          Reduced => s
        } ;
        Pl => table {
          Full => s ;
          Reduced => s
        }
      } ;
      loc = table {
        Sg => s ;
        Pl => s
      } ;
      c = c
    } ;

}
