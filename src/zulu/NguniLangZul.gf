--# -path=.:../abstract:../common:../api:../prelude

concrete NguniLangZul of NguniLangAbs =
  GrammarZul,
  BackwardZul[ComplV2,ComplV3],
  ExtraCatZul,
  ExtraZul,
  MonoLexZul,
  ChunkZul,
  SymbolZul - [Symb],
  -- GuesserZul,
  TempZul
  -- ,ConstructionZul
  -- ,DocumentationZul --# notpresent
  -- ,MarkupZul - [stringMark]
  ** {



} ;
