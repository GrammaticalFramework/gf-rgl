--# -path=.:../abstract:../common:../api:../prelude

concrete NguniLangZul of NguniLang =
  NguniGrammarZul,
  BackwardZul[ComplV2,ComplV3],
  ExtraExtZul,
  NounExtZul,
  VerbExtZul
  -- MonoLexZul,
  -- ChunkZul,
  -- SymbolZul - [Symb],
  -- TempZul
  -- ,ConstructionZul
  -- ,DocumentationZul --# notpresent
  -- ,MarkupZul - [stringMark]
  ** {



} ;
