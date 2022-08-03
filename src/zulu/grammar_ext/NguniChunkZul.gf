--# -path=.:../abstract:../common:../api:../prelude

concrete NguniChunkZul of NguniChunk =
  NguniGrammarZul,
  BackwardZul[ComplV2,ComplV3],
  ExtraExtZul,
  NounExtZul,
  VerbExtZul,
  PhraseExtZul,
  -- MonoLexZul,
  PChunkZul,
  SymbolZul - [Symb]
  -- TempZul
  -- ,ConstructionZul
  -- ,DocumentationZul --# notpresent
  -- ,MarkupZul - [stringMark]
  ** {



} ;
