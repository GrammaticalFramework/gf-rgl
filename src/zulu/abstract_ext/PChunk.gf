abstract PChunk = Cat, CatExt, Symbol [Symb] ** {

cat
  Chunks ;
  Chunk ;
  Chunk_AP ;
  Chunk_Adv ;
  Chunk_S ;
  Chunk_RS ;
  Chunk_QS ;
  Chunk_CN ;
  Chunk_NP ;
  Chunk_N ;
  Chunk_Predet ;
  Chunk_Postdet ;
  Chunk_Symb ;
  VC ;

fun
  OneChunk : Chunk -> Chunks ;
  PlusChunk : Chunk -> Chunks -> Chunks ;
  ChunkPhr : Chunks -> Phr ;

  -- AP_Chunker : Chunk_AP -> Chunk ;
  Adv_Chunker : Chunk_Adv -> Chunk ;
  S_Chunker : Chunk_S -> Chunk ;
  RS_Chunker : Chunk_RS -> Chunk ;
  QS_Chunker : Chunk_QS -> Chunk ;
  CN_Chunker : Chunk_CN -> Chunk ;
  NP_Chunker : Chunk_NP -> Chunk ;
  N_Chunker : Chunk_N -> Chunk ;
  Predet_Chunker : Chunk_Predet -> Chunk ;
  Postdet_Chunker : Chunk_Postdet -> Chunk ;
  Symb_Chunker : Chunk_Symb -> Chunk ;

  -- AP_Chunk  : AP  -> Chunk_AP ;
  Adv_Chunk : Adv -> Chunk_Adv ;
  S_Chunk      : S   -> Chunk_S ;
  RS_Chunk     : RS -> Chunk_RS ;
  QS_Chunk     : QS  -> Chunk_QS ;
  CN_Pl_Chunk  : CN -> Chunk_CN ;
  CN_Sg_Chunk  : CN -> Chunk_CN ;
  NP_Nom_Chunk : NP -> Chunk_NP ;
  NP_Gen_Chunk : NP -> Chunk_NP ;
  Predet_Chunk : Predet -> Chunk_Predet ;
  Postdet_Chunk : Postdet -> Chunk_Postdet ;
  -- N_Sg_Chunk : N -> Chunk_N ;
  -- N_Pl_Chunk : N -> Chunk_N ;

-- for unknown words that are not names

  Symb_Chunk : Symb -> Chunk_Symb ;

-- chunks for punctuation marks
  fullstop_Chunk : Chunk ;
  exclmark_Chunk : Chunk ;
  questmark_Chunk : Chunk ;
  comma_Chunk : Chunk ;
  colon_Chunk : Chunk ;
  semicolon_Chunk : Chunk ;
  quote_Chunk : Chunk ;
  lpar_Chunk : Chunk ;
  rpar_Chunk : Chunk ;
  dash_Chunk : Chunk ;

}
