concrete PChunkZul of PChunk = CatZul, CatExtZul, SymbolZul [Symb] **
  open
    Prelude, ResZul, ParamX,
    (R = ResZul), (P = ParadigmsZul) in {

  lincat
    Chunks = {s : Str} ;
    Chunk = {s : Str};
    Chunk_AP, Chunk_Adv, Chunk_S, Chunk_RS, Chunk_QS, Chunk_CN, Chunk_NP, Chunk_N, Chunk_Symb = {s: Str} ;

    VC = V ;

  lin
    OneChunk c = c ;
    PlusChunk c cs = cc2 c cs ;
    ChunkPhr c = ss ("*" ++ c.s) | c ;

    Adv_Chunker c = c ;
    S_Chunker c = c ;
    RS_Chunker c = c ;
    QS_Chunker c = c ;
    CN_Chunker c = c ;
    NP_Chunker c = c ;
    N_Chunker c = c ;
    Predet_Chunker c = c ;
    Postdet_Chunker c = c ;
    Symb_Chunker c = c ;

    Adv_Chunk a = { s = a.s } ;
    S_Chunk s = { s = s.s } ;
    RS_Chunk rs = { s = rs.s!agr_vars } ;
    QS_Chunk s = { s = s.qword_pre ++ s.s ++ s.qword_post } ;
    CN_Sg_Chunk cn = {
      s = cn.s!Sg!NFull
    } ;
    CN_Pl_Chunk cn = {
      s = cn.s!Pl!NFull
    } ;
    NP_Nom_Chunk np = {
      s = variants {
          np.s!NFull ;
          np.s!NReduced
      }
    } ;
    NP_Gen_Chunk np = {
      s = poss_concord_agr!agr_vars!np.i ++BIND++ np.s!NPoss
    } ;
    Predet_Chunk predet = {
      s = predet.s!agr_vars
    } ;
    Postdet_Chunk postdet = {
      s = postdet.s!agr_vars
    } ;

  -- for unknown words that are not names
    Symb_Chunk symb = { s = symb.s } ;

    fullstop_Chunk = sbSS "." ;
    exclmark_Chunk = sbSS "!" ;
    questmark_Chunk = sbSS "?" ;
    comma_Chunk = sbSS "," ;
    colon_Chunk = sbSS ":" ;
    semicolon_Chunk = sbSS ";" ;
    quote_Chunk = variants {sbSS "\"" ; ss ("\"" ++ SOFT_BIND) } ;
    lpar_Chunk = ss ("(" ++ SOFT_BIND) ;
    rpar_Chunk = sbSS ")" ;
    dash_Chunk = sbSS "-" ;

    oper

      sbSS : Str -> SS = \s -> ss (SOFT_BIND ++ s) ;

      agr_vars : Agr = variants {
        Third C1_2 Sg ;
        Third C1_2 Pl ;
        Third C1a_2a Sg ;
        Third C1a_2a Pl ;
        Third C3_4 Sg ;
        Third C3_4 Pl ;
        Third C5_6 Sg ;
        Third C5_6 Pl ;
        Third C7_8 Sg ;
        Third C7_8 Pl ;
        Third C9_10 Sg ;
        Third C9_10 Pl ;
        Third C11_10 Sg ;
        Third C11_10 Pl ;
        Third C9_6 Sg ;
        Third C9_6 Pl ;
        Third C14 Sg ;
        Third C15 Sg ;
        Third C17 Sg ;
        First Sg ;
        First Pl ;
        Second Sg ;
        Second Pl
      } ;

}
