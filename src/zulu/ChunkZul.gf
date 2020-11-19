concrete ChunkZul of Chunk = CatZul, SymbolZul [Symb] **
  -- ChunkFunctor - [Det_Chunk]
  --  with (Syntax = SyntaxZul) ** --, (Extensions = ExtensionsZul) **
  open
    -- SyntaxZul, Prelude, (E = ExtensionsZul),
    Prelude, ResZul, ParamX,
    (R = ResZul), (P = ParadigmsZul) in {

  lincat
    Chunks = {s : Str} ;
    Chunk = {s : Str};

    VC = V ;

  lin
    OneChunk c = c ;
    PlusChunk c cs = cc2 c cs ;

    ChunkPhr c = ss ("*" ++ c.s) | c ;

    AP_Chunk ap = { s = ap_vars ap } ;
  --   AdA_Chunk : AdA -> Chunk ;
  --   Adv_Chunk : Adv -> Chunk ;
  --   AdV_Chunk : AdV -> Chunk ;
  --   AdN_Chunk : AdN -> Chunk ;
    S_Chunk s = { s = variants { s.s!Princ ; s.s!Part ; s.subjs ; s.pots!Princ ; s.pots!Part } } ;
  --   SSlash_Chunk : SSlash -> Chunk ;
    QS_Chunk s = { s = s.qword ++ s.s } ;
  --   CN_Pl_Chunk  : CN -> Chunk ;
    -- CN_Sg_Chunk cn = { s = variants { cn.s!Sg!Full ; cn.loc!Sg } } ;
  --   CN_Pl_Gen_Chunk : CN -> Chunk ;
  --   CN_Sg_Gen_Chunk : CN -> Chunk ;
  --   Conj_Chunk : Conj -> Chunk ;
  --   Det_Chunk : Det -> Chunk ; -- needed if article form is different from NP form, e.g. English a/an
  --   IAdv_Chunk : IAdv -> Chunk ;
  --   IP_Chunk : IP -> Chunk ;
    NP_Nom_Chunk np = { s = variants { np.s!Full ++ np.desc ; np.loc ++ np.desc } } ;
  --   NP_Acc_Chunk : NP -> Chunk ;
  --   NP_Gen_Chunk : NP -> Chunk ;
  --   Numeral_Nom_Chunk : Numeral -> Chunk ;
  --   Numeral_Gen_Chunk : Numeral -> Chunk ;
  --   Ord_Nom_Chunk : Ord -> Chunk ;
  --   Ord_Gen_Chunk : Ord -> Chunk ;
  --   Predet_Chunk : Predet -> Chunk ;
  --   Prep_Chunk   : Prep -> Chunk ;
  --   RP_Nom_Chunk : RP -> Chunk ;
  --   RP_Gen_Chunk : RP -> Chunk ;
  --   RP_Acc_Chunk : RP -> Chunk ;
  --   Subj_Chunk   : Subj -> Chunk ;
  -- --- PConj_Chunk  : PConj -> Chunk ;
  --
  --   VPS_Chunk    : VPS -> Chunk ;
  --   VPI_Chunk    : VPI -> Chunk ;
  --
  -- -- verbs lifted to one cat
  --
  --   V2_V   : V2  -> VC ;
  --   VA_V   : VA  -> VC ;
  --   VQ_V   : VQ  -> VC ;
  --   VS_V   : VS  -> VC ;
  --   VV_V   : VV  -> VC ;
  --
  --   V3_V   : V3  -> VC ;
  --   V2A_V  : V2A -> VC ;
  --   V2Q_V  : V2Q -> VC ;
  --   V2S_V  : V2S -> VC ;
  --   V2V_V  : V2V -> VC ;
  --
  --   UseVC  : Temp -> Pol -> VC -> VPS ;
  --
  -- -- for unknown words that are not names
  --
    Symb_Chunk symb = { s = symb.s } ;
  --
  -- -- syncategorematic chunks
  --   refl_SgP1_Chunk,
  --   refl_SgP2_Chunk,
  --   refl_SgP3_Chunk,
  --   refl_PlP1_Chunk,
  --   refl_PlP2_Chunk,
  --   refl_PlP3_Chunk : Chunk ;
  --   neg_Chunk : Chunk ;
  --   copula_Chunk : Chunk ;
  --   copula_neg_Chunk : Chunk ;
  --   copula_inf_Chunk : Chunk ;
  --   past_copula_Chunk : Chunk ;
  --   past_copula_neg_Chunk : Chunk ;
  --   future_Chunk : Chunk ;
  --   future_neg_Chunk : Chunk ;
  --   cond_Chunk : Chunk ;
  --   cond_neg_Chunk : Chunk ;
  --   perfect_Chunk : Chunk ;
  --   perfect_neg_Chunk : Chunk ;
  --   past_perfect_Chunk : Chunk ;
  --   past_perfect_neg_Chunk : Chunk ;

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
      -- emptyNP = lin NP {
      --   empty = [] ;
      --   s = table {
      --     Full => [] ; Reduced => []
      --   } ;
      --   loc = [] ;
      --   desc = [] ;
      --   agr = agr_vars ;
      --   isPron = True ;
      --   reqLocS = False
      -- } ;
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

      ap_vars : AP -> Str = \ap -> variants {
        ap_form (Third C1_2 Sg) ap ;
        ap_form (Third C1_2 Pl) ap ;
        ap_form (Third C1a_2a Sg) ap ;
        ap_form (Third C1a_2a Pl) ap ;
        ap_form (Third C3_4 Sg) ap ;
        ap_form (Third C3_4 Pl) ap ;
        ap_form (Third C5_6 Sg) ap ;
        ap_form (Third C5_6 Pl) ap ;
        ap_form (Third C7_8 Sg) ap ;
        ap_form (Third C7_8 Pl) ap ;
        ap_form (Third C9_10 Sg) ap ;
        ap_form (Third C9_10 Pl) ap ;
        ap_form (Third C11_10 Sg) ap ;
        ap_form (Third C11_10 Pl) ap ;
        ap_form (Third C9_6 Sg) ap ;
        ap_form (Third C9_6 Pl) ap ;
        ap_form (Third C14 Sg) ap ;
        ap_form (Third C15 Sg) ap ;
        ap_form (Third C17 Sg) ap ;
        ap_form (First Sg) ap ;
        ap_form (First Pl) ap ;
        ap_form (Second Sg) ap ;
        ap_form (Second Pl) ap
      } ;

      ap_form : Agr -> AP -> Str = \agr,ap ->
      let
        agr = agr_vars ;
        adjf = case ap.b of {
          True => (aformN agr) ;
          -- True => AF2 ;
          False => AF1
        }
      in
        adjConcLookup!agr!adjf ++BIND++ ap.s!adjf ;

}
