concrete NounExtZul of NounExt = CatZul,ExtraCatZul ** open ResZul, Prelude, ParamX in {

  lin

    -- Quant is used for demonstratives, and QuantStem for all/only

    PronPostdetNP pron postdet = {
      empty = pron.empty ;
      s = \\nform => pron.s!nform ++ postdet.s!pron.agr ;
      agr = pron.agr ;
      i = RC ;
      proDrop = pron.proDrop ;
      isPron = True
    } ;

    QuantPostdet q = {
      s = \\a => quantConc!a ++BIND++ q.s
    } ;

    DemPostdet q = {
      s = \\a => dem_pron!q.dist!a ++ q.s
    } ;

    QuantDemPostdet q d = {
      s = \\a => quantConc!a ++BIND++ q.s ++ dem_pron!d.dist!a ++ d.s
    } ;

    DemQuantPostdet d q = {
      s = \\a => dem_pron!d.dist!a ++ quantConc!a ++BIND++ q.s ++ d.s
    } ;

    DetNum n = n ** { spec = Spec } ;

    NonspecDet n = n ** { spec = Nonspec } ;

    PostdetCN cn postdet det = let
      agr = Third cn.c det.n
    in {
      empty = cn.empty ;
      -- s = \\nform => cn.s!det.n!nform ++ cn.mod!det.n ++ postdet.s!agr ;
      s = \\nform => cn.s!det.n!nform ++ postdet.s!agr ++ det.s ;
      agr = agr ;
      i = nominit!agr ;
      proDrop = False ;
      isPron = False
    } ;

    RelN rs n = {
      empty = n.empty ;
      s = \\num,nform => rs.s!(Third n.c num) ++ n.s!num!nform ;
      -- mod = \\_ => [] ;
      c = n.c
    } ;

    -- TODO : check mod
    ApposCN cn n = {
      s = \\num,nform => cn.s!num!nform ++ n.s!num!nform ;
      -- mod = \\num => n.s!num!Full ++ cn.mod!num ;
      c = cn.c ; -- takes agr of cn
      empty = cn.empty
    } ;

    -- TODO : check mod
    ApposN cn n = {
      s = \\num,nform => cn.s!num!nform ++ n.s!num!nform ;
      -- mod = \\num => n.s!num!Full ++ cn.mod!num ;
      c = n.c ; -- takes agr of n
      empty = cn.empty
    } ;

    PredetN predet n = {
      s = case predet.hasDem of {
        True => \\num => table {
          Full | Reduced | Poss => predet.s!(Third n.c num) ++ n.s!num!Reduced ;
          Loc => "ku" ++BIND++ predet.s!(Third n.c num) ++ n.s!num!Reduced
        } ;
        False => \\num,nform => predet.s!(Third n.c num) ++ n.s!num!nform
      } ;
      -- mod = \\_ => [] ;
      c = n.c ;
      empty = n.empty
    };

    QuantPredet q = {
      s = \\a => quantConc!a ++BIND++ q.s ;
      hasDem = False
    } ;

    DemPredet q = {
      s = \\a => dem_pron!q.dist!a ++ q.s ;
      hasDem = True
    } ;

    QuantDemPredet q d = {
      s = \\a => quantConc!a ++BIND++ q.s ++ dem_pron!d.dist!a ++ d.s ;
      hasDem = True
    } ;

    EmphCN cn = {
      s = \\num => table {
        Full => pron_stem!(Third cn.c num) ++BIND++ "na" ++ cn.s!num!Full ;
        Reduced => pron_stem!(Third cn.c num) ++ cn.s!num!Full ;
        Poss => poss_pron_stem!(Third cn.c num) ++ cn.s!num!Full ;
        Loc => "ku" ++BIND++ pron_stem!(Third cn.c num) ++ cn.s!num!Full
      } ;
      -- mod = cn.mod ;
      c = cn.c ;
      empty = cn.empty
    } ;

    ContrastCN cn = {
      s = \\num,nform => cn.s!num!nform ++ pron_stem!(Third cn.c num) ++BIND++ "na" ;
      -- mod = \\num => pron_stem!(Third cn.c num) ++BIND++ "na" ++ cn.mod!num ;
      c = cn.c ;
      empty = cn.empty
    } ;

    UsePNPl pn = let
      agr = Third pn.c Pl
    in {
      empty = pn.empty ;
      s = pn.s!Pl ;
      agr = agr ;
      i = nominit!agr ;
      proDrop = False ;
      isPron = False
    } ;

    Deverb15 v =
    let
      agr = Third C15 Sg ;
    in
    {
      s = \\_ => table {
        Full => case v.r of {
          RC => "uku"++BIND++(v.s!R_a) ;
          (RA|RE) => "ukw"++BIND++(v.s!R_a) ;
          _ => "uk"++BIND++(v.s!R_a)
        } ;
        Reduced => case v.r of {
          RC => "ku"++BIND++(v.s!R_a) ;
          (RA|RE) => "kw"++BIND++(v.s!R_a) ;
          _ => "k"++BIND++(v.s!R_a)
        } ;
        Poss => case v.r of {
          RC => "ku"++BIND++(v.s!R_a) ;
          (RA|RE) => "kw"++BIND++(v.s!R_a) ;
          _ => "k"++BIND++(v.s!R_a)
        } ;
        Loc => case v.r of {
          RC => "eku"++BIND++(v.s!R_e)++BIND++"ni" ;
          (RA|RE) => "ekw"++BIND++(v.s!R_e)++BIND++"ni" ;
          _ => "ek"++BIND++(v.s!R_e)++BIND++"ni"
        }
      } ;
      c = C15 ;
      empty = []
    } ;

}
