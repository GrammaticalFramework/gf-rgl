concrete NounZul of Noun = CatZul ** open ResZul, Prelude, ParamX in {

  flags optimize=all_subs ;

  lin
    -- TODO: check refactor
    DetCN det cn = let
      agr = Third cn.c det.n ;
    in {
      empty = cn.empty ;
      s = \\nform => det.s ++ cn.s ! det.n ! nform ++ cn.mod ! det.n;
      agr = agr ;
      i = nominit!agr ;
      proDrop = False ;
      isPron = False -- ;
      -- reqLocS = True ; -- TODO: change if a Det is ever added that has a non-empty string
      -- qdef = det.qdef ;
    } ;

    -- TODO: check refactor
    UsePN pn = let
      agr = Third pn.c Sg ;
    in {
      empty = pn.empty ;
      s = pn.s!Sg ;
      agr = agr ;
      i = nominit!agr ;
      proDrop = False ;
      isPron = False
    } ;

    -- TODO: check refactor
    UsePron pron = {
      empty = pron.empty ;
      s = case pron.proDrop of {
        False => pron.s ;
        True => table {
          Full => pron.empty ;
          Reduced => pron.s!Reduced ;
          Poss => pron.s!Poss ;
          Loc => pron.s!Loc
        }
      } ;
      agr = pron.agr ;
      i = RC ;
      proDrop = pron.proDrop ;
      isPron = True
    } ;

    -- PredetNP, PPartNP, AdvNP, ExtAdvNP : not implemented

    -- TODO: refactor
    RelNP np rs = {
      empty = np.empty ;
      s = \\nform => np.s!nform ++ rs.s!np.agr ;
      agr = np.agr ;
      i = np.i ;
      proDrop = False ; -- probably right?
      isPron = np.isPron
    } ;

    -- DetNP, DetQuant, DetQuantOrd : not implemented

    NumSg = { s = [] ; n = Sg } ;
    NumPl = { s = [] ; n = Pl } ;

    -- NumCard, NumNumeral, AdNum, OrdNumeral, OrdSuperl, OrdNumeralSuperl : not implemented
    -- NumDigits, OrdDigits : not yet implemented

    -- DefArt, IndefArt, MassNP, PossPron : not implemented

    -- TODO: check refactor (no change?)
    UseN n = n ** { mod = \\_ => [] } ;

    -- ComplN2, ComplN3, UseN2, Use2N3, Use3N3 : not implemented

    -- AdjCN, RelCN, AdvCN, SentCN, ApposCN : not implemented

    -- flashing of the lights / ukukhanya kwezibani
    -- TODO: check refactor (no change?)
    PossNP cn np = {
      empty = cn.empty ;
      s = \\n,nform => cn.s!n!nform ;
      mod = \\num => cn.mod!num ++ poss_concord!cn.c!num!np.i ++BIND++ (poss_NP np) ;
      c = cn.c
    } ;

    -- PartNP, CountNP, AdjDAP, DetDAP : not implemented

}
