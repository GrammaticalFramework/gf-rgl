concrete NounZul of Noun = CatZul ** open ResZul, Prelude, ParamX in {

  flags optimize=all_subs ;

  lin
    -- TODO: check refactor
    DetCN det cn = let
      agr = Third cn.c det.n ;
    in {
      empty = cn.empty ;
      s = \\nform => det.s ++ cn.s ! det.n ! nform ;
      agr = agr ;
      i = case cn.emph of {
        False => nominit!agr ;
        True => RC
      } ;
      proDrop = False ;
      isPron = False ;
      heavy = True
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
      isPron = False ;
      heavy = True
    } ;

    -- TODO: check refactor
    UsePron pron = {
      empty = pron.empty ;
      -- s = case pron.proDrop of {
      --   False => pron.s ;
      --   True => table {
      --     NFull => pron.empty ;
      --     NReduced => pron.s!NReduced ;
      --     NPoss => pron.s!NPoss ;
      --     NLoc => pron.s!NLoc
      --   }
      -- } ;
      s = pron.s ;
      agr = pron.agr ;
      i = RC ;
      proDrop = pron.proDrop ;
      isPron = True ;
      heavy = case pron.proDrop of {
        True => False ;
        False => True
      }
    } ;

    -- PredetNP, PPartNP, AdvNP, ExtAdvNP : not implemented

    -- TODO: refactor
    RelNP np rs = {
      empty = np.empty ;
      s = \\nform => np.s!nform ++ rs.s!np.agr ;
      agr = np.agr ;
      i = np.i ;
      proDrop = False ; -- probably right?
      isPron = np.isPron ;
      heavy = True
    } ;

    -- DetNP, DetQuant, DetQuantOrd : not implemented

    NumSg = { s = [] ; n = Sg } ;
    NumPl = { s = [] ; n = Pl } ;

    -- NumCard, NumNumeral, AdNum, OrdNumeral, OrdSuperl, OrdNumeralSuperl : not implemented
    -- NumDigits, OrdDigits : not yet implemented

    -- DefArt, IndefArt, MassNP, PossPron : not implemented

    -- TODO: check refactor (no change?)
    UseN n = n ** { emph = False } ;

    -- ComplN2, ComplN3, UseN2, Use2N3, Use3N3 : not implemented

    -- AdjCN, RelCN, AdvCN, SentCN, ApposCN : not implemented

    -- flashing of the lights / ukukhanya kwezibani
    -- TODO: check refactor (no change?)
    PossNP cn np = {
      empty = cn.empty ;
      s = \\num,nform => cn.s!num!nform ++ poss_concord!cn.c!num!np.i ++BIND++ np.s!NPoss;
      -- mod = \\num => cn.mod!num ++ poss_concord!cn.c!num!np.i ++BIND++ (poss_NP np) ;
      c = cn.c ;
      emph = cn.emph
    } ;

    -- PartNP, CountNP, AdjDAP, DetDAP : not implemented

}
