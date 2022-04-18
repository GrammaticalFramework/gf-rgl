concrete NounEst of Noun = CatEst ** open ResEst, HjkEst, MorphoEst, Prelude in {

  flags optimize=all_subs ; coding=utf8;

  lin

-- The $Number$ is subtle: "nuo autot", "nuo kolme autoa" are both plural
-- for verb agreement, but the noun form is singular in the latter.

    DetCN det cn =
      let
        n : Number = case det.isNum of {
          True => Sg ;
          _ => det.n
          } ;
        ncase : NPForm -> Case * NForm = \c ->
          let k = npform2case n c
          in
          case <n, c, det.isNum, det.isDef> of {
            <_, NPAcc,      True,_>  => <Nom,NCase Sg Part> ; -- kolm kassi (as object)
            <_, NPCase Nom, True,_>  => <Nom,NCase Sg Part> ; -- kolm kassi (as subject)
            <_, _, True,_>           => <k,  NCase Sg k> ;     -- kolmeks kassiks (all other cases)
            _                        => <k,  NCase n k>        -- kass, kassi, ... (det is not a number)
            }
      in cn ** {
      s = \\c => let
                   k = ncase c ;
                 in
                 det.s ! k.p1 ++ cn.s ! k.p2 ;
      a = agrP3 det.n ;
--	(case det.isNum of {
--            True => Sg ;
--            _ => det.n
--            }) ;
      isPron = False
      } ;

    DetNP det =
      let
        n : Number = case det.isNum of {
          True => Sg ;
          _ => det.n
          } ;
      in emptyNP ** {
        s = \\c => let k = npform2case n c in
                 det.sp ! k ;
        a = agrP3 (case det.isDef of {
            False => Sg ;  -- autoja menee; kolme autoa menee
            _ => det.n
            }) ;
        isPron = False
      } ;

    UsePN pn = emptyNP ** {
      s = \\c => pn.s ! npform2case Sg c ;
      a = agrP3 Sg ;
      isPron = False
      } ;
    UsePron p = p ** {isPron = True ; postmod = []} ;

    PredetNP pred np = np ** {
      s = \\c => pred.s ! complNumAgr np.a ! c ++ np.s ! c ;
      } ;

    PPartNP np v2 =
      let
        num : Number     = complNumAgr np.a ;
        part : Str       = v2.s ! (PastPart Pass) ;
      in np ** {postmod = np.postmod ++ part} ;

    AdvNP np adv = np ** {postmod = np.postmod ++ adv.s} ;

    DetQuantOrd quant num ord = {
      s = \\c => quant.s ! num.n ! c ++ num.s ! Sg ! c ++ ord.s ! NCase num.n c ;
      sp = \\c => quant.sp ! num.n ! c ++ num.s ! Sg ! c ++ ord.s ! NCase num.n c ;
      n = num.n ;
      isNum = num.isNum ;
      isDef = quant.isDef
      } ;

    DetQuant quant num = {
      s = \\c => quant.s ! num.n ! c ++ num.s ! Sg ! c ;
      sp = \\c => quant.sp ! num.n ! c ++ num.s ! Sg ! c ;
      n = num.n ;
      isNum = num.isNum ; -- case num.n of {Sg => False ; _ => True} ;
      isDef = quant.isDef
      } ;

    DetDAP det = det ;

    AdjDAP dap ap = dap ** {
      s = \\c => dap.s ! c ++
                   case ap.infl of {
                     Regular => ap.s ! True ! NCase dap.n c ;
                     _ => ap.s ! True ! NCase dap.n Nom  ---- participle
                 } ;
      sp = \\c => dap.sp ! c ++
                   case ap.infl of {
                     Regular => ap.s ! True ! NCase dap.n c ;
                     _ => ap.s ! True ! NCase dap.n Nom  ---- participle
                 } ;
      } ;

    PossPron p = {
      s,sp = \\_,_ => p.s ! NPCase Gen ;
      isNum = False ;
      isDef = True  --- "minun kolme autoani ovat" ; thus "...on" is missing
      } ;

    PossNP cn np = np ** {s = \\nf => linNP (NPCase Gen) np ++ cn.s ! nf} ;

    NumSg = {s = \\_,_ => [] ; isNum = False ; n = Sg} ;
    NumPl = {s = \\_,_ => [] ; isNum = False ; n = Pl} ;

    NumCard n = n ** {isNum = case n.n of {Sg => False ; _ => True}} ;  -- üks raamat/kaks raamatut

    NumDigits numeral = {
      s = \\n,c => numeral.s ! NCard (NCase n c) ;
      n = numeral.n
      } ;
    OrdDigits numeral = {s = \\nc => numeral.s ! NOrd nc} ;

    NumNumeral numeral = {
      s = \\n,c => numeral.s ! NCard (NCase n c) ;
      n = numeral.n
      } ;
    OrdNumeral numeral = {s = \\nc => numeral.s ! NOrd nc} ;

    AdNum adn num = {
      s = \\n,c => adn.s ++ num.s ! n ! c ;
      n = num.n
      } ;

    -- OrdSuperl a = {s = \\nc => a.s ! Superl ! AN nc} ;
    -- TODO: it is more robust to use: kõige + Compar
    OrdSuperl a = {s = \\nc => "kõige" ++ a.s ! Compar ! AN nc} ;

    DefArt = {
      s = \\_,_ => [] ;
      sp = table {Sg => pronSe.s ; Pl => pronNe.s} ;
      isNum = False ;
      isDef = True   -- autot ovat
      } ;

    IndefArt = {
      s = \\_,_ => [] ; --use isDef in DetCN
      sp = \\n,c =>
         (nForms2N (nForms6 "üks" "ühe" "üht" "ühesse" "ühtede"
         "ühtesid")).s ! NCase n c ;
      isNum,isDef = False -- autoja on
      } ;

    MassNP cn =
      let
        n : Number = Sg ;
        ncase : Case -> NForm = \c -> NCase n c ;
      in cn ** {
        s = \\c => let k = npform2case n c in
                cn.s ! ncase k ;
        a = agrP3 Sg ;
        isPron = False
      } ;

    UseN n = emptyCN ** {
      s = n.s
      } ;

    UseN2 n = n ;

    Use2N3 f = f ** {
      postmod = []
      } ;
    Use3N3 f = f ** {
      c2 = f.c3 ;
      isPre = f.isPre2 ;
      postmod = []
      } ;

    ComplN2 f x = let compl : Str = appCompl True Pos f.c2 x in {
      s = \\nf => case f.isPre of {
            True => f.s ! nf ;         -- N2 is pre, so compl goes into postmod
            False => compl ++ f.s ! nf -- N2 isn't pre, compl goes in s before the N2
            } ;
      postmod = f.postmod ++ if_then_Str f.isPre compl []
      } ;

    -- N2 is subtype of CN, so we can reuse result of ComplN2 as a base for our CN.
    -- The decision of noun-complement order is only done once, in ComplN2.
    ComplN3 f x = let cn : CN = ComplN2 (Use2N3 f) x in cn ** {
      c2 = f.c3 ;
      isPre = f.isPre2
      } ;

    AdjCN ap cn = cn ** {
      s = \\nf =>
        case ap.infl of {
          Invariable|Participle => ap.s ! True ! NCase Sg Nom ++ cn.s ! nf ; --valmis kassile; väsinud kassile
          Regular => ap.s ! True ! nf ++ cn.s ! nf -- Ess,Abess,Comit,Termin will only get case ending after the CN, so suure kassiga, not *suurega kassiga
          }
      } ;

    RelCN cn rs = cn ** { -- exception to postmod rule, because RS depends on Agr
      s = \\nf => cn.s ! nf ++ rs.s ! agrP3 (numN nf)
      } ;

    RelNP np rs = np ** {
      postmod = np.postmod ++ "," ++ rs.s ! np.a ;
      isPron = np.isPron ---- correct ?
      } ;

    AdvCN cn ad = cn ** {postmod = cn.postmod ++ ad.s} ;

    SentCN cn sc = cn ** {postmod = cn.postmod ++ sc.s} ;

    ApposCN cn np = cn ** {postmod = cn.postmod ++ linNP (NPCase Nom) np} ; --- luvun x

  oper
    numN : NForm -> Number = \nf -> case nf of {
      NCase n _ => n ;
      _ => Sg ---
      } ;


}
