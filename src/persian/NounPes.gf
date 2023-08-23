concrete NounPes of Noun = CatPes ** open ResPes, Prelude in {

  flags optimize=all_subs ;

  lin
    DetCN det cn = emptyNP ** cn ** {
      s = \\m =>
        let num : Number = case det.isNum of {
              True  => Sg ; -- noun modified by a number is invariably singular
              False => det.n } ;
            mod : Mod = case <cn.isCmpd,det.mod> of {
              <IsCmpd,Poss> => Ezafe ; -- If CN is compound and Det is poss.pron, don't use poss.suff but Ezafe and full form of pronoun
              _           => det.mod } ;
            detStr : Str = case cn.isCmpd of {
              NotCmpd => det.s ; -- possessive suffix
              IsCmpd  => det.sp } ; -- full form
         in case mod of {
              Bare   => detStr ++ cn.s ! num ! m      ++ cn.compl ! det.n ; -- det doesn't require a special form, keep the Mod=>Str table
              Clitic => detStr ++ cn.s ! num ! Clitic ++ cn.compl ! det.n ; -- In RGL this is only for no_Quant. If other determiners with different word order take Clitic, will change. /IL
              x      => cn.s ! num ! x ++ detStr ++ cn.compl ! det.n } ; -- Ezafe or Poss: comes after noun, Mod table is overwritten to the given form
      a = agrP3 det.n ;
      takesYeAsComp = False ; -- TODO: some rule with det.isDef and cn.hasAdj
      isNeg = det.isNeg ; -- affects polarity in Cl: "*nothing is real" -> "nothing isn't real"
      } ;

    UsePN pn = emptyNP ** pn ** {s = \\_ => pn.s} ;
    UsePron p = emptyNP ** p ** {
       s = \\_ => p.s ;
       clitic = p.ps ;
       isClitic = case p.a of {
                  Ag Sg P3 => True ;
                  _ => False } ;
       animacy = Animate} ;

    PredetNP pred np = np ** {
      s = \\ez => pred.s ++ np.s ! ez
      } ;

    PPartNP np v2 = np ** {
      s = \\ez => np.s ! ez ++ partNP v2
      } ;

    RelNP np rs = np ** {
      s = \\m => case np.relpron of {
              Ance => np.empty      ++ rs2str np.relpron np.a rs ;
              Ke   => np.s ! Clitic ++ rs2str np.relpron np.a rs }
      } ;

    AdvNP np adv = np ** {
      s = \\ez => np.s ! Ezafe ++ adv.s
      } ;

    DetQuantOrd quant num ord =
      let cs : CmpdStatus => Str = case <num.isNum,num.n,quant.isDef> of {
            <True,Sg,False> => \\_ => num.s ++ ord.s ; -- to prevent "a 1"
            _ => \\c => quant.s ! num.n ! c ++ num.s ++ ord.s} ;

      in quant ** {
      s = cs ! NotCmpd ;
      sp = cs ! IsCmpd ; -- only matters for PossPron
      isNum = orB num.isNum ord.isNum ;
      mod = quant.mod ;
      n = case quant.isNeg of {True => Sg ; _ => num.n} ;
      } ;

    DetQuant quant num =
      let cs : CmpdStatus => Str = case <num.isNum,num.n,quant.isDef> of {
            <True,Sg,False> => \\_ => num.s ; -- to prevent "a 1"
            _ => \\c => quant.s ! num.n ! c ++ num.s } ;
      in quant ** {
      s = cs ! NotCmpd ;
      sp = cs ! IsCmpd ; -- only matters for PossPron
      isNum = num.isNum;
      mod = quant.mod ;
      n = case quant.isNeg of {True => Sg ; _ => num.n} ;
      } ;

    DetNP det = emptyNP ** {
      s = \\_ => det.sp ;
      a = agrP3 det.n ;
      hasAdj = False ;
      animacy = Inanimate ;
      --TODO: isClitic, clitic
      relpron = Ance -- TODO check if this works for all Dets
      } ;

    PossPron p = DefArt ** {
       s = \\_ => table {
              NotCmpd => BIND ++ p.ps ;
              IsCmpd  => p.s } ; -- is a compound
       a = p.a ;
       mod = Poss} ; -- is changed into Ezafe in DetCN, if the CN is compound

    NumSg = {s = [] ; n = Sg ; isNum = False} ;
    NumPl = {s = [] ; n = Pl ; isNum = False} ;
-- from here
    NumCard n = n ** {isNum = True} ;

    NumDigits n = n ** {s = n.s ! NCard; isNum = True} ;
    NumDecimal n = n ** {s = n.s ! NCard; isNum = True} ;
    OrdDigits n = n ** {s = n.s ! NOrd ; isNum = True ; isPre=False} ;

    NumNumeral n = n ** {s = n.s ! NCard; isNum = True} ;
    OrdNumeral n = n ** {s = n.s ! NOrd ; isNum = True ; isPre=False} ;
-- to here
    AdNum adn num = num ** {s = adn.s ++ num.s} ;

    OrdSuperl a = {s = a.s ! Comparative ! Bare ++ BIND ++ "ین" ; n = Sg ; isNum=False ; isPre = True} ;

    DefArt = makeQuant [] [] Bare False ;
    IndefArt = makeQuant IndefArticle [] Bare False ** {isDef = False} ;

    MassNP cn = emptyNP ** cn ** {
      s = \\m => cn.s ! Sg ! m ++ cn.compl ! Sg ;
      a = agrP3 Sg ;
      takesYeAsComp = cn.hasAdj ;
      } ;

    UseN,
    UseN2 = useN ;

    Use2N3 n3 = useN n3 ** {
      c2 = n3.c2 ;
      compl = []
      } ;

    Use3N3 n3 = useN n3 ** {
      c2 = n3.c3 ;
      compl = []
      } ;

    ComplN2 n2 np = n2 ** {
      s = \\n => replaceBare n2.c2.mod (n2.s ! n) ;
      compl = \\_ => n2.compl ++ n2.c2.s ++ np2str np ;
      hasAdj = False
     };

    ComplN3 n3 np = n3 ** {
      s = \\n => replaceBare n3.c2.mod (n3.s ! n) ;
      compl = n3.c2.s ++ np2str np ;
      c = n3.c3;
      } ;

    AdjCN ap cn = cn ** {
      s = \\n,m => case ap.isPre of {
              True  => ap.s ! Bare ++ cn.s ! n ! m ;
              False => cn.s ! n ! Ezafe ++ ap.s ! m } ;
      hasAdj = True
     } ;

    RelCN cn rs = cn ** {
      s = \\n,ez => cn.s ! n ! Clitic ;
      compl = \\n => cn.compl ! n ++ rs2str Ke (agrP3 n) rs ;
      } ;

    AdvCN cn ad = cn ** {
      s = \\n,m => cn.s ! n ! Ezafe ;
      compl = \\n => cn.compl ! n ++ ad.s} ;

    SentCN cn sc = cn ** {compl = \\n => cn.compl ! n ++ sc.s} ;

    -- correct for /city Paris/, incorrect for /king John/
    -- ApposNP in ExtendPes works for /king John/ (no ezafe).
    ApposCN cn np = cn ** {s = \\n,m => cn.s ! n ! Ezafe ++ np.s ! m} ;

    --  : CN -> NP -> CN ;     -- house of Paris, house of mine
    PossNP cn np = cn ** {
      s = \\n => replaceBare Ezafe (cn.s ! n) ; -- alternative: place np2str np here for "<house of mine> <on the hill>"
      compl = \\n => cn.compl ! n ++ np2str np } ; -- "<house> <on the hill of mine>"
}
