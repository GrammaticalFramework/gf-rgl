concrete ExtendTur of Extend = CatTur ** open ResTur, SuffixTur, HarmonyTur, ParadigmsTur, Prelude, Predef in {

  lincat
    VPS = {s : Agr => Str} ;
    [VPS] = {s : Agr => Ints 4 => Str} ;
    VPI = {s : Str} ;
    [VPI] = {s : Ints 4 => Str} ;
    [Comp] = {s : Aspect => VForm => Ints 4 => Str} ;
    [Imp] = {s : Polarity => Number => Ints 4 => Str} ;
    RNP = {s : Agr => Case => Str} ;
    RNPList = {s : Agr => Case => Ints 4 => Str} ;

  lin
    GenRP n cn = {
      s = cn.gen ! n.n
    } ;

    GenModNP num np cn = {
      s = \\c => np.s ! Gen ++ num.s ! num.n ! c ++ cn.gen ! num.n ! np.a ;
      h = cn.h ;
      a = {n=num.n; p=P3} ;
    } ;

    CompoundN n1 n2 = {
      s = \\n,c => n1.s ! Sg ! Nom ++ n2.s ! n ! c;
      gen = \\n,a => n1.s ! Sg ! Nom ++ n2.gen ! n ! a;
      h = n2.h
    } ;

    CompoundAP n a = {
      s = \\num,c => n.s ! Sg ! Nom ++ a.s ! num ! c ;
      h = a.h
    } ;

    PresPartAP vp = {
      s = \\_,_ => vp.compl ++ vp.s ! Perf ! VImperfPart Pos ;
      h = mkHar I_Har (SCon Soft)
    } ;

    PastPartAP vp = {
      s = \\_,_ => vp.compl ++ mkVerbForms (passiveVerb vp) ! Perf ! VPerfPart Pos ;
      h = vp.h
    } ;

    PastPartAgentAP vp np = {
      s = \\_,_ => np.s ! Nom ++ "tarafından" ++ vp.compl ++
                    mkVerbForms (passiveVerb vp) ! Perf ! VPerfPart Pos ;
      h = vp.h
    } ;

    GerundCN vp = {
      s = \\_,_ => vp.compl ++ vp.s ! Perf ! VInf Pos ;
      gen = \\_,_ => vp.compl ++ vp.s ! Perf ! VInf Pos ;
      h = mkHar I_Har (SCon Soft)
    } ;

    GerundNP vp = {
      s = \\_ => vp.compl ++ vp.s ! Perf ! VInf Pos ;
      h = mkHar I_Har (SCon Soft) ;
      a = agrP3 Sg
    } ;

    GerundAdv vp = {s = vp.compl ++ vp.s ! Perf ! VInf Pos ++ "suretiyle"} ;
    WithoutVP vp = {s = vp.compl ++ vp.s ! Perf ! VInf Neg ++ "suretiyle"} ;
    ByVP vp = {s = vp.compl ++ vp.s ! Perf ! VInf Pos ++ "suretiyle"} ;
    InOrderToVP vp = {s = vp.compl ++ vp.s ! Perf ! VInf Pos ++ "için"} ;

    MkVPS temp pol vp = {
      s = \\agr => vp.compl ++ vp.s ! Perf ! VFin temp.t temp.a pol.p agr
    } ;

    BaseVPS x y = {s = \\a => table {4 => y.s ! a; _ => x.s ! a}} ;
    ConsVPS x xs = {
      s = \\a => table {4 => xs.s ! a ! 4; i => x.s ! a ++ "," ++ xs.s ! a ! i}
    } ;
    ConjVPS conj xs = {s = \\a => xs.s ! a ! conj.sep ++ conj.s ++ xs.s ! a ! 4} ;
    PredVPS np vps = {s = np.s ! Nom ++ vps.s ! np.a} ;

    MkVPI vp = {s = vp.compl ++ vp.s ! Perf ! VInf Pos} ;
    BaseVPI x y = {s = table {4 => y.s; _ => x.s}} ;
    ConsVPI x xs = {s = table {4 => xs.s ! 4; i => x.s ++ "," ++ xs.s ! i}} ;
    ConjVPI conj xs = {s = xs.s ! conj.sep ++ conj.s ++ xs.s ! 4} ;
    ComplVPIVV vv vpi = {s = mkVerbForms vv; compl = vpi.s} ;

    BaseComp x y = {
      s = \\asp,vf => table {4 => y.s ! asp ! vf; _ => x.s ! asp ! vf}
    } ;
    ConsComp x xs = {
      s = \\asp,vf => table {
        4 => xs.s ! asp ! vf ! 4;
        i => x.s ! asp ! vf ++ "," ++ xs.s ! asp ! vf ! i
      }
    } ;
    ConjComp conj xs = {
      s = \\asp,vf => xs.s ! asp ! vf ! conj.sep ++ conj.s ++ xs.s ! asp ! vf ! 4 ;
      compl = []
    } ;

    BaseImp x y = {
      s = \\p,n => table {4 => y.s ! p ! n; _ => x.s ! p ! n}
    } ;
    ConsImp x xs = {
      s = \\p,n => table {4 => xs.s ! p ! n ! 4; i => x.s ! p ! n ++ "," ++ xs.s ! p ! n ! i}
    } ;
    ConjImp conj xs = {
      s = \\p,n => xs.s ! p ! n ! conj.sep ++ conj.s ++ xs.s ! p ! n ! 4
    } ;

    ReflPron = {s = \\_,_ => "kendi"} ;
    ReflPoss num cn = {s = \\agr,_ => cn.gen ! num.n ! agr} ;
    PredetRNP pred rnp = {s = \\a,c => pred.s ++ rnp.s ! a ! c} ;
    AdvRNP np prep rnp = {
      s = \\a,c => np.s ! c ++ prep.s ++ rnp.s ! a ! c
    } ;
    ReflRNP vp rnp = {
      s = mkVerbForms vp ;
      compl = vp.compl ++ rnp.s ! (agrP3 Sg) ! vp.c.c ++ vp.c.s
    } ;
    AdvRVP vp prep rnp = vp ** {
      compl = vp.compl ++ rnp.s ! (agrP3 Sg) ! prep.c ++ prep.s
    } ;
    AdvRAP ap prep rnp = ap ** {
      s = \\n,c => rnp.s ! (agrP3 n) ! prep.c ++ prep.s ++ ap.s ! n ! c
    } ;
    ReflA2RNP ap rnp = ap ** {
      s = \\n,c => rnp.s ! (agrP3 n) ! ap.c.c ++ ap.c.s ++ ap.s ! n ! c
    } ;
    PossPronRNP pron num cn rnp = {
      s = \\c => pron.s ! Gen ++ cn.gen ! num.n ! pron.a ++ rnp.s ! pron.a ! c ;
      h = cn.h ;
      a = agrP3 num.n
    } ;

    Base_rr_RNP x y = {s = \\a,c => table {4 => y.s ! a ! c; _ => x.s ! a ! c}} ;
    Base_nr_RNP x y = {s = \\a,c => table {4 => y.s ! a ! c; _ => x.s ! c}} ;
    Base_rn_RNP x y = {s = \\a,c => table {4 => y.s ! c; _ => x.s ! a ! c}} ;
    Cons_rr_RNP x xs = {
      s = \\a,c => table {4 => xs.s ! a ! c ! 4; i => x.s ! a ! c ++ "," ++ xs.s ! a ! c ! i}
    } ;
    Cons_nr_RNP x xs = {
      s = \\a,c => table {4 => xs.s ! a ! c ! 4; i => x.s ! c ++ "," ++ xs.s ! a ! c ! i}
    } ;
    ConjRNP conj xs = {s = \\a,c => xs.s ! a ! c ! conj.sep ++ conj.s ++ xs.s ! a ! c ! 4} ;

    UseDAP dap = {
      s = \\c => dap.s ! Sg ! c;
      h = mkHar I_Har (SCon Soft);
      a = agrP3 Sg
    } ;
    UseDAPMasc = UseDAP ;
    UseDAPFem = UseDAP ;

    ProgrVPSlash vp = vp ;

    ApposNP np app = np ** {s = \\c => np.s ! c ++ "," ++ app.s ! Nom} ;

    ComplSlashPartLast vp np = {
      s = mkVerbForms vp ;
      compl = vp.compl ++ np.s ! vp.c.c ++ vp.c.s
    } ;

    ComplBareVS vs sent = {s = mkVerbForms vs; compl = sent.s} ;

    EmptyRelSlash cl = {
      s = \\t,a,p,agr => cl.s ! t ! a ! p ++ "olan"
    } ;

    UttVPShort vp = {s = vp.s ! Perf ! VInf Pos} ;

    UttAdV adv = {s = adv.s} ;

    TPastSimple = {s = []} ** {t = Past} ;  --# notpresent

    PositAdVAdj a = {s = a.s ! Sg ! Nom} ;

    PassVPSlash vps = {
      s = mkVerbForms {
            s = vps.stems ! VPass ++ BIND ++ suffixStr vps.h infinitiveSuffix ;
            stems = \\_ => vps.stems ! VPass ;
            aoristType = vps.aoristType ;
            h = vps.h ;
          } ;
      compl = vps.compl
    } ;

    PassAgentVPSlash vps np = {
      s = mkVerbForms {
            s = vps.stems ! VPass ++ BIND ++ suffixStr vps.h infinitiveSuffix ;
            stems = \\_ => vps.stems ! VPass ;
            aoristType = vps.aoristType ;
            h = vps.h ;
          } ;
      compl = np.s ! Nom ++ "tarafından" ++ vps.compl
    } ;

  oper
    passiveVerb : Verb -> Verb = \v -> {
      s = v.stems ! VPass ++ BIND ++ suffixStr v.h infinitiveSuffix;
      stems = \\_ => v.stems ! VPass;
      aoristType = v.aoristType;
      h = v.h
    } ;

}
