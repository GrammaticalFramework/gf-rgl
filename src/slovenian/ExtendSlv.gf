--# -path=.:../abstract:../common:prelude
concrete ExtendSlv of Extend = CatSlv ** open ResSlv, ParadigmsSlv, GrammarSlv, (P=ParamX), Prelude in {

lincat
  VPS = {s : Agr => Str} ;
  [VPS] = {s1,s2 : Agr => Str} ;
  VPI = {s : Agr => Str} ;
  [VPI] = {s1,s2 : Agr => Str} ;
  VPS2 = VPSlash ;
  [VPS2] = {s1,s2 : Agr => Str; c2 : Prep} ;
  VPI2 = VPSlash ;
  [VPI2] = {s1,s2 : Agr => Str; c2 : Prep} ;
  [Comp] = {s1,s2 : Agr => Str} ;
  [Imp] = {s1,s2 : P.Polarity => Gender => Number => Str} ;
  RNP = {s : Agr => Case => Str} ;
  RNPList = {s1,s2 : Agr => Case => Str} ;

lin
  UttAdV adv = {s = adv.s} ;

  GenModNP num np cn = {
    s = \\c => np.s ! Gen ++ cn.s ! Indef ! c ! (numAgr2num ! num.n);
    a={g=agender2gender cn.g;n=numAgr2num ! num.n;p=P3}; isPron=False
  } ;
  CompBareCN cn = {s = \\a => cn.s ! Indef ! Nom ! a.n} ;
  EmptyRelSlash cl = {s=\\_,t,a,p => cl.s!t!a!p} ;

  MkVPS t p vp = {s = \\a => predV False vp.isCop vp.refl vp.s ! t.t ! p.p ! a ++ vp.s2 ! a} ;
  BaseVPS x y = {s1=x.s;s2=y.s} ;
  ConsVPS x xs = {s1=\\a => x.s ! a ++ "," ++ xs.s1 ! a;s2=xs.s2} ;
  ConjVPS conj xs = {s=\\a => xs.s1 ! a ++ conj.s ++ xs.s2 ! a} ;
  PredVPS np vps = {s=np.s ! Nom ++ vps.s ! np.a} ;
  SQuestVPS np vps = {s=np.s ! Nom ++ vps.s ! np.a} ;
  QuestVPS ip vps = {s=ip.s ! Nom ++ vps.s ! ip.a} ;
  RelVPS rp vps = {s=\\a => rp.s ! inanimateGender a.g ! Nom ! a.n ++ vps.s ! a} ;

  MkVPI vp = {s=\\a => vp.s ! P.Pos ! VInf ++ vp.refl ++ vp.s2 ! a} ;
  BaseVPI x y = {s1=x.s;s2=y.s} ;
  ConsVPI x xs = {s1=\\a => x.s ! a ++ "," ++ xs.s1 ! a;s2=xs.s2} ;
  ConjVPI conj xs = {s=\\a => xs.s1 ! a ++ conj.s ++ xs.s2 ! a} ;
  ComplVPIVV vv vpi = {
    s=\\p,vf => ne ! p ++ vv.s ! vf;s2=\\a => vpi.s ! a;isCop=False;refl=[]
  } ;

  PresPartAP vp = {s=\\_,g,c,n => vp.s ! P.Pos ! VPastPart (agender2gender g) n ++ vp.s2 ! {g=agender2gender g;n=n;p=P3}} ;
  PastPartAP vp = {s=\\_,g,_,n => vp.s ! P.Pos ! VPastPart (agender2gender g) n ++ vp.s2 ! {g=agender2gender g;n=n;p=P3}} ;
  PastPartAgentAP vp np = {s=\\_,g,_,n => vp.s ! P.Pos ! VPastPart (agender2gender g) n ++ vp.s2 ! {g=agender2gender g;n=n;p=P3} ++ "od" ++ np.s ! Gen} ;
  PassVPSlash vp = {s=copula;s2=\\a => vp.s ! P.Pos ! VPastPart a.g a.n ++ vp.s2 ! a;isCop=True;refl=[]} ;
  PassAgentVPSlash vp np = {s=copula;s2=\\a => vp.s ! P.Pos ! VPastPart a.g a.n ++ vp.s2 ! a ++ "od" ++ np.s ! Gen;isCop=True;refl=[]} ;
  ProgrVPSlash vp = vp ;

  ComplBareVS v s = {s=\\p,vf => ne ! p ++ v.s ! vf;s2=\\_ => v.p ++ s.s;isCop=False;refl=v.refl} ;
  ComplSlashPartLast vp np = ComplSlash vp np ;
  UttVPShort vp = {s=vp.s ! P.Pos ! VImper2 Sg ++ vp.s2 ! {g=Masc;n=Sg;p=P2}} ;

  CompoundN a b = b ** {s=\\c,n => b.s ! c ! n ++ a.s ! Gen ! Sg} ;
  CompoundAP n a = {s=\\_,g,c,num => n.s ! Instr ! Sg ++ a.s ! APosit (agender2gender g) num c} ;
  GerundCN vp = {s=\\_,_,_ => vp.s ! P.Pos ! VInf ++ vp.s2 ! {g=Neut;n=Sg;p=P3};g=ANeut} ;
  GerundNP vp = {s=\\_ => vp.s ! P.Pos ! VInf ++ vp.s2 ! {g=Neut;n=Sg;p=P3};a={g=Neut;n=Sg;p=P3};isPron=False} ;
  GerundAdv vp = {s=vp.s ! P.Pos ! VInf ++ vp.s2 ! {g=Neut;n=Sg;p=P3}} ;
  ByVP vp = {s="z" ++ vp.s ! P.Pos ! VInf ++ vp.s2 ! {g=Neut;n=Sg;p=P3}} ;
  InOrderToVP vp = {s="da bi" ++ vp.s ! P.Pos ! VInf ++ vp.s2 ! {g=Neut;n=Sg;p=P3}} ;
  ApposNP a b = a ** {s=\\c => a.s ! c ++ "," ++ b.s ! c} ;
  PositAdVAdj a = {s=a.s ! APosit Neut Sg Nom} ;

  ReflPron = {s=\\_,c => reflexive ! c} ;
  ReflPoss num cn = {s=\\a,c => (mkA "svoj").s ! APosit a.g (numAgr2num ! num.n) c ++ cn.s ! Indef ! c ! (numAgr2num ! num.n)} ;
  PredetRNP pred r = {s=\\a,c => pred.s ++ r.s ! a ! c} ;
  AdvRNP np prep r = {s=\\a,c => np.s ! c ++ prep.s ++ r.s ! a ! prep.c} ;
  AdvRVP vp prep r = vp ** {s2=\\a => vp.s2 ! a ++ prep.s ++ r.s ! a ! prep.c} ;
  AdvRAP ap prep r = ap ** {s=\\sp,g,c,n => ap.s ! sp ! g ! c ! n ++ prep.s ++ r.s ! {g=agender2gender g;n=n;p=P3} ! prep.c} ;
  ReflRNP vp r = vp ** {
    s2=\\a => vp.s2 ! a ++ vp.c2.s ++ r.s ! a ! vp.c2.c
  } ;
  ReflA2RNP a r = {
    s=\\_,g,c,n => a.s ! APosit (agender2gender g) n c ++ a.c.s ++
                    r.s ! {g=agender2gender g;n=n;p=P3} ! a.c.c
  } ;
  PossPronRNP pron num cn r = {
    s=\\c => pron.poss ! agender2gender cn.g ! c ! (numAgr2num ! num.n) ++ cn.s ! Indef ! c ! (numAgr2num ! num.n) ++ r.s ! pron.a ! Gen;
    a={g=agender2gender cn.g;n=numAgr2num ! num.n;p=P3};isPron=False
  } ;
  Base_rr_RNP a b = {s1=a.s;s2=b.s} ;
  Base_nr_RNP a b = {s1=\\_,c=>a.s!c;s2=b.s} ;
  Base_rn_RNP a b = {s1=a.s;s2=\\_,c=>b.s!c} ;
  Cons_rr_RNP a xs = {s1=\\x,c=>a.s!x!c++","++xs.s1!x!c;s2=xs.s2} ;
  Cons_nr_RNP a xs = {s1=\\x,c=>a.s!c++","++xs.s1!x!c;s2=xs.s2} ;
  ConjRNP conj xs = {s=\\a,c => xs.s1!a!c ++ conj.s ++ xs.s2!a!c} ;

  UseDAP dap = {s=\\c=>dap.s!Indef!(AMasc Inanimate)!c!(numAgr2num!dap.n);a={g=Masc;n=numAgr2num!dap.n;p=P3};isPron=False} ;
  UseDAPMasc dap = UseDAP dap ;
  UseDAPFem dap = {s=\\c=>dap.s!Indef!AFem!c!(numAgr2num!dap.n);a={g=Fem;n=numAgr2num!dap.n;p=P3};isPron=False} ;

  CompS s = {s=\\_=>s.s} ;
  CompQS s = {s=\\_=>s.s} ;
  CompVP ant pol vp = {s=\\a=>vp.s!pol.p!VInf++vp.s2!a} ;
  ConjImp conj xs = {s=\\p,g,n=>xs.s1!p!g!n++conj.s++xs.s2!p!g!n} ;
  BaseImp a b = {s1=a.s;s2=b.s} ;
  ConsImp a xs = {s1=\\p,g,n=>a.s!p!g!n++","++xs.s1!p!g!n;s2=xs.s2} ;
  ConjComp conj xs = {s=\\a=>xs.s1!a++conj.s++xs.s2!a} ;
  BaseComp a b = {s1=a.s;s2=b.s} ;
  ConsComp a xs = {s1=\\x=>a.s!x++","++xs.s1!x;s2=xs.s2} ;

  iFem_Pron = mkPron "jàz" "méne" "méne" "méni" "méni" ("menój"|"máno")
                     "mój"  "mòjega" "mòjemu" ("mòj"|"mòjega") "mòjem" "mòjim" 
                     "mòja" "mòjih"  "mòjima"  "mòja"          "mòjih" "mòjima"
                     "mòji" "mòjih"  "mòjim"   "mòje"          "mòjih" "mòjimi" 
                     "mòja" "mòje"   "mòji"    "mòjo"          "mòji"  "mòjo"
                     "mòji" "mòjih"  "mòjima"  "mòji"          "mòjih" "mòjima"
                     "mòje" "mòjih"  "mòjim"   "mòje"          "mòjih" "mòjimi"
                     "mòje" "mòjega" "mòjemu"  "mòjo"          "mòjem" "mòjim"
                     "mòji" "mòjih"  "mòjima"  "mòji"          "mòjih" "mòjima"
                     "mòja" "mòjih"  "mòjim"   "mòja"          "mòjih" "mòjimi" Fem Sg P1 ;
  youFem_Pron = mkPron "tí" "tébe" "tébe" "tébi" "tébi" ("tebój"|"tábo")
                       "tvój"  "tvòjega" "tvòjemu" ("tvòj"|"tvòjega") "tvòjem" "tvòjim" 
                       "tvòja" "tvòjih"  "tvòjima"  "tvòja"           "tvòjih" "tvòjima"
                       "tvòji" "tvòjih"  "tvòjim"   "tvòje"           "tvòjih" "tvòjimi" 
                       "tvòja" "tvòje"   "tvòji"    "tvòjo"           "tvòji"  "tvòjo"
                       "tvòji" "tvòjih"  "tvòjima"  "tvòji"           "tvòjih" "tvòjima"
                       "tvòje" "tvòjih"  "tvòjim"   "tvòje"           "tvòjih" "tvòjimi"
                       "tvòje" "tvòjega" "tvòjemu"  "tvòjo"           "tvòjem" "tvòjim"
                       "tvòji" "tvòjih"  "tvòjima"  "tvòji"           "tvòjih" "tvòjima"
                       "tvòja" "tvòjih"  "tvòjim"   "tvòja"           "tvòjih" "tvòjimi" Fem Sg P2 ;
  weFem_Pron = mkPron "mí" "nàs" "nàs" "nàm" "nàs" "nàmi" 
                      "nàš"  "nášega" "nášemu" ("náši"|"nášega") "nášem" "nášim"
                      "náša" "náših"  "nášima" "náša"            "náših" "nášima"     
                      "náši" "náših"  "nášim"  "náše"            "náših" "nášimi"    
                      "náša" "náše"   "náši"   "nášo"            "náši"  "nášo"
                      "náši" "náših"  "nášima" "náši"            "náših" "nášima"
                      "náše" "náših"  "nášim"  "náše"            "náših" "nášimi"
                      "náše" "nášega" "nášemu" "náše"            "nášem" "nášim"
                      "náši" "náših"  "nášima" "náši"            "náših" "nášima"
                      "náša" "náših"  "nášim"  "náša"            "náših" "nášimi" Fem Pl P1 ;
  youPlFem_Pron = mkPron "ví" "vàs" "vàs" "vàm" "vàs" "vàmi"
                         "vàš"  "vášega" "vášemu" ("váši"|"vášega") "vášem" "vášim"
                         "váša" "váših"  "vášima" "váša"            "váših" "vášima"
                         "váši" "váših"  "vášim"  "váše"            "váših" "vášimi"    
                         "váša" "váše"   "váši"   "vášo"            "váši"  "vášo"
                         "váši" "váših"  "vášima" "váši"            "váših" "vášima"
                         "váše" "váših"  "vášim"  "váše"            "váših" "vášimi"
                         "váše" "vášega" "vášemu" "váše"            "vášem" "vášim"
                         "váši" "váših"  "vášima" "váši"            "váših" "vášima"
                         "váša" "váših"  "vášim"  "váša"            "váših" "vášimi" Fem Pl P2 ;
  theyFem_Pron = mkPron "ôni" "njìh" "njìh" "njìm" "njìh" "njími" 
                        "njíhov"  "njíhovega" "njíhovemu" ("njíhov"|"njíhovega") "njíhovem" "njíhovim" 
                        "njíhova" "njíhovih"  "njíhovima"  "njíhova"             "njíhovih" "njíhovima"
                        "njíhovi" "njíhovih"  "njíhovim"   "njíhove"             "njíhovih" "njíhovimi"
                        "njíhova" "njíhove"   "njíhovi"    "njíhovo"             "njíhovi"  "njíhovo"
                        "njíhovi" "njíhovih"  "njíhovima"  "njíhovi"             "njíhovih" "njíhovima"
                        "njíhove" "njíhovih"  "njíhovim"   "njíhove"             "njíhovih" "njíhovimi"
                        "njíhove" "njíhovega" "njíhovemu"  "njíhovo"             "njíhovem" "njíhovim"
                        "njíhovi" "njíhovih"  "njíhovima"  "njíhovi"             "njíhovih" "njíhovima"
                        "njíhova" "njíhovih"  "njíhovim"   "njíhova"             "njíhovih" "njíhovimi" Fem Pl P3 ;

  youPolFem_Pron = youPlFem_Pron ;
  youPolPl_Pron  = youPol_Pron ;
  youPolPlFem_Pron = youPlFem_Pron ;

  TPastSimple = TPast ;

}
