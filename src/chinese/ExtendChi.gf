--# -path=alltenses:../common:../abstract

concrete ExtendChi of Extend = CatChi **
  ExtendFunctor - [
    VPS, ListVPS, VPI, ListVPI
  , MkVPS, BaseVPS, ConsVPS, ConjVPS
  , PredVPS, SQuestVPS, RelVPS --, QuestVPS -- TODO
  , MkVPI, BaseVPI, ConsVPI, ConjVPI, ComplVPIVV
  , VPS2, ListVPS2, VPI2, ListVPI2
  , MkVPS2, BaseVPS2, ConsVPS2, ConjVPS2, ComplVPS2, ReflVPS2
  , MkVPI2, BaseVPI2, ConsVPI2, ConjVPI2, ComplVPI2
  , ProDrop, ComplDirectVS, ComplDirectVQ
  , PassVPSlash, PassAgentVPSlash
  , PresPartAP, PastPartAP, PastPartAgentAP, ProgrVPSlash
  , ReflRNP, ReflPron, ReflPoss, PredetRNP, ReflA2RNP
  , AdvRNP, AdvRVP, AdvRAP, PossPronRNP
  , CompoundN, CompoundAP, GerundCN, GerundAdv, GerundNP
  , PositAdVAdj, UseDAP, UseDAPMasc, UseDAPFem
  , ListComp, BaseComp, ConsComp, ConjComp
  , ByVP, ApposNP ]
  with (Grammar=GrammarChi) ** open
     Prelude
   , Coordination
   , ResChi
   , (S=StructuralChi)
  in {

  lincat
    VPS, VPI   = SS ;
    [VPS], [VPI] = ListX ;
    VPS2, VPI2 = SS ** {c2 : Preposition ; isPre : Bool} ; -- whether the missing arg is before verb
    [VPS2], [VPI2] = ListX ** {c2 : Preposition ; isPre : Bool} ;
    [Imp] = {s1,s2 : Polarity => Str} ;
    [Comp] = {s1,s2 : Str} ;

  lin
    PassVPSlash vps = insertAdv (mkNP passive_s) vps ;
    PassAgentVPSlash vps np = insertAdv (ss (appPrep S.by8agent_Prep (linNP np))) (insertAdv (mkNP passive_s) vps) ;

    PresPartAP vp = {
      s = table {_ => infVP vp} ; monoSyl = False ; hasAdA = False
      } ;
    PastPartAP vps = {
      s = table {_ => infVP <vps : ResChi.VP>} ;
      monoSyl = False ; hasAdA = False
      } ;
    PastPartAgentAP vps np = {
      s = table {_ => passive_s ++ linNP np ++ infVP <vps : ResChi.VP>} ;
      monoSyl = False ; hasAdA = False
      } ;
    ProgrVPSlash vps = vps ** {
      prePart = "正在" ++ vps.prePart
      } ;

    MkVPS t p vp = {s = t.s ++ p.s ++ (mkClause [] vp).s ! p.p ! t.t} ;
    ConjVPS c = conjunctDistrSS (c.s ! CSent) ;
    BaseVPS = twoSS ;
    ConsVPS = consrSS duncomma ;

    -- : NP -> VPS -> S ;          -- she [has walked and won't sleep]
    PredVPS np vps = {preJiu = (linNP np) ; postJiu = vps.s} ;

    -- : NP -> VPS -> QS ;         -- has she walked
    SQuestVPS np vps = {s = \\_ => linNP np ++ vps.s ++ question_s} ;

    -- : IP -> VPS -> QS ;         -- who has walked
    -- QuestVPS ip vps = -- TODO: probably need to change structure of VPS

    -- : RP -> VPS -> RS ;         -- which won't sleep
    RelVPS rp vps = {s = rp.s ! True ++ vps.s ++ "的"} ;

    MkVPI vp = {s = (mkClause [] vp).s ! Pos ! APlain} ;
    ConjVPI c = conjunctDistrSS (c.s ! CSent) ;
    BaseVPI = twoSS ;
    ConsVPI = consrSS duncomma ;

    BaseComp x y = {s1 = infVP x ; s2 = infVP y} ;
    ConsComp x xs = xs ** {s1 = infVP x ++ duncomma ++ xs.s1} ;
    ConjComp c xs = {
      verb = noVerb ; prePart, topic = [] ; isAdj = False ;
      compl = let cs = c.s ! CPhr CVPhrase
              in cs.s1 ++ xs.s1 ++ cs.s2 ++ xs.s2
      } ;

    BaseImp x y = {s1 = x.s ; s2 = y.s} ;
    ConsImp x xs = xs ** {s1 = \\p => x.s ! p ++ duncomma ++ xs.s1 ! p} ;
    ConjImp c xs = {
      s = \\p => let cs = c.s ! CPhr CVPhrase
                 in cs.s1 ++ xs.s1 ! p ++ cs.s2 ++ xs.s2 ! p
      } ;

    MkVPS2 t p vps = {s = t.s ++ p.s ++ (mkClause [] <vps : ResChi.VP>).s ! p.p ! t.t} ** vps ;
    ConjVPS2 c vs = conjunctDistrSS (c.s ! CSent) vs ** vs ;
    BaseVPS2 v w = twoSS v w ** w ;
    ConsVPS2 v vs = consrSS duncomma v vs ** vs ;

    MkVPI2 vps = {s = (mkClause [] <vps : ResChi.VP>).s ! Pos ! APlain} ** vps ;
    ConjVPI2 c vs = conjunctDistrSS (c.s ! CSent) vs ** vs ;
    BaseVPI2 v w = twoSS v w ** w ;
    ConsVPI2 v vs = consrSS duncomma v vs ** vs ;

    ComplVPIVV vv vpi = predV vv [] ** {
      compl = vpi.s ;
      } ;

    GerundAdv vp = mkAdv (infVP vp) ;
    GerundNP vp = ResChi.mkNP (infVP vp) ;

    GerundCN vp = {s = infVP vp ++ possessive_s ++ "行为" ; c = ge_s} ;

    CompoundN n1 n2 = {s = n1.s ++ n2.s ; c = n2.c} ;
    CompoundAP n a = {
      s = table {_ => n.s ++ a.s ! Attr} ;
      monoSyl = False ; hasAdA = False
      } ;

    PositAdVAdj a = ss (a.s ! Attr ++ deAdvV_s) ;

    ByVP vp =
     let adv : Adv = GerundAdv vp
       in adv ** {s = adv.s ++ "来" ; advType = ATTime} ;

    GenNP np =  {s,pl = linNP np ++ possessive_s ; detType = DTPoss} ;
    GenRP nu cn = {s = \\_ => cn.s ++ relative_s} ;

    ProDrop pron = pron ** {s = []} ;

    ReflPron = lin NP (ResChi.mkNP reflPron) ;
    ReflPoss num cn = lin NP {
      det = reflPron ++ possessive_s ;
      s = case num.numType of {
        NTFull => num.s ++ cn.c ++ cn.s ;
        NTVoid _ => cn.s
        }
      } ;
    PredetRNP pred rnp = rnp ** {s = pred.s ++ rnp.s} ;
    ReflRNP slash rnp = GrammarChi.ComplSlash slash rnp ;
    ReflA2RNP a rnp = GrammarChi.ComplA2 a rnp ;
    AdvRNP np prep rnp = np ** {
      s = appPrep prep (linNP rnp) ++ possessive_s ++ np.s
      } ;
    AdvRVP vp prep rnp = insertAdvPost
      (ss (appPrep prep (linNP rnp))) vp ;
    AdvRAP ap prep rnp = ap ** {
      s = \\af => appPrep prep (linNP rnp) ++ ap.s ! af
      } ;
    PossPronRNP pron num cn rnp = {
      det = [] ;
      s = pron.s ++ "对" ++ linNP rnp ++ possessive_s ++
          case num.numType of {
            NTFull => num.s ++ cn.c ++ cn.s ;
            NTVoid _ => cn.s
            }
      } ;

    UseDAP dap = lin NP (dapNP dap) ;
    UseDAPMasc dap = lin NP (dapNP dap) ;
    UseDAPFem dap = lin NP (dapNP dap) ;
    ComplDirectVS vs utt =
      AdvVP (UseV <lin V vs : V>)
            (mkAdv (":" ++ quoted utt.s)) ; -- DEFAULT complement added as Adv in quotes
    ComplDirectVQ vq utt =
      AdvVP (UseV <lin V vq : V>)
            (mkAdv (":" ++ quoted utt.s)) ; -- DEFAULT complement added as Adv in quotes

  lin
    ApposNP np1 np2 = {s = np1.s ++ np2.s; det = np1.det} ;

  oper
    mkAdv : Str -> CatChi.Adv ;
    mkAdv str = lin Adv {s = str ; advType = ATManner ; hasDe = False} ;

    dapNP : CatChi.DAP -> ResChi.NP = \dap -> {
      s = dap.adj ;
      det = dap.s
      } ;

};
