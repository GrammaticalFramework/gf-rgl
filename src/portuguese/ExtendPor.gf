--# -path=alltenses:../common:../abstract

concrete ExtendPor of Extend =
  CatPor ** ExtendFunctor -
  [
    AdAdV,
      AdjAsCN,
      AdjAsNP,
      ApposNP,
      BaseVPS,
      ByVP,
      CompBareCN,
      ComplBareVS,
      ComplSlashPartLast,
      CompoundAP,
      CompoundN,
      CompVP,
      ConjVPS,
      ConsVPS,
      --EmptyRelSlash,
      ExistsNP,
      GenIP,
      GenModIP,
      GenModNP,
      GenNP,
      GerundAdv,
      GerundCN,
      GerundNP,
      IAdvAdv,
      ICompAP,
      InOrderToVP,
      ListVPS,
      MkVPS,
      PassAgentVPSlash,
      PassVPSlash,
      PastPartAP,
      PastPartAgentAP,
      PositAdVAdj,
      PredVPS,
      PresPartAP,
      ProDrop,
      PurposeVP,
      VPS,
      iFem_Pron,
      theyFem_Pron,
      UttAdV,
      UttVPShort,
      weFem_Pron,
      WithoutVP,
      youFem_Pron,
      youPlFem_Pron,
      youPolFem_Pron,
      youPolPlFem_Pron,
      youPolPl_Pron
   ]                   -- don't forget to put the names of your own
                       -- definitions here
  with
    (Grammar = GrammarPor), (Syntax = SyntaxPor) **
  open
  GrammarPor,
  ResPor,
  MorphoPor,
  Coordination,
  Prelude,
  ParadigmsPor,
  (S = StructuralPor) in {

  lin
    GenNP np =
      let denp = (np.s ! ResPor.genitive).ton in {
        s = \\_,_,_,_ => [] ;
        sp = \\_,_,_ => denp ;
        s2 = denp ;
        isNeg = False ;
      } ;

     GenIP ip = {s = \\_,_,c => ip.s ! c} ;

     GenModNP num np cn = DetCN (DetQuant (GenNP (lin NP np)) num) cn ;

     GenModIP num ip cn = IdetCN (IdetQuant (GenIP (lin IP ip)) num) cn ;

     CompBareCN cn = {
       s = \\agr => cn.s ! agr.n ;
       cop = serCopula
       } ;

     EmptyRelSlash cls = {
       s = \\agr,t,a,p,m => cls.s ! agr ! DDir ! t ! a ! p ! m ++ cls.c2.s ;
       c = Nom
       } ;

  ---- these come from ExtraRomance: how to avoid the repetition?
  ---- can't seem to be able to use two functors
  lincat
    VPS = {s : Mood => Agr => Bool => Str} ;
    [VPS] = {s1,s2 : Mood => Agr => Bool => Str} ;

  lin
    BaseVPS x y = twoTable3 Mood Agr Bool x y ;
    ConsVPS = consrTable3 Mood Agr Bool comma ;

    PredVPS np vpi = {
      s = \\m => (np.s ! Nom).comp ++ vpi.s ! m ! np.a ! np.isNeg
      } ;

    MkVPS tm p vp = {
      s = \\m,agr,isNeg =>
        tm.s ++ p.s ++
        (mkClausePol (orB isNeg vp.isNeg) [] False False agr vp).s
          ! DDir ! tm.t ! tm.a ! p.p ! m
      } ;

    ConjVPS = conjunctDistrTable3 Mood Agr Bool ;

  lin
    ProDrop p = {
      s = table {
        Nom => let pn = p.s ! Nom in {c1 = pn.c1 ; c2 = pn.c2 ; comp = [] ; ton = pn.ton} ;
        c => p.s ! c
        } ;
      a = p.a ;
      poss = p.poss ;
      hasClit = p.hasClit ;
      isPol = p.isPol ;
      isNeg = False
      } ;

    ICompAP ap = {
      s =\\a => "o quão" ++ ap.s ! (genNum2Aform a.g a.n) ;
      cop = serCopula
      } ;

    IAdvAdv adv = {
      s = "o quão" ++ adv.s
      } ;

    CompIQuant iq = {s = \\aa => iq.s ! aa.n ! aa.g ! Nom ; cop = serCopula} ;

    PrepCN prep cn = {s = prep.s ++ prepCase prep.c ++ cn.s ! Sg} ;

  lin
    PresPartAP vp = {
      s = \\af => gerVP vp (aform2aagr af ** {p = P3}) ;
      isPre = False ;
      copTyp = serCopula
      } ;

    PastPartAP vps = pastPartAP vps [] ;

    PastPartAgentAP vps np = pastPartAP vps (let by = <Grammar.by8agent_Prep : Prep> in by.s ++ (np.s ! by.c).ton) ;

    PassVPSlash vps = passVPSlash vps [] ;

    PassAgentVPSlash vps np = passVPSlash vps (let by = <Grammar.by8agent_Prep : Prep> in by.s ++ (np.s ! by.c).ton) ;

    ExistsNP np =
      mkClause [] True False np.a
      (insertComplement (\\_ => (np.s ! Nom).ton)
         (predV (mkV "existir"))) ;

    PurposeVP vp = {
      s = infVP vp (Ag Masc Sg P3)
      } ;

    ComplBareVS = ComplVS ;

    AdjAsCN ap = {
      s =\\n => ap.s ! (genNum2Aform Masc n) ;
      g = Masc
      } ;

    AdjAsNP ap = heavyNP {
      s = \\_c => ap.s ! ASg Masc APred ;
      a = Ag Masc Sg P3
      } ;

  oper
    pastPartAP : VPSlash -> Str -> AP ;
    pastPartAP vps agent = lin AP {
      s = \\af => vps.comp ! (aform2aagr af ** {p = P3}) ++ vps.s.s ! VPart (aform2gender af) (aform2number af) ++ agent ;
      isPre = False ;
      copTyp = serCopula
      } ;

    passVPSlash : VPSlash -> Str -> VP ;
    passVPSlash vps agent = let
      auxvp = predV auxPassive
      in
      vps ** {
        s = auxvp.s ;
        agr = auxvp.agr ;
        comp  = \\a => vps.comp ! a ++ (let agr = complAgr a in vps.s.s ! VPart agr.g agr.n) ++ agent ;
      } ;

  lin
    CompoundN noun noun2 = { -- order is different because that's needed for correct translation from english
      s = \\n => noun2.s ! n
        ++ variants {"de" ; genForms "do" "da" ! noun.g}
        ++ noun.s ! Sg ;
      g = noun2.g
      } ;

    CompoundAP noun adj = {
      s = \\af => case (aform2aagr af) of {
        {n = n} => adj.s ! Posit ! (genNum2Aform noun.g n) ++ "de" ++ noun.s ! n
        } ;
      isPre = adj.isPre ;
      copTyp = adj.copTyp
      } ;

    GerundCN vp = {
      s = \\n => infVP vp {g = Masc ; n = n ; p = P3} ;
      g = Masc
      } ;

    GerundNP vp = let
      neutrAgr = Ag Masc Sg P3
      in heavyNP {
        s = \\_ => gerVP vp neutrAgr ;
        a = neutrAgr
      } ;

    GerundAdv vp = {
      s = gerundStr vp
      } ;

    WithoutVP vp = {
      s = "sem" ++ gerundStr vp
      } ;

    ByVP vp = {
      s = "by" ++ gerundStr vp
      } ;

    InOrderToVP vp = {
      s = "a fim de" ++ gerundStr vp
      } ;

    ApposNP np1 np2 = np1 ** {
      s = \\c => {
        c1 = (np1.s ! c).c1  ++ (np2.s ! c).c1 ;
        c2 = (np1.s ! c).c2 ++ (np2.s ! c).c2 ;
        comp = (np1.s ! c).comp ++ (np2.s ! c).comp ;
        ton = (np1.s ! c).ton ++ (np2.s ! c).ton
        } ;
      } ;

    AdAdV aa av = {
      s = aa.s ++ av.s
      } ;

    UttAdV av = av ;

    PositAdVAdj a = {
      s = a.s ! Posit ! AA
      } ;

    --TODO: actually use ant
    CompVP ant p vp = let
      neg = negation ! p.p
      in {
        s = \\agr => ant.s ++ p.s ++ "de" ++ neg.p1 ++ infVP vp agr ;
        cop = serCopula
      } ;

    UttVPShort = UttVP ;

    ComplSlashPartLast = ComplSlash ;

  oper
    gerundStr : VP -> Str ;
    gerundStr vp = gerVP vp (Ag Masc Sg P3) ;

  lin
    -- Romance
    iFem_Pron = pronAgr S.i_Pron Fem Sg P1 ;
    weFem_Pron = pronAgr S.we_Pron Fem Pl P1 ;
    youFem_Pron = pronAgr S.youSg_Pron Fem Sg P3 ;
    youPlFem_Pron = pronAgr S.youPl_Pron Fem Pl P3 ;
    youPolPl_Pron = mkPronoun "vós" "vos" "vos" "vós"
      "vosso" "vossa" "vossos" "vossas"
      Masc Pl P2 ;
    youPolFem_Pron = pronAgr S.youPol_Pron Fem Sg P2 ;
    youPolPlFem_Pron = pronAgr youPolPl_Pron Fem Pl P2 ;
    theyFem_Pron = mkPronFrom S.they_Pron "elas" "as" "lhes" "elas" Fem Pl P3 ;

} ;
