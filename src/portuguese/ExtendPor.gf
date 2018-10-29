--# -path=alltenses:../common:../abstract

concrete ExtendPor of Extend =
  CatPor ** ExtendFunctor -
  [
    AdAdV,
      ApposNP,
      BaseVPS,
      ByVP,
      CompBareCN,
      ComplBareVS,
      ComplSlashPartLast,
      CompoundAP,
      CompoundN,
      CompVP,
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
      PassAgentVPSlash,
      PassVPSlash,
      PastPartAP,
      PastPartAgentAP,
      PositAdVAdj,
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

  lincat
    VPS = {s : Agr => Mood => Str} ;
    [VPS] = {s1,s2 : Agr => Mood => Str} ;
--    VPI = {s : VType => Agr => Str } ;

  lin
    BaseVPS = twoTable2 Agr Mood ;
    ConsVPS = consrTable2 Agr Mood comma ;

--    MkVPS t p vp = mkVPS (lin Temp t) (lin Pol p) (lin VP vp) ;
--TODO: write mkVPS oper

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
      s =\\a => "o quão" ++ ap.s ! AF a.g a.n ;
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
      isPre = False
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

  oper
    pastPartAP : VPSlash -> Str -> AP ;
    pastPartAP vps agent = lin AP {
      s = \\af => vps.comp ! (aform2aagr af ** {p = P3}) ++ vps.s.s ! VPart (aform2gender af) (aform2number af) ++ agent ;
      isPre = False
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
      s = \\af => case af of {
        AF g n => adj.s ! Posit ! AF noun.g n ++ "de" ++ noun.s ! n ;
        -- do I need do(s)/da(s)?
        _ => adj.s ! Posit ! AF noun.g Sg ++ "de" ++ noun.s ! Sg
        } ;
      isPre = adj.isPre
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
