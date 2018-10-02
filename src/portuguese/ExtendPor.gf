--# -path=alltenses:../common:../abstract

concrete ExtendPor of Extend =
  CatPor ** ExtendFunctor -
  [
    GenNP, GenIP, GenModNP, GenModIP, iFem_Pron, weFem_Pron, youFem_Pron
      , youPlFem_Pron, youPolPl_Pron, youPolFem_Pron, youPolPlFem_Pron, theyFem_Pron
      , ProDrop, PassVPSlash, ExistsNP, VPS, ListVPS, BaseVPS, ConsVPS, PassVPSlash
      , ExistsNP, CompoundN
   ]                   -- put the names of your own definitions here
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

  lin
    PassVPSlash vps =
      let auxvp = predV copula
      in
      insertComplement (\\a => let agr = complAgr a in vps.s.s ! VPart agr.g agr.n) {
        s = auxvp.s ;
        agr = auxvp.agr ;
        neg = vps.neg ;
        clit1 = vps.clit1 ;
        clit2 = vps.clit2 ;
        clit3 = vps.clit3 ;
        isNeg = vps.isNeg ;
        comp  = vps.comp ;
        ext   = vps.ext
      } ;

    ExistsNP np =
      mkClause [] True False np.a
      (insertComplement (\\_ => (np.s ! Nom).ton)
         (predV (mkV "existir"))) ;

  lin
    CompoundN noun noun2 = { -- order is different because that's needed for correct translation from english
      s = \\n => noun2.s ! n
        ++ variants {genNumForms "do" "da" "dos" "das" ! noun.g ! n; "de"}
        ++ noun.s ! n ;
      g = noun2.g
      } ;

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
