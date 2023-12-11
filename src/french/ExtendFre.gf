--# -path=alltenses:../common:../abstract

concrete ExtendFre of Extend =
  CatFre ** ExtendFunctor -
   [
----   iFem_Pron, youFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, youPolFem_Pron, youPolPl_Pron, youPolPlFem_Pron,
   ExistCN, ExistMassCN, ExistPluralCN, RNP, ReflRNP,
   PassVPSlash, PassAgentVPSlash, ApposNP, CompoundN
   ]                   -- put the names of your own definitions here
  with
    (Grammar = GrammarFre) **
  open
    GrammarFre,
    ResFre,
    MorphoFre,
    PhonoFre,
    Coordination,
    Prelude,
    ParadigmsFre in {
    -- put your own definitions here

lincat
  RNP = {s : Agr => Case => Str} ;

lin
   ExistCN cn =
      let
         pos = ExistNP (DetCN (DetQuant IndefArt NumSg) cn) ;
         neg = ExistNP (DetCN (DetQuant de_Quant NumSg) cn) ;
      in posNegClause pos neg PNeg.p ;
   ExistMassCN cn =
      let
         pos = ExistNP (MassNP cn) ;
         neg = ExistNP (DetCN (DetQuant de_Quant NumSg) cn) ;
      in posNegClause pos neg PNeg.p ;
   ExistPluralCN cn =
      let
         pos = ExistNP (DetCN (DetQuant IndefArt NumPl) cn) ;
         neg = ExistNP (DetCN (DetQuant de_Quant NumPl) cn) ;
      in posNegClause pos neg PNeg.p ;

oper
  de_Quant : Quant = IndefArt ** {s = \\_,_,_,_ => elisDe} ;

lin PassVPSlash vps = passVPSlash vps [] ;
    PassAgentVPSlash vps np = passVPSlash 
      vps (let by = <Grammar.by8agent_Prep : Prep> in by.s ++ (np.s ! by.c).ton) ;

    ReflRNP v rnp =      -- VPSlash -> RNP -> VP ; -- love my family and myself
      case v.c2.isDir of {
        True  => insertRefl v ;
        False => insertComplement
                   (\\a => let agr = verbAgr a in v.c2.s ++ rnp.s ! agr ! v.c2.c) v
      } ;

    ReflPron = {         -- RNP ; -- myself
      s = \\agr,c => reflPron agr.n agr.p c
    } ;

oper
    passVPSlash : VPSlash -> Str -> VP = \vps, agent -> 
      let auxvp = predV auxPassive 
      in
      vps ** {
         s = auxvp.s ;
         agr = auxvp.agr ;
         comp  = \\a => (let agr = complAgr a in vps.s.s ! VPart agr.g agr.n) ++ vps.comp ! a ++ agent ;
        } ;

lin ApposNP np1 np2 = np1 ** {    -- guessed by KA
      s = \\c => np1.s ! c ** {ton  =(np1.s ! c).ton  ++ "," ++ (np2.s ! Nom).ton;
                               comp =(np1.s ! c).comp ++ "," ++ (np2.s ! Nom).comp
                              } ;
    } ;

lin CompoundN a b = lin N {
      s = \\n => b.s ! n ++
                 case b.relType of {
                   NRelPrep p => prepCase (CPrep p) ;  -- tasa de suicidio
                   NRelNoPrep => []                    -- connessione internet = internet connection
                 } ++
                 a.s ! Sg ;
      g = b.g ;
      relType = b.relType
      } ;

lin UseDAP = \dap ->
      let
        g = Masc ;
        n = dap.n
      in heavyNPpol dap.isNeg {
        s = dap.spn ;
        a = agrP3 g n ;
        hasClit = False
        } ;
    UseDAPMasc = \dap ->
      let
        g = Masc ;
        n = dap.n
      in heavyNPpol dap.isNeg {
        s = dap.sp ! g ;
        a = agrP3 g n ;
        hasClit = False
        } ;
    UseDAPFem dap =
      let
        g = Fem ;
        n = dap.n
      in heavyNPpol dap.isNeg {
           s = dap.sp ! g ;
           a = agrP3 g n ;
           hasClit = False
           } ;

}
