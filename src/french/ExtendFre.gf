--# -path=alltenses:../common:../abstract

concrete ExtendFre of Extend =
  CatFre ** ExtendFunctor -
   [
----   iFem_Pron, youFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, youPolFem_Pron, youPolPl_Pron, youPolPlFem_Pron,
   ExistCN, ExistMassCN, ExistPluralCN,
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

lin CompoundN a b = lin N {s = \\n => b.s ! n ++ a.s ! Sg ; g = b.g} ; -- connessione internet = internet connection

}
