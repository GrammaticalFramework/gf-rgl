concrete NounLat of Noun = CatLat ** open ResLat, Prelude, ConjunctionLat in {

  flags optimize=all_subs ;

  lin
--  DetCN   : Det -> CN -> NP ;   -- the man
    DetCN det cn =
      {
	s = \\_,c => cn.s ! det.n ! c ;
	n = det.n ; g = cn.g ; p = P3 ;
	adv = cn.adv ;
	preap = cn.preap ;
	postap = cn.postap ;
	det = { s = det.s ! cn.g ; sp = det.sp ! cn.g } ; 
      } ;

--  UsePN   : PN -> NP ;          -- John
    UsePN pn =
      pn **
      {
	s = \\_ => pn.s ;
	p = P3 ;
	adv = "" ;
	preap, postap = { s = \\_ => "" } ;
	det = { s,sp = \\_ => "" ; n = Sg }
      } ;

--  UsePron : Pron -> NP ;        -- he
    UsePron p =
      p.pers **
      {
	p = p.p ;
	s = \\pd,c => p.pers.s ! pd ! PronNonRefl ! c;
	adv = "" ;
	preap, postap = { s = \\_ => "" } ;
	det = { s,sp = \\_ => "" ; n = p.pers.n } ;	
      } ;
    
--  PredetNP : Predet -> NP -> NP ; -- only the man
    PredetNP predet np =
      np ** {
	det = { s = \\c => predet.s ++ np.det.s ! c ; sp = np.det.sp }
      } ;

--  PPartNP : NP -> V2  -> NP ;    -- the man seen
--    PPartNP np v2 = {
--      s = \\c => (combineNounPhrase np) ! PronNonDrop ! c ++ v2.s ! VPPart ;
--      a = np.a
--      } ;
    --

--  AdvNP   : NP -> Adv -> NP ;    -- Paris today
    AdvNP np adv = np ** { adv = np.adv ++ (adv.s ! Posit) } ;
      -- {
      -- s = \\pd,c => combineNounPhrase np ! pd ! c ;
      -- g = np.g ; n = np.n; p = np.p ;
      -- adv = cc2 np.adv adv ;
      -- preap = np.preap ;
      -- postap = np.postap ;
      -- det = np.det;
      -- } ;

--    ExtAdvNP: NP -> Adv -> NP ;    -- boys, such as ..
    ExtAdvNP = AdvNP ;

--  RelNP   : NP -> RS  -> NP ;    -- Paris, which is here
    RelNP np rs = np ** { adv = bindComma ++ rs.s ! np.g ! np.n ++ np.adv } ;

--  DetNP   : Det -> NP ;  -- these five
    DetNP det = {
      s = \\_ => det.s ! Neutr ;
      g = Neutr ;
      n = det.n ;
      p = P3 ;
      adv = "" ;
      preap, postap = { s = \\_ => "" } ;
      det = { s,sp = \\_ => "" ; n = det.n } ;
    } ;
--
--    DetQuantOrd quant num ord = {
--      s  = quant.s ! num.hasCard ! num.n ++ num.s ++ ord.s ; 
--      sp = quant.sp ! num.hasCard ! num.n ++ num.s ++ ord.s ; 
--      n  = num.n
--      } ;
--
    DetQuant quant num = {
      s  = \\g,c => quant.s  ! Ag g num.n c ++ num.s ! g ! c ;
      sp = \\g,c => quant.sp ! Ag g num.n c ;
      n  = num.n
      } ;



    PossPron p = { s = \\a => p.poss.s ! PronNonRefl ! a ; sp = \\_ => "" } ;
--      s = \\_,_ => p.s ! Gen ;
--      sp = \\_,_ => p.sp 
--      } ;
--
    NumSg = {s = \\_,_ => [] ; n = Sg} ;
    NumPl = {s = \\_,_ => [] ; n = Pl} ;

    NumCard n = n ;
--
--    NumDigits n = {s = n.s ! NCard ; n = n.n} ;
    --    OrdDigits n = {s = n.s ! NOrd} ;
    --
  lin
    -- NumNumeral : Numeral -> Card ;  -- fifty-one
    NumNumeral numeral = { s = numeral.s ; n = numeral.n } ;
    -- OrdNumeral : Numeral -> Ord ;  -- fifty-first
    OrdNumeral numeral = { s = numeral.ord } ;
--
--    AdNum adn num = {s = adn.s ++ num.s ; n = num.n} ;
--
--    OrdSuperl a = {s = a.s ! AAdj Superl} ;

    DefArt = {
      s = \\_ => [] ;
      sp = \\_ => [] ;
      } ;

    IndefArt = {
      s = \\_ => [] ;
      sp = \\_ => [] ;
      } ;

    MassNP cn =
      cn ** {
	s = \\_ => cn.s ! Sg ;
	-- s = case cn.massable of { True => cn.s ! Sg ; False => \\_ => nonExist } ;
	n = Sg ;
	p = P3 ;
	det = { s,sp = \\_ => "" ; n = Sg } ;
      };

    UseN n = -- N -> CN
  lin CN ( n ** {preap, postap = {s = \\_ => "" } ; adv = "" }) ; -- massable = n.massable } ) ; 
      
      UseN2 n2 = -- N2 -> CN
  lin CN ( n2 ** {preap, postap = {s = \\_ => "" } ; adv = "" }) ; -- massable = n2.massable } ) ; 
  -----b    UseN3 n = n ;
--
--    Use2N3 f = {
--      s = \\n,c => f.s ! n ! Nom ;
--      g = f.g ;
--      c2 = f.c2
--      } ;
--
--    Use3N3 f = {
--      s = \\n,c => f.s ! n ! Nom ;
--      g = f.g ;
--      c2 = f.c3
--      } ;
--
--    ComplN2 f x = {s = \\n,c => f.s ! n ! Nom ++ f.c2 ++ x.s ! c ; g = f.g} ;
--    ComplN3 f x = {
--      s = \\n,c => f.s ! n ! Nom ++ f.c2 ++ x.s ! c ;
--      g = f.g ;
--      c2 = f.c3
--      } ;

  lin
    -- by default add adjective after the noun, otherwise use AdjCNPre
    AdjCN ap cn =  -- AP -> CN -> CN
      addAdjToCN (lin AP ap) (lin CN cn) Post ;

--    RelCN cn rs = {
--      s = \\n,c => cn.s ! n ! c ++ rs.s ! agrgP3 n cn.g ;
--      g = cn.g
--      } ;

--    AdvCN cn ad = {s = \\n,c => cn.s ! n ! c ++ ad.s ; g = cn.g} ;

--    SentCN cn sc = {s = \\n,c => cn.s ! n ! c ++ sc.s ; g = cn.g} ;
    --
    -- ApposCN : CN -> NP -> CN
    ApposCN cn np =
      cn **
      {
	s = \\n,c => cn.s ! n ! c ++ (combineNounPhrase np) ! PronNonDrop ! APostN ! DPreN ! c ;
      } ; -- massable = cn.massable } ;

    -- CountNP : Det -> NP -> NP ;    -- three of them, some of the boys
    CountNP det np = np ** {
      det = { s = \\c => det.s ! np.g ! c ++ np.det.s ! c ; sp = \\c => det.sp ! np.g ! c ++ np.det.sp ! c } ;
      }; 
}