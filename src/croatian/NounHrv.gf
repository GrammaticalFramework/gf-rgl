concrete NounHrv of Noun =
  CatHrv
**

  open ResHrv, Prelude in {

lin
    DetCN det cn = {
        s,prep,clit = \\c => det.s ! cn.g ! c ++ numSizeForm cn.s det.size c ;
        a = numSizeAgr cn.g det.size P3 ;
        hasClit = False ;
      } ;

    DetNP det = {
        s,prep,clit = \\c => det.s ! Neutr ! c ;
        a = numSizeAgr Neutr det.size P3 ;
        hasClit = False ;
      } ;

    MassNP cn = {
      s,prep,clit = \\c => cn.s ! Sg ! c ;
      a = Ag cn.g Sg P3 ;
      hasClit = False ;
      } ;

    DetQuant quant num = {
      s = \\g,c => num.s ! g ! c ++ quant.s ! g ! numSizeNumber num.size ! c ;
      size = num.size
      } ;

    DefArt = {s = \\_,_,_ => []} ;
    IndefArt = {s = \\_,_,_ => []} ;
    NumPl = {s = \\_,_ => [] ; size = NS_2_4} ; ---- size
    NumSg = {s = \\_,_ => [] ; size = NS_1} ;

    UsePron pron = {
      s, prep = table {  ---- TODO check prep
        Nom | Voc => pron.nom ;
	Gen | Acc => pron.gen ;
	Dat | Loc => pron.dat ;
	Ins => pron.ins
        } ;
      clit = table {  ---- TODO check prep
        Nom | Voc => pron.nom ;
	Gen | Acc => pron.cgen ;
	Dat | Loc => pron.cdat ;
	Ins => pron.ins
        } ;
      a = pron.a ;
      hasClit = True ;
      } ;
      
    PossPron pron = adjFormsAdjective pron.poss ;

    UsePN pn = {
      s,clit,prep = \\c => pn.s ! c ;
      a = Ag pn.g Sg P3 ;
      hasClit = False ;
      } ;

    AdjCN ap cn = {
      s = \\n,c => preOrPost (notB ap.isPost) (ap.s ! cn.g ! n ! c) (cn.s ! n ! c) ;
      g = cn.g
      } ;
      
    PredetNP predet np =
      case np.a of {
        Ag g n _ => { 
          s,clit,prep = \\c => predet.s ! g ! n ! c ++ np.s ! c ;
          a = np.a ;
	  hasClit = False 
	}
      } ;

    RelCN cn rs = {
      s = \\n,c => cn.s ! n ! c ++ rs.s ! Ag cn.g n P3 ;
      g = cn.g
      } ;

    AdvCN cn adv = {
      s = \\n,c => cn.s ! n ! c ++ adv.s ;
      g = cn.g
      } ;
      
    AdvNP np adv = {
      s,clit = \\c => np.s ! c ++ adv.s ;
      prep = \\c => np.prep ! c ++ adv.s ;
      a = np.a ;
      hasClit = False ;
      } ;
      
    UseN n = nounFormsNoun n n.g ;

    ApposCN cn np = {
      s = \\n,c => cn.s ! n ! c ++ np.s ! c ; ---- TODO check apposition order
      g = cn.g
      } ;
      
    NumCard c = c ;
    NumDigits ds = ds ** {s = \\_,_ => ds.s} ;
    NumDecimal dec = dec ** {s = \\_,_ => dec.s} ;
    NumNumeral nu = {
      s = \\g,c => (adjFormsAdjective nu.s).s ! g ! Sg ! c ; ---- TODO Sg?
      size = nu.size
      } ;
      
    AdNum adn card = card ** {s = \\g,c => adn.s ++ card.s ! g ! c} ;
    OrdSuperl a = a.superl ;


}
