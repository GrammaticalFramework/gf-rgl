concrete NounSlo of Noun =
  CatSlo
**

  open ResSlo, Prelude in {

lin
    DetCN det cn = {
        s,prep,clit = \\c => det.s ! cn.g ! c ++ numSizeForm cn.s det.size c ;
        a = numSizeAgr cn.g det.size P3 ;
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
    NumPl = {s = \\_,_ => [] ; size = Num2_4} ; ---- size
    NumSg = {s = \\_,_ => [] ; size = Num1} ;

    UsePron pron = {
      s = table {
        Nom => pron.nom ;
	Gen => pron.gen ;
	Dat => pron.dat ;
	Acc => pron.acc ;
	Loc => pron.loc ;
	Ins => pron.ins
        } ;
      clit = table {
        Nom => pron.cnom ;
	Gen => pron.cgen ;
	Dat => pron.cdat ;
	Acc => pron.cacc ;
	Loc => pron.loc ;
	Ins => pron.ins
        } ;
      prep = table {
        Nom => pron.nom ;
	Gen => pron.pgen ;
	Dat => pron.pdat ;
	Acc => pron.pacc ;
	Loc => pron.loc ;
	Ins => pron.pins
        } ;
      a = pron.a ;
      hasClit = True ;
      } ;

    UsePN pn = {
      s,clit,prep = \\c => pn.s ! c ;
      a = Ag pn.g Sg P3 ;
      hasClit = False ;
      } ;

    AdjCN ap cn = {
      s = \\n,c => preOrPost (notB ap.isPost) (ap.s ! cn.g ! n ! c) (cn.s ! n ! c) ;
      g = cn.g
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
      
    UseN n = nounFormsNoun n ;

    ApposCN cn np = {
      s = \\n,c => cn.s ! n ! c ++ np.s ! c ; ---- TODO check apposition order
      g = cn.g
      } ;
      
    NumCard c = c ;
    NumDigits ds = ds ** {s = \\_,_ => ds.s} ;
    NumNumeral nu = nu ;


}
