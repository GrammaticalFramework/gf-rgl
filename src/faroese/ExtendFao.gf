concrete ExtendFao of Extend = CatFao ** 
	open ParadigmsFao, ResFao, (P = ParamX) in {

lincat
  VPS = {s : Gender => PersNum => Str} ;
  [VPS] = {s1,s2 : Gender => PersNum => Str} ;
  VPI = {s : Str} ;
  [VPI] = {s1,s2 : Str} ;
  VPS2 = {s : Gender => PersNum => Str ; c2 : Compl ; sc : Str} ;
  [VPS2] = {s1,s2 : Gender => PersNum => Str ; c2 : Compl ; sc : Str} ;
  VPI2 = {s : Str ; c2 : Compl ; sc : Str} ;
  [VPI2] = {s1,s2 : Str ; c2 : Compl ; sc : Str} ;
  [Comp] = {s1,s2 : Gender => Number => Str} ;
  [Imp] = {s1,s2 : Polarity => Number => Str} ;
  RNP = {s : Case => Str ; g : Gender ; n : Number ; p : Person} ;
  RNPList = {s1,s2 : Case => Str ; g : Gender ; n : Number ; p : Person} ;
  X = {s : Str} ;

lin
  iFem_Pron  = mkPron "eg" "meg" "mær" "mín" Masc Sg P1 ;
  youFem_Pron = mkPron "tú" "teg" "tær" "tín" Masc Sg P2 ;
  weFem_Pron = mkPron "vit" "okkum" "okkum" "okkara" Masc Pl P1 ;
  youPlFem_Pron = mkPron "tit" "tykkum" "tykkum" "tykkara" Masc Pl P2 ;
  theyFem_Pron = mkPron "tær" "tær" "teimum" "teirra" Masc Pl P3 ;
  youPolFem_Pron = mkPron "tit" "tykkum" "tykkum" "tykkara" Masc Pl P2 ;
  youPolPl_Pron = mkPron "tit" "tykkum" "tykkum" "tykkara" Masc Pl P2 ;
  youPolPlFem_Pron = mkPron "tit" "tykkum" "tykkum" "tykkara" Masc Pl P2 ;

lin
  TPastSimple = {s = [] ; t = P.Past} ;

}
