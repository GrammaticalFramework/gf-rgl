concrete StructuralFao of Structural = CatFao ** 
	open ParadigmsFao, ResFao in {

lin
  i_Pron  = mkPron "eg" "meg" "mær" "mín" Masc Sg P1 ;
  youSg_Pron = mkPron "tú" "teg" "tær" "tín" Masc Sg P2 ;
  he_Pron = mkPron "hann" "hann" "honum" "hansara" Masc Sg P3 ;
  she_Pron = mkPron "hon" "hana" "henni" "hennara" Fem Sg P3 ;
  it_Pron = mkPron "tað" "tað" "tí" "tess" Neuter Sg P3 ;
  we_Pron = mkPron "vit" "okkum" "okkum" "okkara" Masc Pl P1 ;
  youPl_Pron = mkPron "tit" "tykkum" "tykkum" "tykkara" Masc Pl P2 ;
  they_Pron = mkPron "teir" "teir" "teimum" "teirra" Masc Pl P3 ;
  youPol_Pron = mkPron "tit" "tykkum" "tykkum" "tykkara" Masc Pl P2 ;
  this_Quant = {
    s = \\_,g,n,c => case <g,n,c> of {
                       <Masc,Sg,Nom> => "hesin" ;
                       <Masc,Sg,Acc> => "henda" ;
                       <Fem,Sg,Nom> => "henda" ;
                       <Fem,Sg,Acc> => "hesa" ;
                       <Neuter,Sg,Nom> => "hetta" ;
                       <Neuter,Sg,Acc> => "hetta" ;
                       <_,Sg,Dat> => "hesum" ;
                       <_,Sg,Gen> => "hesins" ;
                       <Masc,Pl,Nom> => "hesir" ;
                       <Masc,Pl,Acc> => "hesar" ;
                       <Fem,Pl,Nom> => "hesar" ;
                       <Fem,Pl,Acc> => "hesar" ;
                       <Neuter,Pl,Nom> => "hesi" ;
                       <Neuter,Pl,Acc> => "hesi" ;
                       <_,Pl,Dat> => "hesum" ;
                       <_,Pl,Gen> => "hesa"
                    } ;
    sp = Indef ;
    d = Weak
  } ;
  that_Quant = {
    s = \\_,g,n,c => case <g,n,c> of {
                       <Masc,Sg,Nom> => "tann" ;
                       <Masc,Sg,Acc> => "tann" ;
                       <Fem,Sg,Nom> => "tann" ;
                       <Fem,Sg,Acc> => "ta" ;
                       <Neuter,Sg,Nom> => "tað" ;
                       <Neuter,Sg,Acc> => "tað" ;
                       <_,Sg,Dat> => "tí" ;
                       <_,Sg,Gen> => "tess" ;
                       <Masc,Pl,Nom> => "teir" ;
                       <Masc,Pl,Acc> => "teir" ;
                       <Fem,Pl,Nom> => "tær" ;
                       <Fem,Pl,Acc> => "tær" ;
                       <Neuter,Pl,Nom> => "tey" ;
                       <Neuter,Pl,Acc> => "tey" ;
                       <_,Pl,Dat> => "teimum" ;
                       <_,Pl,Gen> => "teirra"
                     } ;
    sp = Indef ;
    d = Weak
  } ;
}
