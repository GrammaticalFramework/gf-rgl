concrete StructuralUkr of Structural = CatUkr ** open ResUkr, ParadigmsUkr in {
lin
  i_Pron = mkPron "я" "мене" "мені" "мене" "мені" "мною"
    (possIy "м") Masc Sg P1 ;
  youSg_Pron = mkPron "ти" "тебе" "тобі" "тебе" "тобі" "тобою"
    (possIy "тв") Masc Sg P2 ;
  he_Pron = mkPron "він" "його" "йому" "його" "ньому" "ним"
    (\_,_,_ -> "його") Masc Sg P3 ;
  she_Pron = mkPron "вона" "її" "їй" "її" "ній" "нею"
    (\_,_,_ -> "її") Fem Sg P3 ;
  it_Pron = mkPron "воно" "його" "йому" "його" "ньому" "ним"
    (\_,_,_ -> "його") Neuter Sg P3 ;
  we_Pron = mkPron "ми" "нас" "нам" "нас" "наc" "нами"
    (possAsh "н") Masc Pl P1 ;
  youPl_Pron = mkPron "ви" "вас" "вам" "вас" "вас" "вами"
    (possAsh "в") Masc Pl P2 ;
  youPol_Pron = mkPron "ви" "вас" "вам" "вас" "вас" "вами"
    (possAsh "в") Masc Pl P2 ;
  they_Pron = mkPron "вони" "їх" "їм" "їх" "них" "ними"
    possTheir Masc Pl P3 ;
  as_CAdv = {s="так само"; p="як"} ;
}
