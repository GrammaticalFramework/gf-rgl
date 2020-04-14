concrete TenseCze of Tense =
    CatCze **
  open
    ResCze,
    Prelude
  in {
lin
  PNeg = {
    s = "ne" ++ Predef.BIND ;
    p = False
    } ;
  PPos = {
    s = [] ;
    p = True
    } ;
  ASimul = {s = [] ; t = CTPres} ;
  TPres  = {s = [] ; t = CTPres} ;
  TTAnt  t a = {s = t.s ++ a.s ; t = t.t} ; ----

}