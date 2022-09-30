concrete TenseHrv of Tense =
    CatHrv **
  open
    ResHrv,
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
  TPast  = {s = [] ; t = CTPast} ;
  TTAnt  t a = {s = t.s ++ a.s ; t = t.t} ; ----

}
