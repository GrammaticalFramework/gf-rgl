concrete RelativeHun of Relative = CatHun ** open
  ResHun, Prelude, (NS=NounHun), (SS=StructuralHun) in {

lin
  --  : Cl -> RCl ;            -- such that John loves her
  -- RelCl cl = ;

  -- : RP -> VP -> RCl ;
  RelVP = relVP ;

  -- : RP -> ClSlash -> RCl ; -- whom John loves
  RelSlash = relSlash ;

  -- : RP ;
  IdRP = {s =
    table {Sg => caseTable "ami" "amit" "aminek"      -- nom, acc, dat
                           "amibe"  "amiben" "amiből" -- ill, ine, ela
                           "amihez" "aminél" "amitől" -- all, ade, abl
                           "amire"  "amin"   "amiről" -- sub, sup, del
                           "amiért" "amivel" "amivé" ; -- cau, ins, tra
           Pl => caseTable "amik" "amiket" "amiknek"      -- nom, acc, dat
                           "amikbe"  "amikben" "amikből"  -- ill, ine, ela
                           "amikhez" "amiknél" "amiktől"  -- all, ade, abl
                           "amikre"  "amiken"  "amikről"  -- sub, sup, del
                           "amikért" "amikkel" "amikké"}; -- cau, ins, tra
    } ;

  -- : Prep -> NP -> RP -> RP ;  -- the mother of whom
  --FunRP prep np rp = {} ;

}
