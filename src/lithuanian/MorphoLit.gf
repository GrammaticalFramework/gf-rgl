--# -path=.:../prelude:../common:../abstract
--# -coding=utf8

--1 A Lithuanian Resource Morphology 

-- L.Boizou, 2022 <lboizou@gmail.com>

-- Description of the Lithuanian morphology

resource MorphoLit = 
    ResLit, 
    MorphoVerbLit, 
    MorphoPronounLit,
    MorphoAdjectiveLit,
    MorphoNounLit ** {

     flags  coding=utf8; 

}
