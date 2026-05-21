--# -path=.:prelude:../abstract:../common
concrete IrregHun of IrregHunAbs = CatHun ** open ParadigmsHun in {
lin
  áll_V = mkV "állok" "állsz" "áll" "állunk" "álltok" "állnak"
              "álltam" "álltál" "állt" "álltunk" "álltatok" "álltak"
              "állni" "álló" "állva" ;
  átugrik_V = mkV "átugrom" "átugrasz" "átugrik" "átugrunk" "átugrotok" "átugranak"
                  "átugrottam" "átugrottál" "átugrott" "átugrottunk" "átugrottatok" "átugrottak"
                  "átugrani" "átugrikó" "átugrikva" ;
  bovelkedik_V = mkV "bővelkedem" "bővelkedsz" "bővelkedik" "bővelkedünk" "bővelkedtek" "bővelkednek"
                     "bővelkedtem" "bővelkedtél" "bővelkedett" "bővelkedtünk" "bővelkedtetek" "bővelkedtek"
                     "bővelkedni" "bővelkedő" "bővelkedve" ;
  csatlakozik_V = mkV "csatlakozom" "csatlakozol" "csatlakozik" "csatlakozunk" "csatlakoztok" "csatlakoznak"
                       "csatlakoztam" "csatlakoztál" "csatlakozott" "csatlakoztunk" "csatlakoztatok" "csatlakoztak"
                       "csatlakozni" "csatlakozó" "csatlakozva" ;
  csökken_V = mkV "csökkenek" "csökkensz" "csökken" "csökkenünk" "csökkentek" "csökkennek"
                  "csökkentem" "csökkentél" "csökkent" "csökkentünk" "csökkentetek" "csökkentek"
                  "csökkenni" "csökkenő" "csökkenve" ;
  dolgozik_V = mkV "dolgozom" "dolgozol" "dolgozik" "dolgozunk" "dolgoztok" "dolgoznak"
                   "dolgoztam" "dolgoztál" "dolgozott" "dolgoztunk" "dolgoztatok" "dolgoztak"
                   "dolgozni" "dolgozó" "dolgozva" ;
  elvet_V = mkV "elvetek" "elvetsz" "elvet" "elvetünk" "elvettek" "elvetnek"
                "elvetettem" "elvetettél" "elvetett" "elvetettünk" "elvetettetek" "elvetettek"
                "elvetni" "elvető" "elvetve" ;
  eszik_V = mkV "eszem" "eszel" "eszik" "eszünk" "esztek" "esznek"
                "ettem" "ettél" "evett" "ettünk" "ettetek" "ettek"
                "enni" "evő" "éve" ;
  fut_V = mkV "futok" "futsz" "fut" "futunk" "futtok" "futnak"
              "futottam" "futottál" "futott" "futottunk" "futottatok" "futottak"
              "futni" "futó" "futva" ;
  jár_V = mkV "járok" "jársz" "jár" "járunk" "jártok" "járnak"
              "jártam" "jártál" "járt" "jártunk" "jártatok" "jártak"
              "járni" "járó" "járva" ;
  küld_V = mkV "küldök" "küldesz" "küld" "küldünk" "küldötök" "küldenek"
               "küldtem" "küldtél" "küldött" "küldtünk" "küldtetek" "küldtek"
               "küldeni" "küldő" "küldve" ;
  meglátogat_V = mkV "meglátogatok" "meglátogatsz" "meglátogat" "meglátogatunk" "meglátogattok" "meglátogatnak"
                     "meglátogattam" "meglátogattál" "meglátogatott" "meglátogattunk" "meglátogattatok" "meglátogattak"
                     "meglátogatni" "meglátogató" "meglátogatva" ;
  tud_V = mkV "tudok" "tudsz" "tud" "tudunk" "tudtok" "tudnak"
              "tudtam" "tudtál" "tudott" "tudtunk" "tudtatok" "tudtak"
              "tudni" "tudó" "tudva" ;
  úszik_V = mkV "úszom" "úszol" "úszik" "úszunk" "úsztok" "úsznak"
                "úsztam" "úsztál" "úszott" "úsztunk" "úsztatok" "úsztak"
                "úszni" "úszó" "úszva" ;
}
