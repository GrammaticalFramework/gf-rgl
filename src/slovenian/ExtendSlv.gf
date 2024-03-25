--# -path=.:../abstract:../common:prelude
concrete ExtendSlv of Extend = CatSlv ** open ResSlv, ParadigmsSlv, GrammarSlv in {

lin
  iFem_Pron = mkPron "jàz" "méne" "méne" "méni" "méni" ("menój"|"máno")
                     "mój"  "mòjega" "mòjemu" ("mòj"|"mòjega") "mòjem" "mòjim" 
                     "mòja" "mòjih"  "mòjima"  "mòja"          "mòjih" "mòjima"
                     "mòji" "mòjih"  "mòjim"   "mòje"          "mòjih" "mòjimi" 
                     "mòja" "mòje"   "mòji"    "mòjo"          "mòji"  "mòjo"
                     "mòji" "mòjih"  "mòjima"  "mòji"          "mòjih" "mòjima"
                     "mòje" "mòjih"  "mòjim"   "mòje"          "mòjih" "mòjimi"
                     "mòje" "mòjega" "mòjemu"  "mòjo"          "mòjem" "mòjim"
                     "mòji" "mòjih"  "mòjima"  "mòji"          "mòjih" "mòjima"
                     "mòja" "mòjih"  "mòjim"   "mòja"          "mòjih" "mòjimi" Fem Sg P1 ;
  youFem_Pron = mkPron "tí" "tébe" "tébe" "tébi" "tébi" ("tebój"|"tábo")
                       "tvój"  "tvòjega" "tvòjemu" ("tvòj"|"tvòjega") "tvòjem" "tvòjim" 
                       "tvòja" "tvòjih"  "tvòjima"  "tvòja"           "tvòjih" "tvòjima"
                       "tvòji" "tvòjih"  "tvòjim"   "tvòje"           "tvòjih" "tvòjimi" 
                       "tvòja" "tvòje"   "tvòji"    "tvòjo"           "tvòji"  "tvòjo"
                       "tvòji" "tvòjih"  "tvòjima"  "tvòji"           "tvòjih" "tvòjima"
                       "tvòje" "tvòjih"  "tvòjim"   "tvòje"           "tvòjih" "tvòjimi"
                       "tvòje" "tvòjega" "tvòjemu"  "tvòjo"           "tvòjem" "tvòjim"
                       "tvòji" "tvòjih"  "tvòjima"  "tvòji"           "tvòjih" "tvòjima"
                       "tvòja" "tvòjih"  "tvòjim"   "tvòja"           "tvòjih" "tvòjimi" Fem Sg P2 ;
  weFem_Pron = mkPron "mí" "nàs" "nàs" "nàm" "nàs" "nàmi" 
                      "nàš"  "nášega" "nášemu" ("náši"|"nášega") "nášem" "nášim"
                      "náša" "náših"  "nášima" "náša"            "náših" "nášima"     
                      "náši" "náših"  "nášim"  "náše"            "náših" "nášimi"    
                      "náša" "náše"   "náši"   "nášo"            "náši"  "nášo"
                      "náši" "náših"  "nášima" "náši"            "náših" "nášima"
                      "náše" "náših"  "nášim"  "náše"            "náših" "nášimi"
                      "náše" "nášega" "nášemu" "náše"            "nášem" "nášim"
                      "náši" "náših"  "nášima" "náši"            "náših" "nášima"
                      "náša" "náših"  "nášim"  "náša"            "náših" "nášimi" Fem Pl P1 ;
  youPlFem_Pron = mkPron "ví" "vàs" "vàs" "vàm" "vàs" "vàmi"
                         "vàš"  "vášega" "vášemu" ("váši"|"vášega") "vášem" "vášim"
                         "váša" "váših"  "vášima" "váša"            "váših" "vášima"
                         "váši" "váših"  "vášim"  "váše"            "váših" "vášimi"    
                         "váša" "váše"   "váši"   "vášo"            "váši"  "vášo"
                         "váši" "váših"  "vášima" "váši"            "váših" "vášima"
                         "váše" "váših"  "vášim"  "váše"            "váših" "vášimi"
                         "váše" "vášega" "vášemu" "váše"            "vášem" "vášim"
                         "váši" "váših"  "vášima" "váši"            "váših" "vášima"
                         "váša" "váših"  "vášim"  "váša"            "váših" "vášimi" Fem Pl P2 ;
  theyFem_Pron = mkPron "ôni" "njìh" "njìh" "njìm" "njìh" "njími" 
                        "njíhov"  "njíhovega" "njíhovemu" ("njíhov"|"njíhovega") "njíhovem" "njíhovim" 
                        "njíhova" "njíhovih"  "njíhovima"  "njíhova"             "njíhovih" "njíhovima"
                        "njíhovi" "njíhovih"  "njíhovim"   "njíhove"             "njíhovih" "njíhovimi"
                        "njíhova" "njíhove"   "njíhovi"    "njíhovo"             "njíhovi"  "njíhovo"
                        "njíhovi" "njíhovih"  "njíhovima"  "njíhovi"             "njíhovih" "njíhovima"
                        "njíhove" "njíhovih"  "njíhovim"   "njíhove"             "njíhovih" "njíhovimi"
                        "njíhove" "njíhovega" "njíhovemu"  "njíhovo"             "njíhovem" "njíhovim"
                        "njíhovi" "njíhovih"  "njíhovima"  "njíhovi"             "njíhovih" "njíhovima"
                        "njíhova" "njíhovih"  "njíhovim"   "njíhova"             "njíhovih" "njíhovimi" Fem Pl P3 ;

  youPolFem_Pron = youPlFem_Pron ;
  youPolPl_Pron  = youPol_Pron ;
  youPolPlFem_Pron = youPlFem_Pron ;

  TPastSimple = TPast ;

}

