--# -path=.:prelude:../abstract:../common

-- adapted from verb list in
-- http://www.iee.et.tu-dresden.de/~wernerr/grammar/verben_dt.html

concrete IrregGer of IrregGerAbs = CatGer ** open 
  ParadigmsGer,
  (M = MorphoGer), ResGer
in {

  flags optimize=values ;
    coding=utf8 ;

  lin backen_V =  irregV "backen" "bäckt" "backt" "backt" "gebacken" ;
  lin backen_u_V =  irregV "backen" "bäckt" "buk" "buke" "gebacken" ;
  lin befehlen_V =  irregV "befehlen" "befiehlt" "befahl" "beföhle" "befähle" ;
  lin beginnen_V =  irregV "beginnen" "beginnt" "begann" "begönne" "begänne" ;
  lin beißen_V =  irregV "beißen" "beißt" "biss" "bisse" "gebissen" ;
  lin bergen_V =  irregV "bergen" "birgt" "barg" "bärge" "geborgen" ;
  lin bersten_V =  irregV "bersten" "birst" "barst" "bärste" "geborsten" ;
  lin bewegen_V =  irregV "bewegen" "bewegt" "bewog" "bewöge" "bewogen" ;
  lin biegen_V =  irregV "biegen" "biegt" "bog" "böge" "gebogen" ;
  lin bieten_V =  irregV "bieten" "bietet" "bot" "böte" "geboten" ;
  lin binden_V =  irregV "binden" "bindet" "band" "bände" "gebunden" ;
  lin bitten_V =  irregV "bitten" "bittet" "bat" "bäte" "gebeten" ;
  lin blasen_V =  irregV "blasen" "bläst" "blies" "bliese" "geblasen" ; 
  lin bleiben_V =  irregV "bleiben" "bleibt" "blieb" "bliebe" "geblieben" ;
  lin braten_V =  irregV "braten" "brät" "briet" "briete" "gebraten" ;
  lin brechen_V =  irregV "brechen" "bricht" "brach" "bräche" "gebrochen" ;
  lin brennen_V =  irregV "brennen" "brennt" "brannte" "brennte" "gebrannt" ;
  lin bringen_V =  irregV "bringen" "bringt" "brachte" "brachte" "gebracht" ;
  lin denken_V =  irregV "denken" "denkt" "dachte" "dachte" "gedacht" ;
  lin dingen_V =  irregV "dingen" "dingt" "dingte" "dang" "gedungen" ;
  lin dreschen_V =  irregV "dreschen" "drischt" "drosch" "drösche" "gedroschen" ;
  lin dringen_V =  irregV "dringen" "dringt" "drang" "dränge" "gedrungen" ;
  lin dürfen_V = lin V (M.mkV "dürfen" "darf" "darfst" "darf" "dürft" "dürf" 
                          "durfte" "durftest" "durften" "durftet" "dürfte" "gedurft" [] 
                          M.VHaben) ; 
  lin empfehlen_V =  irregV "empfehlen" "empfiehlt" "empfahl" 
    "empfähle" "empfohlen" ;
  lin empfehlen_o_V =  irregV "empfehlen" "empfiehlt" "empfahl" 
    "empföhle" "empfohlen" ;
  lin erlöschen_V =  irregV "erlöschen" "erlischt" "erlosch" "erlösche" "erloschen" ;
  lin erkennen_V =  irregV "erkennen" "erkennt" "erkannte" "erkannte" "erkannt" ;
  lin erschrecken_V =  irregV "erschrecken" "erschrickt" "erschrak" "erschräke" "erschrocken" ;
  lin essen_V =  irregV "essen" "isst" "aß" "äße" "gegessen" ;
  lin fahren_V =  irregV "fahren" "fährt" "fuhr" "führe" "gefahren" ;
  lin fallen_V =  irregV "fallen" "fällt" "fiel" "fiele" "gefallen" ;
  lin fangen_V =  irregV "fangen" "fängt" "fing" "finge" "gefangen" ;
  lin fechten_V =  irregV "fechten" "fechtet" "focht" "föchte" "gefochten" ;
  lin finden_V =  irregV "finden" "findet" "fand" "fände" "gefunden" ;
  lin flechten_V =  irregV "flechten" "flicht" "flocht" "flöchte" "geflochten" ;
  lin fliegen_V =  irregV "fliegen" "fliegt" "flog" "flöge" "geflogen" ;
  lin fliehen_V =  irregV "fliehen" "flieht" "floh" "flöhe" "geflohen" ;
  lin fließen_V =  irregV "fließen" "fließt" "floss" "flösse" "geflossen" ;
  lin fressen_V =  irregV "fressen" "frisst" "fraß" "fräße" "gefressen" ;
  lin frieren_V =  irregV "frieren" "friert" "fror" "fröre" "gefroren" ;
  lin gären_V =  irregV "gären" "gärt" "gärte" "göre" "gegoren" ;
  lin gären_o_V =  irregV "gären" "gärt" "gor" "göre" "gegoren" ;
  lin gebären_V =  irregV "gebären" "gebiert" "gebar" "gebäre" "geboren" ;
  lin geben_V =  irregV "geben" "gibt" "gab" "gäbe" "gegeben" ;
  lin gedeihen_V =  irregV "gedeihen" "gedeiht" "gedieh" "gediehe" "gediehen" ;
  lin gehen_V =  irregV "gehen" "geht" "ging" "ginge" "gegangen" ;
  lin gelingen_V =  irregV "gelingen" "gelingt" "gelang" "gelange" "gelungen" ;
  lin gelten_V =  irregV "gelten" "gilt" "galt" "galte" "gegolten" ;
  lin gelten_o_V =  irregV "gelten" "gilt" "galt" "golte" "gegolten" ;
  lin genesen_V =  irregV "genesen" "genest" "genas" "genäse" "genesen" ;
  lin genießen_V =  irregV "genießen" "genießt" "genoss" "genösse" "genossen" ;
  lin geschehen_V =  irregV "geschehen" "geschieht" "geschah" "geschehen" "geschähe" ;
  lin gewinnen_V =  irregV "gewinnen" "gewinnt" "gewann" "gewänne" "gewonnen" ;
  lin gewinnen_o_V =  irregV "gewinnen" "gewinnt" "gewann" "gewönne" "gewonnen" ;
  lin gießen_V =  irregV "gießen" "gießt" "goss" "gösse" "gegossen" ;
  lin gleichen_V =  irregV "gleichen" "gleicht" "glich" "gliche" "geglichen" ;
  lin gleiten_V =  irregV "gleiten" "gleitet" "glitt" "glitte" "geglitten" ;
  lin glimmen_V =  irregV "glimmen" "glimmt" "glomm" "glimmte" "glömme" ;
  lin graben_V =  irregV "graben" "gräbt" "grub" "grübe" "gegraben" ;
  lin greifen_V =  irregV "greifen" "greift" "griff" "griffe" "gegriffen" ;
  lin haben_V =  irregV "haben" "hat" "hatte" "hätte" "gehabt" ;
  lin halten_V =  irregV "halten" "hält" "hielt" "hielte" "gehalten" ;
  lin hängen_V =  irregV "hängen" "hängt" "hing" "hinge" "gehangen" ;
  lin hauen_V =  irregV "hauen" "haut" "hieb" "hiebe" "gehauen" ;
  lin hauen_te_V =  irregV "hauen" "haut" "haute" "haute" "gehauen" ;
  lin heben_V =  irregV "heben" "hebt" "hob" "höbe" "gehoben" ;
  lin heißen_V =  irregV "heißen" "heißt" "hieß" "hieße" "geheißen" ;
  lin helfen_V =  irregV "helfen" "hilft" "half" "hülfe" "geholfen" ;
  lin kennen_V =  irregV "kennen" "kennt" "kannte" "kennte" "gekannt" ;
  lin klimmen_V =  irregV "klimmen" "klimmt" "klomm" "klömme" "geklommen" ;
  lin klingen_V =  irregV "klingen" "klingt" "klang" "klänge" "geklungen" ;
  lin kneifen_V =  irregV "kneifen" "kneift" "kniff" "kniffe" "gekniffen" ;
  lin kommen_V =  irregV "kommen" "kommt" "kam" "käme" "gekommen" ;
  lin können_V =  lin V (M.mkV "können" "kann" "kannst" "kann" "könnt" "könn" 
                           "konnte" "konntest" "konnten" "konntet"
                           "könnte" "gekonnt" [] M.VHaben) ;
  lin kriechen_V =  irregV "kriechen" "kriecht" "kroch" "kröche" "gekrochen" ;
  lin küren_V =  irregV "küren" "kürt" "kürte" "kor" "gekürt" ;
  lin laden_V =  irregV "laden" "lädt" "lud" "lüde" "geladen" ;
  lin lassen_V =  irregV "lassen" "lässt" "ließ" "ließe" "gelassen" ;
  lin laufen_V =  irregV "laufen" "läuft" "lief" "liefe" "gelaufen" ;
  lin leiden_V =  irregV "leiden" "leidt" "litt" "litte" "gelitten" ;
  lin leihen_V =  irregV "leihen" "leiht" "lieh" "liehe" "geliehen" ;
  lin lesen_V =  irregV "lesen" "liest" "las" "läse" "gelesen" ;
  lin liegen_V =  irregV "liegen" "liegt" "lag" "läge" "gelegen" ;
  lin lügen_V =  irregV "lügen" "lügt" "log" "löge" "gelogen" ;
  lin mahlen_V =  irregV "mahlen" "mahlt" "mahlte" "mahlte" "gemahlen" ;
  lin meiden_V =  irregV "meiden" "meidt" "mied" "miede" "gemieden" ;
  lin melken_V =  irregV "melken" "milkt" "molk" "mölke" "gemolken" ;
  lin messen_V =  irregV "messen" "misst" "maß" "mäße" "gemessen" ;
  lin mißlingen_V =  irregV "misslingen" "misslingt" "misslang" "misslungen" "misslänge" ;  -- old spelling
  lin misslingen_V =  irregV "misslingen" "misslingt" "misslang" "misslungen" "misslänge" ;
  lin mögen_V =  lin V (M.mkV "mögen" "mag" "magst" "mag" "mögt" "mög" 
                          "mochte" "mochtest" "mochten" "mochtet"
                          "möchte" "gemocht" [] M.VHaben) ;
  lin müssen_V = lin V (M.mkV "müssen" "muss" "musst" "muss" "müsst" "müss" 
                          "musste" "musstest" "mussten" "musstet"
                          "müsste" "gemusst" [] M.VHaben) ;
  lin nehmen_V = mk6V "nehmen" "nimmt" "nimm" "nahm" "nähme" "genommen" ;
  lin nennen_V =  irregV "nennen" "nennt" "nannte" "nennte" "genannt" ;
  lin pfeifen_V =  irregV "pfeifen" "pfeift" "pfiff" "pfiffe" "gepfiffen" ;
  lin preisen_V =  irregV "preisen" "preist" "pries" "priese" "gepriesen" ;
  lin quellen_V =  irregV "quellen" "quillt" "quoll" "quölle" "gequollen" ;
  lin raten_V =  irregV "raten" "rät" "riet" "riete" "geraten" ;
  lin reiben_V =  irregV "reiben" "reibt" "rieb" "riebe" "gerieben" ;
  lin reißen_V =  irregV "reißen" "reißt" "riss" "risse" "gerissen" ;
  lin reiten_V =  irregV "reiten" "reitet" "ritt" "ritte" "geritten" ;
  lin rennen_V =  irregV "rennen" "rennt" "rannte" "rennte" "gerannt" ;
  lin riechen_V =  irregV "riechen" "riecht" "roch" "röche" "gerochen" ;
  lin ringen_V =  irregV "ringen" "ringt" "rang" "ränge" "gerungen" ;
  lin rinnen_V =  irregV "rinnen" "rinnt" "rann" "ränne" "geronnen" ;
  lin rufen_V =  irregV "rufen" "ruft" "rief" "riefe" "gerufen" ;
  lin salzen_V =  irregV "salzen" "salzt" "salzte" "salzte" "gesalzen" ;
  lin saufen_V =  irregV "saufen" "säuft" "soff" "söffe" "gesoffen" ;
  lin saugen_V =  irregV "saugen" "saugt" "sog" "soge" "gesogen" ;
  lin schaffen_V =  irregV "schaffen" "schafft" "schuf" "schüfe" "geschaffen" ;
  lin scheiden_V =  irregV "scheiden" "scheidt" "schied" "schiede" "geschieden" ;
  lin scheinen_V =  irregV "scheinen" "scheint" "schien" "schiene" "geschienen" ;
  lin scheißen_V =  irregV "scheißen" "scheißt" "schiss" "schisse" "geschissen" ;
  lin schelten_V =  irregV "schelten" "schilt" "schalt" "schölte" "gescholten" ;
  lin scheren_V =  irregV "scheren" "schert" "schor" "schöre" "geschoren" ;
  lin schieben_V =  irregV "schieben" "schiebt" "schob" "schöbe" "geschoben" ;
  lin schießen_V =  irregV "schießen" "schießt" "schoss" "schösse" "geschossen" ;
  lin schinden_V =  irregV "schinden" "schindt" "schund" "schunde" "geschunden" ;
  lin schlafen_V =  irregV "schlafen" "schläft" "schlief" "schliefe" "geschlafen" ;
  lin schlagen_V =  irregV "schlagen" "schlägt" "schlug" "schlüge" "geschlagen" ;
  lin schleichen_V =  irregV "schleichen" "schleicht" "schlich" "schliche" "geschlichen" ;
  lin schleifen_V =  irregV "schleifen" "schleift" "schliff" "schliffe" "geschliffen" ;
  lin schleißen_V =  irregV "schleißen" "schleißt" "schliss" "schliss" "geschlissen" ;
  lin schließen_V =  irregV "schließen" "schließt" "schloss" "schlösse" "geschlossen" ;
  lin schlingen_V =  irregV "schlingen" "schlingt" "schlang" "schlünge" "geschlungen" ;
  lin schmeißen_V =  irregV "schmeißen" "schmeißt" "schmiss" "schmisse" "geschmissen" ;
  lin schmelzen_V =  irregV "schmelzen" "schmilzt" "schmolz" "schmölze" "geschmolzen" ;
  lin schneiden_V =  irregV "schneiden" "schneidet" "schnitt" "schnitte" "geschnitten" ;
  lin schreiben_V =  irregV "schreiben" "schreibt" "schrieb" "schriebe" "geschrieben" ;
  lin schreien_V =  irregV "schreien" "schreit" "schrie" "schrie" "geschrien" ;
  lin schreiten_V =  irregV "schreiten" "schreitet" "schritt" "schritte" "geschritten" ;
  lin schweigen_V =  irregV "schweigen" "schweigt" "schwieg" "schwiege" "geschwiegen" ;
  lin schwellen_V =  irregV "schwellen" "schwillt" "schwoll" "schwölle" "geschwollen" ;
  lin schwimmen_V =  irregV "schwimmen" "schwimmt" "schwamm" "schwämme" "geschwommen" ;
  lin schwimmen_o_V =  irregV "schwimmen" "schwimmt" "schwamm" "schwömme" "geschwommen" ;
  lin schwinden_V =  irregV "schwinden" "schwindt" "schwand" "schwände" "geschwunden" ;
  lin schwingen_V =  irregV "schwingen" "schwingt" "schwang" "schwänge" "geschwungen" ;
  lin schwören_V =  irregV "schwören" "schwört" "schwor" "schwüre" "geschworen" ;
  lin sehen_V =  irregV "sehen" "sieht" "sah" "sähe" "gesehen" ;
  lin sein_V =  ResGer.sein_V ;
  lin senden_V =  irregV "senden" "sendet" "sandte" "sandte" "gesandt" ;
  lin sieden_V =  irregV "sieden" "siedet" "sott" "sotte" "gesotten" ;
  lin singen_V =  irregV "singen" "singt" "sang" "sänge" "gesungen" ;
  lin sinken_V =  irregV "sinken" "sinkt" "sank" "sänke" "gesunken" ;
  lin sinnen_V =  irregV "sinnen" "sinnt" "sann" "sänne" "gesonnen" ;
  lin sitzen_V =  irregV "sitzen" "sitzt" "saß" "säße" "gesessen" ;
  lin sollen_V =  lin V (M.mkV "sollen" "soll" "sollst" "soll" "sollt" "soll" 
                           "sollte" "solltest" "sollten" "solltet"
                           "sollte" "gesollt" [] M.VHaben) ;

  lin speien_V =  irregV "speien" "speit" "spie" "spie" "gespien" ;
  lin spinnen_V =  irregV "spinnen" "spinnt" "spann" "spänne" "gesponnen" ;
  lin spinnen_o_V =  irregV "spinnen" "spinnt" "spann" "spönne" "gesponnen" ;
  lin spleißen_V =  irregV "spleißen" "spleißt" "spliss" "splisse" "gesplissen" ;
  lin sprechen_V =  irregV "sprechen" "spricht" "sprach" "spräche" "gesprochen" ;
  lin sprießen_V =  irregV "sprießen" "sprießt" "spross" "sprösse" "gesprossen" ;
  lin springen_V =  irregV "springen" "springt" "sprang" "spränge" "gesprungen" ;
  lin stechen_V =  irregV "stechen" "sticht" "stach" "stäche" "gestochen" ;
  lin stehen_V =  irregV "stehen" "steht" "stand" "stände" "gestanden" ;
  lin stehen_u_V =  irregV "stehen" "steht" "stand" "stünde" "gestanden" ;
  lin stehlen_V =  irregV "stehlen" "stiehlt" "stahl" "stähle" "gestohlen" ;
  lin steigen_V =  irregV "steigen" "steigt" "stieg" "stiege" "gestiegen" ;
  lin sterben_V =  irregV "sterben" "stirbt" "starb" "stürbe" "gestorben" ;
  lin stieben_V =  irregV "stieben" "stiebt" "stob" "stöbe" "gestoben" ;
  lin stinken_V =  irregV "stinken" "stinkt" "stank" "stänke" "gestunken" ;
  lin stoßen_V =  irregV "stoßen" "stößt" "stieß" "stieße" "gestoßen" ;
  lin streichen_V =  irregV "streichen" "streicht" "strich" "striche" "gestrichen" ;
  lin streiten_V =  irregV "streiten" "streitet" "stritt" "stritte" "gestritten" ;
  lin tragen_V =  irregV "tragen" "trägt" "trug" "trüge" "getragen" ;
  lin treffen_V =  irregV "treffen" "trifft" "traf" "träfe" "getroffen" ;
  lin treiben_V =  irregV "treiben" "treibt" "trieb" "triebe" "getrieben" ;
  lin treten_V =  irregV "treten" "tritt" "trat" "träte" "getreten" ;
  lin trinken_V =  irregV "trinken" "trinkt" "trank" "tränke" "getrunken" ;
  lin trügen_V =  irregV "trügen" "trügt" "trog" "tröge" "getrogen" ;
--  lin tun_V =  irregV "tun" "tut" "tat" "täte" "getan" ;
  lin tun_V = lin V (M.mkV -- HL
                       "tun" "tue" "tust" "tut" "tut" "tue" 
                       "tat" "tatest" "taten" "tatet"
                       "täte" "getan" [] M.VHaben) ;
  lin verderben_V =  irregV "verderben" "verdirbt" "verdarb" "verdarbe" "verdorben" ;
  lin vergessen_V =  irregV "vergessen" "vergisst" "vergaß" "vergäße" "vergessen" ;
  lin verlieren_V =  irregV "verlieren" "verliert" "verlor" "verlöre" "verloren" ;
  lin wachsen_V =  irregV "wachsen" "wächst" "wuchs" "wüchse" "gewachsen" ;
  lin wägen_V =  irregV "wägen" "wägt" "wog" "woge" "gewogen" ;
  lin waschen_V =  irregV "waschen" "wäscht" "wusch" "wüsche" "gewaschen" ;
  lin weben_V =  irregV "weben" "webt" "wob" "wöbe" "gewoben" ;
  lin weichen_V =  irregV "weichen" "weicht" "wich" "wiche" "gewichen" ;
  lin weisen_V =  irregV "weisen" "weist" "wies" "wiese" "gewiesen" ;
  lin wenden_V =  irregV "wenden" "wendt" "wandte" "wandte" "gewandt" ;
  lin werben_V =  irregV "werben" "wirbt" "warb" "würbe" "geworben" ;
  lin werden_V = lin V (M.mkV "werden" "werde" "wirst" "wird" "werdet" "werd" 
                          "wurde" "wurdest" "wurden" "wurdet"
                          "würde" "geworden" [] M.VHaben) ;
  lin werfen_V =  irregV "werfen" "wirft" "warf" "würfe" "geworfen" ;
  lin wiegen_V =  irregV "wiegen" "wiegt" "wog" "wöge" "gewogen" ;
  lin winden_V =  irregV "winden" "windt" "wand" "wände" "gewunden" ;
  lin wissen_V =  lin V (M.mkV "wissen" "weiß" "weißt" "weiß" "wisst" "wisse" 
                           "wusste" "wusstest" "wussten" "wusstet"
                           "wüsste" "gewusst" [] M.VHaben) ;
  lin wollen_V =  lin V (M.mkV "wollen" "will" "willst" "will" "wollt" "woll" 
                           "wollte" "wolltest" "wollten" "wolltet"
                           "wollte" "gewollt" [] M.VHaben) ;


  lin wringen_V =  irregV "wringen" "wringt" "wrang" "wränge" "gewrungen" ;
  lin zeihen_V =  irregV "zeihen" "zeiht" "zieh" "ziehe" "geziehen" ;
  lin ziehen_V =  irregV "ziehen" "zieht" "zog" "zöge" "gezogen" ;
  lin zwingen_V =  irregV "zwingen" "zwingt" "zwang" "zwänge" "gezwungen" ;


-- old spellings, before the German orthography reform
-- see https://en.wikipedia.org/wiki/German_orthography_reform_of_1996

  lin beißen_old_V =  irregV "beißen" "beißt" "biß" "bisse" "gebissen" ;
  lin fließen_old_V =  irregV "fließen" "fließt" "floß" "flösse" "geflossen" ;
  lin fressen_old_V =  irregV "fressen" "frißt" "fraß" "fräße" "gefressen" ;
  lin genießen_old_V =  irregV "genießen" "genießt" "genoß" "genösse" "genossen" ;
  lin gießen_old_V =  irregV "gießen" "gießt" "goß" "gösse" "gegossen" ;
  lin lassen_old_V =  irregV "lassen" "läßt" "ließ" "ließe" "gelassen" ;
  lin messen_old_V =  irregV "messen" "mißt" "maß" "mäße" "gemessen" ;
  lin mißlingen_old_V =  irregV "mißlingen" "mißlingt" "mißlang" "mißlungen" "mißlänge" ;
  lin müssen_old_V = lin V (M.mkV "müssen" "muß" "mußt" "muß" "müßt" "müß" 
                          "mußte" "mußtest" "mußten" "mußtet"
                          "müßte" "gemußt" [] M.VHaben) ;
  lin reißen_old_V =  irregV "reißen" "reißt" "riß" "riße" "gerissen" ;
  lin scheißen_old_V =  irregV "scheißen" "scheißt" "schiß" "schiße" "geschissen" ;
  lin schießen_old_V =  irregV "schießen" "schießt" "schoß" "schösse" "geschossen" ;
  lin schleißen_old_V =  irregV "schleißen" "schleißt" "schliß" "schliß" "geschlissen" ;
  lin schließen_old_V =  irregV "schließen" "schließt" "schloß" "schlösse" "geschlossen" ;
  lin schmeißen_old_V =  irregV "schmeißen" "schmeißt" "schmiß" "schmiße" "geschmissen" ;
  lin spleißen_old_V =  irregV "spleißen" "spleißt" "spliß" "spliße" "gesplissen" ;
  lin sprießen_old_V =  irregV "sprießen" "sprießt" "sproß" "sprösse" "gesprossen" ;
  lin vergessen_old_V =  irregV "vergessen" "vergißt" "vergaß" "vergäße" "vergessen" ;
}
