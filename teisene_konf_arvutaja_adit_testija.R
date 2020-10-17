############ KIRJUTATUD STATISTIKAAMETIS
############ WRITTEN IN STATISTICS ESTONIA

# Argumentide selgitused:

# KUUP_SISEND - data.frame, peab sisaldama klassifitseerivate tunnuste veerge, veergu "ESMANE_KONF",
# vaartustunnuse ja sagedustunnuse veergu ja võib sisalda veergu "Z_LIPP" (sarnaselt veerule ESMANE_KONF, on tegu binaarse tunnusega,
# ainult siinkohal tähistab 1 välja lahtiseks jätmist ehk z lippu , 0 tähistab tavalist välja)
# - hierarhia_list() - list (vaikimisi tühi list), kuhu lisada hierarhia data frame-id, kui soov on vähemalt ühe klassifitseeriva tunnuse hierarhia ette anda
# - klassifitseerivad_tunnused - vektor, näiteks c("ACTIVITY", "SIZECLASS")
# - vaartustunnus -  tekstistring, näiteks "OBS_VAARTUS"
# - sagedustunnus - tekstistring, näiteks "FREQ"
# - veergude_info_tabel - data.frame (vaikimisi NULL), näiteks: 
# veergude_info_tabel <- 
#   data.frame(VEERU_NIMI = "ACTIVITY", 
#              KOODILOENDI_NIMI = "NACE_R2", 
#              TOTALI_KOODI_NIMI = "C",# kui on TOTAL olemas siis NA
#              FILTER = 'SISENDKOOD == geo_loendi_versioon' , # kui agregegeerimistabelile pole filtreeringut vaja peale panna, siis NA. See filter kutsutakse dplyr::filter() sees esile agregeerimistabeli laadimisel
#              stringsAsFactors = F)
# - meetod - tekstistring - seni on meie kuupide puhul töötanud ainult "SIMPLEHERUSTIC", aga mõne väiksema, lihtsama puhul tasub proovida ka muid meetodeid, mis saab kätte käsuga ?sdcTable::protectTable
# w_lippude_juhuslik_valimine - T või F (vaikimisi F), enam mittevajalik metoodiline hüpe, kus asendatakse z lippe juhuslikult ükshaaval w lippudega
# - aditiivsuse_test_vaartustunnus - T või F (vaikimisi T), kui T siis testib, kas väärtustunnus on aditiivne. Kui on adiitivne, siis vastvalt argumendile "piirduda_aditiivsuse_kontrolliga" kas jätkab teisese konfidentsiaalsuse arvutamist või lõpetab tegevuse
# - aditiivsuse_test_sagedustunnus - T või F (vaikimisi T), kui T siis testib, kas sagedustunnus  on aditiivne. Kui on adiitivne, siis vastvalt argumendile "piirduda_aditiivsuse_kontrolliga" kas jätkab teisese konfidentsiaalsuse arvutamist või lõpetab tegevuse
# - piirduda_aditiivsuse_kontrolliga - T või F (vaikimisi T), kui T, siis funktsioon peatub kohe peale aditiivsuse testi ja teisest konfidentsiaalsust ei arvutata. Kui adtiivsuse test töötas nii sageduse- kui ka väärtustunnuse kohta, tagastatakse NULL. Kui test ei tööta, tagastatakse list, kus on toodud kõikide klassifitsieerivate tunnuste hierarhia ja lõiked ning tasemed teise klassifitseeriva suhtes, kus summad polnud aditiivsed.


# Teisene konf arvutaja sourcimiseks (abifunktsioonid defineeritud ja sourci-tud funktsiooni sees)
# vajalikud paketid:
library(sdcTable)
library(purrr)
library(dplyr)
library(forcats)

teisene_konf_arvutaja_adit_testija <- function(KUUP_SISEND, 
                                               hierarhia_list = list(), 
                                               klassifitseerivad_tunnused,
                                               vaartustunnus,
                                               sagedustunnus,
                                               veergude_info_tabel = NULL,
                                               meetod = "SIMPLEHEURISTIC",
                                               w_lippude_juhuslik_valimine = F,
                                               aditiivsuse_test_vaartustunnus = TRUE,
                                               aditiivsuse_test_sagedustunnus = TRUE,
                                               piirduda_aditiivsuse_kontrolliga = F,
                                               protectionlevel = NULL,
                                               vastupidine_jrk = F,
                                               sisend_vastavustabel = NULL){
  
  # Argumentide loogiline kontroll -----------
  
  if(piirduda_aditiivsuse_kontrolliga == T){
    if(!all(aditiivsuse_test_sagedustunnus, aditiivsuse_test_vaartustunnus)){
      stop("Soovitakse piirduda aditiivsuse kontrolliga, kuid kumbki aditiivsuse testi argument pole TRUE.")
    }
  }
  
  
  # -------- ABIFUNKTSIOONID ------
  
  source("hierarhia_leidja_01.R")
  
  
  # automaatsel lahendamisel vajalik funktsioon hierarhia kontrollimiseks/teisendamiseks TauArgusele sobivaks sisendiks
  
  
  hierarhia_teisendaja_tauargus <- function(saadud_hierarhia_algne, KUUP_PIKAL_KUJUL, tunnus_nimi, klassifitseerivad_tunnused, vaartustunnus){
    
    # RIPPUVATE LOENDITEGA TEGELEMINE:
    # 1-  sellised mille eelmine tase on kõrgem ja järgmine tase sama või veel kõrgem
    # 2-  sellised kui tullakse tagasi kõrgemale kui enne alla mindi
    
    saadud_hierarhia <-  c(saadud_hierarhia_algne[1], sapply(2:length(saadud_hierarhia_algne), function(x) {
      # viimase juhu erand
      if(x == length(saadud_hierarhia_algne)){
        if(stringi::stri_extract_all_fixed(saadud_hierarhia_algne[x], "@") %>% reduce(., c) %>% length >
           stringi::stri_extract_all_fixed(saadud_hierarhia_algne[x-1], "@") %>% reduce(., c) %>% length ){
          return(NULL)
        } else return(saadud_hierarhia_algne[x])}
      # tingimus1
      if(stringi::stri_extract_all_fixed(saadud_hierarhia_algne[x-1], "@") %>% reduce(., c) %>% length >=
         stringi::stri_extract_all_fixed(saadud_hierarhia_algne[x+1], "@") %>% reduce(., c) %>% length  &
         stringi::stri_extract_all_fixed(saadud_hierarhia_algne[x], "@") %>% reduce(., c) %>% length >
         stringi::stri_extract_all_fixed(saadud_hierarhia_algne[x-1], "@") %>% reduce(., c) %>% length){
        return(NULL)
      } else
        # tingimus 2
        praegune <- stringi::stri_extract_all_fixed(saadud_hierarhia_algne[x], "@") %>% reduce(., c) %>% length
      if(stringi::stri_extract_all_fixed(saadud_hierarhia_algne[x+1], "@") %>% reduce(., c) %>% length >
         praegune){
        # print(x)
        edasised <- sapply(saadud_hierarhia_algne[(x+1):length(saadud_hierarhia_algne)], function(y){ stringi::stri_extract_all_fixed(y, "@") %>% reduce(., c) %>% length})
        if(length(unique(edasised)) == 1){
          return(saadud_hierarhia_algne[x])
        }
        if(stringi::stri_extract_all_fixed(saadud_hierarhia_algne[x-1], pattern = "@") %>% reduce(., c) %>% length < praegune & unique(edasised)[2]  < praegune & unique(edasised)[1] > praegune){
          return(NULL)
        } else return(saadud_hierarhia_algne[x])}
      return(saadud_hierarhia_algne[x])
    }
    ) %>% reduce(., c))
    
    
    # 3- sellised, mis on hierarhias otse mingi teise all ja selle väärtused on identsed kõrgemal olevaga (tuleb vaadata reaalseid arvutatud väärtusi)
    # kõigepealt leida koodid kus see esineb (topelt_vaartustega), seejärel rakendada sellist funktsioooni, 
    # mis kontrollib vastavust ja üleminekut
    
    abif1 <- function(x){reduce(as.character(unlist(x)), paste)}
    
    kontroll_freim <- cbind(sapply(saadud_hierarhia_algne, 
                                   function(x) stringi::stri_extract_all_fixed(x, pattern = "@") %>% reduce(., c) %>% reduce(., paste0)),
                            sapply(saadud_hierarhia_algne, gsub, pattern = "@", replacement = "") %>% reduce(., c))
    
    # Topelt väärtused on praegu olulised ainult selle osa jaoks, kus on sizeclass erandid välja võetud
    # 
    if(length(klassifitseerivad_tunnused) > 1){
      
      topelt_vaartustega <- KUUP_PIKAL_KUJUL %>%
        filter(!! as.name(tunnus_nimi) %in% kontroll_freim[,2]) %>% 
        filter(!! as.name(vaartustunnus) != 0) %>% 
        group_by(!! as.name(tunnus_nimi)) %>% 
        mutate(abi := abif1(!! as.name(vaartustunnus))) %>% # luuakse veerg, kus on kõigi selle grupi väärtused kokku paste-itud
        ungroup %>%
        group_by(abi) %>%
        ungroup %>%
        distinct(!! as.name(tunnus_nimi)) %>% # siit saame klassifitseeriva kombinatsioonid, mis väärtuse tunnuse poolest tabelis korduvad
        unlist
    } else {
      
      topelt_vaartustega <- 
        KUUP_PIKAL_KUJUL %>% 
        filter(!! as.name(vaartustunnus) != 0) %>% 
        filter(duplicated(!! as.name(vaartustunnus))) %>% 
        distinct(!! as.name(klassifitseerivad_tunnused)) %>% 
        unlist
        
    }
    if(length(topelt_vaartustega) != 0){
      
      
      # kas see viimase elemendiga töötamine vajab paremat lahendust?
      
      topelt_vaartustega <- topelt_vaartustega[!topelt_vaartustega %in% kontroll_freim[nrow(kontroll_freim),2]]
      
      saadud_hierarhia <- saadud_hierarhia[!saadud_hierarhia %in% sapply(topelt_vaartustega, function(x){
        algusindeks <- which(kontroll_freim[,2] == x)
        if(algusindeks == 1){return(NULL)}
        if(! (nchar(kontroll_freim[algusindeks-1, 1]) < nchar(kontroll_freim[algusindeks, 1]) &
              nchar(kontroll_freim[algusindeks + 1, 1]) > nchar(kontroll_freim[algusindeks , 1])                                                        )){ #mitmeastmeline langemine peab olema, kolm hierarhiat allapoole, kaks astet alla minek ei ole piisav 
          return(NULL)
        } 
        loppindeks <- which(nchar(kontroll_freim[, 1]) <= nchar(kontroll_freim[algusindeks, 1])) # lõplikuks indeksiks on selle taseme viimane kood
        # Ei saa võtta lihtsalt selle taseme järgmist koodi, vaid peab arvestama, et võib olla ka tase madalamal olev järgmine kood
        
        loppindeks <- loppindeks[loppindeks > algusindeks] %>% min() 
        
        
        if(!any(loppindeks)){
          loppindeks <- nrow(kontroll_freim)
        }
        if(loppindeks == Inf){
          loppindeks <- nrow(kontroll_freim)
        }
        
        # print(c(loppindeks, algusindeks))
        for(i in algusindeks:loppindeks){
          # salvestus <- nchar(kontroll_freim[i, 1])
          # if(salvestus >= nchar(kontroll_freim[i-1, 1])){
          
          if(i == nrow(kontroll_freim)){
            if(nchar(kontroll_freim[i, 1]) > nchar(kontroll_freim[algusindeks, 1])){
              return((saadud_hierarhia_algne[algusindeks]))  # kontrollitakse, kas järgmise koodiga kogu hierarhia lõppeb ja kas lõpuindeks on suurem, siis tagastatakse algne kood
            } else {
              return(NULL)
            }
          }
          
          if((nchar(kontroll_freim[i + 1, 1]) == nchar(kontroll_freim[algusindeks - 1, 1])) &
             (nchar(kontroll_freim[i, 1]) >=  (nchar(kontroll_freim[algusindeks, 1]) + 1))  # oluline lisatingimus, et see tase oleks sama, kui on üks hüppe lõppindeksi kohal allapoole, siis sama põhimõte ei kehti
          ){ # kontrollitakse, kas järgmise koodiga tase lõppeb, siis tagastatakse algne kood
             # print("kehtib")
            return(saadud_hierarhia_algne[algusindeks])
            # }
          } 
        }
      }) %>% reduce(., c)]
      
      
    } 
    
    
    if(!identical(saadud_hierarhia,saadud_hierarhia_algne)){
      
      warning(paste("Tauarguse sisendiks võeti välja järgmised koodid: ", reduce(saadud_hierarhia_algne[!saadud_hierarhia_algne %in% saadud_hierarhia], paste)))
    }
    
    return(saadud_hierarhia)
  } 
  
  # funktsiooni lõpp
  # ---------- AUTOMAATSED HIERARHIAD, KUI PUUDU ---------------
  
  laadida_hierarhia_imetast <- klassifitseerivad_tunnused[!klassifitseerivad_tunnused %in% names(hierarhia_list)]
  
  if(length(laadida_hierarhia_imetast) != 0 & is.null(veergude_info_tabel)){
    stop("Soovitakse laadida Imetast hierarhia (käsitsi hierarhiat pole määratud), aga pole täpsustatud veergude info tabel!")
  }
  
  
  puuduvate_salvestus <- list()
  
  for(tunnus_nimi in laadida_hierarhia_imetast){
    
    # Kui KUUP on välja arvutatud, saab anda, ette juba olemasoleva koodiloendi unique käsku rakendades
    etteantud_koodiloend_sisend <- unique(KUUP_SISEND[[tunnus_nimi]]) # näiteks unique(KUUP$COUNTY)
    
    koodiloendi_nimi_sisend <- veergude_info_tabel %>% 
      filter(VEERU_NIMI == tunnus_nimi) %>% 
      select(KOODILOENDI_NIMI) %>% 
      unlist
    
    totali_koodi_nimi_sisend <- veergude_info_tabel %>% 
      filter(VEERU_NIMI == tunnus_nimi) %>% 
      select(TOTALI_KOODI_NIMI) %>% 
      unlist
    
    if(is.na(totali_koodi_nimi_sisend)){
      totali_koodi_nimi_sisend <- NULL
    }
    
    totali_holmamine_sisend <- T # reeglina on see alati TRUE, ehk soovitakse sellist hierarhiat, kus on olemas kõike summeeriv TOTALi tase
    filter_agregeerimistabelile_sisend <- veergude_info_tabel[veergude_info_tabel$VEERU_NIMI == tunnus_nimi, "FILTER"]
    
    if("AGREGEERIMISTABEL" %in% names(veergude_info_tabel)){
    etteantud_agregeerimistabel_sisend <- veergude_info_tabel[veergude_info_tabel$VEERU_NIMI == tunnus_nimi, "AGREGEERIMISTABEL"][[1]][[1]]
    } else {
      etteantud_agregeerimistabel_sisend <- NULL
    }
    
    saadud_hierarhia_algne <- hierarhia_leidja_01(etteantud_koodiloend = etteantud_koodiloend_sisend, 
                                                  koodiloendi_nimi =  koodiloendi_nimi_sisend,
                                                  totali_holmamine = totali_holmamine_sisend,
                                                  koodiloendi_eksemplar = NULL,
                                                  totali_koodi_nimi = totali_koodi_nimi_sisend,
                                                  filter_agregeerimistabelile = filter_agregeerimistabelile_sisend,
                                                  etteantud_agregeerimistabel = etteantud_agregeerimistabel_sisend)
    
    # Automaatne järelteisendus, mis kuvab, mis koodid olid rippuvad ja eemaldati loendist. Siin ei tule midagi muuta.
    # parandatud selliselt, et peaks toimima ühemõõtmelise kui ka mitmemõõtmelise juhu jaoks (EKOMARis vajalik teisendus)
    # kui pole midagi vaja teisendada, peaks tagastama algse hierharhia (pole midagi rippuvat)
    
    
    saadud_hierarhia <- hierarhia_teisendaja_tauargus(saadud_hierarhia_algne, KUUP_PIKAL_KUJUL = KUUP_SISEND , tunnus_nimi = tunnus_nimi, klassifitseerivad_tunnused = klassifitseerivad_tunnused,
                                                      vaartustunnus = vaartustunnus)
    
    
    # objekti nimi peaks vastama hierarhiale, näiteks dim.COUNTY kui automaatselt genereerida, aga muu osa jääb samaks
    # ehk dim.COUNTY <- data.frame(... jne
    
    UUS_HIERARHIA <- data.frame(codes = sapply(saadud_hierarhia, gsub, pattern = "@", replacement = ""), 
                                levels = sapply(saadud_hierarhia, function(x) stringi::stri_extract_all_fixed(x, "@") %>% 
                                                  reduce(., c) %>% 
                                                  reduce(., paste0)),
                                stringsAsFactors = F) %>% 
      select(levels, codes) 
    
    
    # et aditiivsuse tester töötaks, ei tohi holla kahetasemelisi vahesid hierarhiate vahel, kui selleks pole põhjust
    # näiteks kui eelmine kuuenda taseme hierarhia lõppes ja algab uus kolmanda taseme hierarhia ,siis on okei
    # aga kui on kolmanda taseme ülemine kood ja see on järgmisel alamastmel kuuenda taseme või viienda taseme oma,
    # siis tuleb see ära parandada. Võib tekkida tänu hierarhia teisendamisele
    
    # praegu hetkel teha lihtsal kujul hoiatus, kui mingi tase on välja võetud ja ülevalt alla järjestikust langemist ei ole
    
    for(i in 1:(nrow(UUS_HIERARHIA) - 1)){
      if(nchar(UUS_HIERARHIA$levels[i + 1]) > nchar(UUS_HIERARHIA$levels[i]) &
         (nchar(UUS_HIERARHIA$levels[i + 1]) - nchar(UUS_HIERARHIA$levels[i]) > 1)){
        
        # kuni allapoole mineva muutuseni salvestada
        for(j in ((i+1):(nrow(UUS_HIERARHIA)))){
          if(nchar(UUS_HIERARHIA[j,"levels"]) < nchar(UUS_HIERARHIA[i+1,"levels"])){
            salvestus <- UUS_HIERARHIA[i:(j-1),]
            break
          }
        }
        
        puuduvate_salvestus <- append(puuduvate_salvestus, list(salvestus))
        
        
      } 
    }
    
    
    # HIERARHIA LIST
    # Oluline on, et listi objektide nimed vastaksid klassifitseerivate tunnuste nimedele
    # näiteks dimList <- list(COUNTY = dim.COUNTY)
    
    hierarhia_list <- append(hierarhia_list, list(UUS_HIERARHIA))
    
    if(is.null(names(hierarhia_list))){
      names(hierarhia_list) <- tunnus_nimi # kui dimListil veel nimesid pole, ei saa subsettida NA tüüpi objekti
    } else {
      names(hierarhia_list)[length(names(hierarhia_list))] <- tunnus_nimi # ? - teha eraldi loendur, mis tagaks töötamise ka hierarhia listi puudumise puhul
    }
    
    
  } # for tsükli lõpp
  
  
  if(length(puuduvate_salvestus) != 0){
    writeLines("Hierarhiate väljavõtmisel muutusid järjestikused astmed, võib esineda roh kem kui ühetasandilisi hüppeid. Sellisel juhul aditiivsuse test ei toimi! Samuti on Tauarguse sisend sellisel juhule ekslik! Tagastatakse saadud hierarhiad ja list(id), kus on puuduvad hierarhiad toodud.")
    return(append(hierarhia_list, puuduvate_salvestus))
  }
  
  
  # DimListi väärtuse omistamine, hierarhia list on saadud kas täiesti uute Imetast genereeritud hierarhiatega või siis segamini
  # kus ettantud hierarhia_listile on lisatud Imetast genereeritud osa
  
  dimList <- hierarhia_list
  
  
  # Lisaargument - jrk ära vahetada. NB! OLULINE ON ET ON ENNE KUUBI JÄRJESTAMIST; MUIDU TULEB VIGA ÜLEVAATE FREIMIS -------------
  
  if(vastupidine_jrk == T){
    if(length(dimList) != 2){
      stop("Vastupidine järjekord saab olla ainult siis, kui on kaks klassifitseerivat muutujat.")
    }
    
    
    nimed_salvestus <- names(dimList)
    
    dimList <- 
      dimList <- list(hierarhia_list[[2]], hierarhia_list[[1]])
    names(dimList) <- c(nimed_salvestus[2], nimed_salvestus[1]) 
  }
  
  
  # --------- SISENDI KORREKTNE JÄRJESTAMINE --------
  
    for( i in names(dimList)){ # sama järjerkord peab olema nagu hierarhia listis
    KUUP_SISEND <- 
      KUUP_SISEND %>% 
      filter_at(i, function(x) x %in% dimList[[i]]$codes)
    KUUP_SISEND[[i]] <- forcats::fct_relevel(KUUP_SISEND[[i]], as.character(dimList[[i]]$codes))
  }
  
  KUUP_SISEND <- KUUP_SISEND %>% 
    arrange_at(names(dimList)) %>% 
    mutate_if(is.factor, as.character)
  
  
  # ADITIIVSUSE TEST , loodud 14.05.20 ---------
  # peamine funktsioon
  
  if(aditiivsuse_test_vaartustunnus == T){
    
    source("aditiivsuse_testija.R")
    
    tulemus <- aditiivsuse_testija(KUUP_SISEND,
                                   klassifitseerivad_tunnused,
                                   vaartustunnus,
                                   dimList)
    
    if(!is.null(tulemus)){ # kui on null, võib jätkata ja sisend oli aditiivne
      warning("Sisendi väärtustunnus ei ole adiitvne vastavalt ettantud/genereeritud hierarhiale. Tagastatakse list, kus on näidatud hierahia puudulikkus ja täpsed dimensiooniliste tasemete summad.")
      return(tulemus)
    }
  }
  
  
  
  if(aditiivsuse_test_sagedustunnus == T){
    
    source("aditiivsuse_testija.R")
    
    tulemus <- aditiivsuse_testija(KUUP_SISEND,
                                   klassifitseerivad_tunnused,
                                   sagedustunnus,
                                   dimList)
    
    if(!is.null(tulemus)){ # kui on null, võib jätkata ja sisend oli aditiivne
      warning("Sisendi sagedustunnus (FREQ ehk ridade arv gruppides) ei ole adiitvne vastavalt ettantud/genereeritud hierarhiale. Tagastatakse list, kus on näidatud hierahia puudulikkus ja täpsed dimensiooniliste tasemete summad.")
      return(tulemus)
    }
  }
  
  if(piirduda_aditiivsuse_kontrolliga == T){
    return(tulemus)
  } # teisest konfidentsiaalsust ei arvutata
  
  
  # --------- TEISESE OBJEKTI MOODUSTAMINE ---------

  
  naidis_alus <-  sdcTable::makeProblem(
    data = KUUP_SISEND,
    dimList = dimList,
    freqVarInd = which(names(KUUP_SISEND) == sagedustunnus),
    numVarInd = which(names(KUUP_SISEND) == vaartustunnus) # määrab meile pakkuva tunnuse asukoha
  ) # dimlisti võivad jääda hierarhiad, mida sees andmetes ei ole, kui nad on õiges kohas
  
  
  # Luuakse ülevaatlik freim kontrollimaks, kas sisendtabeli ettandmine toimis korrektselt
  
  ylevaade <- data.frame(vaartus = (naidis_alus@problemInstance@numVars %>% unlist), id =
                           (naidis_alus@problemInstance@strID %>% unlist),
                         freq = (naidis_alus@problemInstance@Freq %>% unlist),
                         konf = naidis_alus@problemInstance@sdcStatus)
  
  if(KUUP_SISEND %>% nrow != ylevaade %>% nrow){
    stop("Ridade arv Tauarguse objekti ja sisendi freimi vahel ei võrdu")
  }
  
  ylevaade2 <- cbind(ylevaade, KUUP_SISEND)
  
  # Visuaalne ülevaade Tauarguse objekti ja sisendkuubi väärtuste vahel (pole tingimata vajalik) -----------
  
  vaartuste_ylevaatefreim2 <- ylevaade2 %>% filter(round(vaartus,2) != round(!! as.name(vaartustunnus),2)) %>% mutate(abiks = vaartus - !! as.name(vaartustunnus))
  View(vaartuste_ylevaatefreim2)
  
  FREQ_ylevaatefreim2 <- ylevaade2 %>% filter(freq != !! as.name(sagedustunnus)) %>% mutate(abiks = freq - !! as.name(sagedustunnus))
  View(FREQ_ylevaatefreim2)
  
  
  # KONTROLL ENNE ESMASE LISAMIST ----------
  
  kontroll_vaartus <- ylevaade2 %>% 
    filter(round(vaartus,2) != round(!! as.name(vaartustunnus),2)) %>% 
    mutate(abiks = vaartus - !! as.name(vaartustunnus)) %>% 
    mutate(abiks = round(abiks, 3)) %>% 
    distinct(abiks) %>% 
    unlist
  
  if(any(kontroll_vaartus)){
    if(all(kontroll_vaartus) != 0){
      warning("SdcTable pakett ei anna hierarhiates samu sageduse vaartusi nagu esmase arvutus, ylevaade2$abiks ei ole null!")
    }
  }
  
  kontroll_vaartus_freq <- ylevaade2 %>% 
    filter(freq != !! as.name(sagedustunnus)) %>% 
    mutate(abiks = freq - !! as.name(sagedustunnus)) %>% 
    mutate(abiks = round(abiks, 3)) %>% 
    distinct(abiks) %>% 
    unlist
  if(any(kontroll_vaartus_freq)){
    if(all(kontroll_vaartus_freq) != 0){
      warning("SdcTable pakett ei anna hierarhiates samu sageduse vaartusi nagu esmase arvutus, ylevaade2$abiks ei ole null! Funktsioon tagastab freimi, kus veerus 'abiks' on ettepanekud sageduse väärtuste muuutmiseks. Peale vastavat sageduse tunnuse väärtuse veeru parandamist, proovida käivitada funktsioon uuesti.")
      return(FREQ_ylevaatefreim2)
    }
  }
  
  
  # ESMASE KONFIDENTSIAALSUSE LISAMINE OBJEKTI ------------
  
  objekt_lahendamiseks <-  naidis_alus
  
  
  # sdcStatus: a vector containing the suppression state for each cell 
  # (possible values are 
  #   ???u???: primarysuppression,
  #   ???x???: secondarysuppression,
  #   ???z???: forced for publication, 
  #   ???s???: publishable cell,
  #   ???w???: dummy cells that are considered only when applying the simple greedy heuristic to protect the table)
  
 
valik1 <- (KUUP_SISEND %>%
      filter(ESMANE_KONF == 1) %>%
      filter(!! as.name(vaartustunnus) != 0) %>% # tuleb ette määrata varem
      select( (klassifitseerivad_tunnused)) %>%
      as.data.frame())
  
for (i in 1:nrow(valik1)){
    characteristics <- valik1[i,]
    varNames <- klassifitseerivad_tunnused
    verbose <- FALSE
    rule <- 'u'
    objekt_lahendamiseks <- sdcTable::changeCellStatus(objekt_lahendamiseks, characteristics, varNames, rule, verbose = FALSE)
  }

  # Kui on antud sisendiks ja väljad, mis tuleb avatuks jätta, siis siin need määrata z-ideks
  
  if("Z_LIPP" %in% names(KUUP_SISEND)){
    if(any(KUUP_SISEND$Z_LIPP == 1)){
      
      if(all(KUUP_SISEND %>% 
             filter(Z_LIPP == 1) %>% 
             distinct(!!as.name(vaartustunnus)) %>% 
             unlist == 0)){
        stop("Kõikide Z lippudega väljade puhul on vaartustunnuse väärtus 0!")
      }
      
      valik2 <- (KUUP_SISEND %>%
         filter(Z_LIPP == 1) %>% 
         filter(!! as.name(vaartustunnus) != 0) %>% # tuleb ette määrata varem
         select( (klassifitseerivad_tunnused)) %>%
         as.data.frame())
      
      for (i in 1:nrow(valik2)){
        characteristics <- valik2[i,]
        varNames <- klassifitseerivad_tunnused
        verbose <- FALSE
        rule <- 'z'
        objekt_lahendamiseks <- sdcTable::changeCellStatus(objekt_lahendamiseks, characteristics, varNames, rule, verbose = FALSE)
        
      }
    }
  }
  
  # W lippude ettemääramiseks loodud veeru kasutamine (kui on loodud) ---------------

if("W_LIPP" %in% names(KUUP_SISEND)){
  if(any(KUUP_SISEND$W_LIPP == 1)){
    if(all(KUUP_SISEND %>% 
       filter(W_LIPP == 1) %>% 
       distinct(!!as.name(vaartustunnus)) %>% 
       unlist == 0)){
      stop("Kõikide W lippudega väljade puhul on vaartustunnuse väärtus 0!")
    }
    
    valik3 <- (KUUP_SISEND %>%
                 filter(W_LIPP == 1) %>% 
                 filter(!! as.name(vaartustunnus) != 0) %>% # tuleb ette määrata varem
                 select( (klassifitseerivad_tunnused)) %>%
                 as.data.frame())
    
    for (i in 1:nrow(valik3)){
      characteristics <- valik3[i,]
      varNames <- klassifitseerivad_tunnused
      verbose <- FALSE
      rule <- 'w'
      objekt_lahendamiseks <- sdcTable::changeCellStatus(objekt_lahendamiseks, characteristics, varNames, rule, verbose = FALSE)
      
    }
  }
}

  
  # TEISESE OBJEKTI VÄLJASTAMINE ------------
  # 13.12.19 töötab ainult simpleheuristic
  # W lippude valikuline simuleerimine 
  # vaikimisi argumendina  on FALSE
  
  if(w_lippude_juhuslik_valimine == T){
    
    indeksid <- which(objekt_lahendamiseks@problemInstance@sdcStatus == "z")
    
    # mahukas
    # valitav_koht_vektor <- c()
    valitav_koht_vektor <- sample(indeksid, length(indeksid),replace = F)
    
    algus <- timestamp()
    
    for (i in 1:length(valitav_koht_vektor)){
      
      objekt_lahendamiseks@problemInstance@sdcStatus[valitav_koht_vektor[i]] <- "w"
      test <- try(sdcTable::getInfo(sdcTable::protectTable(objekt_lahendamiseks, verbose=T, useC=TRUE, method = meetod), type = "finalData"))
      if(is.data.frame(test)){
        break
      }
    }
    
  }
  
  if(meetod != "HYPERCUBE"){
    TEISENE_KONF_FREIM <- print(sdcTable::getInfo(sdcTable::protectTable(objekt_lahendamiseks, verbose=T, useC=TRUE, method = meetod), type = "finalData"))
  } else {
    if(!is.null(protectionlevel)){
    TEISENE_KONF_FREIM <- print(sdcTable::getInfo(sdcTable::protectTable(objekt_lahendamiseks, verbose=T, useC=TRUE, method = meetod, protectionLevel = protectionlevel), type = "finalData"))
    } else {
      TEISENE_KONF_FREIM <- print(sdcTable::getInfo(sdcTable::protectTable(objekt_lahendamiseks, verbose=T, useC=TRUE, method = meetod), type = "finalData"))  
    }
    
  }
  
  
  return(TEISENE_KONF_FREIM)
  
}

