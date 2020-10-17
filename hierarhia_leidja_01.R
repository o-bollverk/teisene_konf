# HIERARHIA LEIDJA 01 SOURCIMISEKS 

# Mitmest abifunktsioonist koosnev funktsioon.
# Siin on mitu aspekti, mis puhtalt R-i programmeerimiskeele ja põhimõtete osas vajaks parandamist.
# Näiteks see, et globaalsesse keskkonda luuakse muutujad iga kord selle funktsiooni esilekutsumisel.

# VAJALIKUD PAKETID  ------------

library(purrr)
library(dplyr)
library(stringi)
library(data.table)
library(stringr)

# PEAFUNKTSIOONIS KASUTATAVATE FUNKTSIOONIDE DEFINEERIMINE----------------

rekursiivne_otsija <- function(kood, lugeja = 1, abiindeks = NULL, uus_vahelist){
  indeks <- which(reduce(reduce(stringi::stri_split_fixed(kood, "@"),c), paste0) == names(uus_vahelist))
  indeksi_salvestus <-   c(indeksi_salvestus, indeks)
  if(! exists("abiindeks")){abiindeks <- indeks}
  abiindeksi_salvestus <-  c(abiindeksi_salvestus, abiindeks)
  
  # kui jõuatakse kohta, kus pole koodi üldises listis
  if (!any(indeks)){
    if(! any(indeksi_salvestus)){
      
      indeksvektor_abi <<-  c(indeksvektor_abi, abiindeksi_salvestus[length(abiindeksi_salvestus)])
      pikkus <- length(unique(indeksvektor_abi[which(indeksvektor_abi == 1)[length(which(indeksvektor_abi == 1))]:length(indeksvektor_abi)]))
      if(gsub(sisendvaartus, pattern = "@", replacement = "") %in% names(uus_vahelist)){
      return(paste0(reduce(rep("@", pikkus), paste0) ,uus_vahelist[[abiindeks]]))
      } else return(paste0(reduce(rep("@", pikkus), paste0) ,uus_vahelist[[indeksvektor_abi[length(indeksvektor_abi)]]]))
    }
    
    pikkus <- length(unique(indeksvektor_abi[which(indeksvektor_abi == 1)[length(which(indeksvektor_abi == 1))]:length(indeksvektor_abi)]))
    return(paste0(reduce(rep("@", pikkus), paste0) ,uus_vahelist[[abiindeks]]))
  } else{
    lugeja <-  1 
    vahevektor <- uus_vahelist[[indeks]]
    for (i in uus_vahelist[[indeks]]){
      indeksvektor_abi <<-  c(indeksvektor_abi, abiindeksi_salvestus[length(abiindeksi_salvestus)])
      return(rekursiivne_otsija(i, lugeja = lugeja, abiindeks = indeks, uus_vahelist = uus_vahelist))
    }
  }
}

rekurs_peafun <- function(sisendvaartus, aluslist, duplikaadid_sisse) {
  if (!gsub(sisendvaartus, pattern = "@", replacement = "") %in% names(aluslist)){
    return(sisendvaartus)
  }
  
  
  sisendvaartus <<- sisendvaartus
  aluslist <<- aluslist
  duplikaadid_sisse <<- duplikaadid_sisse
  rekursiivne_lahendaja_string1 <- '
  indeksi_salvestus <- c()
  abiindeksi_salvestus <- c()
  indeksvektor_abi <- c()
  kood_sisendiks <-'
  rekursiivne_lahendaja_string12 <- '
  uus_vahelist <-'
  rekursiivne_lahendaja_string2 <- '
  uus_vahelist_muudetud <-'
  rekursiivne_lahendaja_string3 <- '
  indeksvektor_abi_eelmine <- NA
  
  tulemus <- rekursiivne_otsija(kood_sisendiks, uus_vahelist = uus_vahelist)
  
  ylemkategooria <- names(uus_vahelist)[unique(indeksvektor_abi[which(indeksvektor_abi == 1)[length(which(indeksvektor_abi == 1))]:length(indeksvektor_abi)])]
  ylemkategooria <- paste0(sapply(sapply(seq_along(ylemkategooria), function(x) rep("@", x)), function(y) reduce(y, paste0)), ylemkategooria)
  
  vahevastus <- c(ylemkategooria, tulemus)
  
  oige_muudatus <- (unique(indeksvektor_abi[which(indeksvektor_abi == 1)[length(which(indeksvektor_abi == 1))]:length(indeksvektor_abi)]))
  if(length(oige_muudatus != 1)){
  oige_muudatus <- oige_muudatus[length(oige_muudatus) - 1]
  }
  uus_vahelist_muudetud[[oige_muudatus]] <-  uus_vahelist_muudetud[[oige_muudatus]][-1]
  
  while(!identical(indeksvektor_abi_eelmine, indeksvektor_abi)){
  indeksvektor_abi_eelmine <- indeksvektor_abi
  
  uusvastus <- rekursiivne_otsija(kood_sisendiks, uus_vahelist = uus_vahelist_muudetud)  
  if(length(indeksvektor_abi) == 0){
  next
  }
  
  
  ylemkategooria_eelmine <- ylemkategooria
  ylemkategooria <- names(uus_vahelist)[unique(indeksvektor_abi[which(indeksvektor_abi == 1)[length(which(indeksvektor_abi == 1))]:length(indeksvektor_abi)])]
  ylemkategooria <- paste0(sapply(sapply(seq_along(ylemkategooria), function(x) rep("@", x)), function(y) reduce(y, paste0)), ylemkategooria)
  
# ylemkategooria on ainult uus osa
  ylemkategooria <- ylemkategooria[!(ylemkategooria %in%  ylemkategooria_eelmine)]

  oige_muudatus <- (unique(indeksvektor_abi[which(indeksvektor_abi == 1)[length(which(indeksvektor_abi == 1))]:length(indeksvektor_abi)]))
  
  if(length(oige_muudatus) != 1){
  oige_muudatus <- oige_muudatus[length(oige_muudatus) - 1]
  }
  uus_vahelist_muudetud[[oige_muudatus]] <-  uus_vahelist_muudetud[[oige_muudatus]][-1]
  names(uus_vahelist_muudetud)[sapply(uus_vahelist_muudetud, function(x) length(x) == 0)]  <- ""

  if(length(ylemkategooria) == 0){
  vahevastus <- c(vahevastus,uusvastus)
  } else
  
  if(!(all(ylemkategooria %in% vahevastus))){
  vahevastus <- c(vahevastus,ylemkategooria, uusvastus)
  } else {
  vahevastus <- c(vahevastus, uusvastus)
  }
  }
  
  if(duplikaadid_sisse == F){
  vahevastus <- sapply(seq_along(vahevastus), function(x){ if (vahevastus[x] %in% vahevastus[duplicated(vahevastus)]){
  if (x == min(which(vahevastus == vahevastus[x]))){
  return(vahevastus[x])} else {return(NULL)}} else
  return(vahevastus[x])}) %>% reduce(., c)
  }
  vahevastus
  '
  
  
  eval(parse(text = paste0(rekursiivne_lahendaja_string1, 
                           toString(expression(sisendvaartus)))), envir = .GlobalEnv)
  
  eval(parse(text = paste0(rekursiivne_lahendaja_string12, 
                           toString(expression(aluslist)))), envir = .GlobalEnv)
  
  eval(parse(text = paste0(rekursiivne_lahendaja_string2, 
                           toString(expression(aluslist)))), envir = .GlobalEnv)
  
  vastus <- eval(parse(text = rekursiivne_lahendaja_string3), envir = .GlobalEnv)
  
  return(vastus)
  
}

# kontroll pidevalt kas kõik loendid on sees - kui mitte, mis on puudu # EI KASUTATA
kontrollija <- function(x, loend){
  loend[!loend %in% reduce(sapply(x, gsub, pattern = "@", replacement = ""),c)]
}


# sisendis ei tohiks olla topelttegemist
# hakkab ülevalt alla mineama ja kontrollib koode, mis on juba all olemas, need välja visata
# tuleks teha kontroll, et kas kõik on alles
# abikoodi vajadusel lisamine

# HIERARHIA_LEIDJA_01 DEFINEERIMINE ----------------


hierarhia_leidja_01 <- function(koodiloendi_eksemplar = NULL,
                                koodiloendi_nimi,
                                yhendus = con_defineeritud,
                                skeema = NULL,
                                vastavustabeli_asukoht = NULL,
                                vastavustabel_sisend = NULL,
                                koodiloendi_asukoht = NULL,
                                totali_holmamine = TRUE,
                                duplikaadid_sisse = T, 
                                etteantud_koodiloend = NULL,
                                totali_koodi_nimi = NULL,
                                filter_agregeerimistabelile = NA,
                                etteantud_agregeerimistabel = NULL){

  
  # SISEND -----------------------
  
  if(is.null(etteantud_koodiloend)){
  koodiloend <- dbReadTable(
    conn = yhendus,
    schema  = skeema,
    name  = koodiloendi_asukoht)  %>% 
    filter(EKSEMPLARI_NIMI == koodiloendi_eksemplar) %>% 
    select(KOODILOEND, KOODILOEND_ID) 
  } else{
    koodiloend <- data.frame(KOODILOEND = etteantud_koodiloend, stringsAsFactors = F)
  }
  
  if(is.null(etteantud_agregeerimistabel)){
    
  agregeerimistabel <- dbReadTable(
    conn = yhendus,
    schema  = skeema,
    name   = vastavustabeli_asukoht) %>% 
    filter(VALJUNDIKOOD_PEALKIRI == koodiloendi_nimi) 
  
  } else {
    agregeerimistabel <- etteantud_agregeerimistabel
  }
  
  if(!is.na(filter_agregeerimistabelile)){
    agregeerimistabel <- agregeerimistabel %>% 
      filter(eval(parse(text = filter_agregeerimistabelile)))
  } 
  
  agregeerimistabel <- agregeerimistabel %>% 
    # filter(!(FIRST_CLASSIF_CODE  == "GEOloe2014" & SISENDIKOOD == "688" & VALJUNDIKOOD  ==  "RS")) %>% 
    select(SISENDIKOOD_ID , SISENDIKOOD,VALJUNDIKOOD_ID, VALJUNDIKOOD) %>% 
    filter(if(totali_holmamine == T){VALJUNDIKOOD %in% c(koodiloend$KOODILOEND, "TOTAL")} else {VALJUNDIKOOD %in% c(koodiloend$KOODILOEND)})
  

  if(!is.null(totali_koodi_nimi)){
    agregeerimistabel <- agregeerimistabel %>% 
      mutate(VALJUNDIKOOD = ifelse(VALJUNDIKOOD == totali_koodi_nimi, "TOTAL", VALJUNDIKOOD))
    
    koodiloend <- koodiloend %>% 
      mutate(KOODILOEND = ifelse(KOODILOEND == totali_koodi_nimi, "TOTAL", KOODILOEND))
  }
  
  # Esimene veateade, kui puuduvad mingid koodid Imetast
  if(length(koodiloend$KOODILOEND[!koodiloend$KOODILOEND %in% agregeerimistabel$VALJUNDIKOOD] != 0)){
  warning(paste0("Imetast puuduvad koodid (jäetakse praegu loendist välja): ", reduce(koodiloend$KOODILOEND[!koodiloend$KOODILOEND %in% agregeerimistabel$VALJUNDIKOOD], paste, sep = ", ")))
  }
  
  koodiloend <- koodiloend %>% 
    filter(KOODILOEND %in% agregeerimistabel$VALJUNDIKOOD)
  
  loigatud_tabel <- agregeerimistabel %>%
    arrange(VALJUNDIKOOD) %>% 
    split.data.frame(., .$VALJUNDIKOOD)
  
  # Grupid list moodustamine
  grupid_list <- vector(mode = "list")
  for (i in names(loigatud_tabel)){
    olemasolu_list <-  lapply(map(loigatud_tabel[names(loigatud_tabel) != i], 2), function(y){loigatud_tabel[[i]]$SISENDIKOOD %in% y}) %>% map(., unique)
    grupid_list[[i]] <- lapply(names(olemasolu_list), function(z){ if(all(olemasolu_list[[z]]) == T){
      return(z)
    }}) %>% reduce(., c)
  }
  
  if(length(names(loigatud_tabel)[!names(loigatud_tabel) %in% names(grupid_list)]) != 0 ){
   warning(paste("Leiti koodid, mis ei kuulu ühtegisse teise koodi:", 
           reduce(names(loigatud_tabel)[!names(loigatud_tabel) %in% names(grupid_list)], paste, sep = "; ")))
  }
  
  # TOTALi lisamine valikuliseks teha
  if(!("TOTAL" %in% unique(koodiloend$KOODILOEND)) & 
     totali_holmamine == T){
    nimed_eksemplarist <- c("TOTAL", unique(koodiloend$KOODILOEND))  
  } else {
    nimed_eksemplarist <- c(unique(koodiloend$KOODILOEND))  
  }
  
  grupid_tegelik2 <- list_along(grupid_list[names(grupid_list) %in% nimed_eksemplarist])
  names(grupid_tegelik2) <- names(grupid_list)[names(grupid_list) %in% nimed_eksemplarist]
  
  for (i in names(grupid_list)){
    yksused  <- grupid_list[[i]]
    juba_arvutatud <- names(grupid_tegelik2)[reduce(sapply(names(grupid_tegelik2), function(x){!is.null(grupid_tegelik2[[x]])}), c)]
    yksused <- yksused[!(yksused %in% juba_arvutatud)]
    for (j in yksused[yksused %in% nimed_eksemplarist]){
      for (z in names(grupid_list)){
        if (j %in% grupid_list[[z]]) {
          grupid_tegelik2[[j]] <- append(grupid_tegelik2[[j]], z) 
        }
      }
    }
  }
  
  
  
  # KUI EI ARVUTA ADITIIVSUST. SIIN TULEKS FILTREERIDA VÄLJA AINULT NEED MIS ON ALGSES JA SIIS SAAB JUBA REKURSIIVSE FUNKTSIOONI JAOKS SISENDI -----------
  
  grupid_tegelik2 <- lapply(grupid_tegelik2, function(x) {
    vastus <- x[x %in% koodiloend$KOODILOEND]
    vastus <- sapply(vastus, function(y) paste0("@", y))
    if(length(vastus) != 0){return(vastus)}})
  
  grupid_tegelik2 <- grupid_tegelik2[reduce(lapply(grupid_tegelik2, function(x) length(x) != 0), c)]
  lapply(grupid_tegelik2, function(x) if(length(x) == 1) warning("LEIDUB GRUPP, MIS MOODUSTUB AINULT ÜHEST ALAMGRUPIST."))
  
  # abikoodi pole vaja grupid_tegelik3 <- lapply(names(grupid_tegelik2), function(x) if(length(grupid_tegelik2[[x]]) == 1) c(grupid_tegelik2[[x]], paste0("@", x, "_ABIKOOD")) else grupid_tegelik2[[x]])
  
  # Sorteerimine vastavalt alamgruppide hõlmamise arvule. Eespool peab olema kategooria, mis hõlmab enim alamgruppe jne. Kui on võrdsed, siis pikem eespool.
  
  grupid_tegelik3 <- grupid_tegelik2
  grupid_tegelik3 <- 
    grupid_tegelik3[names(sort(sapply(grupid_tegelik3, function(x) sum(sapply(x, gsub, pattern = "@", replacement = "") %in% names(grupid_tegelik3))), decreasing = T))]
  
  nimede_salvestus <- names(grupid_tegelik3)
  # eeldab eelnevat sorteerimist pikkuse järgi
  
  
  # uus viis sisekuubi jaoks - teha erand gruppidele:
  # hetkel jätan vahele
  # 
  # grupid_tegelik3 <- lapply(1:(length(grupid_tegelik3)), function(x){
  #   if(x == length(grupid_tegelik3)){
  #     return(grupid_tegelik3[[x]])
  #   }
  #   reduce(sapply(grupid_tegelik3[[x]], function(y) { if (y %in% reduce(grupid_tegelik3[(x + 1):length(grupid_tegelik3)], c) & !(y %in% paste0("@",names(grupid_tegelik3)))){
  #     return(NULL) } else return(y)}), c)
  # })

  # vana viis:
  # --------
  # grupid_tegelik3 <- lapply(1:(length(grupid_tegelik3)), function(x){
  #   if(x == length(grupid_tegelik3)){
  #     return(grupid_tegelik3[[x]])
  #   }
  #   reduce(sapply(grupid_tegelik3[[x]], function(y) { if (y %in% reduce(grupid_tegelik3[(x + 1):length(grupid_tegelik3)], c)){
  #     return(NULL) } else return(y)}), c)
  # })
  
  # koodiloendi järjekorra järgi filtreerimine
  # grupid_tegelik3[names(grupid_tegelik3) %in% koodiloend$KOODILOEND]
  # names(grupid_tegelik3)
  # #koodiloend$KOODILOEND
  # names(grupid_tegelik_abi)
  # grupid_tegelik_abi <- grupid_tegelik3[names(grupid_tegelik3) %in% koodiloend$KOODILOEND]
  # grupid_tegelik3 <- 
  #   lapply(1:(length(grupid_tegelik3)), function(x){
  #   if(x == length(grupid_tegelik3)){
  #     return(grupid_tegelik3[[x]])
  #   }
  #   reduce(sapply(grupid_tegelik3[[x]], function(y) { if (y %in% reduce(grupid_tegelik3[(x + 1):length(grupid_tegelik3)], c)){
  #     return(NULL) } else return(y)}), c)
  # })
  #--------
  names(grupid_tegelik3) <- nimede_salvestus
  
  
  # Iga listi objekti sees sorteerimine, et enim kategooriad hõlmav oleks vektoris eespool
  
  grupid_tegelik3 <- lapply(grupid_tegelik3, 
                            function(x){ 
                              klapivad <- match(names(grupid_tegelik3),sapply(x, gsub, pattern = "@", replacement = ""))
                              #print(paste0( klapivad))
                              klapivad <- klapivad[!is.na(klapivad)]
                              if(length(klapivad) != 0) {
                                vastus <- c(x[klapivad], x[-klapivad])} else {vastus <- x}
                              return(vastus)
                            })
  
  
  # ÜHEMÕÕTMELISE ERANDI KONTROLL -----------
  
  if(length(grupid_tegelik3) == 1){
    saadud_hierarhia <- c(paste0("@", names(grupid_tegelik3)[[1]]), paste0("@", grupid_tegelik3[[1]]))
    
    if(!all(c(names(grupid_tegelik3), sapply(reduce(grupid_tegelik3, c), gsub, pattern = "@", replacement = "")) %in% sapply(reduce(saadud_hierarhia, c), gsub, pattern = "@", replacement = ""))){
      stop("KÕIK KOODILOENDI KOODID EI OLE HIERARHILISES LOETELUS!")
    }
    
    # et midagi ei oleks topelt
  if(duplikaadid_sisse == F){
    if(!all(!duplicated(saadud_hierarhia))){
      stop("SAADUD HIERARHIAS ON TOPELTKOODE!")
    }
  }
    return(saadud_hierarhia)
  }
  
  # ÜLDKESKKONNA MUUTMISE HOIATUS ----------
  warning(reduce(c(" Globaalsesse keskkonnda (.GlobalEnv) luuakse uued muutujad: ", c("abiindeksi_salvestus", "aluslist",                 "hierarhia_vastus",         "indeksi_salvestus",        "indeksvektor_abi",         "indeksvektor_abi_eelmine", "kood_sisendiks",           "oige_muudatus",            "salvestus",
                                                                                      "sisendvaartus",            "tulemus",                  "uus_vahelist",             "uus_vahelist_muudetud",    "uusvastus" ,               "vahevastus" ,              "ylemkategooria", "duplikaadid_sisse")), paste, sep = ", "))
  
  # 
  # vastus <- vector(mode = "list")
  # 
  # abivekt <- substr(koodiloend$KOODILOEND,1,1)
  # muutus <- 1
  # for(i in 1:(length(abivekt) - 1)){
  #   if(abivekt[i] != abivekt[i+ 1]){
  #     # print(i)
  #     vahevastus <- koodiloend$KOODILOEND[(muutus + 1): i]
  #     vastus <- c(vastus, list(vastus, vahevastus))
  #     vastus <- vastus[-(length(vastus) -1)]
  #     names(vastus)[length(vastus)] <- vastus[[length(vastus)]][1]
  #     muutus <- i
  #   }
  # }
  # #names(vastus)[1] <- koodiloend$KOODILOEND[1]
  # vastus
  # filtreerimis_list <- vastus
  # filtreerimis_list <- lapply(filtreerimis_list, function(x) paste0("@",x))
  # filtreerimis_list <- c(grupid_tegelik3[1], filtreerimis_list)
  # filtreerimis_list <-      filtreerimis_list[reduce(lapply(filtreerimis_list, function(x) length(x) > 1),c)]
  # nimede_salvestus <- names(filtreerimis_list)
  # filtreerimis_list <- 
  #   lapply(1:(length(filtreerimis_list)), function(x){
  #     if(x == length(filtreerimis_list)){
  #       return(filtreerimis_list[[x]])
  #     }
  #     reduce(sapply(filtreerimis_list[[x]], function(y) { if (y %in% reduce(filtreerimis_list[(x + 1):length(filtreerimis_list)], c)){
  #       return(NULL) } else return(y)}), c)
  #   })
  # names(filtreerimis_list) <- nimede_salvestus
  # # filtreerimis_list[[1]] <- sapply(filtreerimis_list[[1]], function(x) if(!gsub(x, pattern = "@", replacement ="") %in% names(filtreerimis_list)){
  # #   return(NULL)
  # # } else return(x))
  # 
  # filtreerimis_list[[1]] <- paste0("@",names(filtreerimis_list)[2:length(names(filtreerimis_list))])
  # filtreerimis_list <- lapply(filtreerimis_list, function(x) x[2:length(x)])
  # filtreerimis_list <-      filtreerimis_list[reduce(lapply(filtreerimis_list, function(x) length(x) > 1),c)]
  # # 
  
  # 
  # # filtreerimisloogika oleks selline - võtta ainult need sisaldused nagu praegu loendist saab lugeda
  # filtreerimis_list2 <-      filtreerimis_list[reduce(lapply(filtreerimis_list, function(x) length(x) > 3),c)]
  # 
  # filtreerimis_list2 <- filtreerimis_list2[names(filtreerimis_list2) != "M711"]
  # grupid_tegelik3[names(filtreerimis_list2)] <- filtreerimis_list2
  # 
  
  
  # PEAFUNKTSIOONI RAKENDAMINE-----------
  
  # õige rakendamine - ainult nende kategooriate peal kus vaja
  # Valida peakategooria, rakendada selle peal ja kontrollida, et kõik koodid oleksid kaetud ega oleks topeltväärtusi
  
  #saadud_hierarhia <- rekurs_peafun(paste0("@",names(grupid_tegelik3)[1]), grupid_tegelik3, duplikaadid_sisse = duplikaadid_sisse)
  #saadud_hierarhia <- rekurs_peafun(paste0("@",names(filtreerimis_list)[1]), filtreerimis_list, duplikaadid_sisse = duplikaadid_sisse)
  
  # 
  # grupid_tegelik3$TOTAL <- sapply(grupid_tegelik3$TOTAL, function(x) {
  #   if(gsub(x, pattern = "@", replacement = "") %in% names(grupid_tegelik3)){
  #     return(x)
  #   } else {return(NULL)}
  #   }) %>% reduce(., c)
  
  # sellega visatakse välja sellised, mida mida grupid_tegelik3 nimedes ei ole selle mõttega, et teise või madalama taseme alamkoodid saaksid välja võetud
  # sisse tuleb aga jätta need, mis on ainult TOTALi all

  grupid_tegelik3$TOTAL <- sapply(grupid_tegelik3$TOTAL, function(x) {
    if(!(x %in% reduce(grupid_tegelik3[2:length(grupid_tegelik3)], c))){
      return(x)
    } else {return(NULL)}
  }) %>% reduce(., c)
  
  
  saadud_hierarhia <- rekurs_peafun(paste0("@",names(grupid_tegelik3)[1]), grupid_tegelik3, duplikaadid_sisse = duplikaadid_sisse)
  
  
  # SORTEERIMINE
  # filter - mitte piisavalt sügavad tasemed välja jätta
  
  saadud_hierarhia <- sapply(saadud_hierarhia, function(x) {
    if(sum(sapply(saadud_hierarhia, gsub, pattern = "@", replacement = "") == gsub(x, pattern = "@", replacement = "")) > 1) {
      if(nchar(x) != max(nchar((saadud_hierarhia)[sapply(saadud_hierarhia, gsub, pattern = "@", replacement = "") == gsub(x, pattern = "@", replacement = "")]))){
        return(NULL)
      } else return(x)
     } else return(x) 
  }) %>% reduce(., c)
    
 # UUED TEISENDUSED 
  kontrollija(saadud_hierarhia, koodiloend$KOODILOEND)
  
  # 1. TOTAL välja visata välja arvatud esimesel juhul

saadud_hierarhia <- saadud_hierarhia[c(T, saadud_hierarhia[2:length(saadud_hierarhia)] != "@TOTAL")]


# 2. Eelistada duplikaatidest neid, millele järgneb muutus

freim_lopu_lisamiseks <- 
  cbind(sapply(sapply(saadud_hierarhia[duplicated(saadud_hierarhia)], function(x) reduce(reduce(stri_extract_all_fixed(x, "@"), c), paste)), nchar),
sapply(saadud_hierarhia[duplicated(saadud_hierarhia)], function(x) reduce(reduce(stri_extract_all_fixed(x, "@"), c), paste)),
saadud_hierarhia[duplicated(saadud_hierarhia)]) %>% 
  as.data.frame(., stringsAsFactors = F) 

lisada <- c()

if(nrow(freim_lopu_lisamiseks) != 0){
    
    for ( i in c(1:(nrow(freim_lopu_lisamiseks)-1))){
    if(freim_lopu_lisamiseks[i,1] != freim_lopu_lisamiseks[i,1 + 1] &
       length(unique(freim_lopu_lisamiseks[c(i+1):nrow(freim_lopu_lisamiseks),1 + 1])) == 1){
      lisada <- c(lisada, freim_lopu_lisamiseks[i + 1, 3])
    }
  }
  
  # kui on kõik samal tasandil, siis lihtsam juht
  if(length(unique(freim_lopu_lisamiseks[,1])) == 1){
    lisada <- freim_lopu_lisamiseks[,3]
  }

}

# allolev parandus ei tegele selle erandjuhtumiga kui duplikaat on järjekorras viimane, selleks on tehtud vektor lisada

saadud_hierarhia <- sapply(seq_along(saadud_hierarhia), function(x) if( saadud_hierarhia[x] %in% saadud_hierarhia[duplicated(saadud_hierarhia)]){
  if (length(reduce(stringi::stri_extract_all_fixed(saadud_hierarhia[x +1], "@"),c)) > 
      length(reduce(stringi::stri_extract_all_fixed(saadud_hierarhia[x], "@"),c))){
    return(saadud_hierarhia[x])
  }
    else   {return(NULL)}
} else return(saadud_hierarhia[x])
) %>% reduce(., c)

saadud_hierarhia <- c(saadud_hierarhia, lisada)

if(!all(c(names(grupid_tegelik3), sapply(reduce(grupid_tegelik3, c), gsub, pattern = "@", replacement = "")) %in% sapply(reduce(saadud_hierarhia, c), gsub, pattern = "@", replacement = ""))){
  stop(paste0("VIGA SAMMUS '# 2. Eelistada duplikaatidest neid, millele järgneb muutus'. KÕIK KOODILOENDI KOODID EI OLE HIERARHILISES LOETELUS! Puuduvad:", c(names(grupid_tegelik3), sapply(reduce(grupid_tegelik3, c), gsub, pattern = "@", replacement = ""))[!c(names(grupid_tegelik3), sapply(reduce(grupid_tegelik3, c), gsub, pattern = "@", replacement = "")) %in% sapply(reduce(saadud_hierarhia, c), gsub, pattern = "@", replacement = "")] %>% reduce(paste, sep =", ")))
}



  # 3. Valida loetelus duplikaatidest esimene, ülejäänud välja jätta.

saadud_hierarhia <- sapply(seq_along(saadud_hierarhia), function(x) if( saadud_hierarhia[x] %in% saadud_hierarhia[duplicated(saadud_hierarhia)]){
  if (x == min(which(saadud_hierarhia == saadud_hierarhia[x]))){
    return(saadud_hierarhia[x])
  } else return(NULL)
} else return(saadud_hierarhia[x])) %>% reduce(., c) 



  # KONTROLLID - MÕLMEAD PEAVAD ANDMA TRUE --------------
  
  # kontrollid - et kõik koodid ja nimed oleks olemas
  
  if(!all(c(names(grupid_tegelik3), sapply(reduce(grupid_tegelik3, c), gsub, pattern = "@", replacement = "")) %in% sapply(reduce(saadud_hierarhia, c), gsub, pattern = "@", replacement = ""))){
    stop(paste0("KÕIK KOODILOENDI KOODID EI OLE HIERARHILISES LOETELUS! Puuduvad:", c(names(grupid_tegelik3), sapply(reduce(grupid_tegelik3, c), gsub, pattern = "@", replacement = ""))[!c(names(grupid_tegelik3), sapply(reduce(grupid_tegelik3, c), gsub, pattern = "@", replacement = "")) %in% sapply(reduce(saadud_hierarhia, c), gsub, pattern = "@", replacement = "")] %>% reduce(paste, sep =", ")))
  }
  
  # et midagi ei oleks topelt
  
  # kontroll uuesti kõikide koodide kohta algses sisendis
 if(!is.null(etteantud_koodiloend)){
   if(!all(etteantud_koodiloend[etteantud_koodiloend != totali_koodi_nimi] %in% stri_replace_all_fixed(saadud_hierarhia, pattern = "@", replacement = ""))){
     stop("KÕIK ETTEANTUD KOODILOENDI KOODID EI OLE SAADUD HIERARHIAS")
   }
 }

# Välja jätta tühi "@" kui see on sisse jäänud???


  saadud_hierarhia <- saadud_hierarhia[saadud_hierarhia != "@"]

  # kontrollida, kas kõik mis sisendiks anti on saadud hierarhias olemas
  if(!all(koodiloend$KOODILOEND %in% stri_replace_all_fixed(saadud_hierarhia, pattern = "@", replacement = ""))){
    warning("KÕIK SOOVITUD KOODID POLE SAADUD HIERARHIAS")  
  }
  
    if(duplikaadid_sisse == F){
  if(!all(!duplicated(saadud_hierarhia))){
    stop("SAADUD HIERARHIAS ON TOPELTKOODE!")
  }
  }
  
  if(!is.null(totali_koodi_nimi)){
  saadud_hierarhia[saadud_hierarhia == "@TOTAL"] <- paste0("@", totali_koodi_nimi)
  }
  return(saadud_hierarhia)
  
}

