############ KIRJUTATUD STATISTIKAAMETIS
############ WRITTEN IN STATISTICS ESTONIA

# Aditiivsuse_testija
# Aditiivsuse testimiseks kasutada funktsiooni teisene_konf_arvutaja_adit_testija,
# mis sourcib seda funktsiooni.

# Vajalikud paketid:
library(forcats)
library(dplyr)
library(purrr)

# Abifunktsioonid: aditiivsuse_tester, adit_kutsuja ja ymarda  -----------

ymarda <- function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

aditiivsuse_tester <- function(sisendtabel, adit_veerg, vaartusveerg, ymardada = NULL){

  
  if(!is.null(ymardada)){
    sisendtabel[[vaartusveerg]] <- ymarda(sisendtabel[[vaartusveerg]], ymardada)
    
  }
  
  tasemed <- unique(sisendtabel[[adit_veerg]])
  tasemed_jarjekorras <- tasemed[sort(nchar(tasemed), decreasing = F)]
  
  # peab filtreerima kõik sellise taseme koodid, mille all hargneb veel, mitte need, mille all ei hargne
  # või näiteks viienda taseme liitmisel liitma kõik 4-nda taseme, mis ei hargne
  # iga koodi kaupa üles leida
  
  ei_hargne <- c()
  hargneb <- c()
  for(i in 1:(nrow(sisendtabel)-1)){
    if(nchar(sisendtabel[[adit_veerg]][i]) == nchar(sisendtabel[[adit_veerg]][i + 1]) |
       nchar(sisendtabel[[adit_veerg]][i]) > nchar(sisendtabel[[adit_veerg]][i + 1])){
      ei_hargne <- c(ei_hargne, i)
    } else{
      hargneb <- c(hargneb, i)
    }
  }
  ei_hargne <- c(ei_hargne, nrow(sisendtabel)) # viimane kood ei hargne kunagi
  
  tasemete_summad <- sapply(tasemed_jarjekorras, function(x){
    
    # vahesumma 1 - kõik sama taseme summad  
    
    vahesumma1 <- sisendtabel %>% 
      filter(!! as.name(adit_veerg) == x) %>% 
      summarise(summa = sum(!! as.name(vaartusveerg))) %>% 
      unlist %>%
      as.numeric()
    
    # vahesumma 2 - kõik kõrgema taseme summad, mis ei hargne
    # liitma peaks kõik eelnevate tasemete koodid, mis ei hargne
    
    vahesumma2 <- sisendtabel[ei_hargne, ] %>% # filtreerime kõik need, mis ei hargne
      filter(nchar(!! as.name(adit_veerg)) < nchar(x)) %>%  # kõik hierarhiliselt kõrgemad tasemed
      summarise(summa = sum(!! as.name(vaartusveerg))) %>% 
      unlist %>%
      as.numeric()
    
    
    return(vahesumma1 + vahesumma2)
  })
  # kontroll
  if(length(unique(as.integer(tasemete_summad)*10)) != 1){
    return(tasemete_summad)
  } else {return(NULL)}
}

adit_kutsuja <- function(sisendiks,   
                         tunnuse_nimi, 
                         vaartusveerg_sisend, 
                         dimList_sisend
){
  
  sisendiks <- sisendiks %>% 
    mutate(tase = dimList_sisend[[tunnuse_nimi]]$levels)
  
  vastus <- aditiivsuse_tester(
    sisendtabel = sisendiks,
    adit_veerg = "tase",
    vaartusveerg = vaartusveerg_sisend
  )
  vastus <- list(vastus)
  return(vastus)
}

# Peafunktsioon: aditiivsuse_testija  ---------------

aditiivsuse_testija <- function(KUUP_SISEND,
                                klassifitseerivad_tunnused,
                                vaartustunnus,
                                dimList){


  
  # Teha tsükkel üle kõigi klassifitseerivate ja selliselt leida iga taseme juures, kas on aditiivne
  
  kontroll_list <- lapply(klassifitseerivad_tunnused,
                          function(i){
                            
                            tabel_listi <- KUUP_SISEND %>% 
                              group_by(!!! syms(klassifitseerivad_tunnused[klassifitseerivad_tunnused != i])) %>% 
                              summarise(
                                vastusfreim = adit_kutsuja(data.frame(!! as.name(vaartustunnus), !! as.name(i), stringsAsFactors = F),
                                                           tunnuse_nimi = i, 
                                                           vaartusveerg_sisend = vaartustunnus, 
                                                           dimList_sisend = dimList))
                            names(tabel_listi)[names(tabel_listi) == "vastusfreim"] <- paste0(i, "_DIM")
                            return(tabel_listi)
                          })
  
  
  # kuvatav kontroll
  
  tagastatav_list <- lapply(kontroll_list,
                            function(y){
                              y  %>% 
                                ungroup()
                            })

  
  if(!all(sapply(tagastatav_list, function(x){
    x %>% 
      filter_at(vars(ends_with("DIM")), function(y) !is.null(unlist(y))) %>% 
      nrow
  }) == 0)){
    
    warning("Sisend ei ole adiitvne vastavalt ettantud/genereeritud hierarhiale. Tagastatakse list, kus on näidatud hierahia puudulikkus ja täpsed dimensiooniliste tasemete summad.")
    
    tagastatav_list <- lapply(tagastatav_list,
                              function(x) {
                                klass_veerud <- names(x)[!grepl("_DIM", names(x))]
                                for (j in klass_veerud){
                                  x <- x %>% 
                                    mutate(!! as.name(j) := 
                                             forcats::fct_relevel(!! as.name(j), as.character(dimList[[j]]$codes))) %>% 
                                    left_join(., dimList[[j]] %>% 
                                                rename(!! as.name(j) := codes) %>% 
                                                rename(!! as.name(paste0(j, "_levels")) := "levels"))    
                                }
                                x %>% 
                                  arrange_at(klass_veerud) %>% 
                                  mutate_if(is.factor, as.character)
                                
                              }) # järjekorra seadmine ja tasemete veeru lisamine 

    
  } else  {
    writeLines("Sisend on aditiivne.")
    return(NULL)
    }
  
  # parem teisendus
  # väärtused ei ole enam listis peidus vaid on toodud lisaveergudena
  # kus on hierarhia korras seal NA
  
  
  indeksid <- sapply(tagastatav_list, 
         function(x){
           parandatava_veeru_nimi <- names(x)[
             grepl("_DIM", names(x))]
           return(!all(sapply(x[[parandatava_veeru_nimi]], is.null)))
         }) %>% reduce(., c)
  
         
  for(j in (1:length(tagastatav_list))[indeksid]){ # tsükkel ainult üle nende, kus mõni ei ole NULL ehk kus on probleeme hierarhias
    parandatava_veeru_nimi <- names(tagastatav_list[[j]])[
      grepl("_DIM", names(tagastatav_list[[j]]))]
    
    
    lisatav_osa <- map2(tagastatav_list[[j]][[parandatava_veeru_nimi]],
                        max(sapply(tagastatav_list[[j]][[parandatava_veeru_nimi]], length)),
                        function(x,y){
                          if(length(x) == y){
                            return(x)
                          } else {return(rep(NA, y))}
                        }) %>% reduce(., rbind.data.frame, stringsAsFactors = F) 
    names(lisatav_osa) <- lapply(tagastatav_list[[j]][[parandatava_veeru_nimi]], names) %>% 
      reduce(., unique)
    
    lisatav_osa_salvestus <<- lisatav_osa
    print(lisatav_osa_salvestus)
    tagastatav_list_salvestus <<- tagastatav_list
    
    tagastatav_list[[j]] <- tagastatav_list[[j]] %>% 
      cbind(., lisatav_osa) %>% 
      select(- (parandatava_veeru_nimi)) #%>% 
      #tidyr::drop_na(.)
    # NAd hierarhia huvides alles jätta

  }
  
  return(tagastatav_list)
  
}
