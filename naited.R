# Testnäited ----------------
# Pakettide kontroll ---------
soltuvused <- c("sdcTable", "purrr", "dplyr", "stringi", "data.table", "stringr", "forcats")

for (i in soltuvused){
  if(!require(i)){
    install.packages(i)
  }  
}

# Näited: kõige lihtsam ühedimensiooniline juht, kus hierarhia genereeritakse etteantud agregeerimistabeli pealt ------------

# 1) Hierarhia leidja -------------


source("hierarhia_leidja_01.R")

ettantud_agregeerimistabel <- 
  data.frame(SISENDIKOOD = c("01", "01", "011", "011", "011", "0111", "0111", "0111", "0111", "02", "02", "022", "022", "022", "03", "03"),
             VALJUNDIKOOD = c("A01", "TOTAL", "A011", "TOTAL", "A01", "A0111", "A011", "A01", "TOTAL",
                             "A02","TOTAL", "A022","A02", "TOTAL", "A03", "TOTAL"))

ettantud_agregeerimistabel <- ettantud_agregeerimistabel %>% 
  mutate(SISENDIKOOD_ID = 1:nrow(.)) %>% 
  mutate(VALJUNDIKOOD_ID = 1:nrow(.)) %>% 
  mutate(KOODILOEND = "PEAKOODI_LOEND")

etteantud_koodiloend <- c("A01", "TOTAL", "A02", "A03", "A0111", "A011", "A022")

saadud_hierarhia <- hierarhia_leidja_01(etteantud_koodiloend = etteantud_koodiloend,
                                totali_holmamine = TRUE,
                                duplikaadid_sisse = T, 
                                etteantud_agregeerimistabel = ettantud_agregeerimistabel)
print(saadud_hierarhia)

# 2) teisene_konf_arvutaja_adit_testija -------------
# 2.1) Aditiivne tabel ------------

source("teisene_konf_arvutaja_adit_testija.R")

KUUP_NAIDIS <- 
  data.frame(PEAKOOD =  c("TOTAL", "A01", "A02", "A03"),
             VAARTUSED = c(100, 40, 40, 20),
             FREQ = c(20, 8, 8, 4),
              ESMANE_KONF = c(0, 0, 1, 0),
             stringsAsFactors = F)

veergude_info_tabel <- 
  tibble(VEERU_NIMI = c("PEAKOOD"),
             KOODILOENDI_NIMI = c("PEAKOODI_LOEND"),
             AGREGEERIMISTABEL = list(ettantud_agregeerimistabel),
         TOTALI_KOODI_NIMI = NA,
         FILTER = NA)

aditiivsuse_kontroll <- teisene_konf_arvutaja_adit_testija(KUUP_SISEND = KUUP_NAIDIS, 
         hierarhia_list = list(), 
         klassifitseerivad_tunnused = c("PEAKOOD"),
         vaartustunnus = "VAARTUSED",
         sagedustunnus = "FREQ",
         veergude_info_tabel = veergude_info_tabel,
         meetod = "SIMPLEHEURISTIC",
         w_lippude_juhuslik_valimine = F,
         aditiivsuse_test_vaartustunnus = TRUE,
         aditiivsuse_test_sagedustunnus = TRUE,
         piirduda_aditiivsuse_kontrolliga = T,
         protectionlevel = NULL,
         vastupidine_jrk = F)

kontrollitud_freim <- teisene_konf_arvutaja_adit_testija(KUUP_SISEND = KUUP_NAIDIS, 
                                                           hierarhia_list = list(), 
                                                           klassifitseerivad_tunnused = c("PEAKOOD"),
                                                           vaartustunnus = "VAARTUSED",
                                                           sagedustunnus = "FREQ",
                                                           veergude_info_tabel = veergude_info_tabel,
                                                           meetod = "SIMPLEHEURISTIC",
                                                           w_lippude_juhuslik_valimine = F,
                                                           aditiivsuse_test_vaartustunnus = TRUE,
                                                           aditiivsuse_test_sagedustunnus = TRUE,
                                                           protectionlevel = NULL,
                                                           vastupidine_jrk = F)
print(kontrollitud_freim)

# 2.2) Mitteaditiivne tabel ------------


KUUP_NAIDIS <- 
  data.frame(PEAKOOD =  c("TOTAL", "A01", "A02", "A03"),
             VAARTUSED = c(100, 40, 30, 20),
             FREQ = c(20, 8, 8, 4),
             stringsAsFactors = F)

aditiivsuse_kontroll <- teisene_konf_arvutaja_adit_testija(KUUP_SISEND = KUUP_NAIDIS, 
                                                           hierarhia_list = list(), 
                                                           klassifitseerivad_tunnused = c("PEAKOOD"),
                                                           vaartustunnus = "VAARTUSED",
                                                           sagedustunnus = "FREQ",
                                                           veergude_info_tabel = veergude_info_tabel,
                                                           meetod = "SIMPLEHEURISTIC",
                                                           w_lippude_juhuslik_valimine = F,
                                                           aditiivsuse_test_vaartustunnus = TRUE,
                                                           aditiivsuse_test_sagedustunnus = TRUE,
                                                           piirduda_aditiivsuse_kontrolliga = T,
                                                           protectionlevel = NULL,
                                                           vastupidine_jrk = F)

print(aditiivsuse_kontroll)


# 2.3
