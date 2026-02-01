# tbl1_Ordre_T: Samler tabeller via left join ----
tbl1_Ordre_T <- Fact_Ordre %>%

  # Left join Dim1_OrdreStatus
  left_join(y = Dim1_OrdreStatus, na_matches = "never", by = "OrdreStatusKat_ID") %>%
  left_join(y = Dim1_Billet, na_matches = "never", by = "Billet_ID") %>%
  left_join(y = Dim2_EventAar, na_matches = "never", by = "EventAar_ID") %>%
  left_join(y = Dim3_Event, na_matches = "never", by = "Event_ID") %>%
  left_join(y = Dim2_BilletKat, na_matches = "never", by = "BilletKat_ID") %>%
  left_join(y = Dim2_BilletDisciplin, na_matches = "never", by = "BilletDisciplin_ID") %>%
  left_join(y = Dim2_BilletRække, na_matches = "never", by = "BilletRaekke_ID") %>%
  left_join(y = Dim2_BilletSpilformat, na_matches = "never", by = "BilletSpilFormat_ID") %>%
  left_join(y = Dim1_Klub, na_matches = "never", by = "Klub_ID") %>%
  left_join(y = Dim2_KlubLandsdel, na_matches = "never", join_by(
      "KlubPostnr_RD" >= "KlubLandsdelPostnrMin_RD",
      "KlubPostnr_RD" <= "KlubLandsdelPostnrMaks_RD")) %>%
    
  left_join(y = Dim2_KlubRegion, na_matches = "never", by = "KlubRegion_ID") %>%
  left_join(y = Dim1_DeltKoen, na_matches = "never", by = "DeltKoen_ID") %>%
  left_join(y = Dim1_DeltSlutspil, na_matches = "never", by = "DeltSlutspil_ID") %>%
  left_join(y = Dim1_DeltPlacering, na_matches = "never", by = "DeltPlac_ID") %>%
  left_join(y = Dim1_DeltRating, na_matches = "never", join_by(
      "DeltRating2_RD" >= "DeltRatingKatMin_RD",
      "DeltRating2_RD" <= "DeltRatingKatMaks_RD")) %>%
  
  # Left join Dim1_OrdreFoersteTid
  group_by(EventAar_ID, DeltID_RD) %>%
  mutate(OrdreFoersteDatoTid_DW = min(OrdreDatoTid_RD)) %>%
  ungroup() %>%
  mutate(OrdreFoersteTid_DW = format(OrdreFoersteDatoTid_DW, format = "%H%M%S")) %>%
  left_join(y = Dim1_OrdreFoersteTid, na_matches = "never", join_by(
      "OrdreFoersteTid_DW" >= "OrdreFoersteTidKatMin_RD",
      "OrdreFoersteTid_DW" <= "OrdreFoersteTidKatMaks_RD")) %>%

  # Left join Dim1_OrdreKat
  mutate(OrdreKat_ID = case_when(
    grepl("Tilmeldt", OrdreStatusSimpelKat_RD) & OrdreFoersteDatoTid_DW <= EventAarFristDatoTid_RD ~ "Ordinær",
    grepl("Tilmeldt", OrdreStatusSimpelKat_RD) & OrdreFoersteDatoTid_DW >  EventAarFristDatoTid_RD ~ "Drive-in",
    grepl("Afbud",    OrdreStatusSimpelKat_RD) ~ "Afbud")) %>%
  left_join(y = Dim1_OrdreKat, na_matches = "never", by = "OrdreKat_ID") %>%
  
  # Left join Dim1_DeltAlderKat
  group_by(EventAar_ID) %>%
  mutate(EventAarStartDatoTid_DW = min(BilletStartDatoTid_RD)) %>%
  ungroup() %>%
  mutate(DeltFoedtDato_DW = as_date(case_when(
    is.na(DeltID_RD) ~ as.character(EventAarStartDatoTid_DW),
    substr(DeltID_RD, 5, 6) <= substr(EventAarStartDatoTid_DW, 3, 4) ~ paste0(
      as.numeric(substr(EventAarStartDatoTid_DW, 1, 2)), substr(DeltID_RD, 5, 6), "-",
      substr(DeltID_RD, 3, 4), "-",
      substr(DeltID_RD, 1, 2)),
    TRUE ~ paste0(
      as.numeric(substr(EventAarStartDatoTid_DW, 1, 2))-1, substr(DeltID_RD, 5, 6), "-",
      substr(DeltID_RD, 3, 4), "-",
      substr(DeltID_RD, 1, 2))))) %>%
  mutate(DeltAlder_DW = trunc((DeltFoedtDato_DW %--% EventAarStartDatoTid_DW) / years(1))) %>%
  left_join(y = Dim1_DeltAlderKat, na_matches = "never", join_by(
      "DeltAlder_DW" >= "DeltAlderKatMin_RD",
      "DeltAlder_DW" <= "DeltAlderKatMaks_RD")) %>%
  
  # Left join Dim1_DeltGenKat
  mutate(EventAarFra2021_DW = if_else(year(EventAarStartDatoTid_DW) >= 2021, TRUE, FALSE)) %>%
  group_by(DeltID_RD, BilletKat_ID, EventAarFra2021_DW) %>%
  arrange(OrdreDatoTid_RD, BilletKat_ID) %>%
  mutate(DeltGenNr_DW = ifelse(grepl("Tilmeldt", OrdreStatusSimpelKat_RD), 1, 0)) %>%
  mutate(DeltGenNr_DW = cumsum(DeltGenNr_DW)) %>%
  ungroup() %>%
  mutate(DeltGenKat_ID = case_when(
    DeltGenNr_DW == 1 ~ "Debutant",
    DeltGenNr_DW >= 2 ~ "Gentilmelding",
    TRUE ~ "Ikke hidtil")) %>%
  left_join(y = Dim1_DeltGenKat, na_matches = "never", by = "DeltGenKat_ID") %>%

  # Left join Dim1_DeltKat
  left_join(y = Dim1_DeltKat, na_matches = "never", join_by(
      "DeltKat_ID" >= "DeltKatMin_ID",
      "DeltKat_ID" <= "DeltKatMaks_ID")) %>%
  
  # Left join Dim1_Kalender med OrdreDato_DW
  left_join(
    y = Dim1_Kalender %>% rename_with(~ paste0("OrdreDato_DW_", .)),
    by = c("OrdreDato_DW" = "OrdreDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # Left join Dim1_Kalender med OrdreFoersteDato_DW
  mutate(OrdreFoersteDato_DW = as_date(OrdreFoersteDatoTid_DW)) %>%
  left_join(
    y = Dim1_Kalender %>% rename_with(~ paste0("OrdreFoersteDato_DW_", .)),
    by = c("OrdreFoersteDato_DW" = "OrdreFoersteDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # Left join Dim1_Kalender med BilletStartDato_DW
  left_join(
    y = Dim1_Kalender %>% rename_with(~ paste0("BilletStartDato_DW_", .)),
    by = c("BilletStartDato_DW" = "BilletStartDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # Left join Dim1_Kalender med BilletSlutDato_DW
  left_join(
    y = Dim1_Kalender %>% rename_with(~ paste0("BilletSlutDato_DW_", .)),
    by = c("BilletSlutDato_DW" = "BilletSlutDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # Left join Dim1_Kalender med EventAarFristDato_DW
  left_join(
    y = Dim1_Kalender %>% rename_with(~ paste0("EventAarFristDato_DW_", .)),
    by = c("EventAarFristDato_DW" = "EventAarFristDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # Left join Dim1_Kalender med EventAarAabningDato_DW
  left_join(
    y = Dim1_Kalender %>% rename_with(~ paste0("EventAarAabningDato_DW_", .)),
    by = c("EventAarAabningDato_DW" = "EventAarAabningDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # Left join Dim1_Kalender med EventAarRatingDato_RD
  left_join(
    y = Dim1_Kalender %>% rename_with(~ paste0("EventAarRatingDato_RD_", .)),
    by = c("EventAarRatingDato_RD" = "EventAarRatingDato_RD_Dato_DW"),
    na_matches = "never") %>%
  
  # Left join Dim1_Kalender med EventAarStartDato_DW
  mutate(EventAarStartDato_DW = as_date(EventAarStartDatoTid_DW)) %>%
  left_join(
    y = Dim1_Kalender %>% rename_with(~ paste0("EventAarStartDato_DW_", .)),
    by = c("EventAarStartDato_DW" = "EventAarStartDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # Left join Dim1_Kalender med EventAarSlutDato_DW
  group_by(EventAar_ID) %>%
  mutate(EventAarSlutDatoTid_DW = max(BilletSlutDatoTid_RD)) %>%
  ungroup() %>%
  mutate(EventAarSlutDato_DW = as_date(EventAarSlutDatoTid_DW)) %>%
  left_join(
    y = Dim1_Kalender %>% rename_with(~ paste0("EventAarSlutDato_DW_", .)),
    by = c("EventAarSlutDato_DW" = "EventAarSlutDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # Left join Dim1_Kalender med DeltFoedtDato_DW
  left_join(
    y = Dim1_Kalender %>% rename_with(~ paste0("DeltFoedtDato_DW_", .)),
    by = c("DeltFoedtDato_DW" = "DeltFoedtDato_DW_Dato_DW"),
    na_matches = "never")

# tbl1_Ordre_T: Logik på tværs af tabeller ----
tbl1_Ordre_T <- tbl1_Ordre_T %>%

  # OrdreStatusSimpelDeltKat_DW
  group_by(EventAar_ID, DeltID_RD) %>%
  mutate(OrdreStatusSimpelDeltKat_DW = ifelse(any(OrdreStatusSimpelKat_RD == "Tilmeldt"), "Tilmeldt", "<q>Totalafbud</q>")) %>%
  ungroup() %>%
  mutate(across("OrdreStatusSimpelDeltKat_DW", \(x) factor(x, levels = unique(x), ordered = T))) %>%

  # Billettype_DW
  mutate(Billettype_DW = ifelse(
    is.na(BilletDisciplin_RD) & is.na(BilletRaekke_RD) & is.na(BilletSpilFormat_RD),
    BilletKat_ID,
    paste(BilletDisciplin_RD, "-", BilletRaekke_RD, "-", BilletSpilFormat_RD))) %>%

  # BilletKat_DW
  mutate(BilletKat_DW = ifelse(
    is.na(BilletDisciplin_RD) | is.na(BilletRaekke_RD) | is.na(BilletSpilFormat_RD), as.character(BilletKat_RD),
    paste(BilletDisciplin_RD, BilletRaekke_RD))) %>%
  mutate(across(BilletKat_DW, \(x) factor(
    x, levels = unique(x[order(BilletDisciplin_RD, BilletRaekke_RD, BilletSpilFormat_RD)]), ordered = TRUE))) %>%
  
  # BilletPulje_DW
  add_count(EventAar_ID, OrdreStatusSimpelKat_RD, Billet_ID, name = "BilletPulje_DW") %>%
  mutate(BilletPulje_DW = as.integer(ceiling(BilletPulje_DW/BilletPuljeDelt_RD))) %>%
  
  # BilletPuljeStd_DW
  mutate(BilletPuljeStd_DW = as.integer(BilletPuljeDelt_RD*BilletPulje_DW)) %>%
  
  # BilletDelt_DW
  add_count(EventAar_ID, OrdreStatusSimpelKat_RD, Billet_ID, name = "BilletDelt_DW") %>%
  
  # BilletPuljeRest_DW
  mutate(BilletPuljeRest_DW = as.integer(BilletPuljeStd_DW-BilletDelt_DW)) %>%
  
  # EventAarSidst_DW
  mutate(EventAarSidst_DW = lead(EventAar_ID, order_by = EventAar_ID)) %>%
  mutate(across("EventAarSidst_DW", \(x) as.character(x))) %>%
  group_by(EventAar_ID) %>%
  mutate(EventAarSidst_DW = ifelse(EventAar_ID != EventAarSidst_DW, EventAarSidst_DW, NA)) %>%
  fill(EventAarSidst_DW, .direction = "up") %>%
  ungroup %>%
  
  # EventAarDato_DW
  group_by(EventAar_ID) %>%
  mutate(EventAarDato_DW = case_when(
    EventAarStartDato_DW == EventAarSlutDato_DW ~ EventAarStartDato_DW_DMAA_DW,
    TRUE ~ paste(EventAarStartDato_DW_DMAA_DW, "til", EventAarSlutDato_DW_DMAA_DW))) %>%
  ungroup() %>%

  # KlubUnik_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW, Klub_ID) %>%
  mutate(KlubUnik_DW = ifelse(row_number() == 1 & !grepl("Ingen klub|Udlandet", Klub_ID), 1, 0)) %>%
  ungroup() %>%
  
  # DeltYngstAeldst_DW
  group_by(EventAar_ID, OrdreStatusSimpelKat_RD, BilletKat_ID) %>%
  mutate(DeltYngstAeldst_DW = case_when(
    DeltFoedtDato_DW == max(DeltFoedtDato_DW, na.rm = T) ~ "Yngst",
    DeltFoedtDato_DW == min(DeltFoedtDato_DW, na.rm = T) ~ "Ældst",
    TRUE ~ NA_character_)) %>%
  ungroup() %>%
  mutate(across("DeltYngstAeldst_DW", \(x) as.character(x))) %>%

  # DeltAntalBillet_DW
  add_count(Billet_ID, OrdreStatusSimpelKat_RD, BilletKat_ID, name = "DeltAntalBillet_DW") %>%
  group_by(Billet_ID, BilletKat_ID, OrdreStatusSimpelKat_RD) %>%
  mutate(DeltAntalBillet_DW = DeltAntalBillet_DW-sum(is.na(DeltID_RD))) %>%
  ungroup() %>%
  mutate(DeltAntalBillet_DW = case_when(
    !is.na(DeltID_RD) ~ DeltAntalBillet_DW)) %>%
  group_by(Billet_ID) %>%
  fill(DeltAntalBillet_DW, .direction = "updown") %>%
  ungroup() %>%
  mutate(across("DeltAntalBillet_DW", \(x) as.integer(x))) %>%
  
  # DeltPraemiePct_DW
  group_by(EventAar_ID) %>%
  mutate(DeltPraemiePct_DW = DeltPraemie_RD/sum(DeltPraemie_RD, na.rm = T)) %>%
  ungroup() %>%
  mutate(across("DeltPraemiePct_DW", \(x) as.numeric(x))) %>%
  
  # DeltPraemieAkt_DW
  group_by(EventAar_ID) %>%
  mutate(DeltPraemieAkt_DW =  case_when(
    is.na(DeltPraemie_RD) ~ NA,
    EventAarPraemieSpons_RD == 1 ~ DeltPraemiePct_DW*sum(DeltPraemie_RD, na.rm = T),
    EventAarPraemieSpons_RD == 0 ~ DeltPraemiePct_DW*BilletPrisArr_RD*DeltAntalBillet_DW)) %>%
  ungroup() %>%
  
  # DeltPraemiePot_DW
  group_by(EventAar_ID) %>%
  mutate(DeltPraemiePot_DW =  case_when(
    is.na(DeltPraemie_RD) ~ NA,
    EventAarPraemieSpons_RD == 1 ~ DeltPraemiePct_DW*sum(DeltPraemie_RD, na.rm = T),
    EventAarPraemieSpons_RD == 0 ~ DeltPraemiePct_DW*BilletPrisArr_RD*BilletAntalMaks_RD)) %>%
  ungroup() %>%
  
  # DeltNavnKlub_DW
  mutate(DeltNavnKlub_DW = case_when(
    grepl("Aflyst", OrdreStatusSimpelKat_RD) ~ BilletBeskr_RD,
    is.na(DeltID_RD) ~ NA_character_,
    grepl("Ingen klub|Udlandet", Klub_ID) ~ paste0(DeltNavn_RD),
    TRUE ~ paste0(DeltNavn_RD, ", <i>", Klub_ID, "</i>"))) %>%
  
  # DeltNavnBilletKat_DW
  group_by(EventAar_ID, OrdreStatusSimpelKat_RD, DeltID_RD) %>%
  arrange(BilletKat_ID, desc(OrdreFoersteDatoTid_DW)) %>%
  mutate(DeltNavnBilletKat_DW = case_when(
    grepl("Aflyst", OrdreStatusSimpelKat_RD) ~ BilletBeskr_RD,
    is.na(DeltID_RD) ~ NA_character_,
    grepl("Ingen klub|Udlandet", Klub_ID) ~ paste0(DeltNavn_RD, " (", DeltAlder_DW, " år) ", str_c(
      BilletKatIkon_RD, collapse = "<wbr>")),
    TRUE ~ paste0(DeltNavnKlub_DW, " (", DeltAlder_DW, " år) ", str_c(
      BilletKatIkon_RD, collapse = "<wbr>")))) %>%
  ungroup() %>%
  
  # DeltBilletSalgNr_DW
  add_count(EventAar_ID, OrdreStatusSimpelKat_RD, DeltID_RD, name = "DeltBilletSalgNr_DW") %>%
  
  # DeltBilletSalg_DW
  mutate(DeltBilletSalg_DW = paste(DeltBilletSalgNr_DW, "stk. billetsalg")) %>%
  mutate(across(DeltBilletSalg_DW, \(x) factor(
    x, levels = unique(x[order(DeltBilletSalgNr_DW)]), ordered = TRUE))) %>%
  
  # DeltGen_DW
  mutate(DeltGen_DW = paste0(DeltGenNr_DW, ". gang")) %>%
  mutate(across(DeltGen_DW, \(x) factor(
    x, levels = unique(x[order(DeltGenNr_DW)]), ordered = TRUE))) %>%
  
  # DeltSnakeSeedNr_DW
  group_by(EventAar_ID, OrdreStatusSimpelKat_RD, Billet_ID) %>%
  arrange(DeltRang1_RD, desc(DeltRating2_RD), DeltRang3_RD, DeltID_RD) %>%
  mutate(DeltSnakeSeedNr_DW = row_number()) %>%
  ungroup() %>%
  
  # DeltSnakeSeedLagNr_DW
  group_by(EventAar_ID, OrdreStatusSimpelKat_RD, Billet_ID) %>%
  mutate(DeltSnakeSeedLagNr_DW = rep(1:unique(BilletPulje_DW),each = unique(BilletPulje_DW))[seq_len(n())]) %>%
  ungroup() %>%
  mutate(across("DeltSnakeSeedLagNr_DW", \(x) as.integer(x))) %>%
  
  # DeltSnakePuljeNr_DW
  group_by(EventAar_ID, OrdreStatusSimpelKat_RD, Billet_ID, DeltSnakeSeedLagNr_DW) %>%
  mutate(DeltSnakePuljeNr_DW = case_when(
    DeltSnakeSeedLagNr_DW %% 2 == 1 ~ row_number(),
    DeltSnakeSeedLagNr_DW %% 1 == 0 ~ rev(row_number()))) %>%
  mutate(DeltSnakePuljeNr_DW = ifelse(
    DeltSnakeSeedLagNr_DW == unique(BilletPuljeDelt_RD),
    DeltSnakePuljeNr_DW + BilletPuljeRest_DW, DeltSnakePuljeNr_DW)) %>%
  ungroup() %>%
  mutate(across("DeltSnakePuljeNr_DW", \(x) as.integer(x))) %>%

  # DeltUnik_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW, DeltID_RD) %>%
  arrange(OrdreStatusSimpelKat_RD, desc(DeltGen_DW)) %>%
  mutate(DeltUnik_DW = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup() %>%
  mutate(across("DeltUnik_DW", \(x) as.integer(x))) %>%

  # DeltKlubUnik_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW, DeltID_RD) %>%
  arrange(OrdreStatusSimpelKat_RD, KlubKat_DW) %>%
  mutate(DeltKlubUnik_DW = ifelse(grepl("Klub", KlubKat_DW), row_number() == 1, 0)) %>%
  mutate(across("DeltKlubUnik_DW", \(x) as.integer(x))) %>%

  # DeltPingPongUnik_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW, DeltID_RD) %>%
  arrange(OrdreStatusSimpelKat_RD, BilletKat_ID) %>%
  mutate(DeltPingPongUnik_DW = ifelse(grepl("Ping Pong", BilletKat_ID), row_number() == 1, 0)) %>%
  ungroup() %>%
  mutate(across("DeltPingPongUnik_DW", \(x) as.integer(x))) %>%

  # DeltForskudt_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW, DeltUnik_DW, OrdreDatoTid_RD) %>%
  mutate(DeltForskudt_DW = ifelse(OrdreDatoTid_RD != OrdreFoersteDatoTid_DW, 1, 0)) %>%
  ungroup() %>%
  mutate(across("DeltForskudt_DW", \(x) as.integer(x)))

# tbl1_Ordre_T: Stat ----
tbl1_Ordre_T <- tbl1_Ordre_T %>%

  # StatAlderForskelAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW, Billet_ID) %>%
  mutate(StatAlderForskelAntal_DW = paste0(
    "<kbd>", "Aldersforskel ", "<b>", max(DeltAlder_DW)-min(DeltAlder_DW), "</b>", " år", "</kbd>")) %>%
  ungroup() %>%
  
  # StatOrdreAntal_DW
  add_count(EventAar_ID, OrdreStatusSimpelKat_RD, Billet_ID, name = "StatOrdreAntal_DW") %>%
  group_by(EventAar_ID, OrdreStatusSimpelKat_RD) %>%
  mutate(StatOrdreAntal_DW = paste0(
    "<kbd>", "<b>", StatOrdreAntal_DW, "</b>", " ", OrdreStatusSimpelKat_RD,
    " til ", BilletKat_DW, " ", BilletKatIkon_RD, "</kbd>")) %>%
  arrange(OrdreStatusSimpelKat_RD, BilletDisciplin_ID, BilletRaekke_ID, BilletSpilFormat_ID) %>%
  mutate(StatOrdreAntal_DW = str_c(unique(StatOrdreAntal_DW), collapse = "&#8203;")) %>%
  ungroup() %>%
  
  # StatDeltAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW, DeltUnik_DW) %>%
  mutate(StatDeltAntal_DW = sum(DeltUnik_DW)) %>%
  group_by(EventAar_ID) %>%
  mutate(StatDeltAntal_DW = ifelse(StatDeltAntal_DW == 0, NA, paste0(
    "<kbd>", "<b>", StatDeltAntal_DW, "</b>", " ", OrdreStatusSimpelDeltKat_DW , " (",
    percent(StatDeltAntal_DW/sum(ifelse(StatDeltAntal_DW == 0, 0, 1)), digits = 0), ") ",
    OrdreStatusSimpelKatIkon_RD, "</kbd>"))) %>%
  arrange(OrdreStatusSimpelDeltKat_DW) %>%
  mutate(StatDeltAntal_DW = str_c(unique(na.omit(StatDeltAntal_DW)), collapse = "&#8203;")) %>%
  ungroup() %>%
  
  # StatDeltKoenAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW, DeltUnik_DW, DeltKoen_ID) %>%
  mutate(StatDeltKoenAntal_DW = sum(DeltUnik_DW)) %>%
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW) %>%
  mutate(StatDeltKoenAntal_DW = ifelse(StatDeltKoenAntal_DW == 0, NA, paste0(
    "<kbd>", "<b>", StatDeltKoenAntal_DW, "</b>", " ", DeltKoen_ID , " (",
    percent(StatDeltKoenAntal_DW/sum(ifelse(StatDeltKoenAntal_DW == 0, 0, 1)), digits = 0), ") ",
    DeltKoenIkon_RD, "</kbd>"))) %>%
  arrange(DeltKoen_ID) %>%
  mutate(StatDeltKoenAntal_DW = str_c(unique(na.omit(StatDeltKoenAntal_DW)), collapse = "&#8203;")) %>%
  ungroup() %>%
  
  # StatDeltGenKatAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW, DeltUnik_DW, DeltGenKat_ID) %>%
  mutate(StatDeltGenKatAntal_DW = sum(DeltUnik_DW)) %>%
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW) %>%
  mutate(StatDeltGenKatAntal_DW = ifelse(StatDeltGenKatAntal_DW == 0, NA, paste0(
    "<kbd>", "<b>", StatDeltGenKatAntal_DW, "</b>", " ", DeltGenKat_ID, " (",
    percent(StatDeltGenKatAntal_DW/sum(ifelse(StatDeltGenKatAntal_DW == 0, 0, 1)), digits = 0), ") ",
    DeltGenKatIkon_DW, "</kbd>"))) %>%
  arrange(DeltGenKat_ID) %>%
  mutate(StatDeltGenKatAntal_DW = str_c(unique(na.omit(StatDeltGenKatAntal_DW)), collapse = "&#8203;")) %>%
  ungroup() %>%
  
  # StatDeltAlderKatAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW, DeltUnik_DW, DeltAlderKat_RD) %>%
  mutate(StatDeltAlderKatAntal_DW = sum(DeltUnik_DW)) %>%
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW) %>%
  mutate(StatDeltAlderKatAntal_DW = ifelse(StatDeltAlderKatAntal_DW == 0, NA, paste0(
    "<kbd>", "<b>", StatDeltAlderKatAntal_DW, "</b>", " ", DeltAlderKat_RD , " (",
    percent(StatDeltAlderKatAntal_DW/sum(ifelse(StatDeltAlderKatAntal_DW == 0, 0, 1)), digits = 0), ") ",
    IkonFødt_V, "</kbd>"))) %>%
  arrange(DeltAlderKat_RD) %>%
  mutate(StatDeltAlderKatAntal_DW = str_c(unique(na.omit(StatDeltAlderKatAntal_DW)), collapse = "&#8203;")) %>%
  ungroup() %>%
  
  # StatDeltAlderAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW) %>%
  mutate(StatDeltAlderAntal_DW = case_when(
    all(is.na(DeltAlder_DW)) ~ 0,
    DeltUnik_DW == 0 ~ NA,
    TRUE ~ DeltAlder_DW)) %>%
  mutate(StatDeltAlderAntal_DW = paste0(
    "<kbd>", "Yngst " , "<b>", min(StatDeltAlderAntal_DW, na.rm = TRUE), "</b>", " år ",
    IkonFødt_V, "</kbd>", "&#8203;",
    "<kbd>", "Gns. ", "<b>", round(mean(StatDeltAlderAntal_DW, na.rm = TRUE), 0), "</b>", " år ",
    IkonFødt_V, "</kbd>", "&#8203;",
    "<kbd>", "Ældst ", "<b>", max(StatDeltAlderAntal_DW, na.rm = TRUE), "</b>", " år ",
    IkonFødt_V, "</kbd>")) %>%
  ungroup() %>%
  
  # StatDeltLandsdelAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW, DeltKlubUnik_DW, KlubLandsdel_RD) %>%
  mutate(StatDeltLandsdelAntal_DW = sum(DeltKlubUnik_DW)) %>%
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW) %>%
  mutate(StatDeltLandsdelAntal_DW = ifelse(StatDeltLandsdelAntal_DW == 0, NA, paste0(
    "<kbd>", "<b>", StatDeltLandsdelAntal_DW, "</b>", " ", KlubLandsdel_RD , " (",
    percent(StatDeltLandsdelAntal_DW/sum(ifelse(StatDeltLandsdelAntal_DW == 0, 0, 1)), digits = 0), ") ",
    KlubKatIkon_RD, "</kbd>"))) %>%
  arrange(KlubLandsdel_RD) %>%
  mutate(StatDeltLandsdelAntal_DW = str_c(unique(na.omit(StatDeltLandsdelAntal_DW)), collapse = "&#8203;")) %>%
  ungroup() %>%
  
  # StatDeltRegionAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW, DeltKlubUnik_DW, KlubRegion_ID) %>%
  mutate(StatDeltRegionAntal_DW = sum(DeltKlubUnik_DW)) %>%
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW) %>%
  mutate(StatDeltRegionAntal_DW = ifelse(StatDeltRegionAntal_DW == 0, NA, paste0(
    "<kbd>", "<b>", StatDeltRegionAntal_DW, "</b>", " ", KlubRegion_ID , " (",
    percent(StatDeltRegionAntal_DW/sum(ifelse(StatDeltRegionAntal_DW == 0, 0, 1)), digits = 0), ") ",
    KlubKatIkon_RD, "</kbd>"))) %>%
  arrange(KlubRegion_ID) %>%
  mutate(StatDeltRegionAntal_DW = str_c(unique(na.omit(StatDeltRegionAntal_DW)), collapse = "&#8203;")) %>%
  ungroup() %>%
  
  # StatDeltKlubKatAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW, DeltUnik_DW, KlubKat_DW) %>%
  mutate(StatDeltKlubKatAntal_DW = sum(DeltUnik_DW)) %>%
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW) %>%
  mutate(StatDeltKlubKatAntal_DW = ifelse(StatDeltKlubKatAntal_DW == 0, NA, paste0(
    "<kbd>", "<b>", StatDeltKlubKatAntal_DW, "</b>", " ", KlubKat_DW , " (",
    percent(StatDeltKlubKatAntal_DW/sum(ifelse(StatDeltKlubKatAntal_DW == 0, 0, 1)), digits = 0), ") ",
    KlubKatIkon_RD, "</kbd>"))) %>%
  arrange(KlubKat_DW) %>%
  mutate(StatDeltKlubKatAntal_DW = str_c(unique(na.omit(StatDeltKlubKatAntal_DW)), collapse = "&#8203;")) %>%
  ungroup() %>%
  
  # StatKlubAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW, KlubUnik_DW) %>%
  mutate(StatKlubAntal_DW = sum(KlubUnik_DW)) %>%
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW) %>%
  mutate(StatKlubAntal_DW = ifelse(StatKlubAntal_DW == 0, NA, paste0(
    "<kbd>", "<b>", StatKlubAntal_DW, "</b>",
    " ", ifelse(StatKlubAntal_DW == 1, "klub", "forskellige klubber"), " ", KlubKatIkon_RD, "</kbd>"))) %>%
  mutate(StatKlubAntal_DW = ifelse(all(is.na(StatKlubAntal_DW)), NA, unique(na.omit(StatKlubAntal_DW)))) %>%
  ungroup() %>%

  # StatBilletGnsAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelKat_RD) %>%
  mutate(StatBilletGnsAntal_DW = paste0(
    "<kbd>", "Gns. ", "<b>", round(sum(OrdreUnik_DW)/sum(DeltUnik_DW), 1), "</b>",
    " ", ifelse(round(sum(OrdreUnik_DW)/sum(DeltUnik_DW), 1) == 1,
    "billet pr. deltager", "billetter pr. deltager"), " ", IkonBillet_V, "</kbd>")) %>%
  ungroup() %>%

  # StatOrdreKatAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelKat_RD, OrdreUnik_DW, OrdreKat_ID) %>%
  mutate(StatOrdreKatAntal_DW = sum(OrdreUnik_DW)) %>%
  group_by(EventAar_ID) %>%
  mutate(StatOrdreKatAntal_DW = ifelse(StatOrdreKatAntal_DW == 0, NA, paste0(
    "<kbd>", "<b>", StatOrdreKatAntal_DW, "</b>", " ", OrdreKat_ID , " (", 
    percent(StatOrdreKatAntal_DW/sum(ifelse(StatOrdreKatAntal_DW == 0, 0, 1)), digits = 0), ") ",
    OrdreKatIkon_DW, "</kbd>"))) %>%
  arrange(OrdreKat_ID) %>%
  mutate(StatOrdreKatAntal_DW = str_c(unique(na.omit(StatOrdreKatAntal_DW)), collapse = "&#8203;")) %>%
  ungroup() %>%
  
  # StatDeltRatingKatAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW, DeltPingPongUnik_DW, DeltRatingKat_RD) %>%
  mutate(StatDeltRatingKatAntal_DW = ifelse(is.na(DeltRatingKat_RD), 0, sum(DeltPingPongUnik_DW))) %>%
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW) %>%
  mutate(StatDeltRatingKatAntal_DW = ifelse(StatDeltRatingKatAntal_DW == 0, NA, paste0(
    "<kbd>", "<b>", StatDeltRatingKatAntal_DW, "</b>", " ", DeltRatingKat_RD , " (",
    percent(StatDeltRatingKatAntal_DW/sum(ifelse(StatDeltRatingKatAntal_DW == 0, 0, 1)), digits = 0), ") ",
    IkonPingPong_V, "</kbd>"))) %>%
  arrange(DeltRatingKat_RD) %>%
  mutate(StatDeltRatingKatAntal_DW = str_c(unique(na.omit(StatDeltRatingKatAntal_DW)), collapse = "&#8203;")) %>%
  ungroup() %>%
  
  # StatDeltRatingAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW) %>%
  mutate(StatDeltRatingAntal_DW = case_when(
    all(is.na(DeltRating2_RD)) ~ 0,
    DeltPingPongUnik_DW == 0 ~ NA,
    TRUE ~ DeltRating2_RD)) %>%
  mutate(StatDeltRatingAntal_DW = paste0(
    "<kbd>", "Min. ", "<b>", min(StatDeltRatingAntal_DW, na.rm = TRUE), "</b>", " rating ",
    IkonPingPong_V, "</kbd>", "&#8203;",
    "<kbd>", "Gns. ", "<b>", round(mean(StatDeltRatingAntal_DW, na.rm = TRUE), 0), "</b>", " rating ",
    IkonPingPong_V, "</kbd>", "&#8203;",
    "<kbd>", "Maks ", "<b>", max(StatDeltRatingAntal_DW, na.rm = TRUE), "</b>", " rating ",
    IkonPingPong_V, "</kbd>")) %>%
  ungroup() %>%

  # StatOekonomiAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelKat_RD) %>%
  mutate(StatOekonomiAntal_DW = paste0(
    "<kbd>", "Omsætning kr. ", "<b>", format(round(sum(BilletPris_RD), 0), big.mark = "."), "</b>", " ",
    IkonPenge_V, "</kbd>", "&#8203;",
    "<kbd>", "Arrangørpris kr. ", "<b>", format(round(sum(BilletPrisArr_RD), 0), big.mark = "."), "</b>", " ",
    IkonPenge_V, "</kbd>", "&#8203;",
    "<kbd>", "Over-/underskud arrangør kr. ", "<b>", format(round(sum(BilletPrisRes_DW), 0), big.mark = "."), "</b>", " ",
    IkonPenge_V, "</kbd>")) %>%
  
  # StatForskudtTilAntal_DW
  group_by(EventAar_ID, OrdreStatusSimpelDeltKat_DW) %>%
  mutate(StatForskudtTilAntal_DW = sum(DeltForskudt_DW)) %>%
  mutate(StatForskudtTilAntal_DW = paste0(
    "<kbd>", "<b>", StatForskudtTilAntal_DW, "</b>", " forskudt ", OrdreStatusSimpelKat_RD, " ", IkonBillet_V, "</kbd>")) %>%
  ungroup()

# tbl1_Ordre_T: KPI ----
tbl1_Ordre_T <- tbl1_Ordre_T %>%
  
  group_by(EventAar_ID) %>%
  mutate(KPIDeltAntal_DW = sum(DeltUnik_DW)) %>%
  ungroup()

# tbl1_Ordre_T: Info ----
tbl1_Ordre_T <- tbl1_Ordre_T %>%
  
  # InfoNedtælling_DW
  group_by(EventAar_ID) %>%
  mutate(InfoNedtælling_DW = case_when(
    InputInfo1234_V %in% c(1, 2) ~ "",
    InputInfo1234_V %in% c(3, 4) ~ paste0(
     '<p style="text-align:center;width:50em;max-width:100%">
      <b style=font-size:120%;text-transform:uppercase>Nedtælling</b>
    	<br>
    	<b style=font-size:80%;text-transform:uppercase>', EventAar_ID, '</b>
      <br>
      <span id="nedtællingsur"></span>
      <br>
      <b style=font-size:80%>Afholdes ', EventAarDato_DW, '</b>
	    
    	<script>
    	// Opdater nedtællingsur hvert sekund
    	var x = setInterval(function() {
    	  	  
    		// Differencen mellem eventdatoen og dags dato
    		var nedtællingsur_dif = new Date("', EventAarStartDatoTid_DW, '").getTime() - new Date().getTime();
    		  
  	    // Vis resultatet i elementet med id="nedtællingsur"
  	    document.getElementById("nedtællingsur").innerHTML =
		      
    		"<b style=display:inline-block;border-style:solid;padding:5px;width:60px;text-align:center>" +
  	    Math.floor(nedtællingsur_dif / (1000 * 60 * 60 * 24)) +
  	    "<br><span style=font-size:80%>dage</span></b>&ensp;" +
  	      
    		"<b style=display:inline-block;border-style:solid;padding:5px;width:60px;text-align:center>" +
  	    Math.floor((nedtællingsur_dif % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60)) +
  	    "<br><span style=font-size:80%>timer</span></b>&ensp;" +
  	      
    		"<b style=display:inline-block;border-style:solid;padding:5px;width:60px;text-align:center>" +
  	    Math.floor((nedtællingsur_dif % (1000 * 60 * 60)) / (1000 * 60)) +
  	    "<br><span style=font-size:80%>min.</span></b>&ensp;" +
  	      
    		"<b style=display:inline-block;border-style:solid;padding:5px;width:60px;text-align:center>" +
  	    Math.floor((nedtællingsur_dif % (1000 * 60)) / 1000) +
  	    "<br><span style=font-size:80%>sek.</span></b>";
		      
    		// Hvis nedtællingsur er udløbet
  	    if (nedtællingsur_dif < 0) {
    	    clearInterval(x);
    	    document.getElementById("nedtællingsur").innerHTML = "<i>Eventet er udløbet</i>";
  	    }
	    }, 1000);
	    </script>'))) %>%
  ungroup() %>%
  
  # InfoPlakatCTA_DW
  group_by(EventAar_ID) %>%
  mutate(InfoPlakatCTA_DW = case_when(
    InputInfo1234_V %in% c(1) ~ paste0(
      "<img src=filer/medie/billede/forside.png style=width:30em;max-width:100%;border-radius:20px>"),
    InputInfo1234_V %in% c(2) ~ paste0(
      "![](filer/event/", fun_egen_sti(EventAar_ID), "/", fun_egen_sti(EventAar_ID), "-teaserplakat", ".png){width=30em}",
      "<br>",
      "<figcaption>",
      "[<i style=font-size:80%>[Klik her for teaserplakat som PDF til udskrift]</i>]",
      "(filer/event/", fun_egen_sti(EventAar_ID), "/", fun_egen_sti(EventAar_ID), "-teaserplakat", ".pdf){target=_blank}",
      "</figcaption><p><p>"),
    InputInfo1234_V %in% c(3, 4) ~ paste0(
      "<br><br>",
      "<a style=display:inline-block;background:#FF4A6E;color:#FFFFFF;",
      "border-radius:40px;padding-left:50px;padding-right:50px;padding-top:5px;padding-bottom:5px;",
      "text-decoration:none href=indbydelse-tilmelding-", EventAarStartDato_DW_Aar_DW, ".qmd#tilmelding>",
      "<b style=font-size:150%;text-transform:uppercase>", IkonBillet_V, " Tilmeld</b>",
      "<br>",
      "<i style=font-size:90%;text-transform:uppercase>", EventAar_ID, "</i></a>",
      "<br><br>",
      "<i style=font-size:80%>",
      "Hurtigt overblik over eventet ses i indbydelsesplakaten ", IkonHåndNed_V, "</i>",
      "<br>",
      "![](filer/event/", fun_egen_sti(EventAar_ID), "/", fun_egen_sti(EventAar_ID), "-indbydelsesplakat", ".png){width=50em}",
      "<br>",
      "<span>",
      "[<i style=font-size:80%>",
      "[Klik her for indbydelesplakat som PDF til udskrift]</i>]",
      "(filer/event/", fun_egen_sti(EventAar_ID), "/", fun_egen_sti(EventAar_ID), "-indbydelsesplakat", ".pdf){target=_blank}",
      "</span>",
      "</p>"))) %>%
  ungroup() %>%
  
  # InfoForside_DW
  group_by(EventAar_ID) %>%
  mutate(InfoForside_DW = case_when(
    InputInfo1234_V %in% c(1) ~ paste(
      "<i>Nærmere information om Ping Pong DM", EventAarStartDato_DW_Aar_DW+1, "følger.</i>"),
    InputInfo1234_V %in% c(2) ~ paste0(
      "<i>", EventAar_ID, " afholdes ", EventAarDato_DW, " i ",
      EventAarStedURL_DW, ". Der åbnes for tilmelding", EventAarAabningDato_DW_DMAA_DW, 
      "hvor der vil komme en fane med hhv. <q>Indbydelse & tilmelding</q> samt ",
      "<q>Præmier & deltagere</q>, som vil blive opdateret løbende.</i>"),
    InputInfo1234_V %in% c(3, 4) ~ ifelse(!grepl("Tilmeldt", OrdreStatusSimpelKat_RD), NA, paste0(
      "<i style=font-size:100%>",
      "<b>Afholdes ", EventAarDato_DW, " i ", EventAarStedURL_DW, "</b></i>",
      "<br>",
      "<i style=font-size:80%>Først til mølle-princip ∙ Tilmeldingsfrist ",
      EventAarFristDato_DW_DMAA_DW, "</i>",
      "<br><br>",
      "<ul>",
      "<li><p>", IkonBillet_V, " [<b>Indbydelse & tilmelding</b>](indbydelse-tilmelding-", EventAarStartDato_DW_Aar_DW, ".qmd)",
      "<br>",
      "<i>Indbydelse, tidsplan, praktisk info samt tilmelding/betaling til ", EventAar_ID, ".</i></p></li>",
      "<li><p>", IkonGentagelse_V, " ", "[<b>Præmier & deltagere</b>](raekke-sandpapir-aaben-single.qmd)",
      "<br>",
      "<i>Præmier og deltagere opdateres løbende til ", EventAar_ID, ".</i></p>",
      StatOrdreAntal_DW, "</li>",
      "</ul>")))) %>%
  mutate(InfoForside_DW = ifelse(all(is.na(InfoForside_DW)), NA, unique(na.omit(InfoForside_DW)))) %>%
  ungroup() %>%
  
  # InfoFacebook_DW
  group_by(EventAar_ID) %>%
  mutate(InfoFacebook_DW = case_when(
    InputInfo1234_V %in% c(1) ~ paste0(
      "<i>Like og følg den officielle ",
      "[<b>Facebook-side <q>Ping Pong DK</q></b>]",
      "(https://www.facebook.com/{{< var var.facebook_side_id >}}){target=_blank} ",
      "for at holde dig opdateret.</i>"),
    InputInfo1234_V %in% c(2, 3, 4) ~ paste0(
      "<i><p>Del gerne budskabet via ",
      "[<b>Facebook-begivenheden <q>", toupper(EventAar_ID), "</q></b>]",
      "({{< var var.facebook_event_url >}}){target=_blank} ",
      "ved at trykke interesseret/deltager og inviter folk.</p>",
      "Like og følg ",
      "[Facebook-siden <q>Ping Pong DK</q>]",
      "(https://www.facebook.com/{{< var var.facebook_side_id >}}){target=_blank} ",
      "for at holde dig opdateret.</i>"))) %>%
  ungroup() %>%
  
  # InfoTipIndbydelse_DW
  group_by(EventAar_ID) %>%
  mutate(InfoTipIndbydelse_DW = paste0(
    IkonBillet_V, "Indbydelse, tidsplan og praktisk info til ", EventAar_ID,
    " ses [<b>HER</b>](indbydelse-tilmelding-", EventAarStartDato_DW_Aar_DW, ".qmd).</i>")) %>%
  ungroup() %>%
  
  # InfoTipPraemierDeltagere_DW
  group_by(EventAar_ID) %>%
  mutate(InfoTipPraemierDeltagere_DW = ifelse(!grepl("Tilmeldt", OrdreStatusSimpelKat_RD), NA, paste(
    "<p>",
    IkonGentagelse_V, " Præmier og deltagere opdateres løbende til ",
    EventAar_ID, " [<b>HER</b>](raekke-sandpapir-aaben-single.qmd).",
    "</p>",
    StatOrdreAntal_DW))) %>%
  mutate(InfoTipPraemierDeltagere_DW = ifelse(
    all(is.na(InfoTipPraemierDeltagere_DW)), NA, unique(na.omit(InfoTipPraemierDeltagere_DW)))) %>%
  ungroup() %>%
  
  # InfoTipRegler_DW
  group_by(EventAar_ID) %>%
  mutate(InfoTipRegler_DW = paste(
    IkonRegler_V, "I Ping Pong tages det bedste fra fortidens- og nutidens bordtennis og kan", 
    "sammenlignes med ordsproget <q>gammel vin på nye flasker</q>. Der er nogle få regler, der",
    "adskiller Ping Pong fra bordtennis, bl.a. spilles der til 15 point (14-14 er afgørende bold),",
    "alle spiller på lige vilkår med sandpapirsbat, hvor der byttes bat mellem hvert sæt, og der kan",
    "tages <q>dobbeltpoint</q>. Se mere [<b>HER</b>](regler.qmd).")) %>%
  ungroup() %>%
  
  # InfoTipVM_DW
  group_by(EventAar_ID) %>%
  mutate(InfoTipVM_DW = paste(
    IkonGlobus_V, "World Championship of Ping Pong (WCPP) afholdes sædvanligvis",
    "i London med en præmiesum på $100.000 og eksponeres på bl.a. Viaplay Sport og Sky Sports.",
    "Se mere [<b>HER</b>](wcpp.qmd).")) %>%
  ungroup() %>%
  
  # Sorter efter (1) EventAar_ID, (2) OrdreFoersteDatoTid_DW, (3) BilletKat_ID
  arrange(desc(EventAar_ID), desc(OrdreFoersteDatoTid_DW), BilletKat_ID)

# tbl2_EventAar_T ----
tbl2_EventAar_T <- tbl1_Ordre_T %>%
  mutate(across(where(~ is.factor(.)), as.character)) %>%
  distinct(across(starts_with(c("EventAar", "Info", "KPI")))) %>%
  arrange(desc(EventAarNr_RD))

# tbl2_Billet_T ----
tbl2_Billet_T <- tbl1_Ordre_T %>% filter(grepl("Tilmeldt", OrdreStatusSimpelKat_RD)) %>%
  mutate(across(where(~ is.factor(.)), as.character)) %>%
  distinct(across(starts_with(c("EventAar_ID", "Billet", "Stat")))) %>%
  arrange(desc(BilletNr_RD))

# tbl2_EventAarAkt_T ----
tbl2_EventAarAkt_T <- tbl2_EventAar_T %>%
  filter(grepl(InputEventAarAkt_V, EventAar_ID))

# tbl1_Ordre_T: Oprydning ----
tbl1_Ordre_T <- tbl1_Ordre_T %>%
  select(
    starts_with("Ordre"),
    starts_with("Event"),
    starts_with("EventAar"),
    starts_with("Billet"),
    starts_with("Delt"),
    starts_with("Klub"),
    -starts_with(c("Stat", "Info", "KPI")),
    -ends_with("_ID"),
    everything()
  )

# Fjern Dim og Fact fra objekter ----
rm(list = ls(pattern = "^(Dim|Fact)"))