# Dim1_Kalender ----
Dim1_Kalender <- tibble(Dato_DW = seq(
  from = as_date("1900-01-01"),
  to   = as_date(ceiling_date(Sys.Date(), unit = "year")-1+years(10)),
  by   = "days")) %>%
  
  # År
  mutate(Aar_DW = as.integer(format(Dato_DW, "%Y"))) %>%
  mutate(AarDag_DW = as.integer(yday(Dato_DW))) %>%
  
  # Kvartal
  mutate(KvartalNr_DW = as.integer(quarter(Dato_DW))) %>%
  mutate(KvartalNavn_DW = paste0(KvartalNr_DW, ". kvartal")) %>%
  mutate(across(KvartalNavn_DW, \(x) factor(
    x, levels = unique(x[order(KvartalNr_DW)]), ordered = TRUE))) %>%
  
  # Måned
  mutate(MaanedNr_DW = as.integer(month(Dato_DW))) %>%
  mutate(MaanedNavnLang_DW = format(Dato_DW, "%B")) %>%
  mutate(MaanedNavnKort_DW = format(Dato_DW, "%b")) %>%
  mutate(MaanedDag_DW = as.integer(trimws(format(Dato_DW, "%e")))) %>%
  mutate(MaanedDagNavn_DW = paste0(MaanedDag_DW, ". ", MaanedNavnLang_DW)) %>%
  mutate(across(c(MaanedNavnLang_DW, MaanedNavnKort_DW), \(x) factor(
    x, levels = unique(x[order(MaanedNr_DW)]), ordered = T))) %>%
  
  # Uge
  mutate(Uge_DW = as.integer(format(Dato_DW, "%W"))) %>%
  
  # Dag
  mutate(UgeDag_DW = as.integer(format(Dato_DW, "%u"))) %>%
  mutate(UgeDagNavnLang_DW = format(Dato_DW, "%A")) %>%
  mutate(UgeDagNavnKort_DW = substr(format(Dato_DW, "%A"), 1, 3)) %>%
  mutate(across(c(UgeDagNavnLang_DW, UgeDagNavnKort_DW), \(x) factor(
    x, levels = unique(x[order(UgeDag_DW)]), ordered = T))) %>%
  
  # DMAA_DW
  mutate(DMAA_DW = format(Dato_DW, "%d.%m.%Y")) %>%
  
  arrange(Dato_DW)

# Fact_Ordre ----
Fact_Ordre <- read_excel(path = InputData_V, sheet = "✍️ Fact_Ordre", skip = 2) %>%
  mutate(across(DeltID_RD, \(x) as.character(x))) %>%
  mutate(across(DeltNavn_RD, \(x) as.character(x))) %>%
  mutate(across(Klub_ID, \(x) as.character(x))) %>%
  mutate(across(DeltKoen_ID, \(x) as.character(x))) %>%
  mutate(across(OrdreDatoTid_RD, \(x) as_datetime(x))) %>%
  mutate(across(Billet_ID, \(x) as.character(x))) %>%
  mutate(across(OrdreStatusKat_ID, \(x) as.character(x))) %>%
  mutate(across(DeltRang1_RD, \(x) as.integer(x))) %>%
  mutate(across(DeltRating2_RD, \(x) as.integer(x))) %>%
  mutate(across(DeltRang3_RD, \(x) as.integer(x))) %>%
  mutate(across(DeltSlutspil_ID, \(x) as.character(x))) %>%
  mutate(across(DeltPlac_ID, \(x) as.character(x))) %>%
  mutate(across(DeltPraemie_RD, \(x) as.numeric(x))) %>%
  mutate(OrdreDato_DW = as_date(OrdreDatoTid_RD)) %>%
  mutate(DeltKat_ID = str_sub(DeltID_RD, -4, -1)) %>%
  mutate(DeltRating_DW = case_when(
    !is.na(DeltRang1_RD)  ~ paste0("[", DeltRang1_RD, "] ", DeltRating2_RD),
    is.na(DeltRating2_RD) ~ "-",
    TRUE ~ as.character(DeltRating2_RD))) %>%
  mutate(OrdreUnik_DW = as.integer(1)) %>%
  arrange(desc(OrdreDatoTid_RD))

# Dim1_OrdreStatus ----
Dim1_OrdreStatus <- read_excel(path = InputData_V, sheet = "✍️ Dim1_OrdreStatus", skip = 2) %>%
  mutate(across(OrdreStatusKatNr_RD, \(x) as.integer(x))) %>%
  mutate(across(OrdreStatusKat_ID, \(x) as.character(x))) %>%
  mutate(across(c(
    OrdreStatusKat_RD, OrdreStatusKatEmoji_RD, OrdreStatusSimpelKat_RD, OrdreStatusSimpelKatIkon_RD
  ), \(x) factor(x, levels = unique(x[order(OrdreStatusKatNr_RD)]), ordered = TRUE))) %>%
  arrange(OrdreStatusKatNr_RD)

# Dim1_Billet ----
Dim1_Billet <- read_excel(path = InputData_V, sheet = "🎫 Dim1_Billet", skip = 2) %>%
  mutate(across(BilletNr_RD, \(x) as.integer(x))) %>%
  mutate(across(Billet_ID, \(x) as.character(x))) %>%
  mutate(across(EventAar_ID, \(x) as.character(x))) %>%
  mutate(across(BilletStartDatoTid_RD, \(x) as_datetime(x))) %>%
  mutate(across(BilletSlutDatoTid_RD, \(x) as_datetime(x))) %>%
  mutate(across(BilletKat_ID, \(x) as.character(x))) %>%
  mutate(across(BilletDisciplin_ID, \(x) as.character(x))) %>%
  mutate(across(BilletRaekke_ID, \(x) as.character(x))) %>%
  mutate(across(BilletSpilFormat_ID, \(x) as.character(x))) %>%
  mutate(across(BilletBeskr_RD, \(x) as.character(x))) %>%
  mutate(across(BilletTilvalg_RD, \(x) as.character(x))) %>%
  mutate(across(BilletPris_RD, \(x) as.numeric(x))) %>%
  mutate(across(BilletPrisArr_RD, \(x) as.numeric(x))) %>%
  mutate(across(BilletAntalMaks_RD, \(x) as.integer(x))) %>%
  mutate(across(BilletPuljeDelt_RD, \(x) as.integer(x))) %>%
  mutate(BilletStartDato_DW = as_date(BilletStartDatoTid_RD)) %>%
  mutate(BilletSlutDato_DW = as_date(BilletSlutDatoTid_RD)) %>%
  mutate(BilletPrisRes_DW = as.numeric(BilletPris_RD-BilletPrisArr_RD)) %>%
  arrange(desc(BilletNr_RD))

# Dim2_EventAar ----
Dim2_EventAar <- read_excel(path = InputData_V, sheet = "🎫 Dim2_EventAar", skip = 2) %>%
  mutate(across(EventAarNr_RD, \(x) as.integer(x))) %>%
  mutate(across(EventAar_ID, \(x) as.character(x))) %>%
  mutate(across(EventAar_RD, \(x) factor(
    x, levels = unique(x[order(EventAarNr_RD)]), ordered = TRUE))) %>%
  mutate(across(Event_ID, \(x) as.character(x))) %>%
  mutate(across(EventAarFristDatoTid_RD, \(x) as_datetime(x))) %>%
  mutate(across(EventAarAabningDatoTid_RD, \(x) as_datetime(x))) %>%
  mutate(across(EventAarRatingDato_RD, \(x) as_date(x))) %>%
  mutate(across(EventAarPraemieSpons_RD, \(x) as.integer(x))) %>%
  mutate(across(EventAarSted_RD, \(x) as.character(x))) %>%
  mutate(across(EventAarAdr_RD, \(x) as.character(x))) %>%
  mutate(across(EventAarPostnr_RD, \(x) as.integer(x))) %>%
  mutate(across(EventAarBy_RD, \(x) as.character(x))) %>%
  mutate(across(EventAarFarve1_RD, \(x) as.character(x))) %>%
  mutate(across(EventAarFarve2_RD, \(x) as.character(x))) %>%
  mutate(across(EventAarStedURL_RD, \(x) as.character(x))) %>%
  mutate(across(EventAarUUID_RD, \(x) as.character(x))) %>%
  mutate(across(EventAarToken_RD, \(x) as.character(x))) %>%
  mutate(EventAarFristDato_DW = as_date(EventAarFristDatoTid_RD)) %>%
  mutate(EventAarAabningDato_DW = as_date(EventAarAabningDatoTid_RD)) %>%
  mutate(EventAarPostnrBy_DW = paste(EventAarPostnr_RD, EventAarBy_RD)) %>%
  mutate(EventAarStedURL_DW = paste0(
    "<a href=", EventAarStedURL_RD, " target=_blank>", EventAarSted_RD, "</a>")) %>%
  mutate(EventAarStedAdrPostnrURL_DW = paste0(
    "<a href=",EventAarStedURL_RD, " target=_blank>",
    EventAarSted_RD,
    "<br>",
    EventAarAdr_RD,
    "<br>",
    EventAarPostnrBy_DW,
    "</a>")) %>%
  arrange(desc(EventAarNr_RD))

# Dim3_Event ----
Dim3_Event <- read_excel(path = InputData_V, sheet = "🎫 Dim3_Event", skip = 2) %>%
  mutate(across(EventNr_RD, \(x) as.integer(x))) %>%
  mutate(across(Event_ID, \(x) as.character(x))) %>%
  mutate(across(Event_RD, \(x) factor(
    x, levels = unique(x[order(EventNr_RD)]), ordered = TRUE))) %>%
  mutate(across(EventSportsgren_RD, \(x) as.character(x))) %>%
  mutate(across(EventTurnering_RD, \(x) as.character(x))) %>%
  arrange(EventNr_RD)

# Dim2_BilletKat ----
Dim2_BilletKat <- read_excel(path = InputData_V, sheet = "🎫 Dim2_BilletKat", skip = 2) %>%
  mutate(across(BilletKatNr_RD, \(x) as.integer(x))) %>%
  mutate(across(BilletKat_ID, \(x) as.character(x))) %>%
  mutate(BilletKatFarve_DW = sub(".*(#.{6})>.*", "\\1", BilletKatIkon_RD)) %>%
  mutate(across(c(BilletKat_RD, BilletKatEmoji_RD, BilletKatIkon_RD, BilletKatFarve_DW), \(x) factor(
    x, levels = unique(x[order(BilletKatNr_RD)]), ordered = TRUE))) %>%
  arrange(BilletKatNr_RD)

# Dim2_BilletDisciplin ----
Dim2_BilletDisciplin <- read_excel(path = InputData_V, sheet = "🎫 Dim2_BilletDisciplin", skip = 2) %>%
  mutate(across(BilletDisciplinNr_RD, \(x) as.integer(x))) %>%
  mutate(across(BilletDisciplin_ID, \(x) as.character(x))) %>%
  mutate(across(BilletDisciplin_RD, \(x) factor(
    x, levels = unique(x[order(BilletDisciplinNr_RD)]), ordered = TRUE))) %>%
  arrange(BilletDisciplinNr_RD)

# Dim2_BilletRække ----
Dim2_BilletRække <- read_excel(path = InputData_V, sheet = "🎫 Dim2_BilletRække", skip = 2) %>%
  mutate(across(BilletRaekkeNr_RD, \(x) as.integer(x))) %>%
  mutate(across(BilletRaekke_ID, \(x) as.character(x))) %>%
  mutate(across(BilletRaekke_RD, \(x) factor(
    x, levels = unique(x[order(BilletRaekkeNr_RD)]), ordered = TRUE))) %>%
  arrange(BilletRaekkeNr_RD)

# Dim2_BilletSpilformat ----
Dim2_BilletSpilformat <- read_excel(path = InputData_V, sheet = "🎫 Dim2_BilletSpilformat", skip = 2) %>%
  mutate(across(BilletSpilFormatNr_RD, \(x) as.integer(x))) %>%
  mutate(across(BilletSpilFormat_ID, \(x) as.character(x))) %>%
  mutate(across(c(BilletSpilFormat_RD, BilletSpilFormatEmoji_RD, BilletSpilFormatIkon_RD), \(x) factor(
    x, levels = unique(x[order(BilletSpilFormatNr_RD)]), ordered = TRUE))) %>%
  arrange(BilletSpilFormatNr_RD)

# Dim1_OrdreFoersteTid ----
Dim1_OrdreFoersteTid <- read_excel(path = InputData_V, sheet = "🎫 Dim1_OrdreFoersteTid", skip = 2) %>%
  mutate(across(OrdreFoersteTidKatNr_RD, \(x) as.integer(x))) %>%
  mutate(across("OrdreFoersteTidKatMin_RD", \(x) as.character(x))) %>%
  mutate(across("OrdreFoersteTidKatMaks_RD", \(x) as.character(x))) %>%
  mutate(across(OrdreFoersteTidKat_RD, \(x) as.character(x))) %>%
  arrange(OrdreFoersteTidKatNr_RD)

# Dim1_OrdreKat ----
Dim1_OrdreKat <- read_excel(path = InputData_V, sheet = "🎫 Dim1_OrdreKat", skip = 2) %>%
  mutate(across(OrdreKatNr_DW, \(x) as.integer(x))) %>%
  mutate(across(OrdreKat_ID, \(x) as.character(x))) %>%
  mutate(across(c(OrdreKat_DW, OrdreKatEmoji_DW, OrdreKatIkon_DW), \(x) factor(
      x, levels = unique(x[order(OrdreKatNr_DW)]), ordered = TRUE))) %>%
  arrange(OrdreKatNr_DW)

# Dim1_Klub ----
Dim1_Klub <- read_excel(path = InputData_V, sheet = "🛖 Dim1_Klub", skip = 2) %>%
  mutate(across(KlubNr_RD, \(x) as.integer(x))) %>%
  mutate(across(Klub_ID, \(x) as.character(x))) %>%
  mutate(across(KlubPostnr_RD, \(x) as.integer(x))) %>%
  mutate(across(KlubBy_RD, \(x) as.character(x))) %>%
  mutate(across(KlubMail_RD, \(x) as.character(x))) %>%
  mutate(across(KlubRegion_ID, \(x) as.character(x))) %>%
  mutate(KlubPostnrBy_DW = ifelse(
    grepl("Ingen klub|Udlandet", Klub_RD), as.character(Klub_RD), paste(KlubPostnr_RD, KlubBy_RD))) %>%
  mutate(KlubKat_DW = case_when(
    grepl("Ingen klub", Klub_RD) ~ "Ingen klub",
    grepl("Udlandet", Klub_RD) ~ "Udlandet",
    TRUE ~ "Klub")) %>%
  mutate(KlubLogo_DW = ifelse(is.na(Klub_RD), NA_character_,
    paste0("<img src=filer/medie/billede/klublogo/", fun_egen_sti(Klub_RD), ".png width=15>"))) %>%
  mutate(across(c(Klub_RD, KlubKat_DW), \(x) factor(
    x, levels = unique(x[order(KlubNr_RD)]), ordered = TRUE))) %>%
  arrange(KlubNr_RD)

# Dim2_KlubLandsdel ----
Dim2_KlubLandsdel <- read_excel(path = InputData_V, sheet = "🛖 Dim2_KlubLandsdel", skip = 2) %>%
  mutate(across(KlubLandsdelNr_RD, \(x) as.integer(x))) %>%
  mutate(across(KlubLandsdelPostnrMin_RD, \(x) as.integer(x))) %>%
  mutate(across(KlubLandsdelPostnrMaks_RD, \(x) as.integer(x))) %>%
  mutate(across(KlubLandsdel_RD, \(x) factor(
    x, levels = unique(x[order(KlubLandsdelNr_RD)]), ordered = TRUE))) %>%
  mutate(across(KlubEmoji_RD, \(x) as.character(x))) %>%
  mutate(across(KlubKatIkon_RD, \(x) as.character(x))) %>%
  arrange(KlubLandsdelNr_RD)

# Dim2_KlubRegion ----
Dim2_KlubRegion <- read_excel(path = InputData_V, sheet = "🛖 Dim2_KlubRegion", skip = 2) %>%
  mutate(across(KlubRegionNr_RD, \(x) as.integer(x))) %>%
  mutate(across(KlubRegion_ID, \(x) as.character(x))) %>%
  mutate(across(KlubRegion_RD, \(x) factor(
    x, levels = unique(x[order(KlubRegionNr_RD)]), ordered = TRUE))) %>%
  arrange(KlubRegionNr_RD)

# Dim1_DeltKoen ----
Dim1_DeltKoen <- read_excel(path = InputData_V, sheet = "👤 Dim1_DeltKoen", skip = 2) %>%
  mutate(across(DeltKoenNr_RD, \(x) as.integer(x))) %>%
  mutate(across(DeltKoen_ID, \(x) as.character(x))) %>%
  mutate(across(c(DeltKoen_RD, DeltKoenEmoji_RD, DeltKoenIkon_RD), \(x) factor(
      x, levels = unique(x[order(DeltKoenNr_RD)]), ordered = TRUE))) %>%
  arrange(DeltKoenNr_RD)

# Dim1_DeltSlutspil ----
Dim1_DeltSlutspil <- read_excel(path = InputData_V, sheet = "💪 Dim1_DeltSlutspil", skip = 2) %>%
  mutate(across(DeltSlutspilNr_RD, \(x) as.integer(x))) %>%
  mutate(across(DeltSlutspil_ID, \(x) as.character(x))) %>%
  mutate(across(DeltSlutspil_RD, \(x) factor(
    x, levels = unique(x[order(DeltSlutspilNr_RD)]), ordered = TRUE))) %>%
  arrange(DeltSlutspilNr_RD)

# Dim1_DeltPlacering ----
Dim1_DeltPlacering <- read_excel(path = InputData_V, sheet = "💪 Dim1_DeltPlacering", skip = 2) %>%
  mutate(across(DeltPlacNr_RD, \(x) as.integer(x))) %>%
  mutate(across(DeltPlac_ID, \(x) as.character(x))) %>%
  mutate(across(DeltPlac_RD, \(x) factor(
    x, levels = unique(x[order(DeltPlacNr_RD)]), ordered = TRUE))) %>%
  arrange(DeltPlacNr_RD)

# Dim1_DeltRating ----
Dim1_DeltRating <- read_excel(path = InputData_V, sheet = "💪 Dim1_DeltRating", skip = 2) %>%
  mutate(across(DeltRatingKatNr_RD, \(x) as.integer(x))) %>%
  mutate(across(DeltRatingKatMin_RD, \(x) as.integer(x))) %>%
  mutate(across(DeltRatingKatMaks_RD, \(x) as.integer(x))) %>%
  mutate(across(DeltRatingKat_RD, \(x) factor(
    x, levels = unique(x[order(DeltRatingKatNr_RD)]), ordered = TRUE))) %>%
  arrange(DeltRatingKatNr_RD)

# Dim1_DeltAlderKat ----
Dim1_DeltAlderKat <- read_excel(path = InputData_V, sheet = "📅 Dim1_DeltAlderKat", skip = 2) %>%
  mutate(across(DeltAlderKatNr_RD, \(x) as.integer(x))) %>%
  mutate(across(DeltAlderKatMin_RD, \(x) as.integer(x))) %>%
  mutate(across(DeltAlderKatMaks_RD, \(x) as.integer(x))) %>%
  mutate(across(DeltAlderKat_RD, \(x) factor(
    x, levels = unique(x[order(DeltAlderKatNr_RD)]), ordered = TRUE))) %>%
  arrange(DeltAlderKatNr_RD)

# Dim1_DeltKat ----
Dim1_DeltKat <- read_excel(path = InputData_V, sheet = "👤 Dim1_DeltKat", skip = 2) %>%
  mutate(across(DeltKatNr_RD, \(x) as.integer(x))) %>%
  mutate(across(DeltKatMin_ID, \(x) as.character(x))) %>%
  mutate(across(DeltKatMaks_ID, \(x) as.character(x))) %>%
  mutate(across(DeltKat_RD, \(x) factor(
    x, levels = unique(x[order(DeltKatNr_RD)]), ordered = TRUE))) %>%
  arrange(DeltKatNr_RD)

# Dim1_DeltGenKat ----
Dim1_DeltGenKat <- read_excel(path = InputData_V, sheet = "🔃 Dim1_DeltGenKat", skip = 2) %>%
  mutate(across(DeltGenKatNr_DW, \(x) as.integer(x))) %>%
  mutate(across(DeltGenKat_ID, \(x) as.character(x))) %>%
  mutate(across(c(DeltGenKat_DW, DeltGenKatEmoji_DW, DeltGenKatIkon_DW), \(x) factor(
      x, levels = unique(x[order(DeltGenKatNr_DW)]), ordered = TRUE))) %>%
  arrange(DeltGenKatNr_DW)