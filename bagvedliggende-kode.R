#' ---
#' title: Bagvedliggende R-kode for Ping Pong DM
#' output:
#'    html_document:
#'      theme: united
#'      df_print: paged
#'      code_folding: show
#'      code_download: yes
#'      toc: true
#'      toc_float:
#'        smooth_scroll: yes
#' ---

# Opsætning ---------------------------------------------------------------
#+ eval=F, warning=F, message=F

#> setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
for (Packages_V in c(
  "readxl", "cellranger", "openxlsx", "writexl", "dplyr", "tidyr", "stringr",
  "formattable", "lubridate", "plotly", "ggplot2", "ggtext", "forcats",
  "plotDK", "httr", "rvest", "pdftools", "kableExtra", "DT")) {
  if (!requireNamespace(Packages_V, quietly = TRUE)) {
    install.packages(Packages_V, dependencies = TRUE)
  }
  suppressWarnings(suppressPackageStartupMessages(library(Packages_V, character.only = TRUE)))
}

Sys.setlocale("LC_ALL", "da-DK.UTF-8")
format(Sys.Date(), "%A")
options(timeout = max(1000, getOption("timeout")))
options(OutDec= ",")
options(knitr.table.format = "html")
options(knitr.kable.NA = "")

theme_set(theme(
  legend.title     = element_blank(),
  axis.title.x     = element_blank(),
  axis.title.y     = element_blank(),
  legend.position  = "right",
  legend.direction = "vertical",
  plot.title       = element_text(hjust = 0.5),
  plot.subtitle    = element_text(hjust = 0.5),
  legend.text      = element_text(size = 15),
  axis.text.y      = element_text(hjust = 1, size = 15, face = "bold"),
  axis.ticks.y     = element_blank(),
  axis.text.x      = element_blank(),
  axis.ticks.x     = element_blank(),
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()))

options(DT.options = list(
  initComplete = JS(
    "function(settings, json) {", paste0(
      
      "$(this.api().table().container()).css({",
      "'font-family': '", "verdana", "', ",
      "'font-size': '", "90%", "'",
      "});",
      
      "$(this.api().table().header()).css({",
      "'background-color': '", "#1C2833", "', ",
      "'color': '", "#ffffff", "'",
      "});",
      
      "}")
  ),
  searchHighlight = T,
  paging = F,
  ordering = F,
  bInfo = F,
  autoWidth = T,
  scrollX = F,
  language = list(
    search = "Søg:")
  )
)

#' # Mine funktioner
# Mine funktioner ---------------------------------------------------------
#+ eval=F, warning=F, message=F

egen_sti_fun <- function(x) {
  x <- tolower(x)
  x <- gsub("æ|ä", "ae", x)
  x <- gsub("ø|ö", "oe", x)
  x <- gsub("å", "aa", x)
  x <- gsub("\\(|\\)", "", x)
  x <- gsub(" |/|\\+|\\*", "-", x)
  paste0(x)
}

#' # Kalender
# Kalender ----------------------------------------------------------------
#+ eval=F, warning=F, message=F

# DataKal_T
DataKal_T <- tibble(Dato_DW = seq(
  from = as_date("1900-01-01"),
  to   = as_date(ceiling_date(Sys.Date(), unit = "year")-1+years(10)),
  by   = "days")) %>%
  
  # År
  mutate(Aar_DW = as.integer(format(Dato_DW, "%Y"))) %>%
  mutate(AarDag_DW = as.integer(yday(Dato_DW))) %>%
  
  # Kvartal
  mutate(KvartalNr_DW = as.integer(quarter(Dato_DW))) %>%
  mutate(KvartalNavn_DW = paste0(KvartalNr_DW, ". kvartal")) %>%
  arrange(KvartalNr_DW) %>% mutate(across(c(
    "KvartalNavn_DW"), \(x) factor(x, levels = unique(x), ordered = T))) %>%
  
  # Måned
  mutate(MaanedNr_DW = as.integer(month(Dato_DW))) %>%
  mutate(MaanedNavnLang_DW = format(Dato_DW, "%B")) %>%
  mutate(MaanedNavnKort_DW = format(Dato_DW, "%b")) %>%
  mutate(MaanedDag_DW = as.integer(trimws(format(Dato_DW, "%e")))) %>%
  mutate(MaanedDagNavn_DW = paste0(MaanedDag_DW, ". ", MaanedNavnLang_DW)) %>%
  arrange(MaanedNr_DW) %>% mutate(across(c(
    "MaanedNavnLang_DW",
    "MaanedNavnKort_DW"), \(x) factor(x, levels = unique(x), ordered = T))) %>%
  
  # Uge
  mutate(Uge_DW = as.integer(format(Dato_DW, "%W"))) %>%
  
  # Dag
  mutate(UgeDag_DW = as.integer(format(Dato_DW, "%u"))) %>%
  mutate(UgeDagNavnLang_DW = format(Dato_DW, "%A")) %>%
  mutate(UgeDagNavnKort_DW = substr(format(Dato_DW, "%A"), 1, 3)) %>%
  arrange(UgeDag_DW) %>% mutate(across(c(
    "UgeDagNavnLang_DW",
    "UgeDagNavnKort_DW"), \(x) factor(x, levels = unique(x), ordered = T))) %>%
  
  # DMAA_DW
  mutate(DMAA_DW = format(Dato_DW, "%d.%m.%Y")) %>%
  
  arrange(Dato_DW)

#' # Data
# Data --------------------------------------------------------------------
#+ eval=F, warning=F, message=F

# Import Fact
Data_T <- read_excel(
	InputData_V,
	col_names = c(
	  "DeltID_RD", "DeltNavn_RD", "Klub_RD", "DeltKoen_RD", "OrdreDatoTid_RD",
	  "Billet_RD", "BilletStatus_RD", "DeltRang1_RD", "DeltRating2_RD",
	  "DeltRang3_RD", "DeltSlutspil_RD", "DeltPlac_RD", "DeltPraemie_RD"),
	range = cell_cols("A:M")) %>%
	slice_tail(n = -5)
  
# Left join Dim
Data_T <- Data_T %>%

  # Left join OrdreStatus_Dim1
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "BilletStatusNr_RD", "BilletStatus_RD", "BilletStatusEmoji_RD",
        "BilletStatusSimpel_RD", "BilletStatusSimpelIkon_RD"),
      range = cell_cols("O:S")), na_matches = "never", by = "BilletStatus_RD") %>%
  
  # Left join Billet_Dim1
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "BilletNr_RD", "Billet_RD", "EventAar_RD", "BilletStartDatoTid_RD", "BilletSlutDatoTid_RD",
        "BilletKat_RD", "BilletDisciplin_RD", "BilletRaekke_RD", "BilletSpilFormat_RD",
        "BilletBeskr_RD", "BilletTilvalg_RD", "BilletPris_RD",
        "BilletPrisArr_RD", "BilletAntalMaks_RD", "BilletPuljeDelt_RD"),
      range = cell_cols("O:AC")), na_matches = "never", by = "Billet_RD") %>%
  
  # Left join Event_Dim2
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "EventAarNr_RD", "EventAar_RD", "Event_RD", "EventAarFristDatoTid_RD", "EventAarAabningDatoTid_RD",
        "EventAarRatingDatoTid_RD", "EventAarPraemieSpons_RD", "EventAarSted_RD",
        "EventAarAdr_RD", "EventAarPostnr_RD", "EventAarBy_RD", "EventAarFarve1_RD", "EventAarFarve2_RD",
        "EventAarStedURL_RD", "EventAarUUID_RD", "EventAarToken_RD"),
      range = cell_cols("O:AD")), na_matches = "never", by = "EventAar_RD") %>%
  
  # Left join Event_Dim3
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "EventNr_RD", "Event_RD", "EventSportsgren_RD", "EventTurnering_RD"),
      range = cell_cols("O:R")), na_matches = "never", by = "Event_RD") %>%
  
  # Left join BilletKat_Dim2
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "BilletKatNr_RD", "BilletKat_RD", "BilletKatEmoji_RD", "BilletKatIkon_RD"),
      range = cell_cols("O:R")), na_matches = "never", by = "BilletKat_RD") %>%
  
  # Left join BilletDisciplin_Dim2
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "BilletDisciplinNr_RD", "BilletDisciplin_RD"),
      range = cell_cols("O:P")), na_matches = "never", by = "BilletDisciplin_RD") %>%
  
  # Left join BilletRække_Dim2
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "BilletRaekkeNr_RD", "BilletRaekke_RD"),
      range = cell_cols("O:P")), na_matches = "never", by = "BilletRaekke_RD") %>%
  
  # Left join BilletSpilformat_Dim2
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "BilletSpilFormatNr_RD", "BilletSpilFormat_RD", "BilletSpilFormatEmoji_RD", "BilletSpilFormatIkon_RD"),
      range = cell_cols("O:R")), na_matches = "never", by = "BilletSpilFormat_RD") %>%
  
  # Left join Klub_Dim1
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "KlubNr_RD", "Klub_RD", "KlubPostnr_RD", "KlubBy_RD", "KlubMail_RD", "KlubRegion_RD"),
      range = cell_cols("AX:BC")), na_matches = "never", by = "Klub_RD") %>%
  
  # Left join KlubLandsdel_Dim2
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "KlubLandsdelNr_RD", "KlubLandsdelPostnrMin_RD", "KlubLandsdelPostnrMaks_RD",
        "KlubLandsdel_RD", "KlubEmoji_RD", "KlubIkon_RD"),
      range = cell_cols("BE:BJ")), na_matches = "never", join_by(
        "KlubPostnr_RD" >= "KlubLandsdelPostnrMin_RD",
        "KlubPostnr_RD" <= "KlubLandsdelPostnrMaks_RD")) %>%
  
  # Left join KlubRegion_Dim2
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "KlubRegionNr_RD", "KlubRegion_RD"),
      range = cell_cols("AX:AY")), na_matches = "never", by = "KlubRegion_RD") %>%
  
  # Left join DeltKoen_Dim1
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "DeltKoenNr_RD", "DeltKoen_RD", "DeltKoenEmoji_RD", "DeltKoenIkon_RD"),
      range = cell_cols("AX:BA")), na_matches = "never", by = "DeltKoen_RD") %>%
  
  # Left join DeltSlutspil_Dim1
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "DeltSlutspilNr_RD", "DeltSlutspil_RD"),
      range = cell_cols("AY:AZ")), na_matches = "never", by = "DeltSlutspil_RD") %>%
  
  # Left join DeltPlacering_Dim1
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "DeltPlacNr_RD", "DeltPlac_RD"),
      range = cell_cols("AX:AY")), na_matches = "never", by = "DeltPlac_RD") %>%
  
  # Left join DeltRating_Dim1
  mutate(DeltRating2Join_DW = sprintf("%04s", DeltRating2_RD)) %>%
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "DeltRatingKatNr_RD", "DeltRatingKatMin_RD", "DeltRatingKatMaks_RD", "DeltRatingKat_RD"),
      range = cell_cols("BL:BO")), na_matches = "never", join_by(
        "DeltRating2Join_DW" >= "DeltRatingKatMin_RD",
        "DeltRating2Join_DW" <= "DeltRatingKatMaks_RD"))

# Ordre
Data_T <- Data_T %>%
  
  # OrdreDatoTid_RD
  mutate(across("OrdreDatoTid_RD", \(x) convertToDateTime(x))) %>%
  select(-OrdreDatoTid_RD, everything()) %>%
  
  # OrdreDato_DW
  mutate(OrdreDato_DW = OrdreDatoTid_RD) %>%
  mutate(across("OrdreDato_DW", \(x) as_date(x))) %>%
  select(-OrdreDato_DW, everything()) %>%
  
  # Left join DataKal_T
  left_join(
    y = DataKal_T %>% rename_with(~ paste0("OrdreDato_DW_", .)),
    by = c("OrdreDato_DW" = "OrdreDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # OrdreFoersteDatoTid_DW
  group_by(EventAar_RD, DeltID_RD) %>%
  mutate(OrdreFoersteDatoTid_DW = min(OrdreDatoTid_RD)) %>%
  mutate(across("OrdreFoersteDatoTid_DW", \(x) as_datetime(x))) %>%
  ungroup() %>%
  select(-OrdreFoersteDatoTid_DW, everything()) %>%
  
  # OrdreFoersteDato_DW
  mutate(OrdreFoersteDato_DW = OrdreFoersteDatoTid_DW) %>%
  mutate(across("OrdreFoersteDato_DW", \(x) as_date(x))) %>%
  select(-OrdreFoersteDato_DW, everything()) %>%
  
  # Left join DataKal_T
  left_join(
    y = DataKal_T %>% rename_with(~ paste0("OrdreFoersteDato_DW_", .)),
    by = c("OrdreFoersteDato_DW" = "OrdreFoersteDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # OrdreFoersteTidJoin_DW
  mutate(OrdreFoersteTidJoin_DW = format(OrdreFoersteDatoTid_DW, format = "%H%M%S")) %>%
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "OrdreFoersteTidKatNr_RD",
        "OrdreFoersteTidKatMin_RD",
        "OrdreFoersteTidKatMaks_RD",
        "OrdreFoersteTidKat_RD"),
      range = cell_cols("AF:AI")), na_matches = "never", join_by(
        "OrdreFoersteTidJoin_DW" >= "OrdreFoersteTidKatMin_RD",
        "OrdreFoersteTidJoin_DW" <= "OrdreFoersteTidKatMaks_RD")) %>%
  mutate(across("OrdreFoersteTidJoin_DW", \(x) as.character(x))) %>%
  select(-OrdreFoersteTidJoin_DW, everything()) %>%
  
  # OrdreFoersteTidKatNr_RD
  mutate(across("OrdreFoersteTidKatNr_RD", \(x) as.integer(x))) %>%
  select(-OrdreFoersteTidKatNr_RD, everything()) %>%
  
  # OrdreFoersteTidKatMin_RD
  mutate(across("OrdreFoersteTidKatMin_RD", \(x) as.character(x))) %>%
  select(-OrdreFoersteTidKatMin_RD, everything()) %>%
  
  # OrdreFoersteTidKatMaks_RD
  mutate(across("OrdreFoersteTidKatMaks_RD", \(x) as.character(x))) %>%
  select(-OrdreFoersteTidKatMaks_RD, everything()) %>%
  
  # OrdreFoersteTidKat_RD
  arrange(OrdreFoersteTidKat_RD) %>%
  mutate(across("OrdreFoersteTidKat_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-OrdreFoersteTidKat_RD, everything()) %>%

  # OrdreTilKatNr_DW
  mutate(OrdreTilKat_DW = case_when(
    grepl("Tilmeldt", BilletStatusSimpel_RD) & OrdreFoersteDatoTid_DW <= convertToDateTime(EventAarFristDatoTid_RD) ~ "Ordinær",
    grepl("Tilmeldt", BilletStatusSimpel_RD) & OrdreFoersteDatoTid_DW >  convertToDateTime(EventAarFristDatoTid_RD) ~ "Drive-in",
    grepl("Afbud",    BilletStatusSimpel_RD) ~ "Afbud")) %>%
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "OrdreTilKatNr_DW", "OrdreTilKat_DW", "OrdreTilKatEmoji_DW", "OrdreTilKatIkon_RD"),
      range = cell_cols("AK:AN")), na_matches = "never", by = "OrdreTilKat_DW") %>%
  mutate(across("OrdreTilKatNr_DW", \(x) as.integer(x))) %>%
  select(-OrdreTilKatNr_DW, everything()) %>%
  
  # OrdreTilKat_DW
  arrange(OrdreTilKatNr_DW) %>%
  mutate(across("OrdreTilKat_DW", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-OrdreTilKat_DW, everything()) %>%
  
  # OrdreTilKatEmoji_DW
  arrange(OrdreTilKatNr_DW) %>%
  mutate(across("OrdreTilKatEmoji_DW", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-OrdreTilKatEmoji_DW, everything()) %>%
  
  # OrdreTilKatIkon_RD
  arrange(OrdreTilKatNr_DW) %>%
  mutate(across("OrdreTilKatIkon_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-OrdreTilKatIkon_RD, everything())

# Billet
Data_T <- Data_T %>%
  
  # BilletNr_RD
  mutate(across("BilletNr_RD", \(x) as.integer(x))) %>%
  select(-BilletNr_RD, everything()) %>%

  # Billet_RD
  arrange(BilletNr_RD) %>%
  mutate(across("Billet_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-Billet_RD, everything()) %>%
  
  # BilletStartDatoTid_RD
  mutate(across("BilletStartDatoTid_RD", \(x) convertToDateTime(x))) %>%
  select(-BilletStartDatoTid_RD, everything()) %>%
  
  # BilletStartDato_DW
  mutate(BilletStartDato_DW = BilletStartDatoTid_RD) %>%
  mutate(across("BilletStartDato_DW", \(x) as_date(x))) %>%
  select(-BilletStartDato_DW, everything()) %>%
  
  # Left join DataKal_T
  left_join(
    y = DataKal_T %>% rename_with(~ paste0("BilletStartDato_DW_", .)),
    by = c("BilletStartDato_DW" = "BilletStartDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # BilletSlutDatoTid_RD
  mutate(across("BilletSlutDatoTid_RD", \(x) convertToDateTime(x))) %>%
  select(-BilletSlutDatoTid_RD, everything()) %>%
  
  # BilletSlutDato_DW
  mutate(BilletSlutDato_DW = BilletSlutDatoTid_RD) %>%
  mutate(across("BilletSlutDato_DW", \(x) as_date(x))) %>%
  select(-BilletSlutDato_DW, everything()) %>%
  
  # Left join DataKal_T
  left_join(
    y = DataKal_T %>% rename_with(~ paste0("BilletSlutDato_DW_", .)),
    by = c("BilletSlutDato_DW" = "BilletSlutDato_DW_Dato_DW"),
    na_matches = "never") %>%

  # BilletStatusNr_RD
  mutate(across("BilletStatusNr_RD", \(x) as.integer(x))) %>%
  select(-BilletStatusNr_RD, everything()) %>%

  # BilletStatus_RD
  arrange(BilletStatusNr_RD) %>%
  mutate(across("BilletStatus_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-BilletStatus_RD, everything()) %>%
  
  # BilletStatusEmoji_RD
  arrange(BilletStatusNr_RD) %>%
  mutate(across("BilletStatusEmoji_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-BilletStatusEmoji_RD, everything()) %>%
  
  # BilletStatusSimpel_RD
  arrange(BilletStatusNr_RD) %>%
  mutate(across("BilletStatusSimpel_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-BilletStatusSimpel_RD, everything()) %>%
  
  # BilletStatusSimpelIkon_RD
  arrange(BilletStatusNr_RD) %>%
  mutate(across("BilletStatusSimpelIkon_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-BilletStatusSimpelIkon_RD, everything()) %>%
  
  # BilletKatNr_RD
  mutate(across("BilletKatNr_RD", \(x) as.integer(x))) %>%
  select(-BilletKatNr_RD, everything()) %>%
  
  # BilletKat_RD
  arrange(BilletKatNr_RD) %>%
  mutate(across("BilletKat_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-BilletKat_RD, everything()) %>%
  
  # BilletKatEmoji_RD
  arrange(BilletKatNr_RD) %>%
  mutate(across("BilletKatEmoji_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-BilletKatEmoji_RD, everything()) %>%
  
  # BilletKatIkon_RD
  arrange(BilletKatNr_RD) %>%
  mutate(across("BilletKatIkon_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-BilletKatIkon_RD, everything()) %>%
  
  # BilletKatFarve_DW
  arrange(BilletKatNr_RD) %>%
  mutate(BilletKatFarve_DW = sub(".*(#.{6})>.*", "\\1", BilletKatIkon_RD)) %>%
  mutate(across("BilletKatFarve_DW", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-BilletKatFarve_DW, everything()) %>%
  
  # BilletDisciplinNr_RD
  mutate(across("BilletDisciplinNr_RD", \(x) as.integer(x))) %>%
  select(-BilletDisciplinNr_RD, everything()) %>%
  
  # BilletDisciplin_RD
  arrange(BilletDisciplinNr_RD) %>%
  mutate(across("BilletDisciplin_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-BilletDisciplin_RD, everything()) %>%
  
  # BilletRaekkeNr_RD
  mutate(across("BilletRaekkeNr_RD", \(x) as.integer(x))) %>%
  select(-BilletRaekkeNr_RD, everything()) %>%
  
  # BilletRaekke_RD
  arrange(BilletRaekkeNr_RD) %>%
  mutate(across("BilletRaekke_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-BilletRaekke_RD, everything()) %>%
  
  # BilletSpilFormatNr_RD
  mutate(across("BilletSpilFormatNr_RD", \(x) as.integer(x))) %>%
  select(-BilletSpilFormatNr_RD, everything()) %>%
  
  # BilletSpilFormat_RD
  arrange(BilletSpilFormatNr_RD) %>%
  mutate(across("BilletSpilFormat_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-BilletSpilFormat_RD, everything()) %>%
  
  # BilletSpilFormatEmoji_RD
  arrange(BilletSpilFormatNr_RD) %>%
  mutate(across("BilletSpilFormatEmoji_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-BilletSpilFormatEmoji_RD, everything()) %>%
  
  # BilletSpilFormatIkon_RD
  arrange(BilletSpilFormatNr_RD) %>%
  mutate(across("BilletSpilFormatIkon_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-BilletSpilFormatIkon_RD, everything()) %>%

  # BilletKat_DW
  arrange(BilletDisciplin_RD, BilletRaekke_RD, BilletSpilFormat_RD) %>%
  mutate(BilletKat_DW = ifelse(
    is.na(BilletDisciplin_RD) | is.na(BilletRaekke_RD) | is.na(BilletSpilFormat_RD), as.character(BilletKat_RD),
    paste(BilletDisciplin_RD, BilletRaekke_RD))) %>%
  mutate(across("BilletKat_DW", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-BilletKat_DW, everything()) %>%
  
  # BilletBeskr_RD
  mutate(across("BilletBeskr_RD", \(x) as.character(x))) %>%
  select(-BilletBeskr_RD, everything()) %>%
  
  # BilletTilvalg_RD
  mutate(across("BilletTilvalg_RD", \(x) as.character(x))) %>%
  select(-BilletTilvalg_RD, everything()) %>%
  
  # BilletPris_RD
  mutate(across("BilletPris_RD", \(x) as.numeric(x))) %>%
  select(-BilletPris_RD, everything()) %>%
  
  # BilletPrisArr_RD
  mutate(across("BilletPrisArr_RD", \(x) as.numeric(x))) %>%
  select(-BilletPrisArr_RD, everything()) %>%
  
  # BilletAntalMaks_RD
  mutate(across("BilletAntalMaks_RD", \(x) as.integer(x))) %>%
  select(-BilletAntalMaks_RD, everything()) %>%
  
  # BilletPuljeDelt_RD
  mutate(across("BilletPuljeDelt_RD", \(x) as.integer(x))) %>%
  select(-BilletPuljeDelt_RD, everything()) %>%
  
  # BilletPulje_DW
  add_count(
    EventAar_RD,
    BilletStatusSimpel_RD,
    BilletDisciplin_RD,
    BilletRaekke_RD,
    BilletSpilFormat_RD,
    name = "BilletPulje_DW") %>%
  mutate(BilletPulje_DW = ceiling(BilletPulje_DW/BilletPuljeDelt_RD)) %>%
  mutate(across("BilletPulje_DW", \(x) as.integer(x))) %>%
  select(-BilletPulje_DW, everything()) %>%
  
  # BilletPuljeStd_DW
  mutate(BilletPuljeStd_DW = BilletPuljeDelt_RD*BilletPulje_DW) %>%
  mutate(across("BilletPuljeStd_DW", \(x) as.integer(x))) %>%
  select(-BilletPuljeStd_DW, everything()) %>%
  
  # BilletDelt_DW
  add_count(
    EventAar_RD,
    BilletStatusSimpel_RD,
    BilletDisciplin_RD,
    BilletRaekke_RD,
    BilletSpilFormat_RD,
    name = "BilletDelt_DW") %>%
  mutate(across("BilletDelt_DW", \(x) as.integer(x))) %>%
  select(-BilletDelt_DW, everything()) %>%
  
  # BilletPuljeRest_DW
  mutate(BilletPuljeRest_DW = BilletPuljeStd_DW-BilletDelt_DW) %>%
  mutate(across("BilletPuljeRest_DW", \(x) as.integer(x))) %>%
  select(-BilletPuljeRest_DW, everything()) %>%

  # BilletPrisSum_DW
  mutate(BilletPrisSum_DW = ifelse(grepl("Tilmeldt", BilletStatusSimpel_RD), BilletPris_RD, NA)) %>%
  group_by(EventAar_RD) %>%
  mutate(BilletPrisSum_DW = sum(BilletPrisSum_DW, na.rm = TRUE)) %>%
  mutate(BilletPrisSum_DW = unique(na.omit(BilletPrisSum_DW))) %>%
  ungroup() %>%
  mutate(across("BilletPrisSum_DW", \(x) as.integer(x))) %>%
  select(-BilletPrisSum_DW, everything()) %>%

  # BilletPrisArrSum_DW
  mutate(BilletPrisArrSum_DW = ifelse(grepl("Tilmeldt", BilletStatusSimpel_RD), BilletPrisArr_RD, NA)) %>%
  group_by(EventAar_RD) %>%
  mutate(BilletPrisArrSum_DW = sum(BilletPrisArrSum_DW, na.rm = TRUE)) %>%
  mutate(BilletPrisArrSum_DW = unique(na.omit(BilletPrisArrSum_DW))) %>%
  ungroup() %>%
  mutate(across("BilletPrisArrSum_DW", \(x) as.integer(x))) %>%
  select(-BilletPrisArrSum_DW, everything()) %>%

  # BilletPrisRes_DW
  mutate(BilletPrisRes_DW = BilletPrisSum_DW-BilletPrisArrSum_DW) %>%
  mutate(across("BilletPrisRes_DW", \(x) as.integer(x))) %>%
  select(-BilletPrisRes_DW, everything())

# Event
Data_T <- Data_T %>%
  
  # EventNr_RD
  mutate(across("EventNr_RD", \(x) as.integer(x))) %>%
  select(-EventNr_RD, everything()) %>%
  
  # Event_RD
  arrange(EventNr_RD) %>%
  mutate(across("Event_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-Event_RD, everything()) %>%
  
  # EventSportsgren_RD
  mutate(across("EventSportsgren_RD", \(x) as.character(x))) %>%
  select(-EventSportsgren_RD, everything()) %>%
  
  # EventTurnering_RD
  mutate(across("EventTurnering_RD", \(x) as.character(x))) %>%
  select(-EventTurnering_RD, everything())

# EventAar
Data_T <- Data_T %>%
  
  # EventAarNr_RD
  mutate(across("EventAarNr_RD", \(x) as.integer(x))) %>%
  select(-EventAarNr_RD, everything()) %>%
  
  # EventAar_RD
  arrange(EventAarNr_RD) %>%
  mutate(across("EventAar_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-EventAar_RD, everything()) %>%
  
  # EventAarSidst_DW
  mutate(EventAarSidst_DW = lead(EventAar_RD, order_by = EventAar_RD)) %>%
  mutate(across("EventAarSidst_DW", \(x) as.character(x))) %>%
  group_by(EventAar_RD) %>%
  mutate(EventAarSidst_DW = ifelse(EventAar_RD != EventAarSidst_DW, EventAarSidst_DW, NA)) %>%
  fill(EventAarSidst_DW, .direction = "up") %>%
  ungroup %>%
  select(-EventAarSidst_DW, everything()) %>%
  
  # EventAarFristDatoTid_RD
  mutate(across("EventAarFristDatoTid_RD", \(x) convertToDateTime(x))) %>%
  select(-EventAarFristDatoTid_RD, everything()) %>%

  # EventAarFristDato_DW
  mutate(EventAarFristDato_DW = EventAarFristDatoTid_RD) %>%
  mutate(across("EventAarFristDato_DW", \(x) as_date(x))) %>%
  select(-EventAarFristDato_DW, everything()) %>%
  
  # Left join DataKal_T
  left_join(
    y = DataKal_T %>% rename_with(~ paste0("EventAarFristDato_DW_", .)),
    by = c("EventAarFristDato_DW" = "EventAarFristDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # EventAarAabningDatoTid_RD
  mutate(across("EventAarAabningDatoTid_RD", \(x) convertToDateTime(x))) %>%
  select(-EventAarAabningDatoTid_RD, everything()) %>%
  
  # EventAarAabningDato_DW
  mutate(EventAarAabningDato_DW = EventAarAabningDatoTid_RD) %>%
  mutate(across("EventAarAabningDato_DW", \(x) as_date(x))) %>%
  select(-EventAarAabningDato_DW, everything()) %>%
  
  # Left join DataKal_T
  left_join(
    y = DataKal_T %>% rename_with(~ paste0("EventAarAabningDato_DW_", .)),
    by = c("EventAarAabningDato_DW" = "EventAarAabningDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # EventAarRatingDatoTid_RD
  mutate(across("EventAarRatingDatoTid_RD", \(x) convertToDate(x))) %>%
  mutate(across("EventAarRatingDatoTid_RD", \(x) as_date(x))) %>%
  select(-EventAarRatingDatoTid_RD, everything()) %>%
  
  # EventAarRatingDato_DW
  mutate(EventAarRatingDato_DW = EventAarRatingDatoTid_RD) %>%
  mutate(across("EventAarRatingDato_DW", \(x) as_date(x))) %>%
  select(-EventAarRatingDato_DW, everything()) %>%
  
  # Left join DataKal_T
  left_join(
    y = DataKal_T %>% rename_with(~ paste0("EventAarRatingDato_DW_", .)),
    by = c("EventAarRatingDato_DW" = "EventAarRatingDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # EventAarPraemieSpons_RD
  mutate(across("EventAarPraemieSpons_RD", \(x) as.logical(x))) %>%
  select(-EventAarPraemieSpons_RD, everything()) %>%
  
  # EventAarSted_RD
  mutate(across("EventAarSted_RD", \(x) as.character(x))) %>%
  select(-EventAarSted_RD, everything()) %>%
  
  # EventAarAdr_RD
  mutate(across("EventAarAdr_RD", \(x) as.character(x))) %>%
  select(-EventAarAdr_RD, everything()) %>%
  
  # EventAarPostnr_RD
  mutate(across("EventAarPostnr_RD", \(x) as.integer(x))) %>%
  select(-EventAarPostnr_RD, everything()) %>%
  
  # EventAarBy_RD
  mutate(across("EventAarBy_RD", \(x) as.character(x))) %>%
  select(-EventAarBy_RD, everything()) %>%
  
  # EventAarPostnrBy_RD
  mutate(EventAarPostnrBy_RD = paste(EventAarPostnr_RD, EventAarBy_RD)) %>%
  mutate(across("EventAarPostnrBy_RD", \(x) as.character(x))) %>%
  select(-EventAarPostnrBy_RD, everything()) %>%
  
  # EventAarFarve1_RD
  mutate(across("EventAarFarve1_RD", \(x) as.character(x))) %>%
  select(-EventAarFarve1_RD, everything()) %>%
  
  # EventAarFarve2_RD
  mutate(across("EventAarFarve2_RD", \(x) as.character(x))) %>%
  select(-EventAarFarve2_RD, everything()) %>%
  
  # EventAarStedURL_RD
  mutate(across("EventAarStedURL_RD", \(x) as.character(x))) %>%
  select(-EventAarStedURL_RD, everything()) %>%
  
  # EventAarUUID_RD
  mutate(across("EventAarUUID_RD", \(x) as.character(x))) %>%
  select(-EventAarUUID_RD, everything()) %>%
  
  # EventAarToken_RD
  mutate(across("EventAarToken_RD", \(x) as.character(x))) %>%
  select(-EventAarToken_RD, everything()) %>%
  
  # EventAarStartDatoTid_DW
  group_by(EventAar_RD) %>%
  mutate(EventAarStartDatoTid_DW = min(BilletStartDatoTid_RD)) %>%
  ungroup() %>%
  mutate(across("EventAarStartDatoTid_DW", \(x) as_datetime(x))) %>%
  select(-EventAarStartDatoTid_DW, everything()) %>%
  
  # EventAarStartDato_DW
  mutate(EventAarStartDato_DW = EventAarStartDatoTid_DW) %>%
  mutate(across("EventAarStartDato_DW", \(x) as_date(x))) %>%
  select(-EventAarStartDato_DW, everything()) %>%
  
  # Left join DataKal_T
  left_join(
    y = DataKal_T %>% rename_with(~ paste0("EventAarStartDato_DW_", .)),
    by = c("EventAarStartDato_DW" = "EventAarStartDato_DW_Dato_DW"),
    na_matches = "never") %>%

  # EventAarSlutDatoTid_DW
  group_by(EventAar_RD) %>%
  mutate(EventAarSlutDatoTid_DW = max(BilletSlutDatoTid_RD)) %>%
  ungroup() %>%
  mutate(across("EventAarSlutDatoTid_DW", \(x) as_datetime(x))) %>%
  select(-EventAarSlutDatoTid_DW, everything()) %>%
  
  # EventAarSlutDato_DW
  mutate(EventAarSlutDato_DW = EventAarSlutDatoTid_DW) %>%
  mutate(across("EventAarSlutDato_DW", \(x) as_date(x))) %>%
  select(-EventAarSlutDato_DW, everything()) %>%
  
  # Left join DataKal_T
  left_join(
    y = DataKal_T %>% rename_with(~ paste0("EventAarSlutDato_DW_", .)),
    by = c("EventAarSlutDato_DW" = "EventAarSlutDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # EventAarDato_DW
  group_by(EventAar_RD) %>%
  mutate(EventAarDato_DW = case_when(
    EventAarStartDato_DW == EventAarSlutDato_DW ~ EventAarStartDato_DW_DMAA_DW,
    TRUE ~ paste(EventAarStartDato_DW_DMAA_DW, "til", EventAarSlutDato_DW_DMAA_DW))) %>%
  ungroup() %>%
  mutate(across("EventAarDato_DW", \(x) as.character(x))) %>%
  select(-EventAarDato_DW, everything()) %>%
  
  # EventAarStedURL_DW
  mutate(EventAarStedURL_DW = paste0(
    "<a href=", EventAarStedURL_RD, " target=_blank>", EventAarSted_RD, "</a>")) %>%
  mutate(across("EventAarStedURL_DW", \(x) as.character(x))) %>%
  select(-EventAarStedURL_DW, everything()) %>%

  # EventAarStedAdrPostnrURL_DW
  mutate(EventAarStedAdrPostnrURL_DW = paste0(
    "<a href=",EventAarStedURL_RD, " target=_blank>",
    EventAarSted_RD,
    "<br>",
    EventAarAdr_RD,
    "<br>",
    EventAarPostnrBy_RD,
    "</a>")) %>%
  mutate(across("EventAarStedAdrPostnrURL_DW", \(x) as.character(x))) %>%
  select(-EventAarStedAdrPostnrURL_DW, everything()) %>%

  # EventAarFra2021_DW
  mutate(EventAarFra2021_DW = if_else(EventAarStartDato_DW_Aar_DW >= 2021, TRUE, FALSE)) %>%
  mutate(across("EventAarFra2021_DW", \(x) as.logical(x))) %>%
  select(-EventAarFra2021_DW, everything())

# Klub
Data_T <- Data_T %>%

  # KlubNr_RD
  mutate(across("KlubNr_RD", \(x) as.integer(x))) %>%
  select(-KlubNr_RD, everything()) %>%

  # Klub_RD
  arrange(KlubNr_RD) %>%
  mutate(across("Klub_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-Klub_RD, everything()) %>%

  # KlubPostnr_RD
  mutate(across("KlubPostnr_RD", \(x) as.character(x))) %>%
  select(-KlubPostnr_RD, everything()) %>%

  # KlubBy_RD
  mutate(across("KlubBy_RD", \(x) as.character(x))) %>%
  select(-KlubBy_RD, everything()) %>%

  # KlubPostnrBy_DW
  mutate(KlubPostnrBy_DW = ifelse(
    grepl("Ingen klub|Udlandet", Klub_RD), Klub_RD, paste(KlubPostnr_RD, KlubBy_RD))) %>%
  mutate(across("KlubPostnrBy_DW", \(x) as.character(x))) %>%
  select(-KlubPostnrBy_DW, everything()) %>%

  # KlubMail_RD
  mutate(across("KlubMail_RD", \(x) as.character(x))) %>%
  select(-KlubMail_RD, everything()) %>%

  # KlubEmoji_RD
  mutate(across("KlubEmoji_RD", \(x) as.character(x))) %>%
  select(-KlubEmoji_RD, everything()) %>%

  # KlubIkon_RD
  mutate(across("KlubIkon_RD", \(x) as.character(x))) %>%
  select(-KlubIkon_RD, everything()) %>%

  # KlubLandsdelNr_RD
  mutate(across("KlubLandsdelNr_RD", \(x) as.integer(x))) %>%
  select(-KlubLandsdelNr_RD, everything()) %>%

  # KlubLandsdelPostnrMin_RD
  arrange(KlubLandsdelNr_RD) %>%
  mutate(across("KlubLandsdelPostnrMin_RD", \(x) as.character(x))) %>%
  select(-KlubLandsdelPostnrMin_RD, everything()) %>%

  # KlubLandsdelPostnrMaks_RD
  arrange(KlubLandsdelNr_RD) %>%
  mutate(across("KlubLandsdelPostnrMaks_RD", \(x) as.integer(x))) %>%
  select(-KlubLandsdelPostnrMaks_RD, everything()) %>%

  # KlubLandsdel_RD
  arrange(KlubLandsdelNr_RD) %>%
  mutate(across("KlubLandsdel_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-KlubLandsdel_RD, everything()) %>%

  # KlubRegionNr_RD
  mutate(across("KlubRegionNr_RD", \(x) as.integer(x))) %>%
  select(-KlubRegionNr_RD, everything()) %>%

  # KlubRegion_RD
  arrange(KlubRegionNr_RD) %>%
  mutate(across("KlubRegion_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-KlubRegion_RD, everything()) %>%

  # KlubKat_DW
  mutate(KlubKat_DW = case_when(
    grepl("Ingen klub", Klub_RD) ~ "Ingen klub",
    grepl("Udlandet", Klub_RD) ~ "Udlandet",
    TRUE ~ "Klub")) %>%
  arrange(KlubNr_RD) %>%
  mutate(across("KlubKat_DW", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-KlubKat_DW, everything()) %>%

  # KlubLogo_DW
  mutate(KlubLogo_DW = paste0(
    "<img src=filer/klublogo/", egen_sti_fun(Klub_RD), ".png width=15>")) %>%
  mutate(across("KlubLogo_DW", \(x) as.character(x))) %>%
  select(-KlubLogo_DW, everything())

# Delt
Data_T <- Data_T %>%

  # DeltID_RD
  mutate(across("DeltID_RD", \(x) as.character(x))) %>%
  select(-DeltID_RD, everything()) %>%
  
  # DeltNavn_RD
  mutate(across("DeltNavn_RD", \(x) as.character(x))) %>%
  select(-DeltNavn_RD, everything()) %>%
  
  # DeltFoedtDato_DW
  mutate(DeltFoedtDato_DW = if_else(
    substr(DeltID_RD, 5, 6) <= substr(EventAarStartDato_DW_Aar_DW, 3, 4),
    paste0(
      as.numeric(substr(EventAarStartDato_DW_Aar_DW, 1, 2)), substr(DeltID_RD, 5, 6), "-",
      substr(DeltID_RD, 3, 4), "-",
      substr(DeltID_RD, 1, 2)),
    paste0(
      as.numeric(substr(EventAarStartDato_DW_Aar_DW, 1, 2))-1, substr(DeltID_RD, 5, 6), "-",
      substr(DeltID_RD, 3, 4), "-",
      substr(DeltID_RD, 1, 2)))) %>%
  mutate(across("DeltFoedtDato_DW", \(x) as_date(x))) %>%
  select(-DeltFoedtDato_DW, everything()) %>%
  
  # Left join DataKal_T
  left_join(
    y = DataKal_T %>% rename_with(~ paste0("DeltFoedtDato_DW_", .)),
    by = c("DeltFoedtDato_DW" = "DeltFoedtDato_DW_Dato_DW"),
    na_matches = "never") %>%
  
  # DeltAlder_DW
  mutate(DeltAlder_DW = trunc((DeltFoedtDato_DW %--% EventAarStartDatoTid_DW) / years(1))) %>%
  mutate(across("DeltAlder_DW", \(x) as.integer(x))) %>%
  select(-DeltAlder_DW, everything()) %>%

  # DeltAlderJoin_DW
  mutate(DeltAlderJoin_DW = sprintf("%02d", DeltAlder_DW)) %>%
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "DeltAlderKatNr_RD", "DeltAlderKatMin_RD", "DeltAlderKatMaks_RD", "DeltAlderKat_RD"),
      range = cell_cols("BQ:BT")), na_matches = "never", join_by(
        "DeltAlderJoin_DW" >= "DeltAlderKatMin_RD",
        "DeltAlderJoin_DW" <= "DeltAlderKatMaks_RD")) %>%
  mutate(across("DeltAlderJoin_DW", \(x) as.character(x))) %>%
  select(-DeltAlderJoin_DW, everything()) %>%
  
  # DeltAlderKatNr_RD
  mutate(across("DeltAlderKatNr_RD", \(x) as.integer(x))) %>%
  select(-DeltAlderKatNr_RD, everything()) %>%
  
  # DeltAlderKatMin_RD
  mutate(across("DeltAlderKatMin_RD", \(x) as.character(x))) %>%
  select(-DeltAlderKatMin_RD, everything()) %>%
  
  # DeltAlderKatMaks_RD
  mutate(across("DeltAlderKatMaks_RD", \(x) as.character(x))) %>%
  select(-DeltAlderKatMaks_RD, everything()) %>%
  
  # DeltAlderKat_RD
  arrange(DeltAlderKatNr_RD) %>%
  mutate(across("DeltAlderKat_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-DeltAlderKat_RD, everything()) %>%
  
  # DeltYngstAeldst_DW
  group_by(EventAar_RD, BilletStatusSimpel_RD, BilletKat_RD) %>%
  mutate(DeltYngstAeldst_DW = case_when(
    DeltFoedtDato_DW == min(DeltFoedtDato_DW, na.rm = T) ~ "Yngst",
    DeltFoedtDato_DW == max(DeltFoedtDato_DW, na.rm = T) ~ "Ældst",
    TRUE ~ NA_character_)) %>%
  ungroup() %>%
  mutate(across("DeltYngstAeldst_DW", \(x) as.character(x))) %>%
  select(-DeltYngstAeldst_DW, everything()) %>%
  
  # DeltKoenNr_RD
  mutate(across("DeltKoenNr_RD", \(x) as.integer(x))) %>%
  select(-DeltKoenNr_RD, everything()) %>%
  
  # DeltKoen_RD
  arrange(DeltKoenNr_RD) %>%
  mutate(across("DeltKoen_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-DeltKoen_RD, everything()) %>%
  
  # DeltKoenEmoji_RD
  arrange(DeltKoenNr_RD) %>%
  mutate(across("DeltKoenEmoji_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-DeltKoenEmoji_RD, everything()) %>%
  
  # DeltKoenIkon_RD
  arrange(DeltKoenNr_RD) %>%
  mutate(across("DeltKoenIkon_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-DeltKoenIkon_RD, everything()) %>%
  
  # DeltRang1_RD
  mutate(across("DeltRang1_RD", \(x) as.integer(x))) %>%
  select(-DeltRang1_RD, everything()) %>%
  
  # DeltRating2_RD
  mutate(across("DeltRating2_RD", \(x) as.integer(x))) %>%
  select(-DeltRating2_RD, everything()) %>%

  # DeltRang3_RD
  mutate(across("DeltRang3_RD", \(x) as.integer(x))) %>%
  select(-DeltRang3_RD, everything()) %>%
  
  # DeltRating_DW
  mutate(DeltRating_DW = case_when(
    !is.na(DeltRang1_RD)  ~ paste0("[", DeltRang1_RD, "] ", DeltRating2_RD),
    is.na(DeltRating2_RD) ~ "-",
    TRUE ~ as.character(DeltRating2_RD))) %>%
  mutate(across("DeltRating_DW", \(x) as.character(x))) %>%
  select(-DeltRating_DW, everything()) %>%

  # DeltRating2Join_DW
  mutate(across("DeltRating2Join_DW", \(x) as.character(x))) %>%
  select(-DeltRating2Join_DW, everything()) %>%
  
  # DeltRatingKatNr_RD
  mutate(across("DeltRatingKatNr_RD", \(x) as.integer(x))) %>%
  select(-DeltRatingKatNr_RD, everything()) %>%
  
  # DeltRatingKatMin_RD
  mutate(across("DeltRatingKatMin_RD", \(x) as.character(x))) %>%
  select(-DeltRatingKatMin_RD, everything()) %>%

  # DeltRatingKatMaks_RD
  mutate(across("DeltRatingKatMaks_RD", \(x) as.character(x))) %>%
  select(-DeltRatingKatMaks_RD, everything()) %>%
  
  # DeltRatingKat_RD
  arrange(DeltRatingKatNr_RD) %>%
  mutate(across("DeltRatingKat_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-DeltRatingKat_RD, everything()) %>%
  
  # DeltSlutspilNr_RD
  mutate(across("DeltSlutspilNr_RD", \(x) as.integer(x))) %>%
  select(-DeltSlutspilNr_RD, everything()) %>%
  
  # DeltSlutspil_RD
  arrange(DeltSlutspilNr_RD) %>%
  mutate(across("DeltSlutspil_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-DeltSlutspil_RD, everything()) %>%
  
  # DeltPlacNr_RD
  mutate(across("DeltPlacNr_RD", \(x) as.integer(x))) %>%
  select(-DeltPlacNr_RD, everything()) %>%
  
  # DeltPlac_RD
  arrange(DeltPlacNr_RD) %>%
  mutate(across("DeltPlac_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-DeltPlac_RD, everything()) %>%

  # DeltAntalBillet_DW
  add_count(Billet_RD, BilletKat_RD, BilletStatusSimpel_RD,
            name = "DeltAntalBillet_DW") %>%
  group_by(Billet_RD, BilletKat_RD, BilletStatusSimpel_RD) %>%
  mutate(DeltAntalBillet_DW = DeltAntalBillet_DW-sum(is.na(DeltID_RD))) %>%
  ungroup() %>%
  mutate(DeltAntalBillet_DW = case_when(
    !is.na(DeltID_RD) ~ DeltAntalBillet_DW)) %>%
  group_by(Billet_RD) %>%
  fill(DeltAntalBillet_DW, .direction = "updown") %>%
  ungroup() %>%
  mutate(across("DeltAntalBillet_DW", \(x) as.integer(x))) %>%
  select(-DeltAntalBillet_DW, everything()) %>%
  
  # DeltPraemie_RD
  mutate(across("DeltPraemie_RD", \(x) as.numeric(x))) %>%
  select(-DeltPraemie_RD, everything()) %>%
  
  # DeltPraemiePct_DW
  group_by(EventAar_RD) %>%
  mutate(DeltPraemiePct_DW = DeltPraemie_RD/sum(DeltPraemie_RD, na.rm = T)) %>%
  ungroup() %>%
  mutate(across("DeltPraemiePct_DW", \(x) as.numeric(x))) %>%
  select(-DeltPraemiePct_DW, everything()) %>%
  
  # DeltPraemieAkt_DW
  group_by(EventAar_RD) %>%
  mutate(DeltPraemieAkt_DW =  case_when(
    is.na(DeltPraemie_RD) ~ NA,
    EventAarPraemieSpons_RD == T ~ DeltPraemiePct_DW*sum(DeltPraemie_RD, na.rm = T),
    EventAarPraemieSpons_RD == F ~ DeltPraemiePct_DW*BilletPrisArr_RD*DeltAntalBillet_DW)) %>%
  ungroup() %>%
  mutate(across("DeltPraemieAkt_DW", \(x) as.numeric(x))) %>%
  select(-DeltPraemieAkt_DW, everything()) %>%
  
  # DeltPraemiePot_DW
  group_by(EventAar_RD) %>%
  mutate(DeltPraemiePot_DW =  case_when(
    is.na(DeltPraemie_RD) ~ NA,
    EventAarPraemieSpons_RD == T ~ DeltPraemiePct_DW*sum(DeltPraemie_RD, na.rm = T),
    EventAarPraemieSpons_RD == F ~ DeltPraemiePct_DW*BilletPrisArr_RD*BilletAntalMaks_RD)) %>%
  ungroup() %>%
  mutate(across("DeltPraemiePot_DW", \(x) as.numeric(x))) %>%
  select(-DeltPraemiePot_DW, everything()) %>%
  
  # DeltNavnKlub_DW
  mutate(DeltNavnKlub_DW = case_when(
    is.na(DeltID_RD) ~ NA_character_,
    grepl("Ingen klub|Udlandet", Klub_RD) ~ paste0(DeltNavn_RD),
    TRUE ~ paste0(DeltNavn_RD, ", <i>", Klub_RD, "</i>"))) %>%
  mutate(across("DeltNavnKlub_DW", \(x) as.character(x))) %>%
  select(-DeltNavnKlub_DW, everything()) %>%
  
  # DeltNavnBilletKat_DW
  group_by(EventAar_RD, DeltID_RD, BilletStatusSimpel_RD) %>%
  arrange(BilletKat_RD, desc(OrdreFoersteDatoTid_DW)) %>%
  mutate(DeltNavnBilletKat_DW = case_when(
    is.na(DeltID_RD) ~ NA_character_,
    grepl("Ingen klub|Udlandet", Klub_RD) ~ paste0(DeltNavn_RD, " (", DeltAlder_DW, " år) ", str_c(
      BilletKatIkon_RD, collapse = "<wbr>")),
    TRUE ~ paste0(DeltNavnKlub_DW, " (", DeltAlder_DW, " år) ", str_c(
      BilletKatIkon_RD, collapse = "<wbr>")))) %>%
  ungroup() %>%
  mutate(across("DeltNavnBilletKat_DW", \(x) as.character(x))) %>%
  select(-DeltNavnBilletKat_DW, everything()) %>%
  
  # DeltBilletSalgNr_DW
  add_count(EventAar_RD, BilletStatusSimpel_RD, DeltID_RD, name = "DeltBilletSalgNr_DW") %>%
  mutate(across("DeltBilletSalgNr_DW", \(x) as.integer(x))) %>%
  select(-DeltBilletSalgNr_DW, everything()) %>%
  
  # DeltBilletSalg_DW
  arrange(DeltBilletSalgNr_DW) %>%
  mutate(DeltBilletSalg_DW = paste(DeltBilletSalgNr_DW, "stk. billetsalg")) %>%
  mutate(across("DeltBilletSalg_DW", \(x) factor(x, ordered = T))) %>%
  select(-DeltBilletSalg_DW, everything()) %>%
  
  # DeltGenNr_DW
  arrange(OrdreDatoTid_RD, BilletKat_RD) %>%
  group_by(DeltID_RD, BilletKat_RD, EventAarFra2021_DW) %>%
  mutate(DeltGenNr_DW = ifelse(grepl("Afbud", BilletStatusSimpel_RD), 0, 1)) %>%
  mutate(DeltGenNr_DW = cumsum(DeltGenNr_DW)) %>%
  ungroup() %>%
  mutate(across("DeltGenNr_DW", \(x) as.integer(x))) %>%
  select(-DeltGenNr_DW, everything()) %>%
  
  # DeltGen_DW
  arrange(DeltGenNr_DW) %>%
  mutate(DeltGen_DW = paste0(DeltGenNr_DW, ". gang")) %>%
  mutate(across("DeltGen_DW", \(x) factor(x, ordered = T))) %>%
  select(-DeltGen_DW, everything()) %>%
  
  # DeltGenKatNr_RD
  mutate(DeltGenKat_DW = case_when(
    DeltGenNr_DW == 1 ~ "Debutant",
    DeltGenNr_DW >= 2 ~ "Gentilmelding",
    TRUE ~ "Ikke hidtil")) %>%
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "DeltGenKatNr_RD", "DeltGenKat_DW", "DeltGenKatEmoji_RD", "DeltGenKatIkon_RD"),
      range = cell_cols("AP:AS")), na_matches = "never", by = "DeltGenKat_DW") %>%
  mutate(across("DeltGenKatNr_RD", \(x) as.integer(x))) %>%
  select(-DeltGenKatNr_RD, everything()) %>%
  
  # DeltGenKat_DW
  arrange(DeltGenKatNr_RD) %>%
  mutate(across("DeltGenKat_DW", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-DeltGenKat_DW, everything()) %>%
  
  # DeltGenKatEmoji_RD
  arrange(DeltGenKatNr_RD) %>%
  mutate(across("DeltGenKatEmoji_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-DeltGenKatEmoji_RD, everything()) %>%
  
  # DeltGenKatIkon_RD
  arrange(DeltGenKatNr_RD) %>%
  mutate(across("DeltGenKatIkon_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-DeltGenKatIkon_RD, everything()) %>%
  
  # DeltKodeJoin_RD
  mutate(DeltKodeJoin_RD = str_sub(DeltID_RD, -4, -1)) %>%
  left_join(
    y = read_excel(
      InputData_V, col_names = c(
        "DeltKatNr_RD", "DeltKatMin_RD", "DeltKatMaks_RD", "DeltKat_RD"),
      range = cell_cols("BV:BY")), na_matches = "never", join_by(
        "DeltKodeJoin_RD" >= "DeltKatMin_RD",
        "DeltKodeJoin_RD" <= "DeltKatMaks_RD")) %>%
  mutate(across("DeltKodeJoin_RD", \(x) as.character(x))) %>%
  select(-DeltKodeJoin_RD, everything()) %>%
  
  # DeltKatNr_RD
  mutate(across("DeltKatNr_RD", \(x) as.integer(x))) %>%
  select(-DeltKatNr_RD, everything()) %>%
  
  # DeltKatMin_RD
  mutate(across("DeltKatMin_RD", \(x) as.character(x))) %>%
  select(-DeltKatMin_RD, everything()) %>%
  
  # DeltKatMaks_RD
  mutate(across("DeltKatMaks_RD", \(x) as.character(x))) %>%
  select(-DeltKatMaks_RD, everything()) %>%
  
  # DeltKat_RD
  arrange(DeltKatNr_RD) %>%
  mutate(across("DeltKat_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-DeltKat_RD, everything()) %>%

  # DeltStatusSimpel_RD
  arrange(EventAar_RD, BilletStatusSimpel_RD, BilletKat_RD) %>%
  group_by(
    EventAar_RD,
    DeltID_RD) %>%
  mutate(DeltStatusSimpel_RD = ifelse(any(BilletStatusSimpel_RD == "Tilmeldt"), "Tilmeldt", "<q>Totalafbud</q>")) %>%
  ungroup() %>%
  mutate(across("DeltStatusSimpel_RD", \(x) factor(x, levels = unique(x), ordered = T))) %>%
  select(-DeltStatusSimpel_RD, everything()) %>%
  
  # DeltSnakeSeedNr_DW
  group_by(EventAar_RD, Billet_RD, BilletStatusSimpel_RD) %>%
  arrange(
    DeltRang1_RD,
    desc(DeltRating2_RD),
    DeltRang3_RD,
    DeltID_RD) %>%
  mutate(DeltSnakeSeedNr_DW = row_number()) %>%
  ungroup() %>%
  mutate(across("DeltSnakeSeedNr_DW", \(x) as.integer(x))) %>%
  select(-DeltSnakeSeedNr_DW, everything()) %>%
  
  # DeltSnakeSeedLagNr_DW
  group_by(EventAar_RD, Billet_RD, BilletStatusSimpel_RD) %>%
  mutate(DeltSnakeSeedLagNr_DW = rep(1:unique(BilletPulje_DW),each = unique(BilletPulje_DW))[seq_len(n())]) %>%
  ungroup() %>%
  mutate(across("DeltSnakeSeedLagNr_DW", \(x) as.integer(x))) %>%
  select(-DeltSnakeSeedLagNr_DW, everything()) %>%
  
  # DeltSnakePuljeNr_DW
  group_by(EventAar_RD, Billet_RD, BilletStatusSimpel_RD, DeltSnakeSeedLagNr_DW) %>%
  mutate(DeltSnakePuljeNr_DW = case_when(
    DeltSnakeSeedLagNr_DW %% 2 == 1 ~ row_number(),
    DeltSnakeSeedLagNr_DW %% 1 == 0 ~ rev(row_number()))) %>%
  mutate(DeltSnakePuljeNr_DW = ifelse(
    DeltSnakeSeedLagNr_DW == unique(BilletPuljeDelt_RD),
    DeltSnakePuljeNr_DW + BilletPuljeRest_DW, DeltSnakePuljeNr_DW)) %>%
  ungroup() %>%
  mutate(across("DeltSnakePuljeNr_DW", \(x) as.integer(x))) %>%
  select(-DeltSnakePuljeNr_DW, everything()) %>%

  # DeltForskudt_DW
  group_by(EventAar_RD, DeltStatusSimpel_RD, DeltID_RD, OrdreDatoTid_RD) %>%
  mutate(DeltForskudt_DW = ifelse(OrdreDatoTid_RD != OrdreFoersteDatoTid_DW, 1, 0)) %>%
  group_by(EventAar_RD, DeltStatusSimpel_RD) %>%
  mutate(DeltForskudt_DW = sum(DeltForskudt_DW)) %>%
  ungroup() %>%
  mutate(across("DeltForskudt_DW", \(x) as.character(x))) %>%
  select(-DeltForskudt_DW, everything()) %>%

  # DeltUnik_DW
  group_by(EventAar_RD, DeltStatusSimpel_RD, DeltID_RD) %>%
  mutate(DeltUnik_DW = ifelse(row_number() == 1, 1, NA)) %>%
  ungroup() %>%
  mutate(across("DeltUnik_DW", \(x) as.integer(x))) %>%
  select(-DeltUnik_DW, everything())

# Stat
Data_T <- Data_T %>%
  
  # StatOrdreAntal_DW
  add_count(EventAar_RD, BilletStatusSimpel_RD, Billet_RD, name = "StatOrdreAntal_DW") %>%
  group_by(EventAar_RD, BilletStatusSimpel_RD) %>%
  mutate(StatOrdreAntal_DW = paste0(
    "<b>", StatOrdreAntal_DW, " ", BilletStatusSimpel_RD, " til ", BilletKat_DW, " ", BilletKatIkon_RD, "</b>")) %>%
  arrange(BilletStatusSimpel_RD, BilletDisciplin_RD, BilletRaekke_RD, BilletSpilFormat_RD) %>%
  mutate(StatOrdreAntal_DW = str_c(unique(na.omit(StatOrdreAntal_DW)), collapse = "<br>")) %>%
  ungroup() %>%
  mutate(across("StatOrdreAntal_DW", \(x) as.character(x))) %>%
  select(-StatOrdreAntal_DW, everything()) %>%
  
  # StatDeltAntal_DW
  group_by(EventAar_RD, DeltStatusSimpel_RD) %>%
  mutate(StatDeltAntal_DW = ifelse(is.na(DeltUnik_DW), NA, sum(DeltUnik_DW, na.rm = TRUE))) %>%
  group_by(EventAar_RD) %>%
  mutate(StatDeltAntal_DW = ifelse(is.na(StatDeltAntal_DW), NA, paste0(
    StatDeltAntal_DW, " ", DeltStatusSimpel_RD , " (",
    percent(StatDeltAntal_DW/sum(ifelse(is.na(StatDeltAntal_DW), 0, 1)), digits = 0), ") ", BilletStatusSimpelIkon_RD))) %>%
  arrange(DeltStatusSimpel_RD) %>%
  mutate(StatDeltAntal_DW = str_c(unique(na.omit(StatDeltAntal_DW)), collapse = " ∙ ")) %>%
  ungroup() %>%
  mutate(across("StatDeltAntal_DW", \(x) as.character(x))) %>%
  select(-StatDeltAntal_DW, everything()) %>%
  
  # StatDeltKoenAntal_DW
  group_by(EventAar_RD, DeltStatusSimpel_RD, DeltKoen_RD) %>%
  mutate(StatDeltKoenAntal_DW = ifelse(is.na(DeltUnik_DW), NA, sum(DeltUnik_DW, na.rm = TRUE))) %>%
  group_by(EventAar_RD, DeltStatusSimpel_RD) %>%
  mutate(StatDeltKoenAntal_DW = ifelse(is.na(StatDeltKoenAntal_DW), NA, paste0(
    StatDeltKoenAntal_DW, " ", DeltKoen_RD , " (",
    percent(StatDeltKoenAntal_DW/sum(ifelse(is.na(StatDeltKoenAntal_DW), 0, 1)), digits = 0), ") ", DeltKoenIkon_RD))) %>%
  arrange(DeltKoen_RD) %>%
  mutate(StatDeltKoenAntal_DW = str_c(unique(na.omit(StatDeltKoenAntal_DW)), collapse = " ∙ ")) %>%
  ungroup() %>%
  mutate(across("StatDeltKoenAntal_DW", \(x) as.character(x))) %>%
  select(-StatDeltKoenAntal_DW, everything()) %>%
  
  # StatDeltGenKatAntal_DW
  group_by(EventAar_RD, DeltStatusSimpel_RD, DeltGenKat_DW) %>%
  mutate(StatDeltGenKatAntal_DW = ifelse(is.na(DeltUnik_DW), NA, sum(DeltUnik_DW, na.rm = TRUE))) %>%
  group_by(EventAar_RD, DeltStatusSimpel_RD) %>%
  mutate(StatDeltGenKatAntal_DW = ifelse(is.na(StatDeltGenKatAntal_DW), NA, paste0(
    StatDeltGenKatAntal_DW, " ", DeltGenKat_DW , " (",
    percent(StatDeltGenKatAntal_DW/sum(ifelse(is.na(StatDeltGenKatAntal_DW), 0, 1)), digits = 0), ") ", DeltGenKatIkon_RD))) %>%
  arrange(DeltGenKat_DW) %>%
  mutate(StatDeltGenKatAntal_DW = str_c(unique(na.omit(StatDeltGenKatAntal_DW)), collapse = " ∙ ")) %>%
  ungroup() %>%
  mutate(across("StatDeltGenKatAntal_DW", \(x) as.character(x))) %>%
  select(-StatDeltGenKatAntal_DW, everything()) %>%
  
  # StatDeltAlderKatAntal_DW
  group_by(EventAar_RD, DeltStatusSimpel_RD, DeltAlderKat_RD) %>%
  mutate(StatDeltAlderKatAntal_DW = ifelse(is.na(DeltUnik_DW), NA, sum(DeltUnik_DW, na.rm = TRUE))) %>%
  group_by(EventAar_RD, DeltStatusSimpel_RD) %>%
  mutate(StatDeltAlderKatAntal_DW = ifelse(is.na(StatDeltAlderKatAntal_DW), NA, paste0(
    StatDeltAlderKatAntal_DW, " ", DeltAlderKat_RD , " (",
    percent(StatDeltAlderKatAntal_DW/sum(ifelse(is.na(StatDeltAlderKatAntal_DW), 0, 1)), digits = 0), ") ", IkonFødt_V))) %>%
  arrange(DeltAlderKat_RD) %>%
  mutate(StatDeltAlderKatAntal_DW = str_c(unique(na.omit(StatDeltAlderKatAntal_DW)), collapse = " ∙ ")) %>%
  ungroup() %>%
  mutate(across("StatDeltAlderKatAntal_DW", \(x) as.character(x))) %>%
  select(-StatDeltAlderKatAntal_DW, everything()) %>%
  
  # StatDeltAlderAntal_DW
  mutate(StatDeltAlderAntal_DW = ifelse(is.na(DeltUnik_DW), NA, DeltAlder_DW)) %>%
  group_by(EventAar_RD, DeltStatusSimpel_RD) %>%
  mutate(StatDeltAlderAntal_DW = paste(
    "Yngst", min(StatDeltAlderAntal_DW, na.rm = TRUE), "år", IkonFødt_V,
    "∙ Gns.", round(mean(StatDeltAlderAntal_DW, na.rm = TRUE), 0), "år", IkonFødt_V,
    "∙ Ældst", max(StatDeltAlderAntal_DW, na.rm = TRUE), "år", IkonFødt_V)) %>%
  ungroup() %>%
  mutate(across("StatDeltAlderAntal_DW", \(x) as.character(x))) %>%
  select(-StatDeltAlderAntal_DW, everything()) %>%
  
  # StatDeltLandsdelAntal_DW
  group_by(EventAar_RD, DeltStatusSimpel_RD, DeltID_RD) %>%
  mutate(StatDeltLandsdelAntal_DW = ifelse(
    row_number() == 1 & grepl("Tilmeldt", BilletStatusSimpel_RD) & !grepl("Ingen klub|Udlandet", Klub_RD), 1, NA)) %>%
  group_by(EventAar_RD, KlubLandsdel_RD) %>%
  mutate(StatDeltLandsdelAntal_DW = ifelse(is.na(StatDeltLandsdelAntal_DW), NA, sum(StatDeltLandsdelAntal_DW, na.rm = TRUE))) %>%
  group_by(EventAar_RD) %>%
  mutate(StatDeltLandsdelAntal_DW = ifelse(is.na(StatDeltLandsdelAntal_DW), NA, paste0(
    StatDeltLandsdelAntal_DW, " ", KlubLandsdel_RD , " (",
    percent(StatDeltLandsdelAntal_DW/sum(ifelse(is.na(StatDeltLandsdelAntal_DW), 0, 1)), digits = 0), ") ", KlubIkon_RD))) %>%
  arrange(EventAar_RD, KlubLandsdel_RD) %>%
  mutate(StatDeltLandsdelAntal_DW = str_c(unique(na.omit(StatDeltLandsdelAntal_DW)), collapse = " ∙ ")) %>%
  ungroup() %>%
  mutate(across("StatDeltLandsdelAntal_DW", \(x) as.character(x))) %>%
  select(-StatDeltLandsdelAntal_DW, everything()) %>%
  
  # StatDeltRegionAntal_DW
  group_by(EventAar_RD, DeltStatusSimpel_RD, DeltID_RD) %>%
  mutate(StatDeltRegionAntal_DW = ifelse(
    row_number() == 1 & grepl("Tilmeldt", BilletStatusSimpel_RD) & !grepl("Ingen klub|Udlandet", Klub_RD), 1, NA)) %>%
  group_by(EventAar_RD, KlubRegion_RD) %>%
  mutate(StatDeltRegionAntal_DW = ifelse(is.na(StatDeltRegionAntal_DW), NA, sum(StatDeltRegionAntal_DW, na.rm = TRUE))) %>%
  group_by(EventAar_RD) %>%
  mutate(StatDeltRegionAntal_DW = ifelse(is.na(StatDeltRegionAntal_DW), NA, paste0(
    StatDeltRegionAntal_DW, " ", KlubRegion_RD , " (",
    percent(StatDeltRegionAntal_DW/sum(ifelse(is.na(StatDeltRegionAntal_DW), 0, 1)), digits = 0), ") ", KlubIkon_RD))) %>%
  arrange(EventAar_RD, KlubRegion_RD) %>%
  mutate(StatDeltRegionAntal_DW = str_c(unique(na.omit(StatDeltRegionAntal_DW)), collapse = " ∙ ")) %>%
  ungroup() %>%
  mutate(across("StatDeltRegionAntal_DW", \(x) as.character(x))) %>%
  select(-StatDeltRegionAntal_DW, everything()) %>%
  
  # StatDeltKlubKatAntal_DW
  group_by(EventAar_RD, DeltStatusSimpel_RD, DeltID_RD) %>%
  mutate(StatDeltKlubKatAntal_DW = ifelse(
    row_number() == 1 & grepl("Tilmeldt", BilletStatusSimpel_RD), 1, NA)) %>%
  group_by(EventAar_RD, KlubKat_DW) %>%
  mutate(StatDeltKlubKatAntal_DW = ifelse(is.na(StatDeltKlubKatAntal_DW), NA, sum(StatDeltKlubKatAntal_DW, na.rm = TRUE))) %>%
  group_by(EventAar_RD) %>%
  mutate(StatDeltKlubKatAntal_DW = ifelse(is.na(StatDeltKlubKatAntal_DW), NA, paste0(
    StatDeltKlubKatAntal_DW, " ", KlubKat_DW , " (",
    percent(StatDeltKlubKatAntal_DW/sum(ifelse(is.na(StatDeltKlubKatAntal_DW), 0, 1)), digits = 0), ") ", KlubIkon_RD))) %>%
  arrange(EventAar_RD, KlubKat_DW) %>%
  mutate(StatDeltKlubKatAntal_DW = str_c(unique(na.omit(StatDeltKlubKatAntal_DW)), collapse = " ∙ ")) %>%
  ungroup() %>%
  mutate(across("StatDeltKlubKatAntal_DW", \(x) as.character(x))) %>%
  select(-StatDeltKlubKatAntal_DW, everything()) %>%
  
  # StatKlubAntal_DW
  group_by(EventAar_RD, DeltStatusSimpel_RD, Klub_RD) %>%
  mutate(StatKlubAntal_DW = ifelse(
    row_number() == 1 & grepl("Tilmeldt", BilletStatusSimpel_RD) & !grepl("Ingen klub|Udlandet", Klub_RD), 1, NA)) %>%
  group_by(EventAar_RD) %>%
  mutate(StatKlubAntal_DW = ifelse(is.na(StatKlubAntal_DW), NA, sum(StatKlubAntal_DW, na.rm = TRUE))) %>%
  mutate(StatKlubAntal_DW = ifelse(is.na(StatKlubAntal_DW), NA, paste(
    StatKlubAntal_DW, ifelse(StatKlubAntal_DW == 1, "klub", "forskellige klubber"), KlubIkon_RD))) %>%
  mutate(StatKlubAntal_DW = unique(na.omit(StatKlubAntal_DW))) %>%
  ungroup() %>%
  mutate(across("StatKlubAntal_DW", \(x) as.character(x))) %>%
  select(-StatKlubAntal_DW, everything()) %>%

  # StatBilletAntal_DW
  mutate(StatBilletAntal_DW = ifelse(grepl("Tilmeldt", BilletStatusSimpel_RD), 1, NA)) %>%
  group_by(EventAar_RD) %>%
  mutate(StatBilletAntal_DW = sum(StatBilletAntal_DW, na.rm = TRUE)) %>%
  mutate(StatBilletAntal_DW = unique(na.omit(StatBilletAntal_DW))) %>%
  ungroup() %>%
  mutate(across("StatBilletAntal_DW", \(x) as.integer(x))) %>%
  select(-StatBilletAntal_DW, everything()) %>%

  # StatDeltagerAntal_DW
  group_by(EventAar_RD, DeltStatusSimpel_RD, DeltID_RD) %>%
  mutate(StatDeltagerAntal_DW = ifelse(
    row_number() == 1 & grepl("Tilmeldt", BilletStatusSimpel_RD), 1, NA)) %>%
  group_by(EventAar_RD) %>%
  mutate(StatDeltagerAntal_DW = sum(StatDeltagerAntal_DW, na.rm = TRUE)) %>%
  mutate(StatDeltagerAntal_DW = unique(na.omit(StatDeltagerAntal_DW))) %>%
  ungroup() %>%
  mutate(across("StatDeltagerAntal_DW", \(x) as.integer(x))) %>%
  select(-StatDeltagerAntal_DW, everything()) %>%

  # StatBilletGnsAntal_DW
  group_by(EventAar_RD) %>%
  mutate(StatBilletGnsAntal_DW = paste(
    "Gns.", round(StatBilletAntal_DW/StatDeltagerAntal_DW, 1), ifelse(round(StatBilletAntal_DW/StatDeltagerAntal_DW, 1) == 1,
    "billet pr. deltager", "billetter pr. deltager"), IkonBillet_V)) %>%
  ungroup() %>%
  mutate(across("StatBilletGnsAntal_DW", \(x) as.character(x))) %>%
  select(-StatBilletGnsAntal_DW, everything()) %>%

  # StatAlderForskelAntal_DW
  group_by(EventAar_RD, BilletStatusSimpel_RD, Billet_RD) %>%
  mutate(StatAlderForskelAntal_DW = max(DeltAlder_DW)-min(DeltAlder_DW)) %>%
  ungroup() %>%
  mutate(across("StatAlderForskelAntal_DW", \(x) as.character(x))) %>%
  select(-StatAlderForskelAntal_DW, everything()) %>%

  # StatOrdreKatAntal_DW
  add_count(
    EventAar_RD, BilletStatusSimpel_RD, OrdreTilKat_DW,
    name = "StatOrdreKatAntal_DW") %>%
  group_by(EventAar_RD) %>%
  mutate(StatOrdreKatAntal_DW = paste0(
    StatOrdreKatAntal_DW, " ", OrdreTilKat_DW , " (", 
    percent(StatOrdreKatAntal_DW/sum(ifelse(StatOrdreKatAntal_DW == 0, 0, 1)), digits = 0), ") ", OrdreTilKatIkon_RD)) %>%
  arrange(EventAar_RD, OrdreTilKat_DW) %>%
  mutate(StatOrdreKatAntal_DW = str_c(unique(na.omit(StatOrdreKatAntal_DW)), collapse = " ∙ ")) %>%
  ungroup() %>%
  mutate(across("StatOrdreKatAntal_DW", \(x) as.character(x))) %>%
  select(-StatOrdreKatAntal_DW, everything()) %>%
  
  # StatDeltRatingKatAntal_DW
  group_by(EventAar_RD, DeltStatusSimpel_RD, DeltID_RD) %>%
  mutate(StatDeltRatingKatAntal_DW = ifelse(
    row_number() == 1 & grepl("Tilmeldt", BilletStatusSimpel_RD) &
    !is.na(DeltRatingKat_RD) & grepl("Ping Pong", BilletKat_RD), 1, NA)) %>%
  group_by(EventAar_RD, DeltRatingKat_RD) %>%
  mutate(StatDeltRatingKatAntal_DW = ifelse(is.na(StatDeltRatingKatAntal_DW), NA, sum(StatDeltRatingKatAntal_DW, na.rm = TRUE))) %>%
  group_by(EventAar_RD) %>%
  mutate(StatDeltRatingKatAntal_DW = ifelse(is.na(StatDeltRatingKatAntal_DW), NA, paste0(
    StatDeltRatingKatAntal_DW, " ", DeltRatingKat_RD , " (",
    percent(StatDeltRatingKatAntal_DW/sum(ifelse(is.na(StatDeltRatingKatAntal_DW), 0, 1)), digits = 0), ") ", IkonPingPong_V))) %>%
  arrange(EventAar_RD, DeltRatingKat_RD) %>%
  mutate(StatDeltRatingKatAntal_DW = str_c(unique(na.omit(StatDeltRatingKatAntal_DW)), collapse = " ∙ ")) %>%
  ungroup() %>%
  mutate(across("StatDeltRatingKatAntal_DW", \(x) as.character(x))) %>%
  select(-StatDeltRatingKatAntal_DW, everything()) %>%
  
  # StatDeltRatingAntal_DW
  group_by(EventAar_RD, DeltStatusSimpel_RD, DeltID_RD) %>%
  mutate(StatDeltRatingAntal_DW = ifelse(
    row_number() == 1 & grepl("Tilmeldt", BilletStatusSimpel_RD) &
    grepl("Ping Pong", BilletKat_RD), DeltRating2_RD, NA)) %>%
  group_by(EventAar_RD) %>%
  mutate(StatDeltRatingAntal_DW = paste(
    "Min.", min(StatDeltRatingAntal_DW, na.rm = TRUE), "rating", IkonPingPong_V,
    "∙ Gns.", round(mean(StatDeltRatingAntal_DW, na.rm = TRUE), 0), "rating", IkonPingPong_V,
    "∙ Maks", max(StatDeltRatingAntal_DW, na.rm = TRUE), "rating", IkonPingPong_V)) %>%
  ungroup() %>%
  mutate(across("StatDeltRatingAntal_DW", \(x) as.character(x))) %>%
  select(-StatDeltRatingAntal_DW, everything()) %>%

  # StatOekonomiAntal_DW
  group_by(EventAar_RD) %>%
  mutate(StatOekonomiAntal_DW = paste(
    "Omsætning kr.", BilletPrisSum_DW, IkonPenge_V,
    "∙ Arrangørpris kr.", BilletPrisArrSum_DW, IkonPenge_V,
    "∙ Over-/underskud arrangør kr.", BilletPrisRes_DW, IkonPenge_V)) %>%
  mutate(StatOekonomiAntal_DW = ifelse(all(is.na(StatOekonomiAntal_DW)), NA, unique(na.omit(StatOekonomiAntal_DW)))) %>%
  mutate(across("StatOekonomiAntal_DW", \(x) as.character(x))) %>%
  select(-StatOekonomiAntal_DW, everything()) %>%
  
  # StatForskudtTilAntal_DW
  group_by(EventAar_RD) %>%
  mutate(StatForskudtTilAntal_DW = ifelse(grepl("Tilmeldt", BilletStatusSimpel_RD), DeltForskudt_DW, NA)) %>%
  mutate(StatForskudtTilAntal_DW = ifelse(is.na(StatForskudtTilAntal_DW), NA, paste(
    StatForskudtTilAntal_DW, ifelse(
      StatForskudtTilAntal_DW == 1, "forskudt tilmelding", "forskudte tilmeldinger"), IkonBillet_V))) %>%
  mutate(StatForskudtTilAntal_DW = ifelse(all(is.na(StatForskudtTilAntal_DW)), NA, unique(na.omit(StatForskudtTilAntal_DW)))) %>%
  ungroup() %>%
  mutate(across("StatForskudtTilAntal_DW", \(x) as.character(x))) %>%
  select(-StatForskudtTilAntal_DW, everything())

# Info
Data_T <- Data_T %>%
  
  # InfoNedtælling_DW
  group_by(EventAar_RD) %>%
  mutate(InfoNedtælling_DW = case_when(
    InputInfo1234_V %in% c(1, 2) ~ "",
    InputInfo1234_V %in% c(3, 4) ~ paste0(
     '<p style="text-align:center;width:50em;max-width:100%">
      <b style=font-size:120%;text-transform:uppercase>Nedtælling</b>
    	<br>
    	<b style=font-size:80%;text-transform:uppercase>', EventAar_RD, '</b>
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
  select(-InfoNedtælling_DW, everything()) %>%
  
  # InfoPlakatCTA_DW
  group_by(EventAar_RD) %>%
  mutate(InfoPlakatCTA_DW = case_when(
    InputInfo1234_V %in% c(1) ~ paste0(
      "<img src=filer/generelt/forside.png style=width:30em;max-width:100%;border-radius:20px>"),
    InputInfo1234_V %in% c(2) ~ paste0(
      "![](filer/event/", egen_sti_fun(EventAar_RD), "/", egen_sti_fun(EventAar_RD), "-teaserplakat", ".png){width=30em}",
      "<br>",
      "<figcaption>",
      "[<i style=font-size:80%>[Klik her for teaserplakat som PDF til udskrift]</i>]",
      "(filer/event/", egen_sti_fun(EventAar_RD), "/", egen_sti_fun(EventAar_RD), "-teaserplakat", ".pdf){target=_blank}",
      "</figcaption><p><p>"),
    InputInfo1234_V %in% c(3, 4) ~ paste0(
      "<br><br>",
      "<a style=display:inline-block;background:#FF4A6E;color:#FFFFFF;",
      "border-radius:40px;padding-left:50px;padding-right:50px;padding-top:5px;padding-bottom:5px;",
      "text-decoration:none href=indbydelse-tilmelding-", EventAarStartDato_DW_Aar_DW, ".qmd#tilmelding>",
      "<b style=font-size:150%>", IkonBillet_V, " Tilmeld</b>",
      "<br>",
      "<i style=font-size:90%>", EventAar_RD, "</i></a>",
      "<br><br>",
      "<i style=font-size:80%>",
      "Hurtigt overblik over eventet ses i indbydelsesplakaten ", IkonHåndNed_V, "</i>",
      "<br>",
      "![](filer/event/", egen_sti_fun(EventAar_RD), "/", egen_sti_fun(EventAar_RD), "-indbydelsesplakat", ".png){width=50em}",
      "<br>",
      "<span>",
      "[<i style=font-size:80%>",
      "[Klik her for indbydelesplakat som PDF til udskrift]</i>]",
      "(filer/event/", egen_sti_fun(EventAar_RD), "/", egen_sti_fun(EventAar_RD), "-indbydelsesplakat", ".pdf){target=_blank}",
      "</span>",
      "</p>"))) %>%
  ungroup() %>%
  select(-InfoPlakatCTA_DW, everything()) %>%
  
  # InfoForside_DW
  group_by(EventAar_RD) %>%
  mutate(InfoForside_DW = case_when(
    InputInfo1234_V %in% c(1) ~ paste(
      "<i>Nærmere information om Ping Pong DM", EventAarStartDato_DW_Aar_DW+1, "følger.</i>"),
    InputInfo1234_V %in% c(2) ~ paste0(
      "<i>", EventAar_RD, " afholdes ", EventAarDato_DW, " i",
      EventAarStedURL_DW, ". Der åbnes for tilmelding", EventAarAabningDato_DW_DMAA_DW, 
      "hvor der vil komme en fane med hhv. <q>Indbydelse & tilmelding</q> samt ",
      "<q>Præmier & deltagere</q>, som vil blive opdateret løbende.</i>"),
    InputInfo1234_V %in% c(3, 4) ~ paste0(
      "<i style=font-size:100%>",
      "<b>Afholdes ", EventAarDato_DW, " i ", EventAarStedURL_DW, "</b></i>",
      "<br>",
      "<i style=font-size:80%>Først til mølle-princip ∙ Tilmeldingsfrist ",
      EventAarFristDato_DW_DMAA_DW, "</i>",
      "<br><br>",
      "<ul>",
      "<li><p>", IkonBillet_V, " ", "[<b>Indbydelse & tilmelding</b>](indbydelse-tilmelding-2023.qmd)",
      "<br>",
      "<i>Indbydelse, tidsplan, praktisk info samt tilmelding/betaling",
      "&nbsp;til ", EventAar_RD, ".</i></p></li>",
      "<li><p>", IkonGentagelse_V, " ", "[<b>Præmier & deltagere</b>](praemier-deltagere.qmd)",
      "<br>",
      "<i>Præmier og deltagere opdateres løbende",
      "&nbsp;til ", EventAar_RD, ".</i></p>",
      "<i style=font-size:80%;display:inline-block;border:0.6px;border-style:solid;padding:5px>",
      StatOrdreAntal_DW, "</i></li>",
      "</ul>"))) %>%
  ungroup() %>%
  select(-InfoForside_DW, everything()) %>%
  
  # InfoFacebook_DW
  group_by(EventAar_RD) %>%
  mutate(InfoFacebook_DW = case_when(
    InputInfo1234_V %in% c(1) ~ paste0(
      "<i>Like og følg den officielle ",
      "[<b>Facebook-side <q>Ping Pong DK</q></b>]",
      "(https://www.facebook.com/{{< var var.facebook_side_id >}}){target=_blank}",
      "&nbsp;for at holde dig opdateret.</i>"),
    InputInfo1234_V %in% c(2, 3, 4) ~ paste0(
      "<i><p>Del gerne budskabet via ",
      "[<b>Facebook-begivenheden <q>", toupper(EventAar_RD), "</q></b>]",
      "({{< var var.facebook_event_url >}}){target=_blank}",
      "&nbsp;ved at trykke interesseret/deltager og inviter folk.</p>",
      "Like og følg",
      "&nbsp;[Facebook-siden <q>Ping Pong DK</q>]",
      "(https://www.facebook.com/{{< var var.facebook_side_id >}}){target=_blank}",
      "&nbsp;for at holde dig opdateret.</i>"))) %>%
  ungroup() %>%
  select(-InfoFacebook_DW, everything()) %>%
  
  # InfoTipIndbydelse_DW
  group_by(EventAar_RD) %>%
  mutate(InfoTipIndbydelse_DW = paste(
    IkonBillet_V, "Indbydelse, tidsplan og praktisk info til",
    EventAar_RD, "ses [<b>HER</b>](indbydelse-tilmelding-2023.qmd).</i>")) %>%
  ungroup() %>%
  select(-InfoTipIndbydelse_DW, everything()) %>%
  
  # InfoTipPraemierDeltagere_DW
  group_by(EventAar_RD) %>%
  mutate(InfoTipPraemierDeltagere_DW = paste(
    "<p>",
    IkonGentagelse_V, " Præmier og deltagere opdateres løbende til ",
    EventAar_RD, " [<b>HER</b>](praemier-deltagere.qmd).",
    "</p>",
    "<i style=font-size:80%;display:inline-block;border:0.6px;border-style:solid;padding:5px>",
    StatOrdreAntal_DW, "</i>")) %>%
  ungroup() %>%
  select(-InfoTipPraemierDeltagere_DW, everything()) %>%
  
  # InfoTipRegler_DW
  group_by(EventAar_RD) %>%
  mutate(InfoTipRegler_DW = paste(
    IkonRegler_V, "I Ping Pong tages det bedste fra fortidens- og nutidens bordtennis og kan", 
    "sammenlignes med ordsproget <q>gammel vin på nye flasker</q>. Der er nogle få regler, der",
    "adskiller Ping Pong fra bordtennis, bl.a. spilles der til 15 point (14-14 er afgørende bold),",
    "alle spiller på lige vilkår med sandpapirsbat, hvor der byttes bat mellem hvert sæt, og der kan",
    "tages <q>dobbeltpoint</q>. Se mere [<b>HER</b>](regler.qmd).")) %>%
  ungroup() %>%
  select(-InfoTipRegler_DW, everything()) %>%
  
  # InfoTipVM_DW
  group_by(EventAar_RD) %>%
  mutate(InfoTipVM_DW = paste(
    IkonGlobus_V, "World Championship of Ping Pong (WCPP) afholdes sædvanligvis",
    "i London med en præmiesum på $100.000 og eksponeres på bl.a. Viaplay Sport og Sky Sports.",
    "Se mere [<b>HER</b>](wcpp.qmd).")) %>%
  ungroup() %>%
  select(-InfoTipVM_DW, everything()) %>%
  
  # Sorter efter (1) OrdreFoersteDatoTid_DW, (2) BilletKat_RD
  arrange(desc(EventAar_RD), desc(OrdreFoersteDatoTid_DW), BilletKat_RD)

# DataEventAar_T
DataEventAar_T <- Data_T %>%
  mutate(across(where(~ is.factor(.)), as.character)) %>%
  arrange(desc(EventAarNr_RD)) %>%
  distinct(across(starts_with(c("EventAar", "Info"))))

# DataBillet_T
DataBillet_T <- Data_T %>% filter(grepl("Tilmeldt", BilletStatusSimpel_RD)) %>%
  mutate(across(where(~ is.factor(.)), as.character)) %>%
  arrange(desc(BilletNr_RD)) %>%
  distinct(across(starts_with(c("EventAar_RD", "Billet", "Stat"))))

  #' # Billettype
# Billettype --------------------------------------------------------------
#+ eval=F, warning=F, message=F

DataBilletKat_T  <- Data_T %>%
  group_by(Billet_RD) %>%
  mutate(BilletKat_DW = paste0(
    "<b>", BilletKat_DW, "</b>",
    "<br>",
    "<i style=font-size:80%>", BilletBeskr_RD, "</i>")) %>%
  mutate(BilletPris_DW = paste(
    "<b>", "Kr.", format(BilletPris_RD, big.mark = "."), IkonPenge_V, "</b>",
    "<br>", "<i style=font-size:80%>", "Maks.",
    format(BilletAntalMaks_RD, big.mark = "."), IkonPerson_V, "</i>")) %>%
  ungroup() %>%
  arrange(BilletStartDatoTid_RD, desc(Billet_RD)) %>%
  distinct(
    " "            = BilletKatIkon_RD,
    "Billettype"   = BilletKat_DW,
    "Pris & maks." = BilletPris_DW,
    EventAar_RD)

#' # Præmier
# Præmier -----------------------------------------------------------------

#' ## Pengepræmier
#+ eval=F, warning=F, message=F

DataPraemiePenge_T <- Data_T %>%
  filter(!is.na(DeltSlutspil_RD) & !is.na(DeltPlac_RD) & !is.na(DeltPraemie_RD)) %>%
  
  mutate(PraemieRank_DW = "4") %>%

  group_by(EventAar_RD) %>%
  bind_rows(summarise(., across(where(is.numeric), \(x) sum(x)), .groups = "keep")) %>% ungroup() %>%
  mutate(PraemieRank_DW = ifelse(!is.na(PraemieRank_DW), PraemieRank_DW, "1")) %>%
  ungroup() %>%

  group_by(EventAar_RD, Billet_RD) %>%
  bind_rows(summarise(., across(where(is.numeric), \(x) sum(x)), .groups = "keep")) %>% ungroup() %>%
  mutate(PraemieRank_DW = ifelse(!is.na(PraemieRank_DW), PraemieRank_DW, "2")) %>%
  ungroup() %>%
  
  group_by(EventAar_RD, Billet_RD, DeltSlutspil_RD) %>%
  bind_rows(summarise(., across(where(is.numeric), \(x) sum(x)), .groups = "keep")) %>% ungroup() %>%
  mutate(PraemieRank_DW = ifelse(!is.na(PraemieRank_DW), PraemieRank_DW, "3")) %>%
  ungroup() %>%
  
  group_by(EventAar_RD) %>%
  fill(EventAar_RD, EventAarPraemieSpons_RD, .direction = "updown") %>%
  ungroup() %>%
  
  group_by(Billet_RD) %>%
  fill(
    Billet_RD,
    BilletDisciplin_RD,
    BilletRaekke_RD,
    BilletSpilFormat_RD,
    BilletKat_DW, .direction = "updown") %>%
  ungroup() %>%
  
  mutate(across(c("BilletKat_DW", "DeltSlutspil_RD", "DeltPlac_RD"), \(x) as.character(x))) %>%
  mutate(across("PraemieRank_DW", \(x) as.integer(x))) %>%
  mutate(DeltSlutspilPlac_DW = case_when(
    PraemieRank_DW == "1" ~ "Præmiesum",
    PraemieRank_DW == "2" ~ BilletKat_DW,
    PraemieRank_DW == "3" ~ DeltSlutspil_RD,
    PraemieRank_DW == "4" ~ DeltPlac_RD,
    TRUE ~ NA_character_)) %>%
  filter(!is.na(DeltSlutspilPlac_DW)) %>%
  
  group_by(EventAar_RD) %>%
  filter(!(PraemieRank_DW == "2" & n_distinct(BilletKat_DW, na.rm = TRUE) == 1)) %>%
  filter(!(PraemieRank_DW == "3" & n_distinct(DeltSlutspil_RD, na.rm = TRUE) == 1)) %>%
  ungroup() %>%
  
  mutate(DeltPraemieAkt_DW = paste("kr.", format(round(
    0.001+DeltPraemieAkt_DW), big.mark = "."))) %>%
  mutate(DeltPraemiePct_DW = paste0(signif(
    0.001+100*DeltPraemiePct_DW, 3), "%")) %>%
  mutate(DeltPraemiePot_DW = paste("kr.", format(round(
    0.001+DeltPraemiePot_DW), big.mark = "."))) %>%
  
  mutate(across(c(
    BilletDisciplin_RD,
    BilletRaekke_RD,
    BilletSpilFormat_RD,
    DeltSlutspil_RD,
    PraemieRank_DW,
    DeltPlac_RD), ~ ifelse(!is.na(.x), .x, "1"))) %>%
  
  arrange(
    desc(EventAar_RD),
    BilletDisciplin_RD,
    BilletRaekke_RD,
    BilletSpilFormat_RD,
    DeltSlutspil_RD,
    PraemieRank_DW,
    DeltPlac_RD, na.last = TRUE) %>%
  
  select(
    " " = DeltSlutspilPlac_DW,
    "Præmiepenge" = DeltPraemieAkt_DW,
    "Pct." = DeltPraemiePct_DW,
    "Potentielt" = DeltPraemiePot_DW,
    PraemieRank_DW,
    Billet_RD,
    EventAarPraemieSpons_RD,
    EventAar_RD)

#' ## Gaver
#+ eval=F, warning=F, message=F

DataPraemieYngstAeldst_T <- Data_T %>%
  filter(
    !is.na(DeltID_RD) &
      grepl("Tilmeldt", BilletStatusSimpel_RD) &
      grepl("Ping Pong", BilletKat_RD)) %>%
  group_by(EventAar_RD) %>%
  filter(DeltFoedtDato_DW == max(DeltFoedtDato_DW) | DeltFoedtDato_DW == min(DeltFoedtDato_DW)) %>%
  ungroup() %>%
  mutate(Født  = format(DeltFoedtDato_DW, "%d.%m.%Y")) %>%
	arrange(EventAar_RD, desc(DeltFoedtDato_DW), DeltNavn_RD) %>%
  select(
    " "      = DeltYngstAeldst_DW,
    "&emsp;" = KlubLogo_DW,
    "Navn"   = DeltNavnBilletKat_DW,
    "Født"   = Født,
    EventAar_RD)

#' # Deltagere

# Deltagere ---------------------------------------------------------------

#' ## Foreløbige deltagere
#+ eval=F, warning=F, message=F

DataDeltFor_T <- Data_T %>%
  group_by(EventAar_RD, Billet_RD) %>%
  filter(grepl("Tilmeldt", BilletStatusSimpel_RD) & grepl("Ping Pong", BilletKat_RD)) %>%
  distinct(DeltID_RD, BilletStatusSimpel_RD, .keep_all = T) %>%
  arrange(EventAar_RD, BilletStatusSimpel_RD, BilletKat_RD, desc(DeltFoedtDato_DW), DeltNavn_RD) %>%
  group_by(EventAar_RD, Billet_RD, BilletStatusSimpel_RD) %>%
  mutate(RaekkeNr_DW = row_number()) %>%
  ungroup() %>%
  select(
    "Nr." = RaekkeNr_DW,
    "&emsp;" = KlubLogo_DW,
    "Navn" = DeltNavnBilletKat_DW,
    BilletStatusSimpel_RD,
    BilletDisciplin_RD,
    BilletRaekke_RD,
    BilletSpilFormat_RD,
    EventAar_RD)

#' ## Puljer
#+ eval=F, warning=F, message=F

DataDeltPuljer_T <- Data_T %>%
  filter(grepl("Tilmeldt", BilletStatusSimpel_RD) & grepl("Ping Pong", BilletKat_RD)) %>%
  arrange(EventAar_RD, Billet_RD, DeltSnakePuljeNr_DW, DeltSnakeSeedNr_DW) %>%
  select(
    "Nr." = DeltSnakeSeedNr_DW,
    "&emsp;" = KlubLogo_DW,
    "Navn"   = DeltNavnBilletKat_DW,
    "Rating" = DeltRating_DW,
    DeltSnakeSeedLagNr_DW,
    DeltSnakePuljeNr_DW,
    BilletDisciplin_RD,
    BilletRaekke_RD,
    BilletSpilFormat_RD,
    EventAar_RD)

#' ## Kun til festen inkl. afbud
#+ eval=F, warning=F, message=F

DataDeltAndet_T <- Data_T %>%
  filter(
    !grepl("Ping Pong", BilletKat_RD)
    & DeltBilletSalgNr_DW == 1 | grepl("Afbud", BilletStatusSimpel_RD)) %>%
  group_by(EventAar_RD) %>%
  distinct(DeltID_RD, BilletStatusSimpel_RD, .keep_all = T) %>%
  arrange(
    EventAar_RD,
    BilletStatusSimpel_RD,
    desc(DeltBilletSalgNr_DW),
    desc(DeltFoedtDato_DW),
    DeltID_RD) %>%
  group_by(EventAar_RD, BilletStatusSimpel_RD) %>%
  mutate(Nr. = row_number()) %>%
  ungroup() %>%
  select(
    Nr.,
    "&emsp;" = KlubLogo_DW,
    "Navn"   = DeltNavnBilletKat_DW,
    BilletStatusSimpel_RD,
    EventAar_RD)

#' # Resultater
# Resultater --------------------------------------------------------------

#' ## Resultater sidste DM
#+ eval=F, warning=F, message=F

DataResultSidsteEvent_T <- Data_T %>%
  filter(!is.na(DeltID_RD) & !is.na(DeltSlutspil_RD) & grepl("Tilmeldt", BilletStatusSimpel_RD)) %>%
  arrange(desc(EventAarNr_RD), BilletDisciplin_RD, BilletRaekke_RD, DeltSlutspil_RD, DeltPlac_RD) %>%
  select(
    "Placering" = DeltPlac_RD,
    "&emsp;" = KlubLogo_DW,
    "Navn" = DeltNavnKlub_DW,
    DeltSlutspil_RD,
    EventAarSted_RD,
    EventAarDato_DW,
    EventAarFarve1_RD,
    BilletDisciplin_RD,
    BilletRaekke_RD,
    EventAarSidst_DW)

#' ## DM-vindere statistik
#+ eval=F, warning=F, message=F

DataResult_T <- Data_T %>%
  filter(grepl("1", DeltPlac_RD) & grepl("A-slutspil", DeltSlutspil_RD) & grepl("Tilmeldt", BilletStatusSimpel_RD)) %>%
  filter(EventAarStartDatoTid_DW <= Sys.Date()) %>%
  add_row(EventAarStartDato_DW_Aar_DW = 2020, DeltNavnKlub_DW = "Aflyst pga. Covid-19") %>%
  arrange(desc(EventAarStartDato_DW_Aar_DW)) %>%
  select(
    "År"     = EventAarStartDato_DW_Aar_DW,
    "&emsp;" = KlubLogo_DW,
    "Navn"   = DeltNavnKlub_DW)

#' # Tabeller til dashboards
# Tabeller til dashboards -------------------------------------------------

#' ## Klubber
#+ eval=F, warning=F, message=F

DataDeltKlub_T <- Data_T %>%
  group_by(EventAar_RD) %>%
	filter(!is.na(DeltID_RD)) %>%
	add_count(Klub_RD) %>%
	distinct(DeltID_RD, .keep_all = T) %>%
	arrange(
	  EventAar_RD, BilletStatusSimpel_RD, KlubKat_DW, desc(n),
	  Klub_RD, desc(DeltBilletSalgNr_DW), BilletKat_RD, DeltNavn_RD) %>%
  ungroup() %>%
	select(
		" "    = KlubLogo_DW,
		"Navn" = DeltNavnBilletKat_DW,
		"Klub" = Klub_RD,
		BilletStatusSimpel_RD,
		EventAar_RD)

#' ## Deltagere fordelt på Danmarkskort
#+ eval=F, warning=F, message=F

DataDeltBy_T <- Data_T %>%
  group_by(EventAar_RD) %>%
  filter(!is.na(DeltID_RD)) %>%
  distinct(DeltID_RD, .keep_all = T) %>%
  mutate(KlubPostnrBy_DW = ifelse(
    grepl("Ingen klub|Udlandet", Klub_RD), Klub_RD, paste0(KlubPostnrBy_DW, ", ", KlubRegion_RD))) %>%
  arrange(
    EventAar_RD, BilletStatusSimpel_RD, KlubRegion_RD, desc(KlubPostnr_RD),
    desc(DeltBilletSalgNr_DW), BilletKat_RD, DeltNavn_RD) %>%
  ungroup() %>%
  select(
    " "        = KlubLogo_DW,
    "Navn"     = DeltNavnBilletKat_DW,
    "Lokation" = KlubPostnrBy_DW,
    BilletStatusSimpel_RD,
    EventAar_RD)

#' ## Aldersgruppe
#+ eval=F, warning=F, message=F

DataDeltAlderKat_T <- Data_T %>%
  group_by(EventAar_RD) %>%
	filter(!is.na(DeltID_RD)) %>%
	distinct(DeltID_RD, .keep_all = T) %>%
	mutate(DeltAlderKat_RD = paste(
		DeltAlderKat_RD, IkonFødt_V, "<br>", format(DeltFoedtDato_DW, "%d.%m.%Y"))) %>%
	arrange(EventAar_RD, BilletStatusSimpel_RD, desc(DeltFoedtDato_DW), DeltNavn_RD) %>%
  ungroup() %>%
	select(
		" "            = KlubLogo_DW,
		"Navn"         = DeltNavnBilletKat_DW,
		"Aldersgruppe" = DeltAlderKat_RD,
		BilletStatusSimpel_RD,
		EventAar_RD)

#' ## Køn
#+ eval=F, warning=F, message=F

DataDeltKoen_T <- Data_T %>%
  group_by(EventAar_RD) %>%
	filter(!is.na(DeltID_RD)) %>%
	distinct(DeltID_RD, .keep_all = T) %>%
	mutate(DeltKoen_RD_ikon = paste(DeltKoen_RD, DeltKoenIkon_RD)) %>%
	arrange(
	  EventAar_RD, BilletStatusSimpel_RD, DeltKoen_RD,
		desc(DeltBilletSalgNr_DW), BilletKat_RD, DeltNavnBilletKat_DW) %>%
  ungroup() %>%
	select(
		" "    = KlubLogo_DW,
		"Navn" = DeltNavnBilletKat_DW,
		"Køn"  = DeltKoen_RD_ikon,
		BilletStatusSimpel_RD,
		EventAar_RD)

#' ## Gentilmeldinger
#+ eval=F, warning=F, message=F

DataDeltGenTil_T <- Data_T %>%
	filter(!is.na(DeltID_RD)) %>%
	group_by(DeltID_RD) %>%
	slice(which.max(DeltGenNr_DW)) %>%
  group_by(EventAar_RD) %>%
	arrange(
	  EventAar_RD, BilletStatusSimpel_RD, DeltGen_DW,
		desc(DeltBilletSalgNr_DW), BilletKat_RD, DeltNavnBilletKat_DW) %>%
	mutate(DeltGen_DW = paste(
		DeltGenKat_DW, "<br>", DeltGen_DW, DeltGenKatIkon_RD)) %>%
  ungroup() %>%
	select(
		" "             = KlubLogo_DW,
		"Navn"          = DeltNavnBilletKat_DW,
		"Gentilmelding" = DeltGen_DW,
		BilletStatusSimpel_RD,
		EventAar_RD)

#' ## Tilmeldingstype
#+ eval=F, warning=F, message=F

DataDeltOrdreKat_T <- Data_T %>%
	filter(!is.na(DeltID_RD)) %>%
  group_by(EventAar_RD) %>%
	distinct(DeltID_RD, .keep_all = T) %>%
	arrange(EventAar_RD, BilletStatusSimpel_RD, OrdreDatoTid_RD, DeltNavn_RD) %>%
	mutate(OrdreTilKat_DW = paste(
	  OrdreFoersteDato_DW_DMAA_DW,
		"<br>",
		format(OrdreFoersteDatoTid_DW, "kl. %H:%M"), OrdreTilKatIkon_RD)) %>%
  ungroup() %>%
	select(
		" "         = KlubLogo_DW,
		"Navn"      = DeltNavnBilletKat_DW,
		"Ordredato" = OrdreTilKat_DW,
		BilletStatusSimpel_RD,
		EventAar_RD)

#' # Aktuel T/F
# Aktuel T/F --------------------------------------------------------------

DataEventAarAkt_T <- DataEventAar_T %>% filter(EventAar_RD == InputEventAarAkt_V)

#' ## BilletFix eventordre
#+ eval=F, warning=F, message=F

if(InputWebOrdreTF_V == T) {
  
  # Hentning af eventordre
  list5_eventordre <- content(GET(
    url = paste0("https://billetfix.dk/api/v3/events/", DataEventAarAkt_T$EventAarUUID_RD, "/orders"),
    config = c(
      add_headers(Authorization = paste("Token", DataEventAarAkt_T$EventAarToken_RD)),
      content_type("application/json"))))
  
  # Udtrækning af relevante listelementer indsættes i tabel
  tbl5_eventordre <- data.frame(
  	k_id = as.character(do.call(what = rbind, args = as.list(
      list5_eventordre$orders %>% sapply(., '[', "tickets") %>%
        sapply(., '[', seq(max(sapply(., length)))) %>%
        sapply(., '[', "purchase_uuid")))),
  	k_navn = as.character(do.call(what = rbind, args = as.list(
      list5_eventordre$orders %>% sapply(., '[', "tickets") %>%
        sapply(., '[', seq(max(sapply(., length)))) %>%
        sapply(., '[', "full_name")))),
  	BilletKat_RD = as.character(do.call(what = rbind, args = as.list(
      list5_eventordre$orders %>% sapply(., '[', "tickets") %>%
        sapply(., '[', seq(max(sapply(., length)))) %>%
        sapply(., '[', "ticket_type_name")))),
  	BilletPris_RD = as.character(do.call(what = rbind, args = as.list(
      list5_eventordre$orders %>% sapply(., '[', "tickets") %>%
        sapply(., '[', seq(max(sapply(., length)))) %>%
        sapply(., '[', "price"))))) %>%
    left_join(
      y = data.frame(
      	k_id = gsub("-", "", sapply(list5_eventordre$orders, `[[`, c("uuid"))),
        OrdreDatoTid_RD = sapply(list5_eventordre$orders, `[[`, c("date")),
        BilletStatusSimpel_RD    = sapply(list5_eventordre$orders, `[[`, c("state"))),
      na_matches = "never", by = "k_id") %>%
    mutate(across("OrdreDatoTid_RD", \(x) as_datetime(x) + hours(+2))) %>%
    mutate(across(c("BilletKat_RD", "BilletStatusSimpel_RD"), \(x) factor(x, ordered = T))) %>%
    mutate(across("BilletPris_RD", \(x) as.numeric(x))) %>%
    arrange(desc(OrdreDatoTid_RD)) %>%
    select(k_navn, OrdreDatoTid_RD, BilletKat_RD, BilletStatusSimpel_RD, BilletPris_RD)
  View(tbl5_eventordre)
  shell.exec(normalizePath(InputData_V))
  browseURL("https://pingpong.quarto.pub/dm/praemier-deltagere.html")
  browseURL(paste0("https://billetfix.dk/da/dashboard/", DataEventAarAkt_T$EventAarUUID_RD, "/orders"))
  browseURL("https://bordtennisportalen.dk/DBTU/Ranglister")
  cat(paste0(
    tbl5_eventordre %>%
      filter(grepl("PAID", BilletStatusSimpel_RD)) %>%
      summarise(label = paste(
        "💰 Omsætning kr.", format(sum(BilletPris_RD), big.mark = "."), "(PAID)")), "\n",
    tbl5_eventordre %>%
      filter(grepl("PAID", BilletStatusSimpel_RD)) %>%
      group_by(BilletKat_RD) %>%
      summarise(label = paste("kr.", format(sum(BilletPris_RD), big.mark = "."), "(PAID)")) %>%
      mutate(label = paste(BilletKat_RD, label)) %>%
      summarise(label = str_c(label, collapse = "\n")), "\n\n",
    tbl5_eventordre %>%
      count(BilletStatusSimpel_RD) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("🎫 ", BilletStatusSimpel_RD, " ", n, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = "\n")), "\n\n",
    tbl5_eventordre %>%
      count(BilletStatusSimpel_RD, BilletKat_RD) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(BilletKat_RD, " ", BilletStatusSimpel_RD, " ", n, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = "\n")), "\n\n"))
} else if (InputWebOrdreTF_V == F) {"InputWebOrdreTF_V = F"}

#' ## PDF til PNG for indbydelsesplakat
#+ eval=F, warning=F, message=F

if(InputPNGPlakatTF_V == T) {
  pdf_convert(
    pdf       = paste0(
      "filer/event/", egen_sti_fun(DataEventAarAkt_T$EventAar_RD), "/", egen_sti_fun(DataEventAarAkt_T$EventAar_RD), "-teaserplakat", ".pdf"),
    format = "png",
    filenames = paste0(
      "filer/event/", egen_sti_fun(DataEventAarAkt_T$EventAar_RD), "/", egen_sti_fun(DataEventAarAkt_T$EventAar_RD), "-teaserplakat", ".png"),
    verbose   = F,
    dpi       = 300)
  pdf_convert(
    pdf       = paste0(
      "filer/event/", egen_sti_fun(DataEventAarAkt_T$EventAar_RD), "/", egen_sti_fun(DataEventAarAkt_T$EventAar_RD), "-indbydelsesplakat", ".pdf"),
    format = "png",
    filenames = paste0(
      "filer/event/", egen_sti_fun(DataEventAarAkt_T$EventAar_RD), "/", egen_sti_fun(DataEventAarAkt_T$EventAar_RD), "-indbydelsesplakat", ".png"),
    verbose   = F,
    dpi       = 300)
  shell.exec(normalizePath(
    paste0("filer/event/", egen_sti_fun(DataEventAarAkt_T$EventAar_RD), "/", egen_sti_fun(DataEventAarAkt_T$EventAar_RD), "-teaserplakat", ".png")))
  shell.exec(normalizePath(
    paste0("filer/event/", egen_sti_fun(DataEventAarAkt_T$EventAar_RD), "/", egen_sti_fun(DataEventAarAkt_T$EventAar_RD), "-indbydelsesplakat", ".png")))
} else if (InputPNGPlakatTF_V == F) {"InputPNGPlakatTF_V = F"}

#' ## Webscraping af ratinglisten
#+ eval=F, warning=F, message=F

if(InputWebRatingTF_V == T) {
  tbl5_webscraping_rating <- data.frame()
  url1 <- ifelse(
    nrow(rbind(tbl5_webscraping_rating, data.frame(
      "DeltID_RD" = read_html(
        paste0("https://bordtennisportalen.dk/DBTU/Ranglister/Udskriv/?params=,59,4",
               DataEventAarAkt_T$EventAarRatingDato_DW_Aar_DW, ",",
               format(DataEventAarAkt_T$EventAarRatingDatoTid_RD, "%m/%d/%Y"),
               ",,,,True,,,,,", "0", ",,,0,,,,,")) %>% html_nodes(".playerid") %>% html_text(),
      stringsAsFactors = FALSE)) %>% filter(DeltID_RD != "Spiller-Id")) > 0,
    paste0("https://bordtennisportalen.dk/DBTU/Ranglister/Udskriv/?params=,59,4",
           DataEventAarAkt_T$EventAarRatingDato_DW_Aar_DW, ",",
           format(DataEventAarAkt_T$EventAarRatingDatoTid_RD, "%m/%d/%Y")),
    paste0("https://bordtennisportalen.dk/DBTU/Ranglister/Udskriv/?params=,59,4",
           DataEventAarAkt_T$EventAarRatingDato_DW_Aar_DW-1, ",",
           format(DataEventAarAkt_T$EventAarRatingDatoTid_RD, "%m/%d/%Y")))
  
  for (side in seq(from = 1, to = 50, by = 1)) {
  url2 <- paste0(url1, ",,,,True,,,,,", side-1, ",,,0,,,,,")
  
  tbl5_webscraping_rating <- rbind(tbl5_webscraping_rating, data.frame(
    "Plac"          = read_html(url2) %>% html_nodes(".rank")                        %>% html_text(),
    "DeltID_RD" = read_html(url2) %>% html_nodes(".playerid")                    %>% html_text(),
    "Navn"          = read_html(url2) %>% html_nodes(".name")                        %>% html_text(),
    "Rating"        = read_html(url2) %>% html_nodes(".name+ .pointsw")              %>% html_text(),
    "Plus_minus"    = read_html(url2) %>% html_nodes(".pointsw:nth-child(5)")        %>% html_text(),
    "Kampe"         = read_html(url2) %>% html_nodes(".pointsw~ .pointsw+ .pointsw") %>% html_text(),
    stringsAsFactors = FALSE)) %>% filter(DeltID_RD != "Spiller-Id") %>%
    mutate(across(c("Plac", "Rating", "Plus_minus", "Kampe"), \(x) as.numeric(x)))
  print(paste("Side", side))
  }
  
  tbl5_webscraping_rating <- tbl5_webscraping_rating %>%
    separate(Navn, into = c("Navn", "Klub"), sep = ",.", extra = "merge")
  tbl5_join_webscraping_rating <- Data_T %>%
    arrange(desc(OrdreDatoTid_RD)) %>%
    left_join(
      y = tbl5_webscraping_rating,
      na_matches = "never", by = "DeltID_RD") %>%
    select(EventAar_RD, Plac, DeltID_RD, Navn, Klub, Rating, Plus_minus, Kampe)
  
  write_xlsx(
    setNames(
      list(tbl5_webscraping_rating), DataEventAarAkt_T$EventAarRatingDato_DW_DMAA_DW),
    path = normalizePath("filer/generelt/rating.xlsx"))
  write_xlsx(
    setNames(
      list(tbl5_join_webscraping_rating), DataEventAarAkt_T$EventAarRatingDato_DW_DMAA_DW),
    path = normalizePath("filer/generelt/ping-pong-dm-rating.xlsx"))
  shell.exec(normalizePath("filer/generelt/ping-pong-dm-rating.xlsx"))
} else if(InputWebRatingTF_V == F) {"InputWebRatingTF_V = F"}

#' ## Data_T og DataEventAarAkt_T exporteret til TXT-fil
#+ eval=F, warning=F, message=F

if(InputDataTXT_V == T) {
  
  # Data_T
  writeLines(gsub("^\\s*[^[:space:]]+\\s", "", tail(capture.output(
    str(head(Data_T, 1), list.len = ncol(Data_T))), -1)),
    "filer/generelt/z-Data_T.txt")
  shell.exec(normalizePath("filer/generelt/z-Data_T.txt"))
  
  # DataKal_T
  writeLines(gsub("^\\s*[^[:space:]]+\\s", "", tail(capture.output(
    str(head(DataKal_T, 10), list.len = ncol(DataKal_T))), -1)),
    "filer/generelt/z-DataKal_T.txt")
  shell.exec(normalizePath("filer/generelt/z-DataKal_T.txt"))
  
  # DataEventAar_T
  writeLines(paste0("DataEventAar_T$", gsub("^\\s*[^[:space:]]+\\s", "", tail(capture.output(
    str(head(DataEventAar_T, 1), list.len = ncol(DataEventAar_T))), -1))),
    "filer/generelt/z-DataEventAar_T.txt")
  shell.exec(normalizePath("filer/generelt/z-DataEventAar_T.txt"))
  
  # DataBillet_T
  writeLines(paste0("DataBillet_T$", gsub("^\\s*[^[:space:]]+\\s", "", tail(capture.output(
    str(head(DataBillet_T, 1), list.len = ncol(DataBillet_T))), -1))),
    "filer/generelt/z-DataBillet_T.txt")
  shell.exec(normalizePath("filer/generelt/z-DataBillet_T.txt"))
  
} else if(InputDataTXT_V == F) {"InputDataTXT_V = F"}

#' ## Webscraping af BTEX Ping Pong bat
#+ eval=F, warning=F, message=F

DataWebBTEX_T <- data.frame()
link <- paste0("https://www.btex.dk/sanwei-wcpp-sandpapirsbat.html")

DataWebBTEX_T <- rbind(DataWebBTEX_T, data.frame(
  "produkt"      = read_html(link) %>% html_nodes(".name")                        %>% html_text(),
  "pris"         = read_html(link) %>% html_nodes(".price")                       %>% html_text(),
  "lagerstatus"  = read_html(link) %>% html_nodes(".title span")                  %>% html_text(),
  "levering"     = read_html(link) %>% html_nodes("#product_addtocart_form .txt") %>% html_text(),
  stringsAsFactors = FALSE)) %>% mutate(pris = trimws(pris))

#' ## Danmarkskort med lokation
#+ eval=F, warning=F, message=F

ggplot() +
  borders(regions = "Denmark", colour = "black", fill = "#76D7C4") +
  geom_point(aes(y = c(56.2), x = c(9.1)), size = 20, shape = 21, fill = "#943126") +
  theme_void()
# ggsave(filename = "filer/generelt/lokation.png")