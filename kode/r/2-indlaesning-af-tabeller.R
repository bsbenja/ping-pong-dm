# Dim1_Kalender --------------------------------------------------------------------------------------------------------
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

# Importer Dim og Fact fra Excel ---------------------------------------------------------------------------------------
Fact_Ordre <- read_excel(path = InputData_V, sheet = "✍️ Fact_Ordre", skip = 2)
Dim1_OrdreStatus <- read_excel(path = InputData_V, sheet = "✍️ Dim1_OrdreStatus", skip = 2)
Dim1_Billet <- read_excel(path = InputData_V, sheet = "🎫 Dim1_Billet", skip = 2)
Dim2_EventAar <- read_excel(path = InputData_V, sheet = "🎫 Dim2_EventAar", skip = 2)
Dim3_Event <- read_excel(path = InputData_V, sheet = "🎫 Dim3_Event", skip = 2)
Dim2_BilletKat <- read_excel(path = InputData_V, sheet = "🎫 Dim2_BilletKat", skip = 2)
Dim2_BilletDisciplin <- read_excel(path = InputData_V, sheet = "🎫 Dim2_BilletDisciplin", skip = 2)
Dim2_BilletRække <- read_excel(path = InputData_V, sheet = "🎫 Dim2_BilletRække", skip = 2)
Dim2_BilletSpilformat <- read_excel(path = InputData_V, sheet = "🎫 Dim2_BilletSpilformat", skip = 2)
Dim1_OrdreFoersteTid <- read_excel(path = InputData_V, sheet = "🎫 Dim1_OrdreFoersteTid", skip = 2) %>%
  mutate(across("OrdreFoersteTidKatMin_RD", \(x) as.character(x))) %>%
  mutate(across("OrdreFoersteTidKatMaks_RD", \(x) as.character(x)))
Dim1_OrdreKat <- read_excel(path = InputData_V, sheet = "🎫 Dim1_OrdreKat", skip = 2)
Dim1_Klub <- read_excel(path = InputData_V, sheet = "🛖 Dim1_Klub", skip = 2)
Dim2_KlubLandsdel <- read_excel(path = InputData_V, sheet = "🛖 Dim2_KlubLandsdel", skip = 2)
Dim2_KlubRegion <- read_excel(path = InputData_V, sheet = "🛖 Dim2_KlubRegion", skip = 2)
Dim1_DeltKoen <- read_excel(path = InputData_V, sheet = "👤 Dim1_DeltKoen", skip = 2)
Dim1_DeltSlutspil <- read_excel(path = InputData_V, sheet = "💪 Dim1_DeltSlutspil", skip = 2)
Dim1_DeltPlacering <- read_excel(path = InputData_V, sheet = "💪 Dim1_DeltPlacering", skip = 2)
Dim1_DeltRating <- read_excel(path = InputData_V, sheet = "💪 Dim1_DeltRating", skip = 2)
Dim1_DeltAlderKat <- read_excel(path = InputData_V, sheet = "📅 Dim1_DeltAlderKat", skip = 2)
Dim1_DeltKat <- read_excel(path = InputData_V, sheet = "👤 Dim1_DeltKat", skip = 2)
Dim1_DeltGenKat <- read_excel(path = InputData_V, sheet = "🔃 Dim1_DeltGenKat", skip = 2)