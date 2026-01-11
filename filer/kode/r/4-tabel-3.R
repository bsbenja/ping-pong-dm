# Billettype -----------------------------------------------------------------------------------------------------------

tbl3_BilletKat_T  <- tbl1_Ordre_T %>%
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

# Præmier --------------------------------------------------------------------------------------------------------------

tbl3_PraemiePenge_T <- tbl1_Ordre_T %>%
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
    Billettype_DW,
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
    EventAarPraemieSpons_RD,
    Billettype_DW,
    EventAar_RD)

# Yngste- og ældste deltager -------------------------------------------------------------------------------------------

tbl3_DeltYngstAeldst_T <- tbl1_Ordre_T %>%
  filter(
    !is.na(DeltID_RD) &
      grepl("Tilmeldt", OrdreStatusSimpelKat_RD) &
      grepl("Ping Pong", BilletKat_RD)) %>%
  group_by(EventAar_RD) %>%
  filter(DeltFoedtDato_DW == max(DeltFoedtDato_DW) | DeltFoedtDato_DW == min(DeltFoedtDato_DW)) %>%
  ungroup() %>%
  mutate(Født  = DeltFoedtDato_DW_DMAA_DW) %>%
	arrange(EventAar_RD, desc(DeltFoedtDato_DW), DeltNavn_RD) %>%
  distinct(
    " "      = DeltYngstAeldst_DW,
    "&emsp;" = KlubLogo_DW,
    "Navn"   = DeltNavnBilletKat_DW,
    "Født"   = Født,
    EventAar_RD)

# Foreløbige deltagere -------------------------------------------------------------------------------------------------

tbl3_DeltFor_T <- tbl1_Ordre_T %>%
  group_by(EventAar_RD, Billet_RD) %>%
  filter(grepl("Tilmeldt", OrdreStatusSimpelKat_RD) & grepl("Ping Pong", BilletKat_RD)) %>%
  distinct(DeltID_RD, OrdreStatusSimpelKat_RD, .keep_all = T) %>%
  arrange(EventAar_RD, OrdreStatusSimpelKat_RD, BilletKat_RD, desc(DeltFoedtDato_DW), DeltNavn_RD) %>%
  group_by(EventAar_RD, Billet_RD, OrdreStatusSimpelKat_RD) %>%
  mutate(RaekkeNr_DW = row_number()) %>%
  ungroup() %>%
  select(
    "#" = RaekkeNr_DW,
    "&emsp;" = KlubLogo_DW,
    "Navn" = DeltNavnBilletKat_DW,
    OrdreStatusSimpelKat_RD,
    Billettype_DW,
    EventAar_RD)

# Puljer ---------------------------------------------------------------------------------------------------------------

tbl3_DeltPuljer_T <- tbl1_Ordre_T %>%
  filter(grepl("Tilmeldt", OrdreStatusSimpelKat_RD) & grepl("Ping Pong", BilletKat_RD)) %>%
  arrange(EventAar_RD, Billet_RD, DeltSnakePuljeNr_DW, DeltSnakeSeedNr_DW) %>%
  select(
    "#" = DeltSnakeSeedNr_DW,
    "&emsp;" = KlubLogo_DW,
    "Navn" = DeltNavnBilletKat_DW,
    "Rating" = DeltRating_DW,
    DeltSnakeSeedLagNr_DW,
    DeltSnakePuljeNr_DW,
    Billettype_DW,
    EventAar_RD)

# Deltagere kun til festen inkl. afbud ---------------------------------------------------------------------------------

tbl3_DeltAndet_T <- tbl1_Ordre_T %>%
  filter(
    !grepl("Ping Pong", BilletKat_RD)
    & DeltBilletSalgNr_DW == 1 | !grepl("Tilmeldt|Aflyst", OrdreStatusSimpelKat_RD)) %>%
  group_by(EventAar_RD) %>%
  distinct(DeltID_RD, OrdreStatusSimpelKat_RD, .keep_all = T) %>%
  arrange(
    EventAar_RD,
    OrdreStatusSimpelKat_RD,
    desc(DeltBilletSalgNr_DW),
    desc(DeltFoedtDato_DW),
    DeltID_RD) %>%
  group_by(EventAar_RD, OrdreStatusSimpelKat_RD) %>%
  mutate(RaekkeNr_DW = row_number()) %>%
  ungroup() %>%
  select(
    "#" = RaekkeNr_DW,
    "&emsp;" = KlubLogo_DW,
    "Navn" = DeltNavnBilletKat_DW,
    OrdreStatusSimpelKat_RD,
    EventAar_RD)

# Deltagere fordelt på klubber ----------------------------------------------------------------------------------------------

tbl3_DeltKlub_T <- tbl1_Ordre_T %>%
  group_by(EventAar_RD) %>%
	filter(!is.na(DeltID_RD)) %>%
	add_count(Klub_RD) %>%
	distinct(DeltID_RD, .keep_all = T) %>%
	arrange(
	  EventAar_RD, OrdreStatusSimpelKat_RD, KlubKat_DW, desc(n),
	  Klub_RD, desc(DeltBilletSalgNr_DW), BilletKat_RD, DeltNavn_RD) %>%
  mutate(KlubKat_DW = paste0(KlubKat_DW, " ", KlubKatIkon_RD)) %>%
  group_by(EventAar_RD, OrdreStatusSimpelKat_RD) %>%
  mutate(RaekkeNr_DW = row_number()) %>%
  ungroup() %>%
	select(
    "#" = RaekkeNr_DW,
		"&emsp;" = KlubLogo_DW,
		"Navn" = DeltNavnBilletKat_DW,
		"Klubtype" = KlubKat_DW,
		OrdreStatusSimpelKat_RD,
    Billettype_DW,
		EventAar_RD)

# Deltagere fordelt på Danmarkskort ------------------------------------------------------------------------------------

tbl3_DeltBy_T <- tbl1_Ordre_T %>%
  group_by(EventAar_RD) %>%
  filter(!is.na(DeltID_RD)) %>%
  distinct(DeltID_RD, .keep_all = T) %>%
  arrange(
    OrdreStatusSimpelKat_RD, KlubRegion_RD, desc(KlubPostnr_RD),
    desc(DeltBilletSalgNr_DW), BilletKat_RD, DeltNavn_RD) %>%
  mutate(KlubPostnrBy_DW = paste0(ifelse(grepl("Ingen klub|Udlandet", Klub_RD),
      paste0(KlubKat_DW, " ", KlubKatIkon_RD),
      paste0(KlubRegion_RD, "<br>", KlubPostnrBy_DW, " ", KlubKatIkon_RD)))) %>%
  group_by(EventAar_RD, OrdreStatusSimpelKat_RD) %>%
  mutate(RaekkeNr_DW = row_number()) %>%
  ungroup() %>%
  select(
    "#" = RaekkeNr_DW,
    "&emsp;" = KlubLogo_DW,
    "Navn" = DeltNavnBilletKat_DW,
    "Region" = KlubPostnrBy_DW,
    OrdreStatusSimpelKat_RD,
    Billettype_DW,
    EventAar_RD)

# Deltagere fordelt på aldersgruppe ------------------------------------------------------------------------------------

tbl3_DeltAlderKat_T <- tbl1_Ordre_T %>%
  group_by(EventAar_RD) %>%
	filter(!is.na(DeltID_RD)) %>%
	distinct(DeltID_RD, .keep_all = T) %>%
	arrange(OrdreStatusSimpelKat_RD, desc(DeltFoedtDato_DW), DeltNavn_RD) %>%
  mutate(DeltAlderKat_RD = paste0(
    DeltFoedtDato_DW_DMAA_DW, "<br>", DeltAlderKat_RD, " ", IkonFødt_V)) %>%
  group_by(EventAar_RD, OrdreStatusSimpelKat_RD) %>%
  mutate(RaekkeNr_DW = row_number()) %>%
  ungroup() %>%
	select(
    "#" = RaekkeNr_DW,
		"&emsp;" = KlubLogo_DW,
		"Navn" = DeltNavnBilletKat_DW,
		"Aldersgruppe" = DeltAlderKat_RD,
		OrdreStatusSimpelKat_RD,
    Billettype_DW,
		EventAar_RD)

# Deltagere fordelt på køn ---------------------------------------------------------------------------------------------

tbl3_DeltKoen_T <- tbl1_Ordre_T %>%
  group_by(EventAar_RD) %>%
	filter(!is.na(DeltID_RD)) %>%
	distinct(DeltID_RD, .keep_all = T) %>%
	arrange(
	  EventAar_RD, OrdreStatusSimpelKat_RD, DeltKoen_RD,
		desc(DeltBilletSalgNr_DW), BilletKat_RD, DeltNavnBilletKat_DW) %>%
  mutate(DeltKoen_RD_ikon = paste0(DeltKoen_RD, " ", DeltKoenIkon_RD)) %>%
  group_by(EventAar_RD, OrdreStatusSimpelKat_RD) %>%
  mutate(RaekkeNr_DW = row_number()) %>%
  ungroup() %>%
	select(
    "#" = RaekkeNr_DW,
		"&emsp;" = KlubLogo_DW,
		"Navn" = DeltNavnBilletKat_DW,
		"Køn" = DeltKoen_RD_ikon,
		OrdreStatusSimpelKat_RD,
    Billettype_DW,
		EventAar_RD)

# Deltagere fordelt på gentilmeldinger ---------------------------------------------------------------------------------

tbl3_DeltGenTil_T <- tbl1_Ordre_T %>%
  group_by(EventAar_RD) %>%
	filter(!is.na(DeltID_RD)) %>%
  distinct(DeltID_RD, .keep_all = T) %>%
	arrange(
	  EventAar_RD, OrdreStatusSimpelKat_RD, DeltGen_DW,
		desc(DeltBilletSalgNr_DW), BilletKat_RD, DeltNavnBilletKat_DW) %>%
	mutate(DeltGen_DW = paste0(DeltGenKat_DW, "<br>", DeltGen_DW, " ", DeltGenKatIkon_RD)) %>%
  group_by(EventAar_RD, OrdreStatusSimpelKat_RD) %>%
  mutate(RaekkeNr_DW = row_number()) %>%
  ungroup() %>%
	select(
    "#" = RaekkeNr_DW,
		"&emsp;" = KlubLogo_DW,
		"Navn" = DeltNavnBilletKat_DW,
		"Gentilmelding" = DeltGen_DW,
		OrdreStatusSimpelKat_RD,
    Billettype_DW,
		EventAar_RD)

# Deltagere fordelt på tilmeldingstype ---------------------------------------------------------------------------------

tbl3_DeltOrdreKat_T <- tbl1_Ordre_T %>%
  group_by(EventAar_RD) %>%
	filter(!is.na(DeltID_RD)) %>%
	distinct(DeltID_RD, .keep_all = T) %>%
	arrange(EventAar_RD, OrdreStatusSimpelKat_RD, OrdreDatoTid_RD, DeltNavn_RD) %>%
	mutate(OrdreKat_DW = paste0(
	  OrdreFoersteDato_DW_DMAA_DW, "<br>",
    format(OrdreFoersteDatoTid_DW, "kl. %H:%M"), " ", OrdreKatIkon_RD)) %>%
  group_by(EventAar_RD, OrdreStatusSimpelKat_RD) %>%
  mutate(RaekkeNr_DW = row_number()) %>%
  ungroup() %>%
	select(
    "#" = RaekkeNr_DW,
		"&emsp;" = KlubLogo_DW,
		"Navn" = DeltNavnBilletKat_DW,
		"Ordredato" = OrdreKat_DW,
		OrdreStatusSimpelKat_RD,
    Billettype_DW,
		EventAar_RD)

# Resultater -----------------------------------------------------------------------------------------------------------

tbl3_Result_T <- tbl1_Ordre_T %>%
  filter((!is.na(DeltSlutspil_RD) & grepl("Tilmeldt", OrdreStatusSimpelKat_RD)) | grepl("Aflyst", OrdreStatusSimpelKat_RD)) %>%
  filter(EventAarStartDatoTid_DW <= Sys.Date()) %>%
  arrange(desc(EventAarNr_RD), BilletDisciplin_RD, BilletRaekke_RD, DeltSlutspil_RD, DeltPlac_RD) %>%
  select(
    "År" = EventAarStartDato_DW_Aar_DW,
    "#" = DeltPlac_RD,
    "&emsp;" = KlubLogo_DW,
    "Navn" = DeltNavnKlub_DW,
    DeltSlutspil_RD,
    DeltPlacNr_RD,
    OrdreStatusSimpelKat_RD,
    EventAarSidst_DW,
    Billettype_DW,
    EventAar_RD)

# Resultater webscraped ------------------------------------------------------------------------------------------------

tbl3_ResultWeb_T <- rbind(data.frame(), data.frame(
  "Spiller" = read_html("https://bordtennisdanmark.dk/statistik/ping-pong-dm")
  %>% html_nodes("div.elementor-text-editor.elementor-clearfix") %>% html_text(),
  stringsAsFactors = FALSE)) %>%
  mutate(Spiller = strsplit(Spiller, "(?<=.)(?=[0-9]{4}:)", perl = T)) %>%
  unnest(Spiller) %>%
  slice_tail(n = -1) %>%
  mutate(Spiller = gsub("\\\t*", "", Spiller)) %>%
  separate(Spiller, into = c("År", "Spiller"), sep = ":.") %>%
  separate(Spiller, into = c("Spiller", "Klub"), sep = ",.", fill = "right") %>%
  mutate(Klub = ifelse(is.na(Klub), "", paste0(", <i>", Klub, "</i>"))) %>%
  mutate(Spiller = paste0(Spiller, Klub)) %>%
  select(År, Spiller)