# BilletFix eventordre -------------------------------------------------------------------------------------------------

if(InputWebOrdreTF_V == T) {
  
  # Hentning af eventordre
  list5_eventordre <- content(GET(
    url = paste0("https://billetfix.dk/api/v3/events/", tbl2_EventAarAkt_T$EventAarUUID_RD, "/orders"),
    config = c(
      add_headers(Authorization = paste("Token", tbl2_EventAarAkt_T$EventAarToken_RD)),
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
        OrdreStatusSimpelKat_RD    = sapply(list5_eventordre$orders, `[[`, c("state"))),
      na_matches = "never", by = "k_id") %>%
    mutate(across("OrdreDatoTid_RD", \(x) as_datetime(x) + hours(+2))) %>%
    mutate(across(c("BilletKat_RD", "OrdreStatusSimpelKat_RD"), \(x) factor(x, ordered = T))) %>%
    mutate(across("BilletPris_RD", \(x) as.numeric(x))) %>%
    arrange(desc(OrdreDatoTid_RD)) %>%
    select(k_navn, OrdreDatoTid_RD, BilletKat_RD, OrdreStatusSimpelKat_RD, BilletPris_RD)
  View(tbl5_eventordre)
  shell.exec(normalizePath(InputData_V))
  browseURL("https://pingpong.quarto.pub/dm/raekke-sandpapir-aaben-single.html")
  browseURL(paste0("https://billetfix.dk/da/dashboard/", tbl2_EventAarAkt_T$EventAarUUID_RD, "/orders"))
  browseURL("https://bordtennisportalen.dk/DBTU/Ranglister")
  cat(paste0(
    tbl5_eventordre %>%
      filter(grepl("PAID", OrdreStatusSimpelKat_RD)) %>%
      summarise(label = paste(
        "💰 Omsætning kr.", format(sum(BilletPris_RD), big.mark = "."), "(PAID)")), "\n",
    tbl5_eventordre %>%
      filter(grepl("PAID", OrdreStatusSimpelKat_RD)) %>%
      group_by(BilletKat_RD) %>%
      summarise(label = paste("kr.", format(sum(BilletPris_RD), big.mark = "."), "(PAID)")) %>%
      mutate(label = paste(BilletKat_RD, label)) %>%
      summarise(label = str_c(label, collapse = "\n")), "\n\n",
    tbl5_eventordre %>%
      count(OrdreStatusSimpelKat_RD) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("🎫 ", OrdreStatusSimpelKat_RD, " ", n, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = "\n")), "\n\n",
    tbl5_eventordre %>%
      count(OrdreStatusSimpelKat_RD, BilletKat_RD) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(BilletKat_RD, " ", OrdreStatusSimpelKat_RD, " ", n, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = "\n")), "\n\n"))
} else if (InputWebOrdreTF_V == F) {"InputWebOrdreTF_V = F"}

# PDF til PNG for indbydelsesplakat ------------------------------------------------------------------------------------

if(InputPNGPlakatTF_V == T) {
  pdf_convert(
    pdf = paste0(
      "filer/event/", fun_egen_sti(tbl2_EventAarAkt_T$EventAar_RD), "/", fun_egen_sti(tbl2_EventAarAkt_T$EventAar_RD), "-teaserplakat", ".pdf"),
    format = "png",
    filenames = paste0(
      "filer/event/", fun_egen_sti(tbl2_EventAarAkt_T$EventAar_RD), "/", fun_egen_sti(tbl2_EventAarAkt_T$EventAar_RD), "-teaserplakat", ".png"),
    verbose = F,
    dpi = 300)
  pdf_convert(
    pdf = paste0(
      "filer/event/", fun_egen_sti(tbl2_EventAarAkt_T$EventAar_RD), "/", fun_egen_sti(tbl2_EventAarAkt_T$EventAar_RD), "-indbydelsesplakat", ".pdf"),
    format = "png",
    filenames = paste0(
      "filer/event/", fun_egen_sti(tbl2_EventAarAkt_T$EventAar_RD), "/", fun_egen_sti(tbl2_EventAarAkt_T$EventAar_RD), "-indbydelsesplakat", ".png"),
    verbose = F,
    dpi = 300)
  shell.exec(normalizePath(
    paste0("filer/event/", fun_egen_sti(tbl2_EventAarAkt_T$EventAar_RD), "/", fun_egen_sti(tbl2_EventAarAkt_T$EventAar_RD), "-teaserplakat", ".png")))
  shell.exec(normalizePath(
    paste0("filer/event/", fun_egen_sti(tbl2_EventAarAkt_T$EventAar_RD), "/", fun_egen_sti(tbl2_EventAarAkt_T$EventAar_RD), "-indbydelsesplakat", ".png")))
} else if (InputPNGPlakatTF_V == F) {"InputPNGPlakatTF_V = F"}

# Webscraping af ratingliste -------------------------------------------------------------------------------------------

if(InputWebRatingTF_V == T) {
  tbl5_webscraping_rating <- data.frame()
  url1 <- ifelse(
    nrow(rbind(tbl5_webscraping_rating, data.frame(
      "DeltID_RD" = read_html(
        paste0("https://bordtennisportalen.dk/DBTU/Ranglister/Udskriv/?params=,59,4",
               tbl2_EventAarAkt_T$EventAarRatingDato_RD_Aar_DW, ",",
               format(tbl2_EventAarAkt_T$EventAarRatingDato_RD, "%m/%d/%Y"),
               ",,,,True,,,,,", "0", ",,,0,,,,,")) %>% html_nodes(".playerid") %>% html_text(),
      stringsAsFactors = FALSE)) %>% filter(DeltID_RD != "Spiller-Id")) > 0,
    paste0("https://bordtennisportalen.dk/DBTU/Ranglister/Udskriv/?params=,59,4",
           tbl2_EventAarAkt_T$EventAarRatingDato_RD_Aar_DW, ",",
           format(tbl2_EventAarAkt_T$EventAarRatingDato_RD, "%m/%d/%Y")),
    paste0("https://bordtennisportalen.dk/DBTU/Ranglister/Udskriv/?params=,59,4",
           tbl2_EventAarAkt_T$EventAarRatingDato_RD_Aar_DW-1, ",",
           format(tbl2_EventAarAkt_T$EventAarRatingDato_RD, "%m/%d/%Y")))
  
  for (side in seq(from = 1, to = 50, by = 1)) {
  url2 <- paste0(url1, ",,,,True,,,,,", side-1, ",,,0,,,,,")
  
  tbl5_webscraping_rating <- rbind(tbl5_webscraping_rating, data.frame(
    "Plac"          = read_html(url2) %>% html_nodes(".rank")                        %>% html_text(),
    "DeltID_RD" = read_html(url2) %>% html_nodes(".playerid")                        %>% html_text(),
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
  tbl5_join_webscraping_rating <- tbl1_Ordre_T %>%
    arrange(desc(OrdreDatoTid_RD)) %>%
    left_join(
      y = tbl5_webscraping_rating,
      na_matches = "never", by = "DeltID_RD") %>%
    select(EventAar_RD, Plac, DeltID_RD, Navn, Klub, Rating, Plus_minus, Kampe)
  
  write_xlsx(
    setNames(
      list(tbl5_webscraping_rating), tbl2_EventAarAkt_T$EventAarRatingDato_RD_DMAA_DW),
    path = normalizePath("filer/generelt/rating.xlsx"))
  write_xlsx(
    setNames(
      list(tbl5_join_webscraping_rating), tbl2_EventAarAkt_T$EventAarRatingDato_RD_DMAA_DW),
    path = normalizePath("filer/generelt/ping-pong-dm-rating.xlsx"))
  shell.exec(normalizePath("filer/generelt/ping-pong-dm-rating.xlsx"))
} else if(InputWebRatingTF_V == F) {"InputWebRatingTF_V = F"}

# Webscraping af BTEX Ping Pong bat ------------------------------------------------------------------------------------

# DataWebBTEX_T <- data.frame()
# link <- paste0("https://www.btex.dk/sanwei-wcpp-sandpapirsbat.html")
#
# DataWebBTEX_T <- rbind(DataWebBTEX_T, data.frame(
#   "produkt"      = read_html(link) %>% html_nodes(".name")                        %>% html_text(),
#   "pris"         = read_html(link) %>% html_nodes(".price")                       %>% html_text(),
#   "lagerstatus"  = read_html(link) %>% html_nodes(".title span")                  %>% html_text(),
#   "levering"     = read_html(link) %>% html_nodes("#product_addtocart_form .txt") %>% html_text(),
#   stringsAsFactors = FALSE)) %>% mutate(pris = trimws(pris))

# Danmarkskort med lokation --------------------------------------------------------------------------------------------

ggplot() +
  annotation_borders(regions = "Denmark", colour = "black", fill = "#76D7C4") +
  geom_point(aes(y = c(56.2), x = c(9.1)), size = 20, shape = 21, fill = "#943126") +
  theme_void()
# ggsave(filename = "filer/generelt/lokation.png")