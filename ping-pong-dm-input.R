#' ---
#' title: Input for Ping Pong DM
#' output: html_document
#' ---

#+ eval=F, warning=F, message=F
# Kopier/indsæt for offentliggørelse af hjemmeside via Terminal (Alt+Shift+M) =>
# quarto publish quarto-pub --no-prompt --no-browser

# Input
InputInfo1234_V    <- 4 # 1/2/3/4 for tilmelding hhv. lukket, teaser, åben og endelig
InputWebOrdreTF_V  <- F # T/F for hentning af eventordre
InputWebRatingTF_V <- F # T/F for webscraping af rating
InputPNGPlakatTF_V <- F # T/F for plakater fra PDF til PNG
InputDataTXT_V     <- F # T/F for data exporteres til TXT-filer
InputEventAarAkt_V <- "Ping Pong DM 2023" # Event og år
InputData_V        <- "filer/generelt/ping-pong-dm-tilmelding.xlsx" # Sti til Excel-fil

# Kildefiler
source(file = "_variables.R")
source(file = "bagvedliggende-kode.R")