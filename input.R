#' ---
#' title: Input for Ping Pong DM
#' output: html_document
#' ---

#+ eval=F, warning=F, message=F
# Dette Quarto-projekt publiceres som hjemmeside via følgende kommando i kommandoprompt:
# quarto publish quarto-pub --no-prompt --no-browser

# Input
InputWebOrdreTF_V  <- F # T/F for hentning af eventordre
InputWebRatingTF_V <- F # T/F for webscraping af rating
InputPNGPlakatTF_V <- F # T/F for plakater fra PDF til PNG
InputInfo1234_V    <- 4 # Tilmelding angives som (1) lukket, (2) teaser, (3) åben og (4) endelig
InputEventAarAkt_V <- "Ping Pong DM 2023" # Event og år
InputData_V        <- "filer/generelt/ping-pong-dm-tilmelding.xlsx" # Sti til Excel-fil

# Kildefiler
source(file = "variable.R")
source(file = "bagvedliggende-kode.R")