# Dette Quarto-projekt publiceres som hjemmeside via følgende kommando i kommandoprompt:
# quarto publish quarto-pub --no-prompt --no-browser

# Input
InputWebOrdreTF_V  <- F # T/F for hentning af eventordre
InputWebRatingTF_V <- F # T/F for webscraping af rating
InputPNGPlakatTF_V <- F # T/F for plakater fra PDF til PNG
InputInfo1234_V    <- 4 # Tilmelding angives som (1) lukket, (2) teaser, (3) åben og (4) endelig
InputEventAarAkt_V <- "Ping Pong DM 2025" # Event og år
InputData_V        <- "filer/generelt/ping-pong-dm-tilmelding.xlsx" # Sti til Excel-fil

# Kildefiler
source(file = "kode/r/_variables.R")
source(file = "kode/r/0-setup.R")
source(file = "kode/r/1-funktioner.R")
source(file = "kode/r/2-indlaesning-af-tabeller.R")
source(file = "kode/r/3-tabel-1-og-2.R")
source(file = "kode/r/4-tabel-3.R")
source(file = "kode/r/5-aktuel-input.R")