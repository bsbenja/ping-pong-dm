# Krævet pakker --------------------------------------------------------------------------------------------------------

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

# Opsætning generelt ---------------------------------------------------------------------------------------------------

Sys.setlocale("LC_ALL", "da-DK.UTF-8")
format(Sys.Date(), "%A")
options(timeout = max(1000, getOption("timeout")))
options(OutDec= ",")
options(knitr.table.format = "html")
options(knitr.kable.NA = "")

# Opsætning ggplot2 ----------------------------------------------------------------------------------------------------

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

# Opsætning DT ---------------------------------------------------------------------------------------------------------

options(DT.options = list(
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