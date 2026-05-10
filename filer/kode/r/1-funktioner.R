# fun_egen_sti ----

fun_egen_sti <- function(x) {
  x <- gsub("&", "-og-", x)
  x <- tolower(x)
  x <- gsub("æ|ä", "ae", x)
  x <- gsub("ø|ö", "oe", x)
  x <- gsub("å", "aa", x)
  x <- gsub("\\(|\\)", "", x)
  x <- gsub(" |/|\\+|\\*", "-", x)
  paste0(x)
}