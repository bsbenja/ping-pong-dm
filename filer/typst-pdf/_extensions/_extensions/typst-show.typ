// Denne fil kalder skabelonfunktionen for Typst, det vil sige oversætter Pandoc metadata til funktionsargumenter

// YAML - Defineres i typst-template.typ, typst-show.typ samt filter.lua
#show: _extensions.with(

  $if(title)$ title: "$title$".replace("\\", ""), $endif$
  $if(date)$ date: "$date$".replace("\\", ""), $endif$
  $if(mainfont)$ mainfont: "$mainfont$".replace("\\", ""), $endif$
  $if(fontsize)$ fontsize: $fontsize$, $endif$
  $if(lang)$ lang: "$lang$".replace("\\", ""), $endif$

  $if(var_organizer)$ var_organizer: (
    name: "$var_organizer.name$".replace("\\", ""),
    address: "$var_organizer.address$".replace("\\", ""),
    postal_code: "$var_organizer.postal_code$".replace("\\", ""),
    city: "$var_organizer.city$".replace("\\", ""),
  ), $endif$
  
  $if(var_hex)$ var_hex: (
    theme_1: "$var_hex.theme_1$".replace("\\", ""),
    theme_2: "$var_hex.theme_2$".replace("\\", ""),
    body_text: "$var_hex.body_text$".replace("\\", ""),
  ), $endif$

  $if(var_page)$ var_page: (
    width: $var_page.width$,
    height: $var_page.height$,
    margin_x: $var_page.margin_x$,
    margin_y: $var_page.margin_y$,
    background: "$var_page.background$".replace("\\", ""),
  ), $endif$

)