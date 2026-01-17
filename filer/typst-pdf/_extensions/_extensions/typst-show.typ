// YAML - Defineres i typst-template.typ, typst-show.typ samt filter.lua
#show: _extensions.with(
  $if(title)$ title: "$title$".replace("\\", ""), $endif$
  $if(date)$ date: "$date$".replace("\\", ""), $endif$
  $if(mainfont)$ mainfont: "$mainfont$".replace("\\", ""), $endif$
  $if(fontsize)$ fontsize: $fontsize$, $endif$
  $if(lang)$ lang: "$lang$".replace("\\", ""), $endif$
  $if(var_organizer_name)$ var_organizer_name: "$var_organizer_name$".replace("\\", ""), $endif$
  $if(var_organizer_address)$ var_organizer_address: "$var_organizer_address$".replace("\\", ""), $endif$
  $if(var_organizer_postal_code)$ var_organizer_postal_code: "$var_organizer_postal_code$".replace("\\", ""), $endif$
  $if(var_organizer_city)$ var_organizer_city: "$var_organizer_city$".replace("\\", ""), $endif$
  $if(var_hex_theme_1)$ var_hex_theme_1: "$var_hex_theme_1$".replace("\\", ""), $endif$
  $if(var_hex_theme_2)$ var_hex_theme_2: "$var_hex_theme_2$".replace("\\", ""), $endif$
  $if(var_hex_body_text)$ var_hex_body_text: "$var_hex_body_text$".replace("\\", ""), $endif$
  $if(var_page_width)$ var_page_width: $var_page_width$, $endif$
  $if(var_page_height)$ var_page_height: $var_page_height$, $endif$
  $if(var_page_margin_x)$ var_page_margin_x: $var_page_margin_x$, $endif$
  $if(var_page_margin_y)$ var_page_margin_y: $var_page_margin_y$, $endif$
  $if(var_page_background)$ var_page_background: "$var_page_background$".replace("\\", ""), $endif$
)