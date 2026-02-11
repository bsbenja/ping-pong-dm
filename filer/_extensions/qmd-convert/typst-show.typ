// YAML - Defineres også i typst-template.typ
#show: qmd-convert.with(
  $if(title)$ title: "$title$".replace("\\", ""), $endif$
  $if(subtitle)$ subtitle: "$subtitle$".replace("\\", ""), $endif$
  $if(date)$ date: "$date$".replace("\\", ""), $endif$
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
  $if(var_text_font)$ var_text_font: "$var_text_font$".replace("\\", ""), $endif$
  $if(var_text_size)$ var_text_size: $var_text_size$, $endif$
)