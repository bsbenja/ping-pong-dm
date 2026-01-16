// Denne fil kalder skabelonfunktionen for Typst, det vil sige oversætter Pandoc metadata til funktionsargumenter

// YAML - Defineres i typst-template.typ, typst-show.typ samt filter.lua
#show: _extensions.with(
 $if(title)$ title: [$title$], $endif$
 $if(date)$ date: "$date$", $endif$
 $if(lang)$ lang: "$lang$", $endif$
 $if(author_name)$ author_name: [$author_name$], $endif$
 $if(author_address)$ author_address: [$author_address$], $endif$
 $if(author_postal_code)$ author_postal_code: [$author_postal_code$], $endif$
 $if(author_city)$ author_city: [$author_city$"], $endif$
 $if(hex_theme_1)$ hex_theme_1: "$hex_theme_1$", $endif$
 $if(hex_theme_2)$ hex_theme_2: "$hex_theme_2$", $endif$
 $if(hex_body_text)$ hex_body_text: "$hex_body_text$", $endif$
 $if(fontsize)$ fontsize: $fontsize$, $endif$
)