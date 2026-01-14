// Denne fil kalder skabelonfunktionen for Typst, det vil sige oversætter Pandoc metadata til funktionsargumenter

// YAML - Defineres også i typst-template.typ
#show: _extensions.with(
  $if(title)$ title: [$title$], $endif$
  $if(date)$  date:  "$date$",  $endif$
  $if(lang)$  lang:  "$lang$",  $endif$
  $if(author_name)$ author_name: [$author_name$], $endif$
  $if(author_address)$ author_address: [$author_address$], $endif$
  $if(author_postal_code)$  author_postal_code: [$author_postal_code$], $endif$
  $if(author_city)$  author_city: [$author_city$], $endif$
)