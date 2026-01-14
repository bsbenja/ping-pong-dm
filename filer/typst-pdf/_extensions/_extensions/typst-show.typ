// Denne fil kalder skabelonfunktionen for Typst, det vil sige oversætter Pandoc metadata til funktionsargumenter

// YAML - Defineres også i typst-template.typ
#show: _extensions.with(
  $if(title)$ title: [$title$], $endif$
  $if(date)$  date:  "$date$",  $endif$
  $if(lang)$  lang:  "$lang$",  $endif$
  $if(author-name)$ author-name: [$author-name$], $endif$
  $if(author-address)$ author-address: [$author-address$], $endif$
  $if(author-postal-code)$  author-postal-code: [$author-postal-code$], $endif$
  $if(author-city)$  author-city: [$author-city$], $endif$
)