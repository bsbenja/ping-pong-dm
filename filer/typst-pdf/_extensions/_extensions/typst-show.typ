// Denne fil kalder skabelonfunktionen for Typst, det vil sige oversætter Pandoc metadata til funktionsargumenter

// YAML - Defineres også i typst-template.typ
#show: _extensions.with(
  $if(title)$  title:  [$title$],  $endif$
  $if(author)$ author: [$author$], $endif$
  $if(date)$   date:   "$date$",   $endif$
  $if(lang)$   lang:   "$lang$",   $endif$
)