// Denne fil er den centrale skabelonfunktion for Typst

// ANGIV HEX-farver
#let Farve_Tema1     = "#ED1846"
#let Farve_Tema2     = "#211D71"
#let Farve_Brødtekst = "#FFFFFF"

// =================================================================================================
// _extensions-typst
// =================================================================================================

#let _extensions(

  // YAML - Defineres også i typst-show.typ
  title: none,
  author: none,
  date: none,
  lang: none,

  body
) = {

  // Angiver sideopsætning
  set page(
    width: 820pt,
    height: 360pt,
    margin: (y: 24*1.6pt, x: 90*1.6pt),
    footer: none,
    background: image("billede/_baggrund.png", width: 100%)
  )

  // Angiver skrifttype og orddeling
  set text(
    size: 15.5pt,
    font: "Arial",
    fill: rgb(Farve_Brødtekst),
    tracking: 0.04em,
    lang: lang,
    hyphenate: false
  )

  // Angiver linjeafstand, paragrafafstand og margen
  set par(
    leading: 0.6em,
    spacing: 1em,
    justify: false
  )

  // Angiver block
  set block(
    above: 0em,
    below: 0em,
    radius: 0em
  )

  // Justering af indhold
  set align(center)
  
  // body
  body
}