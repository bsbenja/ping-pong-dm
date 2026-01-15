// Denne fil er den centrale skabelonfunktion for Typst

#let _extensions(

  // YAML - Defineres også i typst-show.typ
  title: none,
  date: none,
  lang: none,
  author-name: none,
  author-address: none,
  author-postal-code: none,
  author-city: none,
  hex-theme-1: none,
  hex-theme-2: none,
  hex-body-text: none,

  body
) = {

  // Angiver sideopsætning
  set page(
    width: 820pt,
    height: 360pt,
    margin: (y: 24*1.6pt, x: 90*1.2pt),
    background: image("billede/_baggrund.png", width: 100%),
    footer: none,
  )

  // Angiver skrifttype og orddeling
  set text(
    size: 16.6pt,
    font: "Arial",
    fill: rgb(hex-body-text),
    tracking: 0.04em,
    lang: lang,
    hyphenate: false,
  )

  // Angiver linjeafstand, paragrafafstand og margen
  set par(
    leading: 0.6em,
    spacing: 1em,
    justify: false,
  )

  // Angiver block
  set block(
    above: 0em,
    below: 0em,
    radius: 0em,
  )

  // Justering af indhold
  set align(right)
  
  // body
  body
}