// Denne fil er den centrale skabelonfunktion for Typst

#let _extensions(

  // YAML - Defineres i typst-template.typ, typst-show.typ samt filter.lua
  title: none,
  date: none,
  mainfont: none,
  fontsize: none,
  lang: none,
  var_organizer: none,
  var_hex: none,
  var_page: none,

  body
) = {

  // Angiver sideopsætning
  set page(
    width: var_page.width,
    height: var_page.height,
    margin: (x: var_page.margin_x, y: var_page.margin_y),
    fill: if var_page.fill == "false" {none} else {rgb(var_page.fill)},
    background: if var_page.background == "false" {none} else {image(var_page.background, width: 100%)},
    footer: none,
  )

  // Angiver skrifttype og orddeling
  set text(
    size: fontsize,
    font: mainfont,
    fill: rgb(var_hex.body_text),
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