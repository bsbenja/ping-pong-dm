// YAML - Defineres i typst-template.typ, typst-show.typ samt filter.lua
#let _extensions(
  title: none,
  date: none,
  lang: none,
  var_organizer_name: none,
  var_organizer_address: none,
  var_organizer_postal_code: none,
  var_organizer_city: none,
  var_hex_theme_1: none,
  var_hex_theme_2: none,
  var_hex_body_text: none,
  var_page_width: none,
  var_page_height: none,
  var_page_margin_x: none,
  var_page_margin_y: none,
  var_page_background: none,
  var_text_font: none,
  var_text_size: none,

  body
) = {

  // Angiver sideopsætning
  set page(
    width: var_page_width,
    height: var_page_height,
    margin: (x: var_page_margin_x, y: var_page_margin_y),
    fill: if not var_page_background.contains("billede") {rgb(var_page_background)},
    background: if var_page_background.contains("billede") {image(var_page_background, width: 100%)},
    footer: none,
  )

  // Angiver skrifttype og orddeling
  set text(
    size: var_text_size,
    font: var_text_font,
    fill: rgb(var_hex_body_text),
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