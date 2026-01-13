// Denne fil er den centrale skabelonfunktion for Typst

// 🔹 ANGIV HEX-farver
#let Farve_Tema1     = "#D9534F"
#let Farve_Tema2     = "#211D71"
#let Farve_Brødtekst = "#FFFFFF"

// 🔹 fun-fa-icon
#import "@preview/fontawesome:0.5.0": * /* Importerer Font Awesome ikoner */
#let fun-fa-icon(x) = {
  let radius = 0.5em
  box(inset: (right: 0.1em),
    place(bottom, float: true, dy: 0.16em, clearance: -0.28em,
      fa-stack(
        circle(fill: rgb(Farve_Tema1), radius: radius),
        (x, (fill: rgb(Farve_Brødtekst), size: radius, solid: true))
      )
    )
  )
}

// 🔹 fun-img-color
#let fun-img-color(img, color) = layout(bounds => {
  let size = measure(img, ..bounds)
  img
  place(top + left, block(..size, fill: color))
})

// =================================================================================================
// 🔷 _extensions-typst
// =================================================================================================

#let _extensions(

  // 🔹 YAML - Defineres også i typst-show.typ
  title: none,
  author: none,
  date: none,
  lang: none,

  body
) = {

  // 🔹 Angiver sideopsætning
  set page(
    width: 820pt,
    height: 360pt,
    margin: (y: 24*1.6pt, x: 90*1.6pt),
    footer: none,
    background: fun-img-color(image("billede/_baggrund.png", height: 140%), rgb(gray).transparentize(30%))
  )

  // 🔹 Angiver skrifttype og orddeling
  set text(
    size: 15.3pt,
    font: "Arial",
    fill: rgb(Farve_Brødtekst),
    tracking: 0.04em,
    lang: lang,
    hyphenate: false
  )

  // 🔹 Angiver linjeafstand, paragrafafstand og margen
  set par(
    leading: 0.6em,
    spacing: 0em,
    justify: false
  )

  // Angiver block
  set block(
    radius: 0.1em
  )
  
  body
}