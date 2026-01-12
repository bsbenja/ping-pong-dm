// Denne fil er den centrale skabelonfunktion for Typst

// 🔹 ANGIV HEX-farver
#let Farve_Tema1     = "#D9534F"
#let Farve_Tema2     = "#211D71"
#let Farve_Brødtekst = "#FFFFFF"

// 🔹 fun-fa-stack
#import "@preview/fontawesome:0.5.0": * /* Importerer Font Awesome ikoner */
#let fun-fa-stack(x) = {
  let radius = 0.5em
  box(inset: (right: 0.1em),
    place(bottom, float: true, dy: 0.16em, clearance: -0.28em,
      fa-stack(
        circle(fill: rgb(Farve_Tema1), radius: radius),
        (x, (fill: rgb(Farve_Tema2), size: radius, solid: true))
      )
    )
  )
}

// 🔹 fun-fa-stack
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
  background: fun-img-color(image("billede/_baggrund.png", height: 140%), rgb(gray).transparentize(25%))
  )

  // 🔹 Angiver skrifttype og orddeling
  set text(fill: rgb(Farve_Brødtekst), font: "Arial", size: 13.6pt, tracking: 0.04em, lang: lang, hyphenate: false)

  // 🔹 Angiver linjeafstand, paragrafafstand og margen
  set par(leading: 0.6em, spacing: 1em, justify: false)

  // 🔹 Angiver centreret indhold
  set align(left)

  // 🔹 Angiver billeder
  set image(height: 1.5em)
  
  body
}