// Denne fil er den centrale skabelonfunktion for Typst

// 🔹 ANGIV HEX-farver
#let Farve_Tema      = "#D9534F"
#let Farve_Brødtekst = "#FFFFFF"
#let Farve_Side      = "#211D71"

// 🔹 fun-fa-stack
#import "@preview/fontawesome:0.5.0": * /* Importerer Font Awesome ikoner */
#let fun-fa-stack(x) = {
  let radius = 10pt
  box(inset: (right: 6pt),
    place(bottom, float: true, dy: 3.2pt, clearance: -5.6pt,
      fa-stack(
        circle(fill: rgb(Farve_Tema), radius: radius),
        (x, (fill: rgb(Farve_Brødtekst), size: radius, solid: true))
      )
    )
  )
}

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
      margin: (y: 24pt, x: 90pt),
      footer: none,
      fill: rgb(Farve_Side).lighten(20%),
    )

  // 🔹 Angiver skrifttype og orddeling
  set text(fill: rgb(Farve_Brødtekst), font: "Arial", size: 19pt, lang: lang, hyphenate: false)

  // 🔹 Angiver linjeafstand, paragrafafstand og margen
  set par(leading: 0.6em, spacing: 1em, justify: false)

  // 🔹 Angiver centreret indhold
  set align(center)

  // 🔹 Angiver billeder
  set image(height: 30pt)
  
  // 🔹 Opsætning af dokumenter generelt
  [
    #text(fill: rgb(Farve_Brødtekst), size: 2em)[*#upper(title)*]
    #v(-2pt)
    #body
  ]
}