// Denne fil er den centrale skabelonfunktion for Typst

// 🔹 ANGIV HEX-farver
#let Farve_Tema      = "#D9534F"
#let Farve_Brødtekst = "#FFFFFF"
#let Farve_Side      = "#211D71"

// 🔹 fun-fa-stack
#import "@preview/fontawesome:0.5.0": * /* Importerer Font Awesome ikoner */
#let fun-fa-stack(x) = {
  let radius = 0.5em
  box(inset: (right: 0.1em),
    place(bottom, float: true, dy: 0.16em, clearance: -0.28em,
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
  set page(width: 820pt, height: 360pt, margin: (y: 24pt, x: 90pt), footer: none, fill: rgb(Farve_Side).lighten(20%))

  // 🔹 Angiver skrifttype og orddeling
  set text(fill: rgb(Farve_Brødtekst), font: "Arial", size: 19.1pt, lang: lang, hyphenate: false)

  // 🔹 Angiver linjeafstand, paragrafafstand og margen
  set par(leading: 0.6em, spacing: 0.8em, justify: false)

  // 🔹 Angiver centreret indhold
  set align(center)

  // 🔹 Angiver billeder
  set image(height: 1.5em)
  
  // 🔹 Opsætning af dokumenter generelt
  [
    #text(size: 2em)[*#upper(title)*]
    #v(-1.2em)
    #text(size: 0.9em)[_Klassisk bordtennis på lige vilkår_]
    #v(0em)
    #body
  ]
}