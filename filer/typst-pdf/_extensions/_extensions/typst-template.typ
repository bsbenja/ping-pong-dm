// Denne fil er den centrale skabelonfunktion for Typst

// 🔹 ANGIV HEX-farver
#let Farve_Tema      = "#FFFFFF"
#let Farve_Brødtekst = "#FFFFFF"
#let Farve_Side      = "#211D71"
#let Side_Faktor     = 0.12pt

// 🔹 Import
#import "@preview/fontawesome:0.5.0": * /* Importerer Font Awesome ikoner */

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
      width: 820*Side_Faktor,
      height: 360*Side_Faktor,
      margin: (y: 24*Side_Faktor, x: 90*Side_Faktor),
      footer: none,
      fill: rgb(Farve_Side).lighten(20%),
    )

  // 🔹 Angiver skrifttype og orddeling
  set text(fill: rgb(Farve_Brødtekst), font: "Arial", size: 2.5pt, lang: lang, hyphenate: false)

  // 🔹 Angiver linjeafstand, paragrafafstand og margen
  set par(leading: 0.6em, spacing: 1em, justify: false)

  // 🔹 Angiver centreret indhold
  set align(center)

  // 🔹 Angiver billeder
  set image(height: 4pt)
  
  // 🔹 Opsætning af dokumenter generelt
  [
    #text(fill: rgb(Farve_Brødtekst), size: 2em)[*#upper(title)*]
    #v(-2pt)
    #body
  ]
}