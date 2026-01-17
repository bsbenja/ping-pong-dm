
-- YAML - Defineres i typst-template.typ, typst-show.typ samt filter.lua
function Pandoc(doc)
  local utils = require 'pandoc.utils'
  table.insert(doc.blocks, 1, pandoc.RawBlock('typst', table.concat(
    {

      '#let title = "' .. utils.stringify(doc.meta.title) .. '"',
      '#let date = "' .. utils.stringify(doc.meta.date) .. '"',
      '#let mainfont = "' .. utils.stringify(doc.meta.mainfont) .. '"',
      '#let fontsize = "' .. utils.stringify(doc.meta.fontsize) .. '"',
      '#let lang = "' .. utils.stringify(doc.meta.lang) .. '"',
      
      '#let var_organizer = (',
        'name: "' .. utils.stringify(doc.meta.var_organizer.name) .. '",',
        'address: "' .. utils.stringify(doc.meta.var_organizer.address) .. '",',
        'postal_code: "' .. utils.stringify(doc.meta.var_organizer.postal_code) .. '",',
        'city: "' .. utils.stringify(doc.meta.var_organizer.city) .. '",',
      ')',

      '#let var_hex = (',
        'theme_1: "' .. utils.stringify(doc.meta.var_hex.theme_1) .. '",',
        'theme_2: "' .. utils.stringify(doc.meta.var_hex.theme_2) .. '",',
        'body_text: "' .. utils.stringify(doc.meta.var_hex.body_text) .. '",',
      ')',

      '#let var_page = (',
        'width: "' .. utils.stringify(doc.meta.var_page.width) .. '",',
        'height: "' .. utils.stringify(doc.meta.var_page.height) .. '",',
        'margin_x: "' .. utils.stringify(doc.meta.var_page.margin_x) .. '",',
        'margin_y: "' .. utils.stringify(doc.meta.var_page.margin_y) .. '",',
        'background: "' .. utils.stringify(doc.meta.var_page.background) .. '",',
      ')',

    }, "\n")))
  return doc
end