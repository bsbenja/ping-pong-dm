-- YAML - Defineres i typst-template.typ, typst-show.typ samt filter.lua
function Pandoc(doc)
  local utils = require 'pandoc.utils'
  table.insert(doc.blocks, 1, pandoc.RawBlock('typst', table.concat({
    '#let title = "' .. utils.stringify(doc.meta.title) .. '"',
    '#let date = "' .. utils.stringify(doc.meta.date) .. '"',
    '#let mainfont = "' .. utils.stringify(doc.meta.mainfont) .. '"',
    '#let fontsize = "' .. utils.stringify(doc.meta.fontsize) .. '"',
    '#let lang = "' .. utils.stringify(doc.meta.lang) .. '"',
    '#let var_organizer_name = "' .. utils.stringify(doc.meta.var_organizer_name) .. '"',
    '#let var_organizer_address = "' .. utils.stringify(doc.meta.var_organizer_address) .. '"',
    '#let var_organizer_postal_code = "' .. utils.stringify(doc.meta.var_organizer_postal_code) .. '"',
    '#let var_organizer_city = "' .. utils.stringify(doc.meta.var_organizer_city) .. '"',
    '#let var_hex_theme_1 = "' .. utils.stringify(doc.meta.var_hex_theme_1) .. '"',
    '#let var_hex_theme_2 = "' .. utils.stringify(doc.meta.var_hex_theme_2) .. '"',
    '#let var_hex_body_text = "' .. utils.stringify(doc.meta.var_hex_body_text) .. '"',
    '#let var_page_width = "' .. utils.stringify(doc.meta.var_page_width) .. '"',
    '#let var_page_height = "' .. utils.stringify(doc.meta.var_page_height) .. '"',
    '#let var_page_margin_x = "' .. utils.stringify(doc.meta.var_page_margin_x) .. '"',
    '#let var_page_margin_y = "' .. utils.stringify(doc.meta.var_page_margin_y) .. '"',
    '#let var_page_background = "' .. utils.stringify(doc.meta.var_page_background) .. '"',
  }, "\n")))
  return doc
end