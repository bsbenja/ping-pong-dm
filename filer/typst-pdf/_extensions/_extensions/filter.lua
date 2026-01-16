local utils = require 'pandoc.utils'

function Pandoc(doc)
  local block = pandoc.RawBlock('typst', table.concat(
    {
      '#let title = "' .. utils.stringify(doc.meta.title) .. '"',
      '#let date = "' .. utils.stringify(doc.meta.date) .. '"',
      '#let author_name = "' .. utils.stringify(doc.meta.author_name) .. '"',
      '#let author_address = "' .. utils.stringify(doc.meta.author_address) .. '"',
      '#let author_postal_code = "' .. utils.stringify(doc.meta.author_postal_code) .. '"',
      '#let author_city = "' .. utils.stringify(doc.meta.author_city) .. '"',
      '#let hex_theme_1 = "' .. utils.stringify(doc.meta.hex_theme_1) .. '"',
      '#let hex_theme_2 = "' .. utils.stringify(doc.meta.hex_theme_2) .. '"',
      '#let hex_body_text = "' .. utils.stringify(doc.meta.hex_body_text) .. '"',
    }, "\n"))
  table.insert(doc.blocks, 1, block)
  return doc
end