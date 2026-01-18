function Pandoc(doc)
  local utils = require 'pandoc.utils'
  local typst_vars = {}
  
  for key, value in pairs(doc.meta) do
    table.insert(typst_vars, '#let ' .. key .. ' = "' .. utils.stringify(value) .. '"')
  end
  
  table.insert(doc.blocks, 1, pandoc.RawBlock('typst', table.concat(typst_vars, "\n")))
  
  return doc
end