function Pandoc(doc)
  local typst_vars = {}
  for key, value in pairs(doc.meta) do
    local str_value = require 'pandoc.utils'.stringify(value)
    if str_value:match("^%d+%.?%d*$")   or -- Starter med et tal
       str_value:match("^%d+%.?%d*pt$") or -- Starter med et tal efterfulgt af 'pt'
       str_value:match("^%d+%.?%d*mm$") or -- Starter med et tal efterfulgt af 'mm'
       str_value:match("^%d+%.?%d*cm$") or -- Starter med et tal efterfulgt af 'cm'
       str_value:match("^%d+%.?%d*in$") or -- Starter med et tal efterfulgt af 'in'
       str_value:match("^%d+%.?%d*em$") or -- Starter med et tal efterfulgt af 'em'
       str_value:match("^%d+%.?%d*%%$")    -- Starter med et tal efterfulgt af '%'
    then table.insert(typst_vars, '#let ' .. key .. ' = ' .. str_value)
    else table.insert(typst_vars, '#let ' .. key .. ' = "' .. str_value .. '"')
    end
  end
  table.insert(doc.blocks, 1, pandoc.RawBlock('typst', table.concat(typst_vars, "\n")))
  table.insert(doc.blocks, 1, pandoc.RawBlock('typst', [[
    #import "@preview/fontawesome:0.5.0": * /* Importerer Font Awesome ikoner */
  ]]))

  return doc
end