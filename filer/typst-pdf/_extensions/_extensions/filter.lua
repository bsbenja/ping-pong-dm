-- I denne fil defineres Pandoc Lua Filters til manipulation af Markdown-indhold under konvertering via Pandoc.

function Meta(meta)
  if meta.date then
    local d = pandoc.utils.stringify(meta.date)
    meta.date = d:gsub("^%l", string.upper)
  end
  return meta
end