-- I denne fil defineres Pandoc Lua Filters til manipulation af Markdown-indhold under konvertering via Pandoc.

----------------------------------------------------------------------------------------------------
-- 🌐 Lokale Typst variable
----------------------------------------------------------------------------------------------------

function Pandoc(doc)
  table.insert(doc.blocks, 1, pandoc.RawBlock('typst', [[
    #let DocSpace = 0.4cm
    #let DocTime = 1.9cm
    #let DocGraphic = 0.5cm
  ]]))
  return doc
end

----------------------------------------------------------------------------------------------------
-- 1️⃣ Header Lua Filter
----------------------------------------------------------------------------------------------------

function Header(el)

  -- ------------------------------------------------
  -- 🔹 fa-icon
  -- ------------------------------------------------

  if el.attributes["image"] then
    el.content = {pandoc.RawInline('typst', table.concat({
      '#grid(columns: (DocGraphic, DocSpace, auto),',
      '  [#place(top + center, dy: -3.6pt, block(fill: rgb(Farve_Side), outset: 0.2cm, box(radius: 100%, clip: true, image("' .. el.attributes["image"] .. '"))))],',
      '  [],',
      '  [' .. pandoc.utils.stringify(el.content) .. ']',
      ')',
    }))}
  elseif el.attributes["fa-icon"] then
    el.content = {pandoc.RawInline('typst', table.concat({
      '#grid(columns: (DocGraphic, DocSpace, auto),',
      '  [#place(top + center, fa-icon("' .. el.attributes["fa-icon"] .. '", fill: rgb(Farve_Tema), size: 10pt, solid: true))],',
      '  [],',
      '  [' .. pandoc.utils.stringify(el.content) .. ']',
      ')',
    }))}
  end
  return el
end

----------------------------------------------------------------------------------------------------
-- 2️⃣ Div Lua Filter
----------------------------------------------------------------------------------------------------

function Div(el)

  ------------------------------------------------
  -- 🔹 timeline
  ------------------------------------------------

  if el.classes:includes('timeline') then

    local layout = el.attributes["layout"] or "a" -- default to 'a'
    local blocks = pandoc.List()

    for _, block in ipairs(el.content) do
      if block.t == 'Div' and #block.classes == 0 then

        local content_block_above = (#block.content == 0) and '1.4cm' or '0.7cm'
        local content_DocSpace = (#block.content == 0) and '0cm' or '0.38cm'

        local text_a = block.attributes['text-a'] and '#text(fill: rgb(Farve_Tema))[*#smallcaps("' .. block.attributes['text-a'] .. '")*]' or ''
        local text_b = (text_a ~= '' and block.attributes['text-b']) and ' #text(fill: rgb(Farve_Tema).lighten(40%))[·] #text(fill: rgb(Farve_Tema).lighten(40%))[*#smallcaps("' .. block.attributes['text-b'] .. '")*]' or ''
        local text_c = (text_a ~= '' and block.attributes['text-c']) and ' #text(fill: rgb(Farve_Tema).lighten(40%))[·] #text(fill: rgb(Farve_Tema).lighten(40%))[#smallcaps("' .. block.attributes['text-c'] .. '")]' or ''
        local time = block.attributes["time"] and '#text(fill: rgb(Farve_Brødtekst))[*' .. block.attributes["time"] .. '*]' or ''
        
        local graphic_content = ''
        if block.attributes['image'] then
          graphic_content = 'place(top + center, dy: -3.6pt, block(fill: rgb(Farve_Side), outset: 0.2cm, box(radius: 100%, clip: true, image("' .. block.attributes['image'] .. '"))))'
        elseif block.attributes['fa-icon'] then
          graphic_content = 'place(top + center, dy: -3.8pt, block(fill: rgb(Farve_Side), outset: 0.2cm, fa-stack(circle(fill: rgb(Farve_Tema), radius: 7pt), ("' .. block.attributes['fa-icon'] .. '", (fill: rgb(Farve_Side), size: 7pt, solid: true)))))'
        else
          graphic_content = 'place(top + center, dy: 0.6pt, block(fill: rgb(Farve_Side), outset: 0.2cm, circle(radius: 0.1cm, fill: rgb(Farve_Tema))))'
        end

        table.insert(blocks, pandoc.RawBlock('typst', table.concat({
          '#{',
          '  if layout == "a" {',
          '    block(breakable: false, inset: (left: - (DocSpace + DocGraphic)), above: ' .. content_block_above .. ',',
          '      grid(columns: (auto, DocSpace, auto),',
          '        [#block(width: DocGraphic,' .. graphic_content .. ')],',
          '        [],',
          '        [#stack(dir: ttb, spacing: ' .. content_DocSpace .. ',',
                     ((text_a ~= '') or (text_b ~= '') or (text_c ~= '') or (time ~= '')) and ('grid(columns: (1fr, 0.5cm, auto), align: (left, auto, right), [#text(hyphenate: false)[#set par(justify: false);' .. text_a .. text_b .. text_c .. ']], [], [#text(hyphenate: false)[#set par(justify: false);' .. (time ~= '' and '| ' or '') .. time .. ']]),') or '',
          '          block()[', pandoc.write(pandoc.Pandoc(block.content), 'typst'), ']',
          '        )]',
          '      )',
          '    )',
          '  } else {',
          '    block(breakable: false, inset: (left: - ((DocSpace * 2) + DocTime + DocGraphic)), above: ' .. content_block_above .. ',',
          '      grid(columns: (auto, DocSpace, auto, DocSpace, auto),',
          '        [#block(width: DocTime, align(right)[#text(hyphenate: false)[#set par(justify: false);' .. time .. ']])],',
          '        [],',
          '        [#block(width: DocGraphic,' .. graphic_content .. ')],',
          '        [],',
          '        stack(dir: ttb, spacing: ' .. content_DocSpace .. ',',
                     ((text_a ~= '') or (text_b ~= '') or (text_c ~= '')) and 'block()[#text(hyphenate: false)[#set par(justify: false);' .. text_a .. text_b .. text_c .. ']],' or '',
          '          block()[', pandoc.write(pandoc.Pandoc(block.content), 'typst'), ']',
          '        )',
          '      )',
          '    )',
          '  }',
          '}',
        }, '\n')))
      else
        table.insert(blocks, block)
      end
    end

    -- Add pre- and post-Typst blocks
    blocks:insert(1, pandoc.RawBlock('typst', [[
    #{
      let layout = "]] .. layout .. [["
      
      block(
          inset: if layout == "a" {
            (left: (DocSpace + DocGraphic))
          } else {
            (left: ((DocSpace * 2) + DocTime + DocGraphic))
          },
          outset: if layout == "a" {
            (left: - DocGraphic / 2)
          } else {
            (left: - (DocSpace + DocTime + (DocGraphic / 2)))
          },
          above: 16pt,
          stroke: (left: (paint: rgb(Farve_Tema).lighten(40%), thickness: 1pt)))[
    ]]))

    blocks:insert(pandoc.RawBlock('typst', ']}\n'))
    return blocks
  end
end

----------------------------------------------------------------------------------------------------
-- 3️⃣ Meta Lua Filter
----------------------------------------------------------------------------------------------------

function Meta(meta)
  if meta.date then
    local d = pandoc.utils.stringify(meta.date)
    meta.date = d:gsub("^%l", string.upper)
  end
  return meta
end