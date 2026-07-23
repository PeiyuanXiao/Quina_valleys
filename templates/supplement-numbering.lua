--- Close up supplementary cross-reference labels.
---
--- Quarto builds a cross-reference as the prefix, a separator and the number,
--- so the prefixes "Table S" and "Fig. S" set in supplementary.qmd come out as
--- "Table S 1" rather than "Table S1". This filter removes that one separator,
--- both in the captions and in the in-text references, by joining a lone "S"
--- to the number that follows it. The separator is a plain space in some
--- places and a non-breaking space in others, so both are handled, whether
--- they arrive as their own element or inside a single string.
---
--- It must run after Quarto's own crossref filter, so supplementary.qmd lists
--- it after the `quarto` sentinel in its `filters` option.

local NBSP = "\194\160"           -- U+00A0 as UTF-8 bytes

local function is_gap(el)
  return el and (el.t == "Space"
                 or (el.t == "Str" and (el.text == " " or el.text == NBSP)))
end

function Str(el)
  el.text = el.text:gsub("S" .. NBSP .. "(%d)", "S%1")
  return el
end

function Inlines(inlines)
  local out = pandoc.List()
  local i = 1
  while i <= #inlines do
    local a, b, c = inlines[i], inlines[i + 1], inlines[i + 2]
    if a and a.t == "Str" and a.text == "S" and is_gap(b)
       and c and c.t == "Str" and c.text:match("^%d") then
      out:insert(pandoc.Str("S" .. c.text))
      i = i + 3
    else
      out:insert(a)
      i = i + 1
    end
  end
  return out
end
