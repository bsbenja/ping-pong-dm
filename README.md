# Ping Pong DM

**Hjemmesiden kan tilgås her**: https://pingpong.quarto.pub/dm

## Opsætning

Alle filer vedr. Ping Pong DM ligger i mappen "filer" fordelt i undermapperne "event", "generelt" og "klublogo".\
Kildefilen er "ping-pong-dm-input.R", som primært stammer fra "bagvedliggende-kode.R".\
De forskellige sider på hjemmesiden stammer fra ".qmd-filerne".

Dette Quarto-projekt publiceres som hjemmeside via følgende kommando i kommandoprompt:

``` bash
quarto publish quarto-pub --no-prompt --no-browser
```

### Navngivning

Navngivning af variable er baseret på Pascal Case efterfulgt af underscore og variabeltype. Eksempler på navngivning ses herunder:

-   PascalCase_V = Variable of Pascal Case.
-   PascalCase_T = Table of Pascal Case.
-   PascalCase_RD = Raw Data of Pascal Case.
-   PascalCase_DW = Data Wrangling of Pascal Case.