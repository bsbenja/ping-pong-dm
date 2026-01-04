# Ping Pong DM

***Hjemmesiden kan tilgås her**: https://pingpong.quarto.pub/dm*

## Opsætning

Alle filer vedr. Ping Pong DM ligger i mappen "filer" fordelt i undermapperne "event", "generelt" og "klublogo".\
De forskellige sider på hjemmesiden stammer fra ".qmd-filerne".

Kildefilen er "_input.R", hvorfra følgende er muligt:

-   Hentning af eventordre (TRUE/FALSE).
-   Webscraping af rating (TRUE/FALSE).
-   Plakater fra PDF til PNG (TRUE/FALSE).
-   Tilmelding angives som (1) lukket, (2) teaser, (3) åben og (4) endelig.
-   Event og år angives.

Dette Quarto-projekt publiceres som hjemmeside via følgende kommando i kommandoprompt:

``` bash
quarto publish quarto-pub --no-prompt --no-browser
```

### Navngivning af variable

Navngivning af variable er baseret på Pascal Case efterfulgt af underscore og variabeltype.\
Eksempler på navngivning ses i nedenstående tabel:

| Navngivning   | Beskrivelse                   |
|:--------------|:------------------------------|
| PascalCase_V  | Variable of Pascal Case       |
| PascalCase_T  | Table of Pascal Case          |
| PascalCase_RD | Raw Data of Pascal Case       |
| PascalCase_DW | Data Wrangling of Pascal Case |