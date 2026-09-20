# Licenses

This is a research compendium, and its three kinds of content are released
under three different licenses, as is usual for compendia of this sort.

| Content | License |
|---|---|
| Text and figures | [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) |
| Code | [MIT](https://opensource.org/licenses/MIT) |
| Data | [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/) |

Copyright 2026 Pei-Yuan Xiao, Qi-Jun Ruan, Davide Delpiano, Marco Peresani,
Zhen-Xiu Jia, Li-Jing Yang, Ming Zhang, Jian Sun, Ben Marwick and Hao Li.

A few files come from elsewhere and keep their own licenses; they are listed at
the end.

------------------------------------------------------------------------

## Text and figures — CC BY 4.0

Covers the prose of the manuscript, the supplementary material and the
Supplementary Bayesian Report (`paper/manuscript.qmd`,
`paper/supplementary.qmd`, `paper/barg/barg_report.qmd` and the documents
rendered from them), every figure in `figures/` and `paper/barg/figures/`, and
this README.

You are free to share and adapt this material for any purpose, including
commercially, provided you give appropriate credit, link to the license, and
indicate if changes were made. The full license is at
<https://creativecommons.org/licenses/by/4.0/legalcode>. Credit is given by
citing the paper:

> Xiao, P.-Y., Ruan, Q.-J., Delpiano, D., Peresani, M., Jia, Z.-X., Yang, L.-J.,
> Zhang, M., Sun, J., Marwick, B., & Li, H. (in prep.). The Quina Landscape: A
> regional Middle Palaeolithic technological system on the southeastern margin
> of the Tibetan Plateau.

------------------------------------------------------------------------

## Code — MIT

Covers every `.R` file in `paper/`, `paper/barg/` and `paper/map/`, the
pipeline definition `_targets.R`, the code chunks of the three Quarto
documents, `Dockerfile`, `.binder/Dockerfile`, the GitHub Actions workflows,
and the compendium's own files under `templates/`
(`supplement-numbering.lua`, `template.docx`).

Copyright (c) 2026 Pei-Yuan Xiao and Ben Marwick

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

------------------------------------------------------------------------

## Data — CC0 1.0

Covers the four source spreadsheets in `data/`: the surface-collected Quina
scrapers and resharpening flakes, the excavated Longtan retouched tools, the
river-gravel clast survey and the locality table.

These measurement records are placed in the public domain under
[CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/legalcode), so no
permission is needed to use them for any purpose. CC0 waives the legal
requirement to give credit, but we ask, as a scholarly courtesy, that reuse
cite the paper above.

------------------------------------------------------------------------

## Third-party files

These are redistributed here for convenience and are **not** covered by the
licenses above:

| File | Origin | License |
|---|---|---|
| `templates/author-info-blocks.lua` | Albert Krewinkel | ISC-style permissive license, in the file header |
| `templates/pagebreak.lua` | Benct Philip Jonsson, Albert Krewinkel | ISC-style permissive license, in the file header |
| `templates/scholarly-metadata.lua` | Albert Krewinkel, Robert Winkler | ISC-style permissive license, in the file header |
| `templates/elsevier-harvard.csl` | Citation Style Language project | [CC BY-SA 3.0](https://creativecommons.org/licenses/by-sa/3.0/) |
| `renv/activate.R` | the renv package, Posit Software, PBC | MIT |
