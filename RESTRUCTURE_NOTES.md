# Restructure notes

Executed on branch `restructure` per `restructure-plan.md`. Baseline (pre-edit working
tree, including previously uncommitted changes to `.gitignore` and `paper/manuscript.qmd`)
is commit `e3b2326`; the restructure itself is in six checkpoint commits (block moves ·
Introduction + §2 · §3 + Methods · Results · Discussion + Conclusions · mechanical sweep).

## 1. Per-section word counts (prose only; code chunks and YAML excluded)

| Section | Before | After |
|---|---|---|
| Introduction | 1385 | 738 |
| The Quina system (new §2) | — | 495 |
| Regional geological setting and raw material sources (§3; was "Regional Geological and Palaeoenvironmental Setting") | 1203 | 585 |
| Materials and methods (§4) | 1723 | 1766 |
| Results (§5) | 3212 | 3157 |
| Discussion (§6) | 1614 | 2073 |
| Conclusions (§7) | 413 | 456 |
| Total (these sections) | 9550 | 9270 |

The §3 loss and Discussion gain are the two relocated palaeoenvironment paragraphs; the
Methods gain is the relocated age-control sentence plus the PERMANOVA first-use expansion.

## 2. Newly authored and adapted sentences (verbatim)

Wholly or substantially new (≈12, within the plan's budget):

1. Intro P1 opening gesture (compresses deleted P1, keeps its seven citations, fuses with
   old P5's opener): "In a period whose lithic variability has been read in turn as
   cultural tradition, site activity, change through time, raw material and degree of
   reduction [@bordesMousterianCulturesFrance1961; @bordes1950; @binfordbinford1966;
   @mellars1969; @geneste1985; @kuhn1995; @dibble1987interpretation], whether East Asia
   has a Middle Palaeolithic at all has long been contested."
2. Intro P2: "The technology at issue here is defined not by a tool type but by a
   techno-economic system, the Quina system: thick flakes of markedly asymmetric
   cross-section, carrying a sharp edge on one margin and a thick back opposite; a
   working edge sustained through cyclical Quina retouch rather than merely produced;
   and the ramified reuse of the products of that maintenance, so that stone already in
   hand yielded fresh working edges as tools wore out."
3. Intro P2: "In the strict sense the system is documented in western and southern
   Europe within a narrow interval between MIS 4 and early MIS 3, whereas Quina retouch
   taken alone is far more widely distributed, reaching back in the Levant as far as
   MIS 11; the distinction between the two, and the criteria on which each is
   recognised, are set out in Section 2."
4. Intro P4 (from the plan's skeleton, anglicised to house spelling): "We ask, first,
   whether the Quina-bearing surface assemblages share the technological concept
   documented at Longtan, indistinguishable from the excavated material on the
   attributes that define the system while remaining distinct from other retouched
   tools, and therefore constitute a single regional technological entity." / "If they
   do, we ask, second, how that entity was organised across the landscape: what stone
   was selected against what was available, and whether the technology varies with
   position in the landscape." / "Why such an organisation arose, and what its
   resemblance to the European Quina implies, the surface record cannot test; we take
   these questions up in the Discussion."
5. Intro P5: "This study identifies the first regional-scale Quina entity in East Asia,
   which we term the Quina Valley, characterises its raw material economy and landscape
   organisation, and proposes on that basis a hypothesis of convergence grounded in the
   structure of resources rather than in any resemblance of climate."
6. Intro P5 roadmap: "Sections 2 and 3 supply the background, the Quina system and the
   regional setting respectively; the first question is taken up in Section 5.1, the
   second in Section 5.2."
7. Results 5.2 opening transition: "The preceding section established the technological
   identity of the entity; this one asks how it was organised across the landscape."
8. Results 5.2 rewritten closing (also repairs the missing-verb typo): "Across the study
   area, then, Quina scrapers were produced in essentially the same way, regardless of
   the basin in which a locality occurs, its elevation above the channel, its distance
   from water, or the size of the assemblage it has yielded: whatever governed the
   placement of these localities, it did not condition how the technology was practised
   at them."
9. Discussion block 1 framing (replaces "Three lines of evidence converge on that
   conclusion."): "The primary evidence is the technological indistinguishability
   documented in Section 5.1; the raw material economy and the landscape invariance of
   Section 5.2 corroborate it."
10. Discussion agenda, new opening clause on the consolidated future-work sentence:
    "What would test the hypothesis is correspondingly clear: further excavation at
    Tianhuadong Cave [@hu2020Tianhuadong], and the recovery of primary-context
    assemblages in the Binchuan Basin, would supply the cores, the absolute ages and the
    faunal record that surface material cannot."
11. Conclusions: "To the first question the evidence returns a consistent answer." and
    "The uniformity of raw material selection and the absence of landscape structuring
    corroborate what the technology establishes."
12. Conclusions: "To the second question the answer lies in the raw material economy and
    its uniformity."

Adapted (existing sentences trimmed, fused, or given a back-reference; wording otherwise
retained):

- Intro P3 geography (old P6 compressed; citations released, all keys survive
  elsewhere): "The two basins lie in the Hengduan Mountains, in what is today the
  dry-hot valley zone of the Jinsha River drainage, under a monsoon-governed climatic
  regime unlike the mid-latitude, glacial-stage settings of western Europe in which the
  European Quina is documented; whether the two settings nonetheless resemble one
  another in the respects that bear on how technology was organised is a question we
  return to once the technological results are in place."
- §3 closing: "At Longtan, optically stimulated luminescence (OSL) dating places the
  *in situ* cultural layer at ~60–50 ka [@ruanQuinaLithicTechnology2025a]." (relocated,
  "its" → "the … at Longtan") and "Taken together, the regional and local records,
  presented in the Discussion where they bear on the question of convergence, indicate
  that…" (inserted pointer clause).
- Results 5.1 blank-selection opener: "Quina flaking yields thick flakes with markedly
  asymmetric cross-sections (Section 2), and the blanks used here carry the attributes
  that this strategy produces…" (two sentences fused; principle exposition deferred to §2).
- Results 5.1 edge-maintenance opener: "The convex-to-concave rhythm by which a Quina
  edge is sustained (Section 2) is clearly expressed in this assemblage."
- Results 5.1 ramification: "Resharpening flakes carry cutting edges of their own and
  were therefore usable as struck (Section 2); some Quina scrapers additionally bear…"
  (general use-wear statement reduced to the back-reference).
- Discussion block 1: "With no independent age control (Section 4.1), what their
  recurrence establishes is distributional rather than chronological."
- Discussion local setting: "Longtan provides the local counterpart to this regional
  picture, since it preserves both a robust chronology (Section 3) and a record of
  vegetation change." and "Dry-hot valleys of the Jinsha drainage today carry the
  strongly seasonal, rain-shadowed climate described in Section 3, in which surface
  water is concentrated onto the valley floors…" (climatic re-explanation trimmed to the
  structural facts, citations retained).
- Discussion block 4: "Two limitations qualify these results." (was "Four…").
- Conclusions P1: "…and asked two questions of them: whether they share a technological
  concept with the excavated material from Longtan, and, if they do, how the entity they
  define was organised across the landscape."
- Conclusions P2: "It establishes that Longtan is no longer a single case: one
  technological concept recurs across two basins, three valleys and several terrace
  levels, in a recurrence that is distributional rather than chronological; but that
  finding does not depend on whether their affinity with the European Quina is granted."
  (two former sentences merged; distributional point reduced to a clause).
- Abstract, realigned to the two questions using only existing claims: "Here we ask,
  first, whether the Quina-bearing surface assemblages of the surrounding basins share
  the technological concept documented at Longtan, and, second, how the entity they
  define was organised across the landscape." / "Analysing 166 Quina scrapers and 56
  resharpening flakes from 27 surface localities across two basins, we find them
  statistically indistinguishable in technology from the excavated Longtan material:
  Longtan represents one expression of a broader regional practice, which we term the
  Quina Valley." / "Its raw material economy rests on a strong preference for trachyte,
  selected against local abundance for its cobble size and predictable fracture, and its
  technology shows no systematic variation across the landscape."

## 3. Relocation map as executed

- Intro P1 → deleted; one opening-gesture clause survives at the head of new P1,
  carrying all seven of P1's citations (they occur nowhere else, and the citation-set
  diff had to be empty).
- Intro P2 → §2 paragraph 1, verbatim.
- Intro P3 → deleted; its faunal-dominance sentence (with
  @castelNeandertalSubsistenceStrategies2017) inserted verbatim into the Discussion's
  European paragraph after "In Europe that problem is reasonably well understood."
- Intro P4 → §2 paragraphs 2–3, verbatim, split after "…climatic instability […]"; its
  final sentence ("Neither the system nor Quina retouch, however, had been securely
  documented in the East Asia.") relocated to intro P2, where it restores the antecedent
  of "It is against this background…". §2 paragraph 3 closes on the recurrent-solution
  hook as planned.
- Intro P5 → new P1 (East Asian problem), with its Longtan portion closing new P2 and
  its "More importantly…" portion opening new P3.
- Intro P6 → compressed to two sentences inside new P3 (above); full setting remains §3.
- Intro P7 → rewritten as new P4/P5 (above).
- §3 ¶3 (monsoon record) and ¶4 (Longtan counterpart) → Discussion block 2, in order,
  after the "setting…same structure" opening sentence, except: the age-control sentence
  → Methods 4.1, appended verbatim to the "Two consequences" paragraph; the Tianhuadong
  dating sentence → deleted, @hu2020Tianhuadong attached to "further excavation at
  Tianhuadong Cave" in the research-agenda paragraph; the OSL sentence → §3 closing.
  The transitional "What Longtan adds beyond a date is a local environmental record."
  was dropped in the merge (orphaned once the dating sentences left the paragraph).
- §3 ¶5 → adapted as the new §3 closing (with the OSL sentence).
- Discussion 802's mid-paragraph monsoon/pollen summary and its pollen qualification →
  deleted as duplicates of the relocated fuller text (all keys survive in that text).
- Methods reordered to 4.1 Survey and selection · 4.2 Techno-typological analysis ·
  4.3 Raw materials · 4.4 Landscape structure (the former in-4.2 landscape paragraph,
  now its own subsection) · 4.5 Reproducibility. The global "All statistical analyses
  were carried out in R…" paragraph was placed at the end of 4.4 so it remains the
  closing statement of the statistical methods.
- Results split: 5.1 = former "Characterising the Quina retouch products" (internal
  order unchanged) under "Quina technology of the surface assemblages and its
  consistency with Longtan"; 5.2 = former "Landscape distribution and raw material
  economy" (internal order unchanged) plus the "[Landscape structure.]" block from the
  end of former 4.2.2, including the tbl-distance-correlations chunk, under "Raw
  material economy and landscape organisation".
- Discussion block order: 1 (paras 1–4) · 2 (hinge → Europe+fauna → monsoon → Longtan
  pollen → modern counterparts → comparison → convergence vs diffusion → research
  agenda) · 3 (East Asian Middle Palaeolithic) · 4 (two remaining limitations).
- No code chunk was edited internally; every chunk moved as a block, and all chunk
  dependencies resolve to `_analysis.R` via the setup chunk, so no ordering constraint
  was broken (jitter/repel seeds are all explicit, so figure randomness is unaffected).

## 4. Supplementary numbering (old → new)

The supplementary numbers floats by position (crossref + `supplement-numbering.lua`),
grouped in four sections that its own text declares to "follow the order of the
Results"; Methods mentions were forward references under the old numbering too. The
renumbering therefore reorders the four sections to the new Results order
(techno-typological · technological consistency · raw material economy · landscape
structure), leaves each section's internal order untouched, and updates every
hand-written mention in the manuscript. The section-order paragraph in the
supplementary was updated to match.

Tables: S1→S7 (composition), S2→S8 (electivity), S3→S9 (raw-material PERMANOVA),
S4→S10 (clast size), S5→S1 (retouch products), S6→S2 (consistency PERMANOVA),
S7→S3 (PERMDISP), S8→S4 (univariate), S9→S5 (PCA loadings), S10→S6 (sensitivity),
S11→S11 (locality matrix), S12→S12 (landscape PERMANOVA).

Figures: S1→S5 (sampling points), S2→S1 (further scrapers), S3→S2 (categorical
attributes), S4→S3 (Q–Q), S5→S4 (sensitivity PCA), S6→S6, S7→S7, S8→S8 (unchanged).

## 5. Typo fixes (only in touched text, as instructed)

- "a latral thick back" → "a lateral thick back" (Results 5.1.1).
- "resharpeninhg" → "resharpening" (Results 5.1.1 ramification paragraph).
- "Consistency of this concerns" → "Consistency of this kind concerns" (Results 5.1.2).
- "largly treeless in which" — disappeared with the deletion of intro P3 (mandated).
- "Quina scraper production in essentially the same way" (missing verb) — repaired in
  the mandated rewrite of the 5.2 closing ("Quina scrapers were produced…").
- "…which is what the alternating plan-secant exploitation." — left unfixed; the
  missing word is ambiguous (produces? entails?), so a TODO comment was placed instead.

## 6. Other decisions the author should review

- PERMANOVA's spelled-out form and @anderson2001new moved with the acronym's first use,
  which the Methods reorder shifted from 4.3 to 4.2; 4.3 now uses the bare acronym.
  Without this the acronym would be used before it is defined.
- Old P4's system/retouch distinction is delivered in §2 across paragraphs 2–3 with the
  system's distribution in ¶2 and retouch-alone in ¶3: keeping to relocation-only meant
  the strict-sense definition and the European interval could not be separated without
  writing a new sentence.
- The baseline commit necessarily includes the pre-existing uncommitted edits to
  `.gitignore` and `paper/manuscript.qmd` that were in the working tree when work began.

## 7. TODOs left in the source

- `paper/manuscript.qmd`, Results 5.1.1 (after the *talons à pans* sentence):
  `<!-- TODO author: the sentence above is incomplete ("...which is what the alternating
  plan-secant exploitation [produces? entails?]"); the intended final word is ambiguous,
  so it is left unchanged. -->`
- Observation (pre-existing, outside the plan's scope, not edited): the Jacobs' D
  equation line in Methods 4.3 contains a duplicated, escaped copy of the same LaTeX
  (`$$D = …$$\$\$D = \\frac…\$\$`), which renders as stray text; and 4.1 has "reality
  densities" where "real densities" may be intended.

## 8. Render verification

- Baseline render (pre-edit) of both documents completed without errors; copies kept in
  the session scratchpad (`baseline_manuscript.docx/.txt`, `baseline_supplementary.docx/.txt`).
- Post-edit render of both documents completed and produced fresh
  `paper/manuscript.docx` and `paper/supplementary.docx`.
- Inline statistics spot-checked against the pre-edit render and found identical:
  median distance to channel ("a median of 612 m"), trachyte share of artefacts
  ("trachyte accounts for 95.8%"), retouch generations ("a mean of 2.76 retouch
  generations"), and the two-Quina-group PERMANOVA ("R² = 0.004, pseudo-F = 0.81,
  p = 1.000"). Nothing computational changed.
- Every crossref-resolved float number in the rendered supplementary was checked
  against the mapping in §4 (all twelve tables, all eight figures); each matches the
  hand-written mention now in the manuscript.
- The in-text citation key set was extracted before and after editing; the diff is
  empty.
