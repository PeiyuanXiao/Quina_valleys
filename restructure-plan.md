# Task: Restructure the Quina manuscript (.qmd)

You are editing a Quarto manuscript (English; R code chunks compute every inline
statistic; figures and tables are generated in-document; the supplementary lives in
`paper/supplementary.qmd` with hand-numbered Table S / Fig. S references — see the
HTML comment in the Results section). The restructuring plan below was fixed in a
prior review. Your job is to execute it by **relocating, trimming, and deduplicating
existing text**. Read the entire file first, including YAML and setup chunks, before
editing anything.

## Hard constraints

1. **Preserve the authorial voice exactly.** The manuscript's style: long periodic
   sentences built through subordinate clauses; colons introducing elaborations and
   semicolons chaining coordinate reasoning; thematic constructions ("What
   distinguishes it is...", "It is against this background that...", "What remains is
   fracture predictability"); measured concession and litotes ("not unexpected but",
   "Diffusion cannot be ruled out, however"); qualifications woven into the argument
   rather than appended as disclaimers; British spelling (analysed, organised,
   artefact, palaeo-); continuous prose with no bullet lists; underlined inline
   labels (`[...]{.underline}`) in Results. Any sentence you must write new imitates
   this voice. Do not "improve", modernise, or simplify sentences you merely relocate.
2. **No new substantive content.** No new citations, data, analyses, or interpretive
   claims. New text is limited to the connective tissue explicitly called for below
   (roadmap, transitions, reframed closing sentences) — aim for no more than ~12
   newly authored sentences in total, and log every one verbatim in the final report.
   Wherever possible, build transitions by adapting sentences that already exist.
3. **Deliver-once.** Each piece of material appears once, at its point of use;
   elsewhere by brief back-reference. The plan names the duplications to resolve.
4. **Code chunks move as blocks, never edited internally.** Do not change
   computations, aesthetics, labels, or chunk options. After every move, verify that
   every object a chunk uses is defined in a chunk that still precedes it (grep for
   where `sz`, `tc`, `q`, `disp_tests`, `ls_dist_cor`, `variable_labels`, etc. are
   created); if a dependency would break, preserve the relative order of the chunks
   involved rather than editing code.
5. **Ask, don't invent.** If a deletion would lose content that has no destination in
   this plan, or an instruction conflicts with what is actually in the file, stop and
   leave a `<!-- TODO author: ... -->` comment plus a note in the report instead of
   improvising.

## Target structure

1 Introduction (≈5 paragraphs) · 2 The Quina system (new, ≈3 paragraphs) ·
3 Regional geological setting and raw material sources (slimmed) · 4 Materials and
methods (reordered) · 5 Results — 5.1 Quina technology of the surface assemblages
and its consistency with Longtan; 5.2 Raw material economy and landscape
organization · 6 Discussion (four blocks) · 7 Conclusions. Heading wording may be
adjusted to the author's idiom but the scopes are fixed.

## Edit plan

### A. Introduction → five paragraphs (cut roughly one third)

Current paragraphs are identified by their opening words.

- P1 "The Middle Palaeolithic (~300-40 ka)..." — delete; the paper never returns to
  the Mousterian-variability debate. At most one clause survives as an opening gesture.
- P2 "The Quina Mousterian is one of the best-defined..." — relocate to §2 (its
  first paragraph).
- P3 "Quina technology has been interpreted as an adaptation to high mobility..." —
  delete here; its content already exists in the Discussion ("In Europe that problem
  is reasonably well understood...", "...seasonally relocated camps"). Before
  deleting, carry the one element unique to P3 into that Discussion paragraph: the
  faunal-dominance sentence with its citations (@delpechLenvironnementAnimalMousteriens1996,
  @delagnesShiftsNeandertalMobility2011, @delpiano2022,
  @castelNeandertalSubsistenceStrategies2017 — the last appears nowhere else and
  must not be lost).
- P4 "The distinction between the Quina system and Quina retouch alone..." —
  relocate to §2 (paragraphs 2–3), leaving a one-to-two-sentence trace in the new
  intro P2.
- P5 "Whether East Asia has a Middle Palaeolithic..." — becomes new P1, essentially
  as is; its Longtan portion feeds new P2.
- P6 "The Huangping and Binchuan basins lie in the Hengduan Mountains..." — compress
  to 2–3 sentences inside new P3; the full setting is §3. The foreshadowing clause
  ("...a question we return to once the technological results are in place") may
  survive in compressed form.
- P7 "The analysis proceeds in two steps..." — rewrite as new P4 and P5.

New paragraph scheme:
- **P1** The East Asian Middle Palaeolithic problem (from old P5).
- **P2** Minimal Quina: two-three sentences on what the system is (thick asymmetric
  blanks, cyclical Quina retouch, ramified reuse — a techno-economic system, not a
  tool type); one-two sentences on the system/retouch asymmetry (system: western and
  southern Europe, MIS 4–early MIS 3; retouch alone far wider, e.g. the Levant from
  MIS 11), full treatment deferred to §2; Longtan as the first complete system in
  East Asia.
- **P3** This study: surface collections in the two basins suggest a regional
  phenomenon; compressed geography from old P6.
- **P4** The two questions, condensed with their criteria built in, keeping old P7's
  conditional structure. Base the wording on this skeleton, adapting existing
  sentences where possible:
  > We ask, first, whether the Quina-bearing surface assemblages share the
  > technological concept documented at Longtan — indistinguishable from the
  > excavated material on the attributes that define the system, while remaining
  > distinct from other retouched tools — and therefore constitute a single regional
  > entity. If they do, we ask, second, how that entity was organized across the
  > landscape: what stone was selected against what was available, and whether the
  > technology varies with landscape position. Why such an organization arose, and
  > what its resemblance to the European Quina implies, the surface record cannot
  > test; we take these up in the Discussion.
- **P5** Contribution and roadmap: first regional-scale Quina entity in East Asia
  (the Quina Valley), characterization of its raw material economy and landscape
  organization, and a resource-structure convergence hypothesis proposed on that
  basis; one roadmap sentence naming §§2–3 as background and mapping the first
  question to §5.1 and the second to §5.2.

### B. New §2 "The Quina system"

Assemble from relocated old P2 and P4; do **not** import the European mobility
interpretation (that lives in the Discussion — §2 is what/when/where, not why).
Three paragraphs: (1) definition and the three dimensions with their observable
criteria — keep complete, since Results will back-reference it (see E); (2) the
system/retouch distinction; (3) spatiotemporal distribution, closing on old P4's
final sentence as the hook ("...a recurrent solution to a recurrent problem...").
Every citation from old P2/P4 must survive here.

### C. §3 (currently "Regional Geological and Palaeoenvironmental Setting")

Retitle (suggest "Regional geological setting and raw material sources"). Keep
paragraphs 1–2 and both figures — they must remain before Methods, which cites
@fig-geology.

Relocations out of this section:
- Paragraph 3 ("The regional monsoon record places...") and paragraph 4 ("Longtan
  provides the local counterpart...") → Discussion Block 2 (see F), except:
  - the sentence "The surface collections examined below have no independent age
    control, and the Longtan dates cannot be transferred to them..." → Methods §4.1,
    appended to the "Two consequences" paragraph as a further statement;
  - the Tianhuadong sentence → delete here; attach @hu2020Tianhuadong to the
    existing "Further excavation at Tianhuadong Cave" mention in the Discussion.
- Paragraph 5 ("Taken together...") → adapt as the new short closing of §3 (2–4
  sentences, reusing existing wording): Longtan OSL ~60–50 ka; occupation spanning
  the MIS 4/3 transition; regional aridity giving way to a strengthening monsoon;
  local vegetation opening; full treatment in the Discussion.

### D. Methods (§4) — reorder to mirror Results

New order: 4.1 Survey and selection of localities (+ the relocated age statement) ·
4.2 Techno-typological analysis of Quina retouch products (content unchanged, minus
the landscape paragraph) · 4.3 Survey and analysis of raw materials (unchanged) ·
4.4 Landscape structure: move the paragraph "To test whether technological variation
is systematically structured by the landscape..." out of 4.2 into its own short
subsection; it may back-reference the distance proxy introduced in 4.3 ·
4.5 Reproducibility and open materials (unchanged).

### E. Results (§5) — split

**5.1** = current 4.2 minus the landscape block, internal order unchanged
(Techno-typological features, then Technological consistency). Trim the principle
expositions that §2 now delivers:
- Blank-selection opener ("Quina flaking yields thick flakes... [@Bourguignon1997].")
  → one sentence with a back-reference to §2; keep everything from "The blanks used
  here carry the attributes..." onward.
- Edge-maintenance opener (through "...alternating across the life of the Quina
  scraper [@peresani2023; @lemoriniScrapersLifeHistory2016].") → one sentence;
  before trimming, verify the convex→concave rhythm is fully stated in §2 (it is,
  via old P2).
- Ramification: keep the data-anchored sentences; reduce the general use-wear
  statement to a clause, since §2 carries the fuller version.
If a trim would orphan a citation not present in §2, carry it into §2 rather than
delete it.

**5.2** "Raw material economy and landscape organization" = current 4.1 in its
current internal order (distribution and distances → gravel composition → artefact
composition and Jacobs' D → trachyte properties), followed by the landscape block
from the end of current 4.2.2 ("[Landscape structure.]{.underline}..." through
"...across the region.", including the tbl-distance-correlations chunk). Two
adjustments:
- one new transition sentence opening 5.2 (identity established in 5.1 → how the
  entity operated);
- rewrite the block's final paragraph, which currently argues in first-question
  terms ("...evidence for the repeated application of a shared technological
  concept"): reframe by trimming toward organization — production was uniform
  regardless of basin, height, distance and assemblage size, i.e. the technology was
  not conditioned by landscape position; the shared-concept inference moves to
  Discussion Block 1. Keep the distance-gradient paragraph and its two
  qualifications intact.

### F. Discussion (§6) — four blocks

- **Block 1 (first question).** Current paragraphs 1–4 in order. Adjust paragraph
  1's "Three lines of evidence converge" framing: technological indistinguishability
  (5.1) as the primary evidence, raw-material uniformity and landscape invariance
  (5.2) as corroboration — reassign existing sentences rather than writing new ones.
  In paragraph 3, trim "These collections have no independent age control, and none
  is claimed for them;" to a clause-level back-reference, keeping "what their
  recurrence establishes is distributional rather than chronological."
- **Block 2 (second question → explanation).** Order: the existing hinge ("Whether
  the same technology in two regions implies any connection...") → the European
  paragraph ("In Europe that problem is reasonably well understood...", augmented
  with the faunal clause carried from intro P3) → the local setting, merging three
  things in one pass: the relocated §3 palaeoenvironment paragraphs, the existing
  "The setting on the southeastern margin..." paragraph, and the modern dry-hot-
  valley material. Deduplicate as you merge: the pollen qualification (altitudinal
  integration, relative reading) exists in both the relocated text and the existing
  paragraph — keep one; the rain-shadow mechanism stays in §3, so trim the
  Discussion's climatic re-explanation to the structural facts (water concentrated
  on valley floors, altitudinal zonation; citations retained) with a back-reference
  to §3 → the comparison paragraph ("Both landscapes were open...") → convergence
  vs diffusion ("A shared problem of this kind...") → research agenda: consolidate
  "One term in the argument is untested..." with the limitations items on fauna and
  future work ("Further excavation at Tianhuadong Cave..., and the recovery of
  primary-context assemblages...", now carrying @hu2020Tianhuadong) into a positive
  closing statement of what would test the hypothesis.
- **Block 3.** The East Asian Middle Palaeolithic paragraph ("The Quina Valley also
  bears on...") — unchanged.
- **Block 4.** Limitations compressed to what is not already delivered elsewhere:
  cores and flakes not analysed (blank production indirect); no local record for the
  Binchuan Basin, Heqing treated as context. The age-control and fauna items are
  removed here (now in Methods/back-references and in the agenda respectively).

### G. Conclusions (§7) and abstract

Restructure Conclusions by question, reassigning existing sentences: first-question
answer (indistinguishability, with raw-material uniformity and landscape invariance
as corroboration; the Quina Valley; Longtan no longer a single case; a one-clause
echo of distributional-not-chronological); second-question answer (trachyte selected
against abundance; its size and fracture properties; uniform practice across
landscape positions); then the convergence hypothesis and the open question of
origin, largely as currently written. The "Three lines of evidence converge"
sentence must be rewritten to match the new logic.

Locate the abstract (YAML `abstract:` field or a separate file). If present, align
it to the two-question structure using only claims already in the manuscript; if
absent, note that in the report and skip.

## Mechanical sweep (after all moves)

1. Hand-written section references: grep for "Section" and update every occurrence
   to the new numbering (e.g. "Section 2" → "Section 3"; "Section 4.2.1" → its new
   address).
2. Supplementary numbering: determine the new first-mention order of Table S1–S12
   and Fig. S1–S8; renumber in `paper/supplementary.qmd` and update every in-text
   mention; include the old→new mapping in the report. (The HTML comment in the
   source flags this requirement — keep the comment.)
3. Citation integrity: extract the full set of @keys before and after editing; the
   diff must be empty. If any key disappears, restore it per the carry instructions
   above.
4. Render the manuscript and the supplementary; both must complete without new
   errors. Spot-check three inline statistics against a pre-edit render to confirm
   nothing computational changed.
5. Objective typos only, and only in text you touch (present in the current file:
   "largly treeless in which", "a latral thick back", "resharpeninhg", "Consistency
   of this concerns", and the incomplete sentence "...which is what the alternating
   plan-secant exploitation."). Fix where the intended word is unambiguous;
   otherwise leave a TODO. Log every fix. Do not restyle any sentence beyond this.

## Process and deliverables

Work on a git branch (`restructure`). Before editing: render the current version and
record per-section word counts and the citation-key list. Commit at checkpoints:
block moves · Introduction + §2 · §3 + Methods · Results · Discussion + Conclusions
· mechanical sweep. Finish with `RESTRUCTURE_NOTES.md` containing: per-section word
counts before/after; every newly authored sentence, quoted; the relocation map as
executed; the S-number mapping; typo fixes; and any TODOs left for the author.