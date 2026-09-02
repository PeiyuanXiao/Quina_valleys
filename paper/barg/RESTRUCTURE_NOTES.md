# Restructure notes — `barg_report.qmd`

What changed when the BARG report was reorganised from checklist order into a
reading narrative for an archaeological readership, and what was deliberately
left alone.

> **Superseded in part, 2026-09-02.** This is a record of one past change, kept
> as written. The machinery it refers to is gone: `paper/barg/fits/`,
> `fits_quick/`, `barg_fits.R` and the `BARG_QUICK` / `BARG_NOREFIT` switches
> were replaced by the `targets` pipeline in `_targets.R`, which is now built
> with `tar_make()` and has no reduced mode. Read the notes below as history,
> not as instructions.

No number, statistic or result changed. Every code chunk body and every
inline `` `r ... ` `` expression was moved verbatim; the 102 distinct inline
expressions in the file before the restructure are the same 102 afterwards,
none added and none dropped. The prose around them was rewritten.

The work is in two commits: a mechanical one that hoists the cross-chapter
computations and takes code out of the reading path, and the restructure
proper.

---

## 1. Section mapping, old to new

| Old | New |
|---|---|
| — | 1 Summary `{#sec-summary}` (new) |
| — | 2 BARG conformance `{#sec-barg}` (new) |
| — | 3 Reading guide `{#sec-reading-guide}` (new) |
| 1 Preamble | 4 The analysis `{#sec-analysis}` |
| 1.1 Why the analysis is Bayesian `{#sec-why-bayes}` | 4.1, unchanged position in sequence |
| 1.2 Goals of the analysis `{#sec-goals}` + 1.3 The two research questions | 4.2 Goals and the two research questions `{#sec-goals}` (merged) |
| 2 The model `{#sec-model}` | dissolved into 4 The analysis |
| 2.1 Data variables `{#sec-data}` | 4.3 Data `{#sec-data}` |
| 2.2 Likelihood and parameters `{#sec-likelihood}` | 4.4 Model and likelihoods `{#sec-likelihood}` |
| 2.3 Prior distributions `{#sec-priors}` | 4.5 Priors `{#sec-priors}`, condensed |
| — "The problem with the original prior" | 4.5.1 "Why the slope prior is scaled to each response" (rewritten, see §5) |
| — "The reference prior" | 4.5.2, kept |
| — "The prior in full" | → Appendix A |
| 2.4 Formal specification `{#sec-formal}` | → Appendix B |
| 2.5 Prior predictive check `{#sec-prior-pred}` | → Appendix C |
| — | 4.6 Fitting and model checks `{#sec-fitting}` (new, headline diagnostics only) |
| 3 Computation `{#sec-computation}` | → Appendix D, with `{#sec-software}`, `{#sec-convergence}`, `{#sec-resolution}` as its subsections |
| 3.4 Items not applicable `{#sec-2d}` | deleted; BARG table row 2.D |
| 4 The posterior distribution `{#sec-posterior}` | dissolved |
| 4.1 Posterior predictive check `{#sec-post-pred}` (+ `{#sec-ppc-stats}`) | → Appendix E |
| 4.2 Posterior summary `{#sec-posterior-summary}` | → Appendix F |
| 4.3 Locality variance `{#sec-icc}`, 4.3.1 correlations `{#sec-cor}` | → Appendix F, both promoted to `##` |
| 4.4 Item not applicable `{#sec-3c}` | deleted; BARG table row 3.C |
| 5 Decisions `{#sec-decisions}` | 5 Results `{#sec-results}` |
| 5.1 Why decisions, and under what rule `{#sec-decision-rule}` | 5.1 The decision rule and the ROPE `{#sec-decision-rule}` |
| 5.3 The ROPE and its justification `{#sec-rope}` | 5.1.1, merged under 5.1 as a subsection |
| 5.2 / 5.4 Items not applicable `{#sec-4b}` / `{#sec-4d}` | deleted; BARG table rows 4.B and 4.D |
| 5.5 Decisions, with the estimates `{#sec-verdicts}` | dissolved: `tbl-icc-decision` → 5.2, `tbl-coef-decision` → 5.3, the BARG 4.E callout → the Results chapter opening |
| — "What this design could have decided" `{#sec-min-rope}` | 5.3.1, kept, now directly under Q2 |
| 6 Sensitivity analysis `{#sec-sensitivity}` (and `{#sec-5a}`, `{#sec-5c}`, `{#sec-5e}`) | → Appendix G |
| 6.2 / 6.4 Items not applicable `{#sec-5b}` / `{#sec-5d}` | deleted; BARG table rows 5.B and 5.D |
| 7 Interpretation `{#sec-interpretation}` | dissolved into 5 Results |
| 7.1 Q1 `{#sec-q1}` | 5.2 |
| 7.2 Q2 `{#sec-q2}` | 5.3 |
| — | 5.4 How far the conclusions depend on the prior `{#sec-prior-dependence}` (new) |
| 7.3 What this study can and cannot resolve `{#sec-resolution-limits}` | 5.5 |
| 8 Reproducibility `{#sec-reproducibility}` | 6, unchanged |
| 9 References | last, unchanged |

Appendices are plain numbered `# Appendix X — ...` headings. Quarto's
`{.appendix}` handling does not combine well with `number-sections: true` and
`@sec-` cross-references here, so they render as sections 7–13 with "Appendix
A" and so on in the title. Prose that points at one names it both ways —
"Appendix A (@sec-priors-full)".

### Tables and figures that moved between chapters

| Object | From | To |
|---|---|---|
| `fig-main` | `sec-posterior-summary` | the opening of 5 Results |
| `prior-dom-readout` + its paragraph | `sec-posterior-summary` | 5.3 Q2 (it is about Panel B) |
| `tbl-icc-decision` | `sec-verdicts` | 5.2 Q1 |
| `tbl-coef-decision` | `sec-verdicts` | 5.3 Q2 |
| `tbl-rope-grid` | `sec-rope` | Appendix F, new subsection `{#sec-rope-grid}` |
| `tbl-prior-summary`, `prior-source`, `tbl-prior-default-rows`, `stan-lprior`, `tbl-default-priors` | `sec-priors` | Appendix A |
| the sensitivity summary paragraph (`n_changed`, `worst_shift`) | `sec-5e` | 5.4, so that Appendix G holds the tables and the main text the summary |
| `tbl-min-rope` | `sec-min-rope` | stays; its headline numbers are also quoted in the Summary |

Moving `fig-main` out of Appendix F is the one relocation the task
specification did not name explicitly. It is the figure that answers both
research questions, and leaving the paper's main figure in an appendix
defeated the point of the reorder. The section it came from keeps its ID and
its BARG 3.B callout.

---

## 2. Anchor IDs

**Added** (all new sections): `sec-summary`, `sec-barg`, `sec-reading-guide`,
`sec-analysis`, `sec-fitting`, `sec-results`, `sec-prior-dependence`,
`sec-priors-full`, `sec-brms-defaults`, `sec-rope-grid`.

**Retired.** No `@sec-` reference to any of these survives anywhere in the
repository; a grep for each returns nothing outside the scratch copies under
`analysis/` and `.Rproj.user/`, which are not part of the paper.

| Retired | Why | Was it referenced? |
|---|---|---|
| `sec-2d`, `sec-3c`, `sec-4b`, `sec-4d`, `sec-5b`, `sec-5d` | the six "Item not applicable" sections, now rows of the BARG conformance table | `@sec-3c` was cited once, from the `sec-4d` block; both are gone, and the repository-wide grep is clean |
| `sec-model` | chapter heading replaced by `sec-analysis` | no |
| `sec-posterior` | chapter dissolved into Appendix F | no |
| `sec-decisions` | chapter replaced by `sec-results` | yes — once, from `sec-why-bayes`. Repointed to `@sec-decision-rule`, which is where the three-way rule is actually stated |
| `sec-interpretation` | chapter dissolved into 5 Results | no |
| `sec-verdicts` | section dissolved; its two tables moved to `sec-q1` and `sec-q2`, its BARG 4.E callout to the Results opening | no |

**Preserved through a move:** `sec-why-bayes`, `sec-goals`, `sec-data`,
`sec-likelihood`, `sec-priors`, `sec-formal`, `sec-prior-pred`,
`sec-computation`, `sec-software`, `sec-convergence`, `sec-resolution`,
`sec-post-pred`, `sec-ppc-stats`, `sec-posterior-summary`, `sec-icc`,
`sec-cor`, `sec-decision-rule`, `sec-rope`, `sec-min-rope`, `sec-sensitivity`,
`sec-5a`, `sec-5c`, `sec-5e`, `sec-q1`, `sec-q2`, `sec-resolution-limits`,
`sec-reproducibility`.

**Chunk labels:** all 57 original labels survive with no duplicates. One was
added, `sens-verdict-calc`, when `tbl-sens-verdict` was split into a
computation chunk (hoisted to the front) and a display chunk (left in
Appendix G).

---

## 3. Passages deleted rather than moved

Only compliance boilerplate was deleted. Every limitation, caveat and
admission in the document is still there.

| Deleted | Reason |
|---|---|
| the `.na` block under `sec-goals` covering 3.C / 4.D / 5.D | folded into BARG table rows 3.C, 4.D and 5.D. Its closing observation, that where the goal is estimation the analysis is complete once the posterior and its sensitivity are reported, was kept as a sentence in `sec-goals` |
| `sec-2d` block | BARG table row 2.D, wording preserved |
| `sec-3c` block | BARG table row 3.C |
| `sec-4b` block | BARG table row 4.B, including the point that the ROPE curve reports the whole family of decisions instead of committing to one |
| `sec-4d` block | BARG table row 4.D |
| `sec-5b` block | BARG table row 5.B. Its substantive half — that no published prior exists for these seven measures in Quina assemblages, so the inapplicability is itself informative — is now a paragraph in `sec-priors` |
| `sec-5d` block | BARG table row 5.D, which keeps the extra sentence about Bayes factors being prior-dependent in a way posterior estimates are not |

The `.na` CSS class was used for two different jobs. Every compliance note in
that style is gone, and the one substantive block styled that way — "A trap in
the zero-one-inflated beta parameterisation", under `sec-likelihood` — is now
an ordinary `###` subsection in plain prose. With no `.na` divs left, the rule
was dropped from the inline stylesheet.

### Prose habits removed

Roughly forty paragraphs opened with a bolded assertion. All but a handful
became either a real `###` subheading (parameters of interest and nuisance
parameters; the zoib trap; the three posterior-predictive questions; why the
slope prior is scaled) or plain prose. The bold that survives is in table
cells and in the two `sprintf` outputs, where it is a label rather than a
sentence.

Every phrase on the "narration of frankness" list was removed and its caveat
kept, except the two noted in §5 below: `One honest qualification`,
`declared here rather than buried`, `it would be dishonest to construct one
here`, `the honest consequence`, `it would be wrong to leave that
unexplained`, `Two things must be said plainly about it`, `none of which is a
matter of taste`, `The order is not cosmetic`, `worth being precise about`,
`rather than a bare dismissal`.

The `not X, but Y` construction went from eighteen instances to six. Three of
the six are in text carried over verbatim — an inline `sprintf`, the BARG 3.A
callout, and a cell of the reference-prior table — leaving three in running
prose, at the points where the contrast does real work: the Summary's
"`r n_loc` localities, not `r n_spec` specimens", and the Reading guide's
distinction between a credible and a confidence interval.

Aphoristic closers were folded into the sentence before them ("The prior
passes.", "Partial pooling handles this correctly but cannot manufacture
information.", "That pattern is noise about zero, not explanation.").

Em-dash asides were cut from roughly twenty to nine. The ones kept are doing
appositive work, including the ICC gloss at first use, which the task
specification itself sets out in that form.

---

## 4. Text edited inside chunk bodies

Constraint: chunk bodies move verbatim. Workstream 7: every reference to
"the original specification" goes. Five caption strings sit in both, so they
were edited. No number, column name, variable or computation was touched, and
each edit is applied by exact string match in the assembly script so it is
auditable.

| Where | Old | New |
|---|---|---|
| `tbl-prior-rope` caption | "before and after the correction… Under the original prior… under the corrected prior" | "under a single unscaled normal(0, 1) and under the per-response scaling used here… One nominal specification ranges…" |
| `tbl-ppsd` caption | "under the original prior (S1)" | "under an unscaled normal(0, 1) slope prior (specification S1)" |
| `tbl-default-priors` caption | "The brms defaults the original specification inherited without reporting them" | "The brms defaults for every class other than b and sd. None of them is used by the reference fit; each was replaced by an explicit prior" |
| `tbl-default-priors` `cor` comment | "the 7 x 7 LKJ prior, never reported" | "the 7 x 7 LKJ prior" |
| the reference-prior markdown table, `sd` row | "carried over unchanged from the original specification" | "the one class not rescaled per response" |

The `.barg` callout on `sec-cor` said "These 21 parameters were estimated by
the original model and never reported". That is markdown, not code, and was
rewritten as a forward-looking statement.

---

## 5. Left alone deliberately

**Two delete-list phrases that live inside inline R.** Both are inside
`sprintf()` strings in `` `r ... ` `` expressions, which the task specifies
must move verbatim and which the verification step checks for byte equality.
Rewriting them would have failed that check, so they were left and are flagged
here instead.

1. `div-readout`'s following paragraph, in the branch taken when some run has
   divergent transitions, ends "**They are reported rather than removed.**"
   Suggested replacement if you want it gone: "They are reported here in
   full."
2. The Q1 sensitivity paragraph ends "**so this is a decision flipping on a
   boundary, not an estimate changing.**" Suggested replacement: "so the
   decision flips on a boundary while the estimate stands still."

**`barg_priors.R` line 75** labels S1 "the original specification, kept for
comparison", and that string is printed in `tbl-sens-specs`. The file is on
the do-not-modify list, so the string stands. Appendix G now introduces S1 as
"the unscaled `normal(0, 1)` slope prior", which reads consistently with it.

**`tbl-prior-rope`'s hard-coded 7.97%** in its caption is unchanged. It is a
number, and numbers were not to be touched.

**Nothing in the argument was decided unilaterally.** The only structural
judgement not spelled out by the task specification is the relocation of
`fig-main`, recorded in §1 above.

---

## 6. Open issue: the main text and this supplement use different priors

Flagged, not fixed, as instructed. Nothing was re-run and `paper/_analysis.R`
was not touched.

`paper/_analysis.R` fits the same landscape model for the two summary tables
of the supplementary information and leaves the slope prior at
`normal(0, 1)` — which is specification **S1** of the sensitivity set, not the
reference prior this supplement reports. The main text and this supplement
therefore report the same model under two different priors, and the difference
is not cosmetic: under `normal(0, 1)` the edge-angle slopes have
posterior/prior SD ratios close to 1, meaning the posterior is very nearly the
prior (see `tbl-ppsd`).

The supplement states the fact plainly in `sec-reproducibility` and again in
Appendix G, but resolving it is an author's decision. Three options:

1. **Re-run the main analysis under the reference prior.** Cleanest, and makes
   the two documents describe one fit. Costs a refit and a pass over the
   numbers quoted in the manuscript and the supplementary information.
2. **Reframe this supplement's reference fit as a robustness check on the
   published fit.** Cheapest. It requires the main text to say that the
   published numbers use `normal(0, 1)` and that the supplement re-examines
   them under a per-response prior, and it leaves the edge-angle result in the
   main text resting on a prior-dominated posterior.
3. **Make S1 the reference and the rescaled prior a sensitivity variant.**
   Consistent, but it inverts the argument of `sec-priors`, which is that a
   single prior scale across three link scales is a different statement about
   each response. That argument would have to be reframed or dropped.

Option 1 is the only one that removes the inconsistency rather than
documenting it.

---

## 7. Verification performed

- 102 distinct inline `` `r ... ` `` expressions before and after; the set
  difference is empty in both directions, so no expression body was altered
  and none was dropped. (Total occurrences rise from 128 to 156 because the
  Summary and §5.4 re-use expressions already in the file.)
- All 57 original chunk labels present, no duplicates, one added
  (`sens-verdict-calc`).
- All 52 `@sec-` / `@tbl-` / `@fig-` references resolve to a heading ID or a
  chunk label in the same file; none unresolved. The rendered HTML contains no
  `?sec-` / `?tbl-` / `?fig-` placeholder, which is Quarto's marker for a
  reference it could not resolve.
- 38 section IDs before, 37 after: 11 retired (all listed in §2, none still
  referenced anywhere) and 10 added.
- Every object produced by an `echo: false` chunk was traced to every consumer
  and checked by reading that definition precedes first use in the new order.
  The cross-chapter ones were hoisted into a single commented block after
  `setup`; the rest are local to their appendix.
- `quarto render paper/barg/barg_report.qmd` completes against `fits/`.
  `BARG_QUICK=1` cannot be used to render this document, before or after the
  restructure: `fits_quick/manifest.rds` has no `diagnostics` element, so
  `tbl-diag-all` fails on the quick cache. That is pre-existing and unrelated
  to this change.
- The file still has CRLF line endings — all 1,770 of them — and is still pure
  ASCII apart from the four bytes of the two `ü` characters in the Bürkner
  references. Em dashes are written `---`, as before.
- Bolded paragraph lead-ins: 38 before, 1 after (`**Archive DOI:**`, a genuine
  label).
