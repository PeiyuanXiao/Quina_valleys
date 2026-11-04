K_GRID  <- seq(1, 3, by = 0.1)
N_CHECK <- 200                   
fresh    <- "--fresh" %in% commandArgs(trailingOnly = TRUE)
env_file <- here::here("_targets", "objects", "analysis_env")

if (!fresh && file.exists(env_file)) {
  e <- targets::tar_read(analysis_env, store = here::here("_targets"))
  source_note <- sprintf("targets store, analysis_env built %s",
                         format(file.mtime(env_file), "%Y-%m-%d %H:%M"))
} else {
  e <- new.env(parent = globalenv())
  sys.source(here::here("paper", "_analysis.R"), envir = e)
  source_note <- "paper/_analysis.R, sourced afresh"
}

needed <- c("eq_Zsc", "eq_Zlq", "eq_Zlo", "eq_loc_rows", "eq_n_loc", "eq_n_lq",
            "eq_n_lo", "eq_dr0_m", "eq_dr_b", "eq_K", "eq_UB95_multi", "B_BOOT",
            "pd_scq", "pd_ltq", "pd_lto")
missing <- needed[!vapply(needed, exists, logical(1), envir = e, inherits = FALSE)]
if (length(missing))
  stop("The analysis environment lacks ", paste(missing, collapse = ", "),
       ". Rebuild it with targets::tar_make(names = \"analysis_env\") or rerun with --fresh.")

Zsc <- e$eq_Zsc; Zlq <- e$eq_Zlq; Zlo <- e$eq_Zlo
B   <- e$B_BOOT
c_lo <- colMeans(Zlo)   # the non-Quina centroid, held fixed for every k

set.seed(2226)
Csc <- Clq <- Clo <- matrix(NA_real_, B, ncol(Zsc))
idx <- vector("list", N_CHECK)
for (b in seq_len(B)) {
  i_sc <- unlist(e$eq_loc_rows[sample.int(e$eq_n_loc, e$eq_n_loc, replace = TRUE)],
                 use.names = FALSE)
  i_lq <- sample.int(e$eq_n_lq, e$eq_n_lq, replace = TRUE)
  i_lo <- sample.int(e$eq_n_lo, e$eq_n_lo, replace = TRUE)
  Csc[b, ] <- colMeans(Zsc[i_sc, , drop = FALSE])
  Clq[b, ] <- colMeans(Zlq[i_lq, , drop = FALSE])
  Clo[b, ] <- colMeans(Zlo[i_lo, , drop = FALSE])
  if (b <= N_CHECK) idx[[b]] <- list(sc = i_sc, lq = i_lq, lo = i_lo)
}

inflate <- function(X, k) sweep(sweep(X, 2, c_lo) * k, 2, c_lo, "+")
num   <- sqrt(rowSums((Csc - Clq)^2))
dr_at <- function(k) num / sqrt(rowSums((Clq - inflate(Clo, k))^2))
ub95  <- function(x) unname(quantile(x, 0.95, na.rm = TRUE))

stopifnot(isTRUE(all.equal(dr_at(1), unname(e$eq_dr_b[, e$eq_K + 1]))),
          isTRUE(all.equal(ub95(dr_at(1)), e$eq_UB95_multi)))
Zlo_2 <- inflate(Zlo, 2)
literal <- vapply(seq_len(N_CHECK), function(b) {
  j <- idx[[b]]
  a <- colMeans(Zsc[j$sc, , drop = FALSE]); l <- colMeans(Zlq[j$lq, , drop = FALSE])
  sqrt(sum((a - l)^2)) / sqrt(sum((l - colMeans(Zlo_2[j$lo, , drop = FALSE]))^2))
}, numeric(1))
stopifnot(isTRUE(all.equal(literal, dr_at(2)[seq_len(N_CHECK)])))

k_ltq <- e$pd_ltq / e$pd_lto  
k_sc  <- e$pd_scq / e$pd_lto   
ks    <- sort(unique(c(K_GRID, k_ltq, k_sc)))

out <- do.call(rbind, lapply(ks, function(k) {
  dr <- dr_at(k)
  data.frame(k = k,
             reference = if (isTRUE(all.equal(k, 1))) "observed"
                         else if (isTRUE(all.equal(k, k_ltq))) "as dispersed as Longtan Quina"
                         else if (isTRUE(all.equal(k, k_sc))) "as dispersed as surface Quina"
                         else "",
             nonquina_dispersion = k * e$pd_lto,
             dr_median = median(dr),
             dr_ub95   = ub95(dr))
}))

write.csv(transform(out, k = round(k, 3), nonquina_dispersion = round(nonquina_dispersion, 3),
                    dr_median = round(dr_median, 4), dr_ub95 = round(dr_ub95, 4)),
          here::here("paper", "response", "R1_DR_sensitivity.csv"), row.names = FALSE)

shown <- out[out$reference != "" | round(out$k, 6) %in% c(1.5, 2, 2.5, 3), ]
shown <- transform(shown, k = sprintf("%.2f", k),
                   nonquina_dispersion = sprintf("%.2f", nonquina_dispersion),
                   dr_median = sprintf("%.3f", dr_median), dr_ub95 = sprintf("%.3f", dr_ub95))

cat("\nDR sensitivity to the spread of the Longtan non-Quina scrapers\n")
cat("Source:", source_note, "\n")
cat(sprintf("Bootstrap replicates: %d. Checks passed: k = 1 reproduces the paper's replicates.\n", B))
cat(sprintf("Observed dispersion (PERMDISP): surface Quina %.2f, Longtan Quina %.2f, Longtan non-Quina %.2f\n",
            e$pd_scq, e$pd_ltq, e$pd_lto))
cat(sprintf("Multivariate DR point estimate: %.3f, the same at every k\n\n", e$eq_dr0_m))
print(shown, row.names = FALSE, right = FALSE)
cat(sprintf("\nAcross k = %.1f to %.1f, the 95%% upper bound on DR lies between %.3f and %.3f",
            min(K_GRID), max(K_GRID), min(out$dr_ub95), max(out$dr_ub95)))
cat(sprintf(" (paper: %.3f).\n", e$eq_UB95_multi))
cat("Full grid written to paper/response/R1_DR_sensitivity.csv\n")
