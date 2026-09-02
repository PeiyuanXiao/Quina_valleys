# ==========================================================================
# The Quina Landscape -- research compendium image.
#
# Contains everything the manuscript, the supplementary material and the
# Supplementary Bayesian Report are built from: R 4.6.1, the package versions
# pinned in renv.lock, and CmdStan 2.39.0.  The CmdStan version matters and is
# the reason this image exists: renv pins the cmdstanr R package, but the
# sampler itself is a C++ toolchain outside renv's reach, and paper/barg
# reports the CmdStan version as part of the model specification.
#
#   docker build -t quina .
#   docker run --rm -it -e PASSWORD=quina -p 8787:8787 quina
#   # then open http://localhost:8787  (user rstudio, password quina)
#
# To build the paper inside the container:
#   docker run --rm -v "$PWD/_targets:/home/rstudio/Quina_valleys/_targets" \
#     quina Rscript -e "targets::tar_make()"
#
# The pipeline is NOT run at build time by default: a cold tar_make() is
# eleven MCMC fits and takes 30-60 minutes, which does not belong in an image
# build. Pass --build-arg RUN_PIPELINE=true to run it anyway.
# ==========================================================================
FROM rocker/verse:4.6.1

LABEL org.opencontainers.image.title="Quina Landscape research compendium"
LABEL org.opencontainers.image.source="https://github.com/PeiyuanXiao/Quina_valleys"
LABEL maintainer="Peiyuan Xiao <pyxiao@uw.edu>"

ENV PROJ_DIR=/home/rstudio/Quina_valleys
ENV CMDSTAN_VERSION=2.39.0
ENV CMDSTAN=/opt/cmdstan/cmdstan-${CMDSTAN_VERSION}

# The library location is pinned to a fixed, user-independent path. Left at its
# default, renv puts the project library under the *current user's* cache, so a
# library restored as root during the build is invisible to the rstudio user at
# run time and to whatever UID CI happens to use -- every package reports as
# missing. Pinning it here, and making it writable, is what makes one restore
# serve every user of the image.
ENV RENV_PATHS_LIBRARY=/opt/renv/library
ENV RENV_PATHS_CACHE=/opt/renv/cache

# renv's sandbox shields the system library from being written to. In a
# disposable container that protects nothing, and it costs: the sandbox is
# built under the *current user's* cache, so it exists only for whoever built
# the image and any other user silently rebuilds it at every R startup. Off.
ENV RENV_CONFIG_SANDBOX_ENABLED=FALSE

# ---- CmdStan -------------------------------------------------------------
# Installed before the project so that this layer, the slowest to build, is
# not invalidated by a change to the analysis code.
RUN R -q -e "install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))" \
 && mkdir -p /opt/cmdstan \
 && R -q -e "cmdstanr::install_cmdstan(dir = '/opt/cmdstan', version = Sys.getenv('CMDSTAN_VERSION'), cores = 4)" \
 && chmod -R a+rwX /opt/cmdstan

# ---- R packages ----------------------------------------------------------
# The lockfile is copied on its own first, so that editing the analysis does
# not force renv::restore() to run again on the next build.
WORKDIR ${PROJ_DIR}
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
COPY .Rprofile .Rprofile
RUN R -q -e "install.packages('renv', repos = c(CRAN = 'https://packagemanager.posit.co/cran/latest'))" \
 && R -q -e "renv::restore()" \
 && chmod -R a+rwX /opt/renv

# ---- the compendium ------------------------------------------------------
# --chown on the COPY rather than a separate chown -R: the latter rewrites
# every file into a second layer, doubling what the project contributes.
COPY --chown=rstudio:rstudio . ${PROJ_DIR}

# Traversable by any UID, so that `docker run --user $(id -u)` -- the usual way
# to keep a container from writing root-owned files into a bind mount -- can
# still reach the project. Without this, renv's activate.R fails to normalize
# the project path and every package appears to be missing.
RUN chmod a+rX /home/rstudio ${PROJ_DIR}

# Optional: build the paper as part of the image. Off by default; see above.
ARG RUN_PIPELINE=false
RUN if [ "$RUN_PIPELINE" = "true" ]; then R -q -e "targets::tar_make()"; fi

# No USER instruction: rocker's entrypoint (/init, s6-overlay) has to start as
# root to bring up RStudio Server before dropping to the rstudio user. Setting
# USER rstudio here silently breaks `docker run -p 8787:8787`.
