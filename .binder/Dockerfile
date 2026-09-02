# ==========================================================================
# Binder image -- RStudio in the browser, no local install.
#
# What this image can and cannot do, stated plainly because the limits are
# real: mybinder.org gives about 2 GB of memory and no persistent storage, so
# it can run paper/_analysis.R -- every frequentist result in the manuscript
# and the supplementary material -- and it can open and read every file in the
# compendium. It cannot fit the Bayesian landscape models: those are eleven
# CmdStan runs of 30-60 minutes total, and CmdStan is deliberately not
# installed here because it would roughly double the image. For the Bayesian
# half, use the root Dockerfile, which pins CmdStan 2.39.0.
#
# Built from rocker/verse rather than rocker/binder because rocker/binder
# stops at R 4.4.2 and this project is pinned to R 4.6.1 by renv.lock; an
# image whose R disagreed with the lockfile would force every package to be
# rebuilt from source and time out the Binder build. The Binder layer below
# is what rocker/binder would otherwise have added.
# ==========================================================================
FROM rocker/verse:4.6.1

LABEL maintainer="Peiyuan Xiao <pyxiao@uw.edu>"

ARG NB_USER=rstudio
ARG NB_UID=1000
ENV NB_USER=${NB_USER}
ENV NB_UID=${NB_UID}
ENV HOME=/home/${NB_USER}

# Pinned to a fixed, user-independent path: renv otherwise puts the project
# library under the current user's cache, so a library restored as root during
# the build is invisible to NB_USER at run time and every package reports as
# missing. See the same note in the root Dockerfile.
ENV RENV_PATHS_LIBRARY=/opt/renv/library
ENV RENV_PATHS_CACHE=/opt/renv/cache

# Required here, not merely an optimisation. renv builds its sandbox under the
# current user's cache; this image restores as root but runs as NB_USER, who
# then rebuilds it on every R startup and hangs doing so. The sandbox only
# shields the system library from writes, which a disposable container does
# not need. See the same note in the root Dockerfile.
ENV RENV_CONFIG_SANDBOX_ENABLED=FALSE

USER root

# ---- the Binder layer ----------------------------------------------------
# jupyter-server-proxy plus jupyter-rsession-proxy is what makes
# ?urlpath=rstudio serve the RStudio session Binder launches.
RUN apt-get update \
 && apt-get install -y --no-install-recommends python3-pip python3-venv \
 && rm -rf /var/lib/apt/lists/*

ENV VIRTUAL_ENV=/opt/venv
ENV PATH=${VIRTUAL_ENV}/bin:${PATH}
RUN python3 -m venv ${VIRTUAL_ENV} \
 && pip install --no-cache-dir \
      jupyterhub notebook jupyterlab jupyter-server-proxy jupyter-rsession-proxy \
 && chmod -R a+rX ${VIRTUAL_ENV}

# ---- R packages ----------------------------------------------------------
WORKDIR ${HOME}/Quina_valleys
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
COPY .Rprofile .Rprofile
RUN R -q -e "install.packages('renv', repos = c(CRAN = 'https://packagemanager.posit.co/cran/latest'))" \
 && R -q -e "renv::restore()" \
 && chmod -R a+rwX /opt/renv

# ---- the compendium ------------------------------------------------------
COPY --chown=${NB_UID}:${NB_UID} . ${HOME}/Quina_valleys

# Binder requires the container to run as NB_USER; unlike the root Dockerfile
# there is no s6 entrypoint here that needs root first.
USER ${NB_USER}
WORKDIR ${HOME}/Quina_valleys
