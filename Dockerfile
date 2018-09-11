FROM rocker/binder:3.5.0

# Copy repo into ${HOME}, make user own $HOME
USER root

RUN apt-get update && apt-get install -y --no-install-recommends apt-utils

COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

## run any install.R script we find
RUN if [ -f install.R ]; then R --quiet -f install.R; fi