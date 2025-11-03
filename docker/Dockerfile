FROM docker.io/rso9192/cava:version3.2.6

# Install remotes and the required package
RUN R -e "remotes::install_github('Risk-Team/CAVAanalytics')"

# Keep the original entrypoint (Docker will inherit it if not changed)

