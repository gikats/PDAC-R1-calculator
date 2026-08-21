# =============================================================
#  Build the static (shinylive) version for GitHub Pages
#
#  Run from the project root:
#      source("build.R")
#
#  Output goes to docs/ , which GitHub Pages serves.
# =============================================================

if (!requireNamespace("shinylive", quietly = TRUE)) {
  install.packages("shinylive")
}

shinylive::export(appdir = "app", destdir = "docs")

cat("\nDone. Now:\n",
    "  1. Preview locally:  httpuv::runStaticServer('docs')\n",
    "  2. Commit and push the docs/ folder\n",
    "  3. GitHub -> Settings -> Pages -> Deploy from branch -> main / docs\n\n")
