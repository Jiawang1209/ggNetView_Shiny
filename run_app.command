#!/bin/bash
# Double-click this file to launch the ggNetView Shiny app in your browser.
# It runs in the folder it lives in, so keep it at the package root.

cd "$(dirname "$0")" || exit 1

# Find Rscript (PATH first, then common install locations).
RSCRIPT="$(command -v Rscript)"
if [ -z "$RSCRIPT" ]; then
  for p in /usr/local/bin/Rscript /opt/homebrew/bin/Rscript \
           /Library/Frameworks/R.framework/Resources/bin/Rscript; do
    [ -x "$p" ] && RSCRIPT="$p" && break
  done
fi
if [ -z "$RSCRIPT" ]; then
  echo "Could not find Rscript. Please install R or launch from RStudio instead."
  read -r -p "Press Enter to close..."
  exit 1
fi

echo "Launching ggNetView Shiny with: $RSCRIPT"
exec "$RSCRIPT" -e 'shiny::runApp("inst/app", launch.browser = TRUE)'
