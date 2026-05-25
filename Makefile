R ?= /usr/local/bin/R
RSCRIPT ?= /usr/local/bin/Rscript

.PHONY: shiny-startup shiny-smoke shiny-manual-smoke shiny-browser-smoke shiny-smoke-all shiny-build shiny-test-helpers shiny-run

shiny-startup:
	cd inst/app && $(RSCRIPT) ../../tests/run_shiny_app_startup.R

shiny-smoke:
	$(RSCRIPT) tests/run_shiny_core_workflow_smoke.R

shiny-manual-smoke:
	$(RSCRIPT) tests/run_shiny_manual_workflow_smoke.R

shiny-browser-smoke:
	NOT_CRAN=true $(RSCRIPT) tests/run_shiny_phase2_workflow_smoke.R

shiny-smoke-all: shiny-startup shiny-smoke shiny-manual-smoke shiny-browser-smoke

shiny-build:
	$(R) CMD build . --no-build-vignettes --no-manual

shiny-test-helpers:
	$(RSCRIPT) -e 'library(testthat); source("R/app_registry.R"); source("R/app_validation.R"); source("R/app_adapters.R"); source("R/app_exports.R"); source("R/launch_ggNetView.R"); source("tests/testthat/test-app-registry.R"); source("tests/testthat/test-app-adapters.R"); source("tests/testthat/test-app-validation.R"); source("tests/testthat/test-app-exports.R"); source("tests/testthat/test-launch.R"); source("tests/testthat/test-shiny-files.R"); source("tests/testthat/test-shiny-modules.R"); if (file.exists("tests/testthat/test-shiny-workflow-helpers.R")) source("tests/testthat/test-shiny-workflow-helpers.R")'

shiny-run:
	$(RSCRIPT) -e 'shiny::runApp("inst/app", launch.browser = TRUE)'
