# ggNetView Shiny Release Evidence Plan

## Goal

Create a durable release-readiness evidence report that summarizes manual coverage, validation commands, recent commits, known limits, and final release steps.

## Steps

- [x] Add failing tests for release evidence summary, Markdown rendering, and report writing.
- [x] Implement release evidence helpers in `R/app_release_evidence.R`.
- [x] Generate `docs/ggnetview-shiny-release-evidence.md` from the current manual smoke coverage JSON and git history.
- [x] Run focused verification and final diff review.
- [ ] Commit this release evidence slice.

## Verification

- `/usr/local/bin/Rscript -e 'testthat::test_file("tests/testthat/test-release-evidence.R")'`
- `/usr/local/bin/Rscript -e 'source("R/app_smoke_coverage.R"); source("R/app_release_evidence.R"); generate_release_evidence_report("docs/ggnetview-shiny-release-evidence.md")'`
