# Task Feedback Browser Smoke Implementation Plan

Goal: close the remaining long-running feedback gap by proving in a real browser that Shiny action buttons enter and leave the shared busy state during a deliberately slow workflow action.

Architecture: keep the production feedback mechanism in `R/app_task_feedback.R`; add an opt-in test delay controlled by `GGNV_TASK_FEEDBACK_TEST_DELAY`; use a dedicated shinytest2 smoke so normal workflow smokes stay fast.

Tasks:

- [x] Add a failing browser smoke that expects a slow action button to become disabled and receive `ggnetview-task-busy`.
- [x] Route manual example loading through shared task feedback and add the opt-in test delay hook.
- [x] Add focused unit coverage for the delay hook and shared-feedback source coverage.
- [x] Run focused unit tests, startup, task-feedback browser smoke, and Phase 2 browser smoke.
