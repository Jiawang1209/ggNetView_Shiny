# Unreplayable Object Restore Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:test-driven-development for this focused feature slice.

**Goal:** Improve workflow manifest restore for graph/plot objects that cannot be reconstructed from snapshotted inputs, gallery recipes, or graph-builder metadata.

**Architecture:** Keep replay-first behavior for recipe outputs and graph-builder outputs. Add direct data snapshots only for otherwise unreplayable graph/plot objects, then restore those snapshots through the existing manifest input restore path.

**Tech Stack:** R, Shiny registry helpers, testthat, workflow manifest JSON, `/usr/local/bin/Rscript`.

## Tasks

- [x] Add failing tests for unreplayable graph/plot snapshot export and restore.
- [x] Extend workflow snapshot eligibility and data serialization safely.
- [x] Preserve replay-first behavior for builder/recipe outputs.
- [x] Update TODO docs and run focused/browser verification.
