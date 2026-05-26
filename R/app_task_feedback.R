task_feedback_message <- function(label, phase = c("running", "success", "failure")) {
  phase <- match.arg(phase)
  if (is.null(label)) {
    label <- "task"
  }
  label <- trimws(as.character(label))
  if (!nzchar(label)) {
    label <- "task"
  }

  switch(phase,
    running = paste0("Running ", label, "..."),
    success = paste0("Finished ", label, "."),
    failure = paste0("Failed ", label, ".")
  )
}

task_busy_payload <- function(id, busy) {
  list(id = as.character(id), busy = isTRUE(busy))
}

app_task_feedback_script <- function() {
  shiny::tags$script(shiny::HTML(
    "Shiny.addCustomMessageHandler('ggnetview-task-busy', function(message) {
      var el = document.getElementById(message.id);
      if (!el) return;
      if (message.busy) {
        el.setAttribute('disabled', 'disabled');
        el.classList.add('ggnetview-task-busy');
      } else {
        el.removeAttribute('disabled');
        el.classList.remove('ggnetview-task-busy');
      }
    });"
  ))
}

send_task_busy <- function(session, button_ids, busy) {
  if (is.null(session) || !length(button_ids)) {
    return(invisible(FALSE))
  }
  for (id in button_ids) {
    session$sendCustomMessage("ggnetview-task-busy", task_busy_payload(id, busy))
  }
  invisible(TRUE)
}

task_feedback_test_delay <- function() {
  delay <- suppressWarnings(as.numeric(Sys.getenv("GGNV_TASK_FEEDBACK_TEST_DELAY", "0")))
  if (is.na(delay) || delay <= 0) {
    return(invisible(FALSE))
  }
  Sys.sleep(delay)
  invisible(TRUE)
}

with_task_feedback <- function(session, label, button_ids = character(), expr) {
  send_task_busy(session, button_ids, TRUE)
  if (!is.null(session) && is.function(session$flushReact)) {
    session$flushReact()
  }
  task_feedback_test_delay()
  on.exit(send_task_busy(session, button_ids, FALSE), add = TRUE)

  if (is.null(shiny::getDefaultReactiveDomain())) {
    return(force(expr))
  }

  shiny::withProgress(
    message = task_feedback_message(label, "running"),
    value = 0,
    {
      shiny::incProgress(0.25, detail = "Calling ggNetView API")
      value <- force(expr)
      shiny::incProgress(0.75, detail = "Finalizing result")
      value
    }
  )
}
