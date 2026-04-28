% Generated manually
\name{launch_ggNetView}
\alias{launch_ggNetView}
\title{Launch the ggNetView Shiny application}
\usage{
launch_ggNetView(host = "127.0.0.1", port = NULL,
                 launch.browser = interactive(),
                 max_upload_size_mb = 200, ...)
}
\arguments{
\item{host}{Character. Host address. Defaults to \code{"127.0.0.1"}.}
\item{port}{Integer. TCP port (random by default).}
\item{launch.browser}{Logical. Open browser automatically.}
\item{max_upload_size_mb}{Maximum upload size (MB). Default 200.}
\item{...}{Forwarded to \code{\link[shiny]{runApp}}.}
}
\value{Invisibly \code{NULL}; called for its side-effect.}
\description{
Starts the interactive \pkg{shinydashboard} GUI built on top of
\pkg{ggNetView}.
}
\examples{
\dontrun{
launch_ggNetView()
}
}
