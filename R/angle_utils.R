#' Angle helpers for ggNetView layout parameters
#'
#' Several layout parameters in ggNetView accept an angle. Internally
#' everything is stored in radians, but for convenience the public API
#' allows the user to pass either radians (\code{pi/2}) or degrees
#' (\code{45}) directly. The unit is auto-detected from the magnitude.
#'
#' @name angle_utils
NULL


#' Convert degrees to radians
#'
#' Tiny helper, used to disambiguate when the magnitude rule below
#' would otherwise misclassify a small degree value as radians (e.g.
#' if you actually want \code{5} degrees, write \code{deg(5)}).
#'
#' @param d Numeric. Degrees.
#' @return Numeric. Radians (\code{d * pi / 180}).
#' @export
#' @examples
#' deg(45)   # 0.7853982
#' deg(180)  # 3.141593
deg <- function(d) {
  d * pi / 180
}


#' Auto-detect angle unit and normalise to radians
#'
#' Internal helper. Accepts a single numeric and returns the equivalent
#' angle in radians. The rule is:
#' \itemize{
#'   \item \code{|x| <= 2*pi} -> treated as radians (e.g. \code{pi/2}, \code{pi}, \code{2*pi}).
#'   \item \code{|x| > 2*pi} -> treated as degrees and converted (e.g. \code{45}, \code{90}, \code{180}).
#' }
#' If the value falls in the ambiguous "small-but-bigger-than-pi" zone
#' (i.e. \code{(pi, 2*pi]}), a \code{message()} is emitted reporting the
#' interpretation and pointing the user at \code{deg()} as a way to be
#' explicit.
#'
#' @param x Single finite numeric.
#' @param name Parameter name used in error / message text.
#' @return Numeric. Radians.
#' @keywords internal
.normalize_angle <- function(x, name = "angle") {
  if (length(x) != 1L || !is.finite(x)) {
    stop(sprintf("`%s` must be a single finite numeric value.", name), call. = FALSE)
  }
  if (x == 0) return(0)
  ax <- abs(x)
  if (ax <= 2 * pi) {
    if (ax > pi) {
      message(sprintf("`%s = %g` interpreted as radians (= %.1f degrees). ",
                      name, x, x * 180 / pi),
              sprintf("If you meant degrees, write `%s = deg(%g)`.", name, x))
    }
    return(x)              # already radians
  }
  x * pi / 180             # degrees -> radians
}
