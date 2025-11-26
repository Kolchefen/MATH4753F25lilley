#' Launch Shiny MLE Demo
#'
#' @returns Shiny App
#' @export
#'
#' @examples
#' \dontrun{shinymle()}
shinymle <- function ()
{
  shiny::runApp(system.file("SHINY", "mle", package = "MATH4753F25lilley"), launch.browser = TRUE)
}


