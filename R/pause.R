#' @title Pause
#' @export
#' @description Pause a loop
#' @references https://diego.assencio.com/?index=86c137b502561d44b8be02f06d80ee16
#'
pause = function()
{
  if (interactive()) {
    invisible(readline(prompt = "Press <Enter> to continue..."))
  }
  else {
    cat("Press <Enter> to continue...")
    invisible(readLines(file("stdin"), 1))
  }
}







