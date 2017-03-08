#' Safe piping
#'
#' This S3 object is inspired by [purrr::safely()].
#'
#'
#' It is a list with elements:
#'
#' * `result`
#' * `error`
#'
#' One of these, at most, will be not `NULL`
#'
#' @param result object, thing to send into the safe pipe
#' @param error  error, thing to send into the safe pipe
#' @return safe object
#'
#' @export
safe <- function(result){
  structure(list(result = result, error = NULL), class = "safe")
}

#' @export
fmap.safe <- function(.m, .f, ...){

  if (!is.null(.m[["error"]])){
    return(.m)
  }

  .m_new <-
    structure(
      purrr::safely(.f)(.m[["result"]], ...),
      class = "safe"
    )

  # if we have an error, can we perform surgery
  # on the call to be more informative?
  if (!is.null(.m_new[["error"]])){
    # cat(.f)
  }

  .m_new
}

#' @export
print.safe <- function(x, ...){
  if (is.null(x[["error"]])){
    cat("<Result>\n")
    print(x[["result"]])
  } else {
    cat("<Error>\n")
    print(x[["error"]])
  }
}
