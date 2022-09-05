#'
#' @return
#' @author Michal Burda
#' @export
load_data <- function(name, package = NULL) {
    env <- new.env()
    name <- strsplit(name, ' ', fixed = TRUE)
    name <- name[[1]][1]
    if (!exists(name, where = env)) {
        data(list = name, package = package, envir = env)
    }
    if (!exists(name, where = env)) {
        return(NULL)
    }

    result <- get(name, envir = env)
    rm(env)

    result
}
