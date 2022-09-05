.foreach_dataset <- function(datasets, fun_value, na, fun) {
    f <- function(i) {
        package <- datasets[i, 'package']
        name <- datasets[i, 'name']
        dd <- load_data(name, package = package)
        if (is.null(dd)) {
            return(na)
        }
        fun(dd)
    }

    vapply(seq_len(nrow(datasets)), f, fun_value)
}


.count_variables <- function(datasets, test) {
    fun <- function(d) {
        if (test(d)) {
            return(1L)
        }
        if (is.array(d) && test(d)) {
            return(ncol(d))
        }
        if (is.data.frame(d)) {
            return(as.integer(sum(sapply(d, test))))
        }
        return(NA_integer_)
    }

    .foreach_dataset(datasets, integer(1), NA_integer_, fun)
}


#'
#' @return
#' @author Michal Burda
#' @export
list_data <- function(all = TRUE) {
    if (all) {
        datasets <- data(package = .packages(all.available = TRUE))$result
    } else {
        datasets <- data()$result
    }
    datasets <- as.data.frame(datasets)
    colnames(datasets) <- c('package', 'libpath', 'name', 'title')

    # remove libpath in order to put it later as the last column
    libpath <- datasets$libpath
    datasets <- datasets[, -2]

    datasets$records <- .foreach_dataset(datasets, integer(1), NA_integer_, function(d) {
        result <- nrow(d)
        ifelse(is.null(result), length(d), result)
    })

    datasets$variables <- .foreach_dataset(datasets, integer(1), NA_integer_, function(d) {
        result <- ncol(d)
        ifelse(is.null(result), NA_integer_, result)
    })

    datasets$class <- .foreach_dataset(datasets, character(1), NA_character_, function(d) {
        preferred <- c('ts', 'data.frame', 'matrix')
        clazz <- class(d)
        if (length(clazz) == 1) {
            return(clazz)
        }
        intersection <- intersect(preferred, clazz)
        if (length(intersection) > 0) {
            return(intersection[1])
        }
        paste(clazz, collapse = ', ')
    })

    datasets$mode <- .foreach_dataset(datasets, character(1), NA_character_, mode)
    datasets$type <- .foreach_dataset(datasets, character(1), NA_character_, typeof)

    datasets$numericVariables <- .count_variables(datasets, is.numeric)
    datasets$factorVariables <- .count_variables(datasets, is.factor)
    datasets$characterVariables <- .count_variables(datasets, is.character)
    datasets$logicalVariables <- .count_variables(datasets, is.logical)

    datasets$loadable <- .foreach_dataset(datasets, logical(1), FALSE, function(d) {
        !is.null(d)
    })

    for (col in c('package', 'class', 'mode', 'type')) {
        datasets[[col]] <- as.factor(datasets[[col]])
    }

    datasets$libpath <- libpath

    datasets
}
