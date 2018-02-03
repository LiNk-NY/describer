#' @rdname describers
#'
#' @title A collection of functions to summarize vectors in matrix format
#'
#' @description These functions work on character, numeric, or factor
#' vectors. They are meant to make outputs easier to paste into table.
#'
#' @param var A character or numeric vector
#' @param var2 A second categorical variable for crosstabulation
#' @param na.rm (default TRUE) whether to remove NA values when summarizing
#' numeric values
#' @param varname An optional character string for the name of the vector
#' (defaults to object name)
#' @param digits The number of significant digits to show
#' @param outcome The comparison variable, divides the data into categories
#'
#' @examples
#'
#' data(mtcars)
#'
#' meansd(mtcars$mpg)
#'
#' with(mtcars, groupMeans(mpg, am))
#'
#' am <- plyr::mapvalues(mtcars$am, c(0, 1), c("Auto", "Manual"))
#' groupMeans(mtcars$mpg, am)
#'
#' @export
meansd <- function(var, na.rm = TRUE, varname = NULL, digits = 2) {
    if (is.null(varname)) {
    varname <- as.character(substitute(var))
    varname <- varname[[length(varname)]]
    }
    stopifnot(S4Vectors::isSingleString(varname))
    var <- as.numeric(var)
    m <- round(mean(var, na.rm = na.rm), digits)
    stddev <- round(sd(var, na.rm = na.rm), digits)
    matrix(paste0(m, " (", stddev, ")"), ncol = 1L,
        dimnames = list(varname, "M (SD)"))
}

#' @export
proportion <- function(var, digits = 1) {
    if (is.data.frame(var))
        var <- var[[1L]]
    counts <- table(var)
    props <- round(prop.table(table(var))*100, digits)
    vals <- paste0(counts, " (", props, ")")
    matrix(vals, ncol = 1, dimnames = list(names(table(var)), "n (%)"))
}

#' @export
crosstab <- function(var, var2, digits = 2) {
    if (is.data.frame(var))
        var <- var[[1L]]
    if (is.data.frame(var2))
        var2 <- var2[[1L]]
    counts <- table(var, var2)
    props <- round(prop.table(table(var, var2), 1L)*100, digits)
    crossnames <- dimnames(counts)
    vals <- paste0(counts, " (", props, ")")
    matrix(vals, ncol = 2, dimnames = crossnames)
}

#' @export
groupMeans <- function(var, outcome, digits = 2) {
    varname <- as.character(substitute(var))
    varname <- varname[[length(varname)]]
    stopifnot(S4Vectors::isSingleString(varname))
    outcome <- as.factor(outcome)
    var <- as.numeric(var)
    splitSet <- split(var, outcome)
    ## Enforce levels
    splitSet <- splitSet[levels(outcome)]
    groupNames <- names(splitSet)
    res <- vapply(seq_along(splitSet), function(i, x) {
        m <- round(mean(x[[i]], na.rm = TRUE), digits)
        std <- round(sd(x[[i]], na.rm = TRUE), digits)
        paste0(m, " (", std, ")")
    }, character(1L), x = splitSet)
    resMat <- matrix(res, nrow = 1L, dimnames = list(varname, groupNames))
    resMat[, rev(groupNames), drop = FALSE]
}
