#' @name describers
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
#' @param ... Variables to describe by the outcome, can be verctors or
#' unquoted names when the data argument is supplied
#' @param data A data.frame containing the comparison variables
#' @param headerRow A character vector for each variable in \ldots that serves
#' as the descriptive name in the table
#' @param headerFrame A data.frame that relates or maps each variable name
#' to its respective descriptive name
#' @param includeHeaders logical (default TRUE) indicating whether or not
#' to add a header row to the table with group counts for the outcome variable
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
NULL

#' @name describers
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

#' @name describers
#' @export
proportion <- function(var, digits = 1) {
    if (is.data.frame(var))
        var <- var[[1L]]
    counts <- table(var)
    props <- round(prop.table(table(var))*100, digits)
    vals <- paste0(counts, " (", props, ")")
    matrix(vals, ncol = 1, dimnames = list(names(table(var)), "n (%)"))
}

#' @name describers
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

#' @name describers
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

#' @name describers
#' @export
describe <- function(..., outcome, data, headerRow = NULL, headerFrame = NULL,
    includerHeader = TRUE, deparse.level = 2, digits = 2)
{
    listvars <- as.list(substitute(list(...)))[-1L]
    ## code from table()
    nams <- vapply(listvars, function(x) {
        switch(deparse.level + 1L,
               "", if (is.symbol(x)) as.character(x) else "",
               gsub("\\w+\\$", "", deparse(x, nlines = 1L)[1L]))
    }, character(1L))

    outname <- rev(as.character(substitute(outcome)))[1L]
    if (!missing(data)) {
        args <- as.list(data[, nams])
        if (length(outname) == 1L)
            outcome <- data[, outname]
        else
            stop("Provide a single outcome")
    } else {
        args <- list(...)
    }
    if (!is.factor(outcome))
        outcome <- as.factor(outcome)
    lengthArgs <- seq_along(args)

    if (!is.null(headerRow))
        names(lengthArgs) <- names(args) <- headerRow
    else if (!is.null(headerFrame))
        names(lengthArgs) <- names(args) <- headerRow <-
        headerFrame[[2L]][match(nams, headerFrame[[1L]])]
    else
        names(lengthArgs) <- names(args) <- nams

    outlevels <- rownames(contrasts(outcome))
    headrow <- if (includerHeader) {
        list(matrix(
            c("", paste0("n = ", table(outcome)[outlevels]), ""), nrow = 1L,
            dimnames = list("Characteristic", c("n (%)",
            paste(outname, outlevels, sep = "-"), "p.value"))
        ))
    } else {
        NULL
    }

    numeric <- vapply(args, is.numeric, logical(1L))
    results <- lapply(lengthArgs, function(i, x, compVar) {
        vari <- x[[i]]
        if (is.data.frame(vari))
            vari <- vari[[1L]]
        if (is.character(vari))
            vari <- as.factor(vari)
        if (numeric[[i]]) {
            cbind(.meansd(vari, varName = names(x[i]), digits = digits),
                  .groupMeans(vari, compVar, digits = digits),
                  .ttestPval(vari, compVar, varName = names(x[i])))
        } else {
            fourth <- .chitestPval(vari, compVar)[[1L]]
            if (!is.null(headerRow)) {
                header <- matrix(c(rep("", 3L), fourth), nrow = 1L,
                                 dimnames = list(headerRow[[i]], NULL))
                p.value <- rep("", length(levels(vari)))
            } else {
                header <- character(0L)
                p.value <- c(fourth, rep("", length(levels(vari))-1))
            }
            rbind(header,
                  cbind(.prop(vari, digits = digits),
                        .crossTab(vari, compVar, digits = digits),
                        p.value))
        }
    }, compVar = outcome, x = args)
    c(headrow, results)
}
