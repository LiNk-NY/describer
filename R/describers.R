#' @name describers
#'
#' @title A collection of functions to summarize vectors in matrix format
#'
#' @description These functions work on character, numeric, or factor
#' vectors. They are meant to make outputs easier to paste into table.
#'
#' @param var1 A character or numeric vector
#' @param var2 A second categorical variable for crosstabulation
#' @param numvar A numeric vector
#' @param catvar A factor vector (categorical)
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
#' @param includeHeader logical (default TRUE) indicating whether or not
#' to add a header row to the table with group counts for the outcome variable
#' @param deparse.level controls how the default dnn is constructed. See 'table'.
#'
#' @examples
#'
#' data(mtcars)
#'
#' meansd(mtcars$mpg)
#' nprops(mtcars$gear)
#'
#' with(mtcars, gmeans(mpg, am))
#'
#' am <- plyr::mapvalues(mtcars$am, c(0, 1), c("Auto", "Manual"))
#' mtcars$vs1 <- plyr::mapvalues(mtcars$vs, c(0, 1), c("V Engine", "Straight"))
#'
#' gmeans(mtcars$mpg, am)
#'
#' crosstab(mtcars$vs1, am)
#'
#' mtcars$cyl <- factor(mtcars$cyl)
#' describe(mpg, cyl, disp, outcome = am, data = mtcars)
#'
NULL

#' @name describers
#' @export
meansd <- function(numvar, na.rm = TRUE, varname = NULL, digits = 2) {
    if (is.null(varname)) {
        varname <- as.character(substitute(numvar))
        varname <- varname[[length(varname)]]
    }
    stopifnot(S4Vectors::isSingleString(varname))
    numvar <- as.numeric(numvar)
    m <- round(mean(numvar, na.rm = na.rm), digits)
    stddev <- round(stats::sd(numvar, na.rm = na.rm), digits)
    matrix(paste0(m, " (", stddev, ")"), ncol = 1L,
        dimnames = list(varname, "M (SD)"))
}

#' @name describers
#' @export
nprops <- function(catvar, digits = 1) {
    if (is.data.frame(catvar))
        catvar <- catvar[[1L]]
    counts <- table(catvar)
    props <- round(prop.table(table(catvar))*100, digits)
    vals <- paste0(counts, " (", props, ")")
    matrix(vals, ncol = 1, dimnames = list(names(table(catvar)), "n (%)"))
}

#' @name describers
#' @export
crosstab <- function(var1, var2, digits = 2) {
    if (is.data.frame(var1))
        var1 <- var1[[1L]]
    if (is.data.frame(var2))
        var2 <- var2[[1L]]
    counts <- table(var1, var2)
    props <- round(prop.table(table(var1, var2), 1L)*100, digits)
    crossnames <- dimnames(counts)
    vals <- paste0(counts, " (", props, ")")
    matrix(vals, ncol = 2, dimnames = crossnames)
}

#' @name describers
#' @export
gmeans <- function(numvar, catvar, digits = 2) {
    varname <- as.character(substitute(numvar))
    varname <- varname[[length(varname)]]
    stopifnot(S4Vectors::isSingleString(varname))
    catvar <- as.factor(catvar)

    numvar <- as.numeric(numvar)
    splitSet <- split(numvar, catvar)
    ## Enforce levels
    splitSet <- splitSet[levels(catvar)]
    groupNames <- names(splitSet)
    res <- vapply(seq_along(splitSet), function(i, x) {
        m <- round(mean(x[[i]], na.rm = TRUE), digits)
        std <- round(stats::sd(x[[i]], na.rm = TRUE), digits)
        paste0(m, " (", std, ")")
    }, character(1L), x = splitSet)
    matrix(res, nrow = 1L, dimnames = list(varname, groupNames))
}

#' @name describers
#' @export
describe <- function(..., outcome, data, headerRow = NULL, headerFrame = NULL,
    includeHeader = TRUE, deparse.level = 2, digits = 2)
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

    outlevels <- rownames(stats::contrasts(outcome))
    headrow <- if (includeHeader) {
        list(matrix(
            c("", paste0("n = ", table(outcome)[outlevels])), nrow = 1L,
            dimnames = list("Characteristic", c("n (%)",
            paste(outname, outlevels, sep = "-")))
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
            cbind(meansd(vari, varname = names(x[i]), digits = digits),
                  gmeans(vari, compVar, digits = digits))
        } else {
            if (!is.null(headerRow)) {
                header <- matrix(rep("", 3L), nrow = 1L,
                    dimnames = list(headerRow[[i]], NULL))
            } else {
                header <- character(0L)
            }
            rbind(header,
                cbind(nprops(vari, digits = digits),
                    crosstab(vari, compVar, digits = digits))
            )
        }
    }, compVar = outcome, x = args)
    reslist <- c(headrow, results)
    do.call(rbind, reslist)
}
