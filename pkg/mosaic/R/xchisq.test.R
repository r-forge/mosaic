xchisq.test <-
function (...) 
{
    ttt <- chisq.test(...)
    if (is.matrix(ttt$observed)) {
        dd <- dim(ttt$observed)
    }
    else {
        dd <- c(1, length(ttt$observed))
    }
    obs <- .surround(ttt$observed, " ", " ", digits = 2, nsmall = 2)
    exp <- .surround(ttt$expected, "(", ")", digits = 2, nsmall = 2)
    contrib <- .surround(ttt$residuals^2, "[", "]", digits = 2, 
        nsmall = 2)
    resid <- .surround(ttt$residuals, "<", ">", digits = 2, nsmall = 2)
    blank <- rep(" ", prod(dd))
    result <- c(obs, exp, contrib, resid, blank)
    dim(result) <- c(dd, 5)
    print(ttt)
    for (i in 1:dd[1]) {
        for (j in 1:5) {
            cat(result[i, , j])
            cat("\n")
        }
    }
    cat("key:\n")
    cat("\tobserved\n")
    cat("\t(expected)\n")
    cat("\t[contribution to X-squared]\n")
    cat("\t<residual>\n")
    invisible(ttt)
}
