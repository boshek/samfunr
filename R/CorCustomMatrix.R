#' @title Custom Correlation Matrix
#' @description Custom Correlation Matrix - This really isn't useful yet; taken from Poster_plot.R
#' @export
#'
#'
#' @source http://druedin.com/2012/08/11/moving-averages-in-r/
#' @examples
#' CorMatrixCustom(hy.Fish[hy.Fish$Lake=="Arrow", c(3:ncol(hy.Fish))],
#' yvar=as.numeric(hy.Fish[hy.Fish$Lake=="Arrow",]$Year),
#' method="spearman")


CorMatrixCustom <- function(method = c("pearson", "kendall","spearman"),...)
{
  panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs",
                        method, cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = use, method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor))
      cex <- 0.8/strwidth(txt)
    test <- cor.test(x, y, method = method)
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***",
                                                                              "**", "*", ".", " "))
    text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3)/1.3)
    text(0.8, 0.8, Signif, cex = cex, col = 2)
  }
  panel.splot <- function(x,yv)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 2, usr[3:4]), new = TRUE)
    plot(yv,x, type="l", axes = FALSE, col='blue')
    #grid(7, 5, lwd = 2)
    axis(1,mgp=c(1.2, 0, 0), tck=0.03, col.axis='blue')
  }
  dots=list(...)
  yvar=dots[["yvar"]]
  dots[["yvar"]] <- NULL
  pairs(dots,
        lower.panel = panel.smooth,
        upper.panel = function(x,y) panel.cor(x,y,method=method),
        diag.panel = function(x) panel.splot(x, yvar), cex.labels=0.75, font.labels = 2)
}
