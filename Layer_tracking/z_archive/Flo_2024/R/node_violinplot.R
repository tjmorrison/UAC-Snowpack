#' Plot terminal nodes of ctrees as violins
#'
#' Can be used exactly as other partykit functions for terminal nodes, e.g. `node_boxplot`.
#'
#' @author fherla
#' @seealso [node_mvar_violin]
#' @export
node_violinplot <- function (obj, col = "black", fill = "lightgray", bg = "white",
                             width = 0.8, yscale = NULL, ylines = 3, cex = 0.5, id = TRUE,
                             mainlab = NULL, gp = gpar(),
                             col.box = "black", fill.box = "black", fill.median = "white")
{
  y <- obj$fitted[["(response)"]]
  stopifnot(is.numeric(y))
  if (is.null(yscale))
    yscale <- range(y) + c(-0.1, 0.1) * diff(range(y))
  rval <- function(node) {
    nid <- id_node(node)
    dat <- data_party(obj, nid)
    yn <- dat[["(response)"]]
    wn <- dat[["(weights)"]]
    if (is.null(wn))
      wn <- rep(1, length(yn))

    ## compute kernel density estimate
    kde <- stats::density(rep.int(yn, wn), from = yscale[1], to = yscale[2], na.rm = TRUE)
    ## limit kde to range(yn)
    idx <- which(kde$x <= range(yn)[2] & kde$x >= range(yn)[1])
    if (length(idx) > 0) {
      kde$y <- kde$y[idx]
      kde$x <- kde$x[idx]
      ## construct polygon coordinates
      width.scalingfactor <- width / 2 / max(kde$y, na.rm = TRUE)
      polX <- c((0.5 - (kde$y * width.scalingfactor)), rev(0.5 + (kde$y * width.scalingfactor)))
      polY <- c(kde$x, rev(kde$x))
    }

    ## compute boxplot characteristics
    x <- boxplot(rep.int(yn, wn), plot = FALSE)

    top_vp <- viewport(layout = grid.layout(nrow = 2, ncol = 3,
                                            widths = unit(c(ylines, 1, 1), c("lines", "null",
                                                                             "lines")), heights = unit(c(1, 1), c("lines",
                                                                                                                  "null"))), width = unit(1, "npc"), height = unit(1,
                                                                                                                                                                   "npc") - unit(2, "lines"), name = paste("node_boxplot",
                                                                                                                                                                                                           nid, sep = ""), gp = gp)
    pushViewport(top_vp)
    grid.rect(gp = gpar(fill = bg, col = 0))
    top <- viewport(layout.pos.col = 2, layout.pos.row = 1)
    pushViewport(top)
    if (is.null(mainlab)) {
      mainlab <- if (id) {
        function(id, nobs) sprintf("[%s] nWKL = %s",  # Node %s
                                   id, length(unique(VS_$wkl_uuid[fitted_node(obj$node, obj$data) == nid])))  # id, nobs
        # function(id, nobs) sprintf("[%s] n = %s",  # Node %s
        #                            id, nobs)  # id, nobs
      }
      else {
        function(id, nobs) sprintf("n = %s", nobs)
      }
    }
    if (is.function(mainlab)) {
      mainlab <- mainlab(names(obj)[nid], sum(wn))
    }
    grid.text(mainlab)
    popViewport()
    plot <- viewport(layout.pos.col = 2, layout.pos.row = 2,
                     xscale = c(0, 1), yscale = yscale, name = paste0("node_boxplot",
                                                                      nid, "plot"), clip = FALSE)
    pushViewport(plot)
    grid.yaxis()
    grid.rect(gp = gpar(fill = "transparent"))
    grid.clip()
    ## draw violin
    if (length(idx) > 0) {
      grid.polygon(unit(polX,"npc"), unit(polY, "native"),
                   gp = gpar(col = col, fill = fill))

    }
    ## draw boxplot
    if (length(idx) > 0) {
      box.width <- max(polX-0.5, na.rm = TRUE) * 0.08
    } else {
      box.width <- width
    }
    grid.rect(unit(0.5, "npc"), unit(x$stats[2], "native"),
              width = unit(box.width, "npc"), height = unit(diff(x$stats[c(2, 4)]), "native"),
              just = c("center", "bottom"),
              gp = gpar(col = col.box, fill = fill.box))
    grid.lines(unit(0.5, "npc"), unit(x$stats[1:2], "native"),
               gp = gpar(col = col))
    grid.lines(unit(0.5, "npc"), unit(x$stats[4:5], "native"),
               gp = gpar(col = col))
    grid.points(unit(0.5, "npc"), unit(x$stats[3], "native"),
                size = unit(0.5, "char"),
                gp = gpar(col = fill.median, fill = fill.median), pch = 19)
    upViewport(2)
  }
  return(rval)
}
class(node_violinplot) <- "grapcon_generator"
