## This script contains a function for plotting terminal nodes of multivariate ctrees as violins

.nobs_party <- function(party, id = 1L) {
  dat <- data_party(party, id = id)
  if("(weights)" %in% names(dat)) sum(dat[["(weights)"]]) else NROW(dat)
}

#' Plot terminal nodes of multivariate ctrees as violins
#'
#' Can be used exactly as the partykit function `node_mvar`.
#'
#' @author fherla
#' @seealso [node_violinplot]
#' @export
node_mvar_violin <- function(obj, which = NULL, id = TRUE, pop = TRUE, ylines = NULL, mainlab = NULL, varlab = TRUE, bg = "white", terminal_panel_mvar = node_violinplot, ...)
{
  ## obtain dependent variables
  y <- obj$fitted[["(response)"]]

  ## fitted node ids
  fitted <- obj$fitted[["(fitted)"]]

  ## number of panels needed
  if(is.null(which)) which <- 1L:NCOL(y)
  k <- length(which)

  rval <- function(node) {

    tid <- id_node(node)
    nobs <- .nobs_party(obj, id = tid)

    ## set up top viewport
    top_vp <- viewport(layout = grid.layout(nrow = k, ncol = 2,
                                            widths = unit(c(ylines, 1), c("lines", "null")), heights = unit(k, "null")),
                       width = unit(1, "npc"), height = unit(1, "npc") - unit(2, "lines"),
                       name = paste("node_mvar", tid, sep = ""))
    pushViewport(top_vp)
    grid.rect(gp = gpar(fill = bg, col = 0))

    ## main title
    if (is.null(mainlab)) {
      mainlab <- if(id) {
        function(id, nobs) sprintf("Node %s (n = %s)", id, nobs)
      } else {
        function(id, nobs) sprintf("n = %s", nobs)
      }
    }
    if (is.function(mainlab)) {
      mainlab <- mainlab(tid, nobs)
    }

    for(i in 1L:k) {
      tmp <- obj
      tmp$fitted[["(response)"]] <- y[,which[i]]
      if(varlab) {
        nm <- names(y)[which[i]]
        if(i == 1L) nm <- paste(mainlab, nm, sep = ": ")
      } else {
        nm <- if(i == 1L) mainlab else ""
      }
      pfun <- switch(sapply(y, class)[which[i]],
                     "Surv" = node_surv(tmp, id = id, mainlab = nm, ...),
                     "factor" = node_barplot(tmp, id = id, mainlab = nm,  ...),
                     "ordered" = node_barplot(tmp, id = id, mainlab = nm, ...),
                     do.call("terminal_panel_mvar", list(tmp, id = id, mainlab = nm, ...)))
      ## select panel
      plot_vpi <- viewport(layout.pos.col = 2L, layout.pos.row = i)
      pushViewport(plot_vpi)

      ## call panel function
      pfun(node)

      if(pop) popViewport() else upViewport()
    }
    if(pop) popViewport() else upViewport()
  }

  return(rval)
}
class(node_mvar_violin) <- "grapcon_generator"
