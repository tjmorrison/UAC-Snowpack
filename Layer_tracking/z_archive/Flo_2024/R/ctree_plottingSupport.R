## This script contains some useful functions for plotting nice ctrees


#' Group variables for ctree coloring
#' @export
setVars4ctreeColoring <- function(Tbl) {
  VarGroups <- list(Time = c('season', 'drySpell_median', 'wklmonth', 'nPDays'),
                    Layer = c('gtype_class', 'gsizeAtBurial_median'),
                    Terrain = c('band', 'region'),
                    Quality = c('dq')
  )
  VarLabels <- list(var = c('season', 'drySpell_median', 'wklmonth', 'nPDays', 'band', 'region', 'dq',
                            'gtype_class', 'gsizeAtBurial_median'),
                    lab = c("Season", "Length of Preceding\nDry Period",
                            "Month of burial","Number of Problem Days",
                            "Elevation band", "Forecast Region", "Data Quality Rating",
                            "Grain Type", "Grain Size at Burial")
  )
  VarCols <- data.frame(type = c('Time', 'Layer', 'Terrain', 'Quality'),
                        col = RColorBrewer::brewer.pal(4, 'Pastel1'))

  Vars <- data.frame(var = names(Tbl), stringsAsFactors = F)

  Vars$type <- NA
  Vars$type[Vars$var %in% VarGroups$Quality] <- 'Quality'
  Vars$type[Vars$var %in% VarGroups$Time] <- 'Time'
  Vars$type[Vars$var %in% VarGroups$Terrain] <- 'Terrain'
  Vars$type[Vars$var %in% VarGroups$Layer] <- 'Layer'
  Vars <- merge(Vars, VarCols, all.x = TRUE)
  Vars <- merge(Vars, VarLabels, all.x = TRUE)

  return(Vars)
}


#' Define color, shapes, and labels for inner ctree nodes
#' @export
innerWeights <- function(node, digits = 3) {
  pval <- node$info$p.value
  var <- names(pval) #node$split$varid
  Col <- Vars$col[which(Vars$var == var)]
  Label <- Vars$lab[which(Vars$var == var)]
  Label <- toupper(gsub('_', ' ', Label))
  grid.polygon(x = c(-0.1, 1.1, 1.1, -0.1),
               y = c(0.1, 0.1, 0.9, 0.9),
               gp = gpar(fill = Col, col = "black"))
  plab <- ifelse(pval < 10^(-digits),
                 paste("p <", 10^(-digits)),
                 paste("p =", round(pval, digits = digits)))
  mainlab <- paste0(Label, "\n", "n = ", node$info$nobs, "\n", plab)
  grid.text(mainlab, gp = gpar(col = 'black'))
}
innerWeights <- function (obj, id = TRUE, pval = TRUE, abbreviate = FALSE, fill = "white",
                          gp = gpar())
{
  meta <- obj$data
  nam <- names(obj)
  extract_label <- function(node) {
    if (is.terminal(node))
      return(rep.int("", 2L))
    varlab <- character_split(split_node(node), meta)$name
    Col <- Vars$col[which(Vars$var == varlab)]
    Label <- Vars$lab[which(Vars$var == varlab)]
    Label <- toupper(gsub('_', ' ', Label))
    if (pval) {
      nullna <- function(x) is.null(x) || is.na(x)
      pval <- suppressWarnings(try(!nullna(info_node(node)$p.value),
                                   silent = TRUE))
      pval <- if (inherits(pval, "try-error"))
        FALSE
      else pval
    }
    if (pval) {
      pvalue <- node$info$p.value
      plab <- ifelse(pvalue < 10^(-3L), paste("p <", 10^(-3L)),
                     paste("p =", round(pvalue, digits = 3L)))
    }
    else {
      plab <- ""
    }
    return(c(Label, plab))
  }
  maxstr <- function(node) {
    lab <- extract_label(node)
    klab <- if (is.terminal(node))
      ""
    else unlist(lapply(kids_node(node), maxstr))
    lab <- c(lab, klab)
    lab <- unlist(lapply(lab, function(x) strsplit(x, "\n")))
    lab <- lab[which.max(nchar(lab))]
    if (length(lab) < 1L)
      lab <- ""
    return(lab)
  }
  nstr <- maxstr(node_party(obj))
  if (nchar(nstr) < 6)
    nstr <- "aAAAAa"
  rval <- function(node) {
    pushViewport(viewport(gp = gp, name = paste("node_inner",
                                                id_node(node), "_gpar", sep = "")))
    node_vp <- viewport(x = unit(0.5, "npc"), y = unit(0.5,
                                                       "npc"), width = unit(1, "strwidth", nstr) * 1.3,
                        height = unit(3, "lines"), name = paste("node_inner",
                                                                id_node(node), sep = ""), gp = gp)
    pushViewport(node_vp)
    # xell <- c(seq(0, 0.2, by = 0.01), seq(0.2, 0.8, by = 0.05),
    #           seq(0.8, 1, by = 0.01))
    xell <- c(0, 1, 1, 0)
    # yell <- (xell * (1 - xell))**(1/3.2)
    yell <- 0.65*c(-1, -1, 1, 1)
    lab <- extract_label(node)
    # fill <- rep(fill, length.out = 2L)
    varlab <- character_split(split_node(node), meta)$name
    fill <- Vars$col[which(Vars$var == varlab)]
    # grid.polygon(x = unit(c(xell, rev(xell)), "npc"), y = unit(c(yell,
    #                                                              -yell) + 0.5, "npc"), gp = gpar(fill = fill[1]))
    grid.polygon(x = unit(c(xell), "npc"), y = unit(c(yell) + 0.5, "npc"), gp = gpar(fill = fill[1]))
    # grid.text(lab[1L], y = unit(1.5 + 0.5 * (lab[2L] != ""),
    #                             "lines"))
    # if (lab[2L] != "")
    # grid.text(lab[2L], y = unit(1, "lines"))
    grid.text(paste0(lab, collapse = "\n"), y = unit(1.3, "lines"))
    if (id) {
      nodeIDvp <- viewport(x = unit(0.5, "npc"), y = unit(1.25,
                                                          "npc"), width = max(unit(1, "lines"), unit(1.3,
                                                                                                     "strwidth", nam[id_node(node)])), height = max(unit(1,
                                                                                                                                                         "lines"), unit(1.3, "strheight", nam[id_node(node)])))
      pushViewport(nodeIDvp)
      grid.rect(gp = gpar(fill = "white"))
      grid.text(nam[id_node(node)])
      popViewport()
    }
    upViewport(2)
  }
  return(rval)
}
class(innerWeights) <- "grapcon_generator"

#' Plot customized ctree
#' @export
plotCTree <- function(fit, ...) {
  terminal_panel <- node_violinplot
  edge_panel <- edge_simple(fit, digits = 1)
  plot(fit, inner_panel = innerWeights, terminal_panel = terminal_panel, edge_panel = edge_panel, ...)
}



