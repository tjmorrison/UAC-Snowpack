#' Retrieve comprehensive wklvalidation data from DB
#'
#' This function retrieves the vframe joined with execution table, and corresponding captWKLs & poorWKLs
#'
#' remote usage prerequisite: on a terminal keep running SSH tunnel:
#' `ssh -L 8888:cedar-mysql-vm.int.cedar.computecanada.ca:3306 fherla@cedar.computecanada.ca`
#'
#' For gtype_rank choices: You can retrieve all crust entries from the DB by specifying `gtype_rank = 'tertiary'`. If you also specify `'primary'`, you will get all persistent grain types that were listed in the
#' forecasters' assessments. If you specify `'secondary'` instead, you will get all persistent grain types whether they were mentioned in the assessments or not (i.e., a relaxed prerequisite for the validation).
#' Note, however, that 'secondary' ranks only exist where 'primary' ranks not already contain all persistent grain types. Therefore you can specify both `c('primary', 'secondary')` to retrieve secondary ranks when
#' available and primary ones otherwise. If you set gtype_rank to NULL, you will get all DB entries despite the gtype_rank. Note that this is only possible when retrieving only the vframe without captWKLs and poorWKLs.
#'
#' @param DBCon DB connection
#' @param DB which DB to connect to? "Snow_DB" / "Steed_DB"
#' @param season season(s) you're interested
#' @param region region(s) you're interested
#' @param captWKL should this table be appended to the output list?
#' @param poorWKL should this table be appended to the output list?
#' @param studyArea include studyArea table to output list (will be limited to those grid points present in either captWKL or poorWKL)
#' @param limitWKL2vframe TRUE/FALSE; False = get all WKL layers from DB
#' @param gtype_rank to extract into vframe, captWKL, and poorWKL? see Details below.
#' @param Renviron_path path to Renviron file for connection details
#'
#' @return list of tables `executions`, `vframe`, `wkl`, `captWKL`, `poorWKL`
#'
#' @export
getVdata <- function(DBCon = NULL, DB = "Snow_DB",
                     season = NA, region = NA, band = NA,
                     captWKL = TRUE,
                     poorWKL = TRUE, poorWKLconstraint = "pu >= 0.77 AND gtype NOT IN ('PP', 'DF')",
                     studyArea = FALSE,
                     limitWKL2vframe = TRUE,
                     gtype_rank = c("primary", "secondary", "tertiary"),
                     Renviron_path = ifelse(Sys.getenv("USER") == "flo",
                                            "/home/flo/documents/sfu/code/cedarstuff/CedarTunnel.Renviron",
                                            "/home/fherla/Cedar.Renviron"),
                     verbose = FALSE) {


  closeDBCon <- FALSE
  if (is.null(DBCon)) {
    closeDBCon <- TRUE
    readRenviron(path = Renviron_path)
    DBCon <- SarpGeneral::tryMultipleTimes(RMySQL::dbConnect(DBI::dbDriver("MySQL"),
                                                             host = Sys.getenv(paste0(DB, "_Host")),
                                                             user = Sys.getenv(paste0(DB, "_Un")),
                                                             password = Sys.getenv(paste0(DB, "_Pw")),
                                                             dbname = "fherla_wklvalidation",
                                                             port = as.integer(Sys.getenv(paste0(DB, "_Tunnel_Port")))),
                                           MaxNumTries = 3)
  }

  OUT <- list()
  ## Retrieve executions
  query <- paste0("SELECT * FROM `executions`")
  if (!is.na(season)) {
    query <- paste0(query,
                    " WHERE season IN (", paste0("'", season, "'", collapse = ", "), ")", ifelse(!is.na(region) | !is.na(band), " AND", ""))
  }
  if (!is.na(region)) {
    if (is.na(season)) query <- paste0(query, " WHERE")
    query <- paste0(query, " region IN (", paste0("'", region, "'", collapse = ", "), ")", ifelse(!is.na(band), " AND", ""))
  }
  if (!is.na(band)) {
    if (is.na(season) & is.na(region)) query <- paste0(query, " WHERE")
    query <- paste0(query, " band IN (", paste0("'", band, "'", collapse = ", "), ")")
  }
  message("Retrieving executions")
  if (verbose) message(query)
  Result <- RMySQL::dbSendQuery(DBCon, query)
  OUT$executions <- RMySQL::dbFetch(Result, n = -1)

  ## Retrieve vframe
  if (is.null(gtype_rank)) {
    if (captWKL | poorWKL) stop("If you want to retrieve captWKLs or poorWKLs, you have to specify gtype_ranks! See description!")
    query <- paste0("SELECT `vframe`.* FROM `vframe`",
                    " JOIN executions USING (exec_uuid)",
                    " WHERE exec_uuid IN (", paste0("'", unique(OUT$executions$exec_uuid), "'", collapse = ", "), ")")
  } else {
    if (all(c("primary", "secondary") %in% gtype_rank)) {
      ## retrieve vf_uuids from all wkl_uuid-vdate-band combinations that have both primary *and* secondary gtype_ranks
      prequery <- paste0("SELECT vfid, gtr FROM (SELECT `wkl_uuid`, `vdate`, `band`, GROUP_CONCAT(`vf_uuid`) as vfid, GROUP_CONCAT(`gtype_rank`) as gtr",
                         " FROM (SELECT * FROM `vframe` WHERE exec_uuid IN (", paste0("'", unique(OUT$executions$exec_uuid), "'", collapse = ", "), ") AND `gtype_rank` IN ('primary', 'secondary')) as inner_subset",
                         " GROUP BY `wkl_uuid`, `vdate`, `band` HAVING COUNT(*) > 1) as outer_subset;")
      Result <- RMySQL::dbSendQuery(DBCon, prequery)
      vfids <- RMySQL::dbFetch(Result, n = -1)
      ## retrieve vf_uuids from all wkl_uuid-vdate-band combinations that have *only* secondary gtype_ranks
      prequery2 <- paste0("SELECT vfid, gtr FROM (SELECT `wkl_uuid`, `vdate`, `band`, GROUP_CONCAT(`vf_uuid`) as vfid, GROUP_CONCAT(`gtype_rank`) as gtr",
                          " FROM (SELECT * FROM `vframe` WHERE exec_uuid IN (", paste0("'", unique(OUT$executions$exec_uuid), "'", collapse = ", "), ") AND `gtype_rank` IN ('secondary')) as inner_subset",
                          " GROUP BY `wkl_uuid`, `vdate`, `band` HAVING COUNT(*) = 1) as outer_subset;")
      Result <- RMySQL::dbSendQuery(DBCon, prequery2)
      vfids2 <- RMySQL::dbFetch(Result, n = -1)
      vfid_secondary <- c(unlist(strsplit(vfids$vfid, ",")), unlist(strsplit(vfids2$vfid, ",")))
      ## retrieve actual vframe
      query <- paste0("SELECT `vframe`.* FROM `vframe`",
                      " JOIN executions USING (exec_uuid)",
                      " WHERE exec_uuid IN (", paste0("'", unique(OUT$executions$exec_uuid), "'", collapse = ", "), ")",
                      " AND",
                      " ( (gtype_rank IN ('secondary') AND vf_uuid IN (", paste0("'", vfid_secondary, "'", collapse = ", "), "))",
                      "  OR (gtype_rank IN ('primary') AND vf_uuid NOT IN (", paste0("'", vfid_secondary, "'", collapse = ", "), "))",
                      ifelse("tertiary" %in% gtype_rank,
                             yes = " OR gtype_rank IN ('tertiary') )",
                             no = " )"))
    } else {
      query <- paste0("SELECT `vframe`.* FROM `vframe`",
                      " JOIN executions USING (exec_uuid)",
                      " WHERE exec_uuid IN (", paste0("'", unique(OUT$executions$exec_uuid), "'", collapse = ", "), ")",
                      " AND gtype_rank IN (", paste0("'", gtype_rank, "'", collapse = ", "), ")")
    }
  }

  message("Retrieving vframe")
  if (verbose) message(query)
  Result <- RMySQL::dbSendQuery(DBCon, query)
  OUT$vframe <- RMySQL::dbFetch(Result, n = -1)

  ## Retrieve wkl
  query <- paste0("SELECT * FROM `wkl`")
  if (limitWKL2vframe) {
    query <- paste0(query, " WHERE wkl_uuid IN (", paste0("'", unique(OUT$vframe$wkl_uuid), "'", collapse = ", "), ")")
  }
  message("Retrieving wkl")
  if (verbose) message(query)
  Result <- RMySQL::dbSendQuery(DBCon, query)
  OUT$wkl <- RMySQL::dbFetch(Result, n = -1)

  ## Retrieve captWKL
  if (captWKL) {
    query <- paste0("SELECT * FROM `captWKL`",
                    " WHERE vf_uuid IN (", paste0("'", unique(OUT$vframe$vf_uuid), "'", collapse = ", "), ")")
    message("Retrieving captWKL")
    if (verbose) message(query)
    Result <- RMySQL::dbSendQuery(DBCon, query)
    OUT$captWKL <- RMySQL::dbFetch(Result, n = -1)
  }
  ## Retrieve poorWKL
  if (poorWKL) {
    query <- paste0("SELECT `poorWKL`.* FROM `poorWKL`",
                    " JOIN `executions` USING (exec_uuid)",
                    " WHERE exec_uuid IN (", paste0("'", unique(OUT$executions$exec_uuid), "'", collapse = ", "), ")",
                    ## don't take poorWKLs which are present in captWKLs (and overarching constraint of exec_uuid, which is satisfied by the more severe vf_uuid constraint):
                    " AND NOT EXISTS (SELECT * FROM `captWKL` WHERE vf_uuid IN (", paste0("'", unique(OUT$vframe$vf_uuid), "'", collapse = ", "), ")",
                                     " AND `captWKL`.vdate = `poorWKL`.vdate AND `captWKL`.vstation_id = `poorWKL`.vstation_id AND `captWKL`.depth = `poorWKL`.depth)",
                    " AND ", poorWKLconstraint)
    message("Retrieving poorWKL")
    if (verbose) message(query)
    Result <- RMySQL::dbSendQuery(DBCon, query)
    OUT$poorWKL <- RMySQL::dbFetch(Result, n = -1)
  }
  ## Retrieve studyArea
  if (studyArea) {
    vstations2get <- unique(c(OUT$captWKL$vstation, OUT$poorWKL$vstation))
    query <- paste0("SELECT `studyArea`.* FROM `studyArea`",
                    " WHERE `studyArea`.`vstation` IN (", paste0("'", vstations2get, "'", collapse = ", "), ")")
    message("Retrieving studyArea")
    if (verbose) message(query)
    Result <- RMySQL::dbSendQuery(DBCon, query)
    OUT$studyArea <- RMySQL::dbFetch(Result, n = -1)
  }

  OUT$call <- match.call()
  OUT$config <- formals()
  for (arg in names(OUT$call)) {
    if (arg %in% names(OUT$config)) {
      if (!arg %in% "DBCon") {
        OUT$config[[arg]] <- eval(OUT$call[[arg]])
        OUT$call[[arg]] <- eval(OUT$call[[arg]])
      }
    }
  }
  ## clean up
  RMySQL::dbClearResult(Result)
  if (closeDBCon) {
    ## Disconnect database connection
    RMySQL::dbDisconnect(DBCon)
    rm(DBCon)
  }
  ## brief checks and return
  flagid <- which(is.na(OUT$captWKL$capt_id) | is.na(OUT$captWKL$tliml))
  if (length(flagid) > 0) message("Some captWKL$capt_id and/or $tliml are NA, which should never be the case and has led to weird errors in the past. Check why and remove!")
  return(OUT)
}
