## This script contains very useful functions to conveniently interact with the data base `wklvalidation`
## All functions in here are used to fill or manipulate data in the data base
## For functions to read data from the DB, see file DB-interactions2.R

#' Retrieve wkl_uuids from DB
#'
#' This function retrieves the wkl_uuids alongside the wkl_name, season, region, dq from the `wkl` table.
#' @param DBCon DB connection
#' @param wkl_name wkl_name of format 'season.name'
#' @param region DB query condition
#'
#' @return data.frame
#'
#' @export
getWKLuuid <- function(DBCon, wkl_name, region = NULL) {

  query <- paste0("SELECT `wkl_uuid`, `wkl_name`, `season`, `region`, `dq`, `comment` FROM `wkl`",
                  " WHERE `wkl_name` IN (", paste0("'", wkl_name, "'", collapse = ", "), ")")
  if (!is.null(region)) query <- paste0(query,
                                        " AND `region` IN (", paste0("'", region, "'", collapse = ", "), ")")

  ## Retrieve data
  Result <- RMySQL::dbSendQuery(DBCon, query)
  ResultDF <- RMySQL::dbFetch(Result, n = -1)
  RMySQL::dbClearResult(Result)

  if (nrow(ResultDF) == 0) warning("No wkl in DB with desired name(s)!")

  return(ResultDF)
}



## --- saveExecutionsTable2DB ----
#' Write execution information to mysql database at cedar/steed
#'
#' @param DBCon DB connection
#' @param execFrame data frame 1 row including execution details
#'
#' @return no return, but writes to DB
#' @export
saveExecutionsTable2DB <- function(DBCon, execFrame) {
  if (!all(c("exec_uuid", "vdir", "region", "season", "band", "flat", "pm", "prcp") %in% names(execFrame))) stop("execFrame missing columns!")
  if (execFrame$vdir == 1 && !execFrame$tw %in% c("wx", "fix")) stop("For 'vdir == 1', 'tw' must be either 'wx' or 'fix'!")

  succ <- insertTable(DBCon, execFrame, "executions")
  if (inherits(succ, "error")) {
    try({message(paste0("\nError in saveExecutionsTable2DB, query:\n ", succ$query, "\n"))}, silent = TRUE)
    stop(succ)
  }
  message(paste0("Wrote to executions table. exec_uuid ", execFrame$exec_uuid))
  # RMySQL::dbWriteTable(DBCon, 'executions', execFrame, append = TRUE, row.names = FALSE)
}



## --- saveCapturedLayers2DB ----
#' Save all captured layers to mysql database at cedar/steed
#'
#' @param profile profile which contains captured layers
#' @param idx indices of captured layers which will be saved to DB
#' @param DBCon DB connection
#' @param extdata 1 row data.frame with columns exec_uuid, wkl_uuid, vf_uuid, vdate
#'
#' @return no return, but writes data to DB
#' @export
saveCapturedLayers2DB <- function(profile, idx, DBCon, extdata) {

  ## assemble data frame with correct order and names
  vars <- c("height", "depth", "thickness", "ddate", "bdate", "density", "gsize", "gtype", "hardness", "shear_strength", "sk38", "crit_cut_length", "rta", "p_unstable", "slab_rhogs", "slab_rho")
  vars_rename <- c("height", "depth", "thickness", "ddate", "bdate", "density", "gsize", "gtype", "hardness", "sstrength", "sk38", "rc", "rta", "pu", "sl_rhogs", "sl_rho")
  vars_avail <- which(vars %in% names(profile$layers))

  captlyrs <- profile$layers[idx, vars[vars_avail]]
  names(captlyrs) <- vars_rename[vars_avail]
  captlyrs <- data.frame(extdata, vstation = profile$station, vstation_id = profile$station_id,
                         angle = profile$angle, aspect = profile$aspect,
                         captlyrs,
                         tliml = profile$timelimit_lower, tlimu = profile$timelimit_upper)
  # ## ensure character gtypes
  # captlyrs$gtype <- as.character(captlyrs$gtype)
  # ## handle dates
  # captlyrs$ddate <- as.character(format(captlyrs$ddate, format = "%Y-%m-%d %H:%M:%S"))
  # captlyrs$bdate <- as.character(format(captlyrs$bdate, format = "%Y-%m-%d %H:%M:%S"))
  # ## ensure strings are enquoted with single quotes
  # strcols <- c("exec_uuid", "wkl_uuid", "vf_uuid", "vdate", "ddate", "bdate", "vstation_id", "gtype")
  # captlyrs[, strcols] <- sQuote(captlyrs[, strcols], q = FALSE)
  # ## assemble query
  # query <- paste0('INSERT INTO `captWKL` (', paste0(colnames(captlyrs), collapse = ','),') VALUES ')
  # vals <- paste0('(', paste0(captlyrs, collapse = ','), ')')
  # query <- paste0(query, vals)
  # ## ensure NAs get inserted as NULL
  # query <- gsub("'NA'", "NULL", query)
  # query <- gsub("NA", "NULL", query)
  # query <- gsub("NaN", "NULL", query)
  # query <- gsub("'NULL'", "NULL", query)

  succ <- insertTable(DBCon, captlyrs, "captWKL")
  if (inherits(succ, "error")) {
    try({message(paste0("\nError in saveCapturedLayers2DB, query:\n ", succ$query, "\n"))}, silent = TRUE)
    stop(succ)
  }
}


## --- savePoorLayers2DB ----
#' Save all layers with poor stability to mysql database at cedar/steed
#'
#' @param lyrs (stacked) profile layers incl. station(_id), angle, aspect
#' @param idx indices of poor layers which will be saved to DB
#' @param DBCon DB connection
#' @param extdata 1 row data.frame with columns exec_uuid, vdate, nSP
#'
#' @return no return, but writes data to DB
#' @export
savePoorLayers2DB <- function(lyrs, idx, DBCon, extdata) {

  ## assemble data frame with correct order and names
  vars <- c("station", "station_id", "angle", "aspect", "height", "depth", "thickness", "ddate", "bdate", "density", "gsize", "gtype", "hardness", "shear_strength", "sk38", "crit_cut_length", "rta", "p_unstable", "slab_rhogs", "slab_rho")
  vars_rename <- c("vstation", "vstation_id", "angle", "aspect","height", "depth", "thickness", "ddate", "bdate", "density", "gsize", "gtype", "hardness", "sstrength", "sk38", "rc", "rta", "pu", "sl_rhogs", "sl_rho")
  vars_avail <- which(vars %in% names(lyrs))

  poorlyrs <- lyrs[idx, vars[vars_avail]]
  names(poorlyrs) <- vars_rename[vars_avail]
  poorlyrs <- data.frame(extdata, poorlyrs)

  succ <- insertTable(DBCon, poorlyrs, "poorWKL")
  if (inherits(succ, "error")) {
    try({message(paste0("\nError in savePoorLayers2DB, query:\n ", succ$query, "\n"))}, silent = TRUE)
    stop(succ)
  }
}


## --- insertTable ----
#' Custom insertTable command
#'
#' Equivalent to `RMySQL::dbWriteTable`, appends into existing table, but issues error if query can't fully be executed.
#' Note that the data.frame to be inserted needs to be named correctly and columns need to be ordered correctly. Data types correctly.
#' Character arrays will be quoted automatically, NA/NaN/NULL values will be handled automatically, and factors will be converted to characters.
#'
#' @param DBCon DB connection
#' @param DF data.frame in correct order of columns, correct colnames, and correct data types (see Description)
#' @param tblname table to insert into (must exist)
#'
#' @return Returns TRUE if successful, returns the error silently otherwise. The query that caused the error is stored in `$query`!
#' Tip: Wrap function call in tryCatch to catch error and informative traceback etc..
#'
#' @export
insertTable <- function(DBCon, DF, tblname) {

  nDF <- nrow(DF)
  if (nDF > 10000) stop("DF too large, use different (more efficient) insert method!")

  ## convert factors and Date(s) to characters
  dtypes <- sapply(DF, function(x) class(x)[1])
  cols <- which(dtypes %in% c("factor", "Date"))
  DF[, cols] <- as.character(DF[, cols])

  ## handle datetimes (POSIXct)
  cols <- which(dtypes %in% c("POSIXct", "POSIXlt", "POSIX"))
  for (col in cols) {
    DF[, col] <- as.character(format(DF[, col], format = "%Y-%m-%d %H:%M:%S"))
  }

  ## ensure strings are enquoted with single quotes
  dtypes <- sapply(DF, function(x) class(x)[1])  # need to update dtypes
  cols <- which(dtypes %in% c("character"))
  for (col in cols) {
    DF[, col] <- sQuote(DF[, col], q = FALSE)
  }

  ## assemble query
  query <- paste0('INSERT INTO `', tblname, '` (', paste0(colnames(DF), collapse = ','),') VALUES ')
  vals <- sapply(seq(nDF), function(j) {
    paste0('(', paste0(DF[j, ], collapse = ','), ')')
  })
  query <- paste0(query, paste0(vals, collapse = ','))

  ## ensure NAs get inserted as NULL
  query <- gsub("'NA'", "NULL", query)
  query <- gsub("NA", "NULL", query)
  query <- gsub("'NaN'", "NULL", query)
  query <- gsub("NaN", "NULL", query)
  query <- gsub("'NULL'", "NULL", query)

  succ <- tryCatch(DBI::dbExecute(DBCon, query), error = function(err) {
    err$query <- query
    err
  })

  if (inherits(succ, "error")) return(succ)
  else return(TRUE)
}



## ---deleteExecutionFromDB----
#' Delete all data related to exec_uuid from entire database
#'
#' User will be prompted before final deletion!
#'
#' remote usage prerequisite: on a terminal keep running SSH tunnel:
#' `ssh -L 8888:cedar-mysql-vm.int.cedar.computecanada.ca:3306 fherla@cedar.computecanada.ca`
#' @export
deleteExecutionFromDB <- function(exec_uuid, DBCon = NULL, DB = "Steed_DB", rf = FALSE,
                                  Renviron_path = ifelse(Sys.getenv("USER") == "flo",
                                                         "/home/flo/documents/sfu/code/cedarstuff/CedarTunnel.Renviron",
                                                         "/home/fherla/Cedar.Renviron"),
                                  verPlot_path = ifelse(DB == "Steed_DB",
                                                        "/home/flo/documents/sfu/code/rpackages_SARP/sarp.2021.herla.snowprofilevalidation/output/figures/validationVerificationPlots/",
                                                        "/home/fherla/project/fherla/snowprofilevalidation/output/figures/validationVerificationPlots/")) {

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



  ## handle multi-exec_uuid deletions: recursively and forced without user prompt
  if (length(exec_uuid) > 1) {
    if (rf) {
      for (eid in exec_uuid) {
        poorR <- DBI::dbExecute(DBCon, paste0("DELETE FROM poorWKL WHERE exec_uuid = '", eid, "'"))
        captR <- DBI::dbExecute(DBCon, paste0("DELETE FROM captWKL WHERE exec_uuid = '", eid, "'"))
        vfR <- DBI::dbExecute(DBCon, paste0("DELETE FROM vframe WHERE exec_uuid = '", eid, "'"))
        execR <- DBI::dbExecute(DBCon, paste0("DELETE FROM executions WHERE exec_uuid = '", eid, "'"))
        message(paste0("Deleted ", eid, " (corresp. dir: ", substr(eid, 1, 8), ")"))
      }
      message("")
      message(paste0("Check successful deletion manually in the executions table of the DB and try deleting the paths manually:"))
      message(paste0("1. cd ", verPlot_path, "REGION"))
      message(paste0("2. rm -rf corresponding directories above"))

    } else {
      stop("Deleting more than one exec_uuid at a time is only possible if you are really certain about this and set the flag `rf = TRUE`!")
    }

  } else {

    ## handle single exec_uuid case with extra user prompt and safety net

    ## Retrieve info about exec_uuid entries:
    exec <- DBI::dbGetQuery(DBCon, paste0("SELECT * FROM executions WHERE exec_uuid = '", exec_uuid, "'"))
    vf <- DBI::dbGetQuery(DBCon, paste0("SELECT * FROM vframe WHERE exec_uuid = '", exec_uuid, "'"))
    capt <- DBI::dbGetQuery(DBCon, paste0("SELECT * FROM captWKL WHERE exec_uuid = '", exec_uuid, "'"))
    poor <- DBI::dbGetQuery(DBCon, paste0("SELECT * FROM poorWKL WHERE exec_uuid = '", exec_uuid, "'"))

    ppath_exists <- FALSE
    if (!is.null(verPlot_path)) {
      ppath <- paste0(verPlot_path, exec$region, "/", substr(exec_uuid, 1, 8))
      ppath_exists <- dir.exists(ppath)
    }

    ## message and prompt
    message(paste0("You're about to delete the following execution from the database", ifelse(closeDBCon, DB, ""), ":\n",
                   paste0(capture.output(exec), collapse = "\n"), "\n",
                   "vframe: ", nrow(vf), " rows\n",
                   "captWKL: ", nrow(capt), " rows\n",
                   "poorWKL: ", nrow(poor), " rows\n",
                   "png files: ", ifelse(ppath_exists, paste0(length(list.files(ppath)), " in ", ppath, "\n"), "0 (?)\n"),
                   "Do you want to proceed? (y/n)"))
    prompt <- readline()

    if (prompt %in% c("y", "Y")) {
      poorR <- DBI::dbExecute(DBCon, paste0("DELETE FROM poorWKL WHERE exec_uuid = '", exec_uuid, "'"))
      captR <- DBI::dbExecute(DBCon, paste0("DELETE FROM captWKL WHERE exec_uuid = '", exec_uuid, "'"))
      vfR <- DBI::dbExecute(DBCon, paste0("DELETE FROM vframe WHERE exec_uuid = '", exec_uuid, "'"))
      execR <- DBI::dbExecute(DBCon, paste0("DELETE FROM executions WHERE exec_uuid = '", exec_uuid, "'"))

      if (all(c(nrow(exec), nrow(vf), nrow(capt), nrow(poor)) == c(execR, vfR, captR, poorR))) {
        message("DB deletion successful!")
      } else {
        message(paste0("Deleted some, but not all rows! Number of rows with specified exec_uuid remaining in\n",
                       "executions:", nrow(exec) - execR, " rows\n",
                       "vframe: ", nrow(vf) - vfR, " rows\n",
                       "captWKL: ", nrow(capt) - captR, " rows\n",
                       "poorWKL: ", nrow(poor) - poorR, " rows\n"))
      }

      if (ppath_exists) {
        if (unlink(ppath, recursive = TRUE) == 0) message("PNG deletion successful!")
      } else {
        message(paste0("Could not delete png files in\n", ppath))
      }

    } else {
      message("Aborting, nothing deleted!")
    }

  }  # END IF multi or single exec_uuid case

  if (closeDBCon) {
    ## Disconnect database connection
    RMySQL::dbDisconnect(DBCon)
    rm(DBCon)
  }
}

