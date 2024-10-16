


GISAID_URL = "https://www.epicov.org/epi3/frontend"

headers = c(
  "accept" = "application/json, text/javascript, */*; q=0.01",
  "content-type" = "application/x-www-form-urlencoded; charset=UTF-8",
  "referer" = "https://www.epicov.org/epi3/frontend",
  "sec-ch-ua-mobile" = "?0",
  "sec-ch-ua-platform" = "Windows"
)

display_list <- function(data){
  paste(
    unlist(
      lapply(names(data), function(name){paste0(name, "=", data[[name]])})
      ),
    collapse=" "
  )
}

timestamp <- function() {
  return(as.character(as.integer(Sys.time()) * 1000))
}

createCommand <-
  function(wid,
           pid,
           cid,
           cmd,
           params = setNames(list(), character(0)),
           equiv = NULL) {
    ev = list(
      wid = wid,
      pid = pid,
      cid = cid,
      cmd = cmd,
      params = params,
      equiv = equiv
    )
    return(ev)
  }

formatDataForRequest <-
  function(sid, wid, pid, queue, timestamp, mode = 'ajax') {
    data <- paste0(
      "sid=",
      sid,
      "&wid=",
      wid,
      "&pid=",
      pid,
      "&data=",
      utils::URLencode(rjson::toJSON(queue), reserved = TRUE),
      "&ts=",
      timestamp,
      "&mode=",
      mode
    )
    return(data)
  }

parseResponse <- function(response) {
  j = httr::content(response, as = 'parsed')
  if (length(j$responses) == 0 & length(j) == 2) {
    log.warn(paste("The response data has unexpected content:", display_list(j)))
    stop(log.error("Unhandled response Error."))
  }
  if (isTRUE(grep('Error', j$responses[[1]]$data) == 1)) {
    log.warn(utils::URLdecode(strsplit(j$responses[[1]]$data, '"')[[1]][2]))
    stop(log.error("Internal server Error."))
  }
  if (isTRUE(grep('expired', j$responses[[1]]$data) == 1)) {
    stop(log.error("The session has expired. Please login again."))
  }
  if (isTRUE(grep('password', j$responses[[1]]$data) == 1)) {
    # make a better check
    stop(log.error("Username or password wrong!"))
  }
  if (isTRUE(grep('No data.', j$responses[[1]]$data) == 1)) {
    # make a better check
    stop(log.error("No data found."))
  }
  if (isTRUE(grep('captcha', j$responses[[1]]$data) == 1)) {
    # make a better check
    stop(log.error("Captcha window encountered, access denied."))
  }
  return(j)

}



get_accession_ids <- function(credentials) {
  # select all check box
  #onclick="sys.getC("c_rfsc9v_w9").selectAll(this)"
  queue = list()
  # data: {"queue":[{"wid":"wid_rfsc9v_2ktt","pid":"pid_rfsc9v_2kwd","cid":"c_rfsc9v_w9","cmd":"CallAsync","params":{"col_name":"c","checked":true,"_async_cmd":"SelectAll"},"equiv":null}]}
  command <- createCommand(
    wid = credentials$wid,
    pid = credentials$pid,
    cid = credentials$query_cid,
    cmd = 'CallAsync',
    params = list(col_name = 'c', checked = TRUE, '_async_cmd' = 'SelectAll')
  )
  queue <- append(queue, list(command))
  command_queue <- list(queue = queue)

  data <-
    formatDataForRequest(
      sid = credentials$sid,
      wid = credentials$wid,
      pid = credentials$pid,
      queue = command_queue,
      timestamp = timestamp()
    )
  log.debug("Sending request: get_accession_ids call_async")
  response      <- send_request(method="POST", data=data)
  response_data <- httr::content(response, as = 'parsed')
  display_response <- display_list(response_data)

  # Truncate long responses
  if (length(display_response) > 1000){
    display_response <- paste(substr(display_list(response_data), 1, 1000), "...")
  }
  log.debug(sprintf("Received response: get_accession_ids call_async: %s", display_response))

  # {"callback_response": {"msg": null, "async_id": "_rfsc9v_2o8a"}, "__ready__": true}
  # wait for selection
  # extract check_async
  check_async_id = response_data$callback_response$async_id
  # while generateDownloadDone not ready
  is_ready = FALSE
  while (!is_ready) {
    base_url= paste0("https://www.epicov.org/epi3/check_async/", check_async_id)
    parameter_string <- paste0("_=", timestamp())
    log.debug("Sending request: get_accession_ids check_async")
    response <- send_request(method="GET", base_url=base_url, parameter_string=parameter_string)
    response_data <- parseResponse(response)
    log.debug(sprintf("Received response: get_accession_ids check_async: %s", display_list(response_data)))
    is_ready <- response_data$is_ready
  }

  # select button
  selection_pid_wid <- get_selection_panel(credentials$sid, credentials$wid, credentials$pid, credentials$query_cid)
  parameter_string  <- paste0('sid=', credentials$sid, '&pid=', selection_pid_wid$pid)
  log.debug("Sending request: get_accession_ids selection_page")
  selection_page    <- send_request(method="GET", parameter_string=parameter_string)
  log.debug("Received response: get_accession_ids selection_page")

  # csv button
  #{"queue":[{"wid":"wid_rfsc9v_2p1c","pid":"pid_rfsc9v_2p1d","cid":"c_rfsc9v_15u","cmd":"Download","params":{},"equiv":null}]}
  queue = list()
  command <- createCommand(
    wid = selection_pid_wid$wid,
    pid = selection_pid_wid$pid,
    cid = credentials$selection_panel_cid,
    cmd = 'Download',
    params = setNames(list(), character(0))
  )
  queue <- append(queue, list(command))
  command_queue <- list(queue = queue)

  data <-
    formatDataForRequest(
      sid = credentials$sid,
      wid = credentials$wid,
      pid = credentials$pid,
      queue = command_queue,
      timestamp = timestamp()
    )

  log.debug("Sending request: get_accession_ids download")
  response      <- send_request(method="POST", data=data)
  response_data <- httr::content(response, as = 'parsed')
  log.debug(sprintf("Received response: get_accession_ids download: %s", display_list(response_data)))

  url <- extract_first_match("sys.downloadFile\\(\"(.*)\",", response_data$responses[[1]]$data)
  csv_url <- paste0('https://www.epicov.org/', url)
  log.debug(sprintf("get_accession_ids csv: %s", url))
  tryCatch({
    df <- read.csv(csv_url, header=F, col.names = c('accession_id'))
    },
    error = function(e) df <- data.frame(col.names = c('accession_id'))
  )
  # back
  send_back_cmd(credentials$sid, selection_pid_wid$wid, selection_pid_wid$pid, credentials$selection_panel_cid)
  resetQuery(credentials)
  return(df)
}

get_selection_panel <- function(session_id, WID, customSearch_page_ID, query_cid) {
  # selection changes every time you open it
  selection_command <- createCommand(
    wid = WID,
    pid = customSearch_page_ID,
    cid = query_cid,
    cmd = 'Selection',
    params = setNames(list(), character(0)) #hack for empty {}
  )
  queue <- list(queue = list(selection_command))

  data <- formatDataForRequest(session_id, WID, customSearch_page_ID, queue, timestamp())

  log.debug(sprintf("Sending request: get_selection_panel"))
  response <- send_request(method='POST', data=data)
  response_data <- parseResponse(response)
  log.debug(sprintf("Received response: get_selection_panel: %s", display_list(response_data)))

  # extract PID
  # selection changes every time
  selection_pid <-
    strsplit(response_data$responses[[1]]$data, "'")[[1]][4]
  selection_wid <-
    strsplit(response_data$responses[[1]]$data, "'")[[1]][2]
  list(pid=selection_pid, wid=selection_wid)
}

get_download_panel <- function(session_id, WID, customSearch_page_ID, query_cid) {
  # selection changes every time you open it
  selection_command <- createCommand(
    wid = WID,
    pid = customSearch_page_ID,
    cid = query_cid,
    cmd = 'DownloadAllSequences',
    params = setNames(list(), character(0)) #hack for empty {}
  )
  log.debug(sprintf("selection_command: %s", display_list(selection_command)))
  queue <- list(queue = list(selection_command))

  data <- formatDataForRequest(session_id, WID, customSearch_page_ID, queue, timestamp())
  log.debug(sprintf("get_download_panel (data): %s", display_list(data)))
  response <- send_request(method='POST', data=data)

  response_data <- parseResponse(response)
  log.debug(sprintf("get_download_panel_pid_wid (response_data): %s", display_list(response_data)))
  # extract PID
  # selection changes every time
  download_pid <-
    strsplit(response_data$responses[[1]]$data, "'")[[1]][4]
  download_wid <-
    strsplit(response_data$responses[[1]]$data, "'")[[1]][2]

  list(pid=download_pid, wid=download_wid)
}

send_back_cmd <- function(session_id, WID, PID, CID ) {
  # send back command to get back to page
  # {"queue":[{"wid":"wid_r8fuui_7jgp","pid":"pid_r8fuui_7jgq","cid":"c_r8fuui_3uj","cmd":"Back","params":{},"equiv":null}]}
  selection_command <- createCommand(
    wid = WID,
    pid = PID,
    cid = CID,
    cmd = 'Back',
    params = setNames(list(), character(0)) #hack for empty {}
  )
  queue <- list(queue = list(selection_command))

  data <- formatDataForRequest(session_id, WID, PID, queue, timestamp())

  response <- send_request(method='POST', data=data)
  response_data <- parseResponse(response)
  log.debug(sprintf("send_back_cmd (response_data): %s", display_list(response_data)))
}

select_entries <- function(credentials, list_of_accession_ids) {
  accession_ids_string <- paste(list_of_accession_ids, collapse=", ")

  selection_pid_wid <- get_selection_panel(credentials$sid, credentials$wid, credentials$pid, credentials$query_cid)

  #load panel
  selection_page <-
    send_request(paste0('sid=', credentials$sid, '&pid=', selection_pid_wid$pid))

  ev1 <- createCommand(
    wid = selection_pid_wid$wid,
    pid = selection_pid_wid$pid,
    cid = credentials$selection_panel_cid,
    cmd = 'setTarget',
    params = list(cvalue=accession_ids_string, ceid=credentials$selection_ceid), #hack for empty {}
    equiv = paste0("ST", credentials$selection_ceid)
  )

  ev2 <- createCommand(
    wid = selection_pid_wid$wid,
    pid = selection_pid_wid$pid,
    cid = credentials$selection_panel_cid,
    cmd = 'ChangeValue',
    params = list(cvalue=accession_ids_string, ceid=credentials$selection_ceid), #hack for empty {}
    equiv = paste0("CV", credentials$selection_ceid)
  )
  ev3 <- createCommand(
    wid = selection_pid_wid$wid,
    pid = selection_pid_wid$pid,
    cid = credentials$selection_panel_cid,
    cmd = 'OK',
    params = setNames(list(), character(0)) #hack for empty {}
  )
  json_queue <- list(queue = list(ev1, ev2, ev3))
  data       <- formatDataForRequest(credentials$sid, selection_pid_wid$wid, selection_pid_wid$pid, json_queue, timestamp())
  log.debug("Sending request: select_entries") 
  response      <- send_request(method='POST', data=data)
  response_data <- parseResponse(response)
  log.debug(sprintf("Received response: select_entries: %s", display_list(response_data)))  
  if (isTRUE(grep('Back', response_data$responses[[2]]$data) == 1)) {
    send_back_cmd(credentials$sid, selection_pid_wid$wid, selection_pid_wid$pid, credentials$selection_panel_cid)
  }

  return(response)
}

resetQuery <- function(credentials) {
  queue = list()
  command <- createCommand(
    wid = credentials$wid,
    pid = credentials$pid,
    cid = credentials$search_cid,
    cmd = "Reset"
  )
  queue <- append(queue, list(command))
  command_queue <- list(queue = queue)
  data <-
    formatDataForRequest(
      sid = credentials$sid,
      wid = credentials$wid,
      pid = credentials$pid,
      queue = command_queue,
      timestamp = timestamp()
    )
  response <- send_request(method="POST", data=data)
}

extract_search_ceid <- function(identifier, t) {
  regex <- paste0(".createFI\\('(.*)','.*Widget','", identifier)
  log.debug(sprintf("Extracting '%s' from '%s'", regex, substr(t, 0, 30)))
  ceid <-
    regmatches(t,
               regexpr(
                 regex,
                 t,
                 perl = TRUE
               ))
  ceid <- strsplit(ceid, "'")
  tryCatch(
    ceid <- ceid[[1]][length(ceid[[1]]) - 4],
    error = function(e) {
      log.warn(paste0("Could not extract ", regex, " from ", substr(t, 0, 30)))
      e
    }
  )

  return(ceid)
}

log.debug <- function(msg) {
  if (Sys.getenv("GISAIDR_DEBUG") == 1) {
    message(paste0(Sys.time(), "\tDEBUG: ", gsub("\n", " ", msg)))
  }
  invisible()
}

log.error <- function(msg) {
  message(paste0(Sys.time(), "\tERROR: ", gsub("\n", " ", msg)))
  flush.console()
  invisible()
}

log.warn <- function(msg) {
  message(paste0(Sys.time(), "\tWARNING: ", gsub("\n", " ", msg)))
  flush.console()
  invisible()
}

log.info <- function(msg, level=1) {
  if (Sys.getenv("GISAIDR_VERBOSITY") != ""){
    verbosity <- Sys.getenv("GISAIDR_VERBOSITY")
  } else {
    verbosity <- 1
  }
  if (verbosity >= level){
    message(paste0(Sys.time(), "\tINFO: ", gsub("\n", " ", msg)))
  }
  flush.console()
  invisible()
}

send_request <-
  function(parameter_string = "",
           data = NULL,
           method = 'GET',
           sleep_min = 5,
           sleep_max = 10,
           base_url = GISAIDR::GISAID_URL
           ) {
    # Set sleep timer
    sleep_min_env <- Sys.getenv("GISAIDR_SLEEP_MIN")
    if (sleep_min_env != "" && sleep_min_env > 0){
      sleep_min <- as.numeric(sleep_min_env)
    }
    sleep_max_env <- Sys.getenv("GISAIDR_SLEEP_MAX")
    if (sleep_max_env != "" && sleep_max_env > 0){
      sleep_max <- as.numeric(sleep_max_env)
    }

    URL <- paste0(base_url, '?', parameter_string)
    if (is.null(data)) {
      data <- ""
    }
    log.debug(sprintf("Method->%s, URL->%s, data->%s, headers->%s", method, URL, data, display_list(headers)))
    random_sleep <- sleep_min + (sleep_max - sleep_min) * runif(1)
    log.debug(sprintf("Sleep for %s seconds.", random_sleep))
    Sys.sleep(random_sleep)
    if (method == 'GET') {
      response <- httr::GET(URL)
    } else if (method == 'POST') {
      response <-
        httr::POST(URL, httr::add_headers(.headers = GISAIDR::headers), body = data)
    } else {
      stop(log.error(sprintf("Method '%s' not allowed", method)))
    }
    if (response$status_code >= 500) {
      log.warn(sprintf("An error occurred while trying to %s %s", method, URL))
      stop(log.error("Server error!"))
    }
    response
  }

extract_first_match <- function(regex, text) {
  log.debug(sprintf("Extracting '%s' from '%s'", regex, substr(text, 0, 30)))
  matches <- regmatches(text, regexec(regex, text))
  return(matches[[1]][[2]])
}


go_to_page <- function(session_id, WID, PID, CID, link) {
  go_command <- createCommand(
    wid = WID,
    pid = PID,
    cid = CID,
    cmd = 'Go',
    params = list(link = link)
  )

  queue <- list(queue = list(go_command))

  data <-
    formatDataForRequest(session_id, WID, PID, queue, timestamp())

  response <- send_request(data)
  response_data <- parseResponse(response)
  return(response_data)
}

#
# Variants <-
#   list(
#     alpha = 'B.1.1.7 / Q.*',
#     beta = 'B.1.351 / B.1.351.2 / B.1.351.3',
#     gamma = 'P.1 / P.1.*',
#     delta = 'B.1.617.2 / AY.*',
#     epsilon = 'B.1.427 / B.1.429',
#     eta = 'B.1.525',
#     iota = 'B.1.526',
#     kappa = 'B.1.617.1',
#     lambda = 'C.37 / C.37.1',
#     mu = 'B.1.621 / B.1.621.1',
#     omicron = 'B.1.1.529 / BA.*',
#     GH_490R = 'B.1.640 / B.1.640.*'
#   )


create_batches <- function(start_index, nrows, batch_size=50) {
  batches <- cbind(
      seq(0,nrows,batch_size),
      c(seq(batch_size,nrows,batch_size),
        nrows)
  )
  batches <- batches + start_index
  if (batches[nrow(batches),1] - batches[nrow(batches),2] == 0) {
    batches <- head(batches, -1)
  }
  #colnames(batches) <- c('start_index', 'nrows')
  return (batches)
}

covid_order_by_col_map <-
  list(
    id = "b",
    virus_name = "d",
    passage_details_history = "e",
    accession_id = "f",
    collection_date = "g",
    submission_date = "h",
    information = "i",
    length = "j",
    host = "k",
    location = "l",
    originating_lab = "m",
    submitting_lab = "n"
  )

other_order_by_col_map <-
  list(
    id = "b",
    virus_name = "d",
    passage_details_history = "e",
    accession_id = "f",
    collection_date = "g",
    submission_date = "h",
    information = "i",
    length = "j",
    location = "k",
    originating_lab = "l",
    submitting_lab = "m"
  )


setColumnNames <- function(df, database) {
  if (database == 'EpiRSV') {
    names(df)[names(df) == "b"] <- "id"
    names(df)[names(df) == "d"] <- "virus_name"
    names(df)[names(df) == "e"] <- "passage_details_history"
    names(df)[names(df) == "f"] <- "accession_id"
    names(df)[names(df) == "g"] <- "collection_date"
    names(df)[names(df) == "h"] <- "submission_date"
    names(df)[names(df) == "i"] <- "information"
    names(df)[names(df) == "j"] <- "length"
    names(df)[names(df) == "k"] <- "location"
    names(df)[names(df) == "l"] <- "originating_lab"
    names(df)[names(df) == "m"] <- "submitting_lab"
  } else if (database == 'EpiPox') {
    names(df)[names(df) == "b"] <- "id"
    names(df)[names(df) == "d"] <- "virus_name"
    names(df)[names(df) == "e"] <- "passage_details_history"
    names(df)[names(df) == "f"] <- "accession_id"
    names(df)[names(df) == "g"] <- "collection_date"
    names(df)[names(df) == "h"] <- "submission_date"
    names(df)[names(df) == "i"] <- "information"
    names(df)[names(df) == "j"] <- "length"
    names(df)[names(df) == "k"] <- "location"
    names(df)[names(df) == "l"] <- "originating_lab"
    names(df)[names(df) == "m"] <- "submitting_lab"
  } else {
    colnames(df)[colnames(df) %in% c("b", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n")] <-
      c(
        "id",
        "virus_name",
        "passage_details_history",
        "accession_id",
        "collection_date",
        "submission_date",
        "information",
        "length",
        "host",
        "location",
        "originating_lab",
        "submitting_lab"
      )
  }
  return(df)
}

setDataTypes <- function(df) {
  # date
  return(df)
}

getWebRows <- function(database) {
  # TBD check this!
  if (database == 'EpiFlu') { return(27) }
  else { return(50) }
}

create_search_queue <- function(credentials, ceid, cvalue, cmd) {
  queue = list()
  command <- createCommand(
    wid = credentials$wid,
    pid = credentials$pid,
    cid = credentials$search_cid,
    cmd = 'setTarget',
    params = list(cvalue = cvalue, ceid = ceid),
    equiv = paste0('ST', ceid)
  )
  queue <- append(queue, list(command))

  command <- createCommand(
    wid = credentials$wid,
    pid = credentials$pid,
    cid = credentials$search_cid,
    cmd = 'ChangeValue',
    params = list(cvalue = cvalue, ceid = ceid),
    equiv = paste0('CV', ceid)
  )

  queue <- append(queue, list(command))

  command <- createCommand(
    wid = credentials$wid,
    pid = credentials$pid,
    cid = credentials$search_cid,
    cmd = cmd,
    params = list(ceid = ceid),
  )

  queue <- append(queue, list(command))

  return(queue)
}

readFasta <- function(file_path){
  sample <- NULL
  sequence <- NULL
  records <- list()
  for (line in readLines(file_path)){
    # Skip empty lines
    if (line == ""){ next }
    if ( startsWith(line, ">") ){
      # first record
      if (is.null(sample)){ 
        sample <- gsub(">", "", line)
      } else {
        records[sample] <- sequence
        sample <- gsub(">", "", line)
        sequence <- NULL
      }
    } else {
      sequence <- paste0(sequence, line)
    }
  }
  # Add the final record
  records[sample] <- sequence

  return(records)
}