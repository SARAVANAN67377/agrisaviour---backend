# Agrisaviour Plumber API — risk analysis (+ optional health check)
# Run from repo root:
#   Rscript -e "plumber::plumb('r-api/plumber.R')$run(host='0.0.0.0', port=8000)"
# Or: open plumber.R in RStudio and "Run API"

library(plumber)
rf <- if (file.exists(file.path("r-api", "risk_model.R"))) {
  file.path("r-api", "risk_model.R")
} else if (file.exists("risk_model.R")) {
  "risk_model.R"
} else {
  stop("risk_model.R not found (run from project root or r-api/)")
}
source(rf, local = FALSE)

lf <- if (file.exists(file.path("r-api", "loan_model.R"))) {
  file.path("r-api", "loan_model.R")
} else if (file.exists("loan_model.R")) {
  "loan_model.R"
} else {
  stop("loan_model.R not found")
}
source(lf, local = FALSE)

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (identical(req$REQUEST_METHOD, "OPTIONS")) {
    res$status <- 204L
    return(list())
  }
  plumber::forward()
}

#* Health check
#* @get /health
function() {
  list(
    ok = TRUE,
    service = "agrisaviour-r-api",
    endpoints = c("/health", "/risk-analysis", "/loan-advice")
  )
}

#* Farmer risk analysis (same contract as frontend `riskApi.js`)
#* @post /risk-analysis
#* @serializer json
function(req) {
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(body)) {
    return(list(error = "Invalid JSON body"))
  }

  state <- body$state
  district <- body$district
  crop <- body$crop
  years <- body$years

  if (is.null(state) || !nzchar(as.character(state))) {
    return(list(error = "Missing state"))
  }
  if (is.null(district) || !nzchar(as.character(district))) {
    return(list(error = "Missing district"))
  }
  if (is.null(crop) || !nzchar(as.character(crop))) {
    return(list(error = "Missing crop"))
  }
  if (is.null(years)) years <- 3
  years <- suppressWarnings(as.integer(years))
  if (!years %in% c(1L, 3L, 5L)) {
    return(list(error = "years must be 1, 3, or 5"))
  }

  tryCatch(
    compute_risk_analysis(
      as.character(state),
      as.character(district),
      as.character(crop),
      years
    ),
    error = function(e) list(error = paste("compute_risk_analysis:", conditionMessage(e)))
  )
}

#* Safe loan advisory (same contract as LoanAdvisoryPage → OutputPanel / RiskExplanation)
#* @post /loan-advice
#* @serializer json
function(req) {
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(body)) {
    return(list(error = "Invalid JSON body"))
  }

  state <- body$state
  district <- body$district
  crop <- body$crop
  land <- body$land
  loan_requested <- body$loan_requested
  season <- body$season
  irrigation <- body$irrigation

  if (is.null(state) || !nzchar(as.character(state))) {
    return(list(error = "Missing state"))
  }
  if (is.null(crop) || !nzchar(as.character(crop))) {
    return(list(error = "Missing crop"))
  }
  if (is.null(land)) {
    return(list(error = "Missing land (hectares)"))
  }
  land <- suppressWarnings(as.numeric(land))
  if (is.na(land) || land < 0) {
    return(list(error = "land must be a non-negative number (hectares)"))
  }

  crop <- as.character(crop)
  if (is.null(CROP_DATA[[crop]])) {
    return(list(error = paste0("Unknown crop: ", crop)))
  }

  if (is.null(loan_requested)) loan_requested <- 0
  loan_requested <- suppressWarnings(as.numeric(loan_requested))
  if (is.na(loan_requested) || loan_requested < 0) {
    return(list(error = "loan_requested must be non-negative"))
  }

  tryCatch(
    compute_loan_advice(
      as.character(state),
      if (is.null(district)) "" else as.character(district),
      crop,
      land,
      loan_requested,
      season,
      irrigation
    ),
    error = function(e) list(error = paste("compute_loan_advice:", conditionMessage(e)))
  )
}


