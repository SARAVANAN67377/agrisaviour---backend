# Loan advisory — aligned with src/loanAdvisoryLogic.js (crop names: Rice, Wheat, …)

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1L && is.na(x))) y else x

COST_FRACTION <- 0.35
SAFE_REPAYMENT_SHARE <- 0.5
FLAT_RATE_PER_YEAR <- 0.12
DEFAULT_TERM_MONTHS <- 18L
HECTARES_TO_ACRES <- 2.47105

CROP_DATA <- list(
  Rice = list(yield_per_acre = 25, price_per_quintal = 2000),
  Wheat = list(yield_per_acre = 22, price_per_quintal = 2200),
  Cotton = list(yield_per_acre = 8, price_per_quintal = 6500),
  Maize = list(yield_per_acre = 28, price_per_quintal = 1800),
  Sugarcane = list(yield_per_acre = 350, price_per_quintal = 320),
  Gram = list(yield_per_acre = 10, price_per_quintal = 5500),
  Jute = list(yield_per_acre = 22, price_per_quintal = 4500)
)

SEASON_MODIFIER <- list(kharif = 1, rabi = 1, zaid = 0.85)
IRRIGATION_MODIFIER <- list(rainfed = 0.75, canal = 1, borewell = 1, mixed = 0.92)
CROP_PRICE_RISK <- list(Rice = 1L, Wheat = 1L, Cotton = 3L, Maize = 2L, Sugarcane = 3L, Gram = 2L, Jute = 2L)

to_acres_from_ha <- function(land_ha) as.numeric(land_ha) * HECTARES_TO_ACRES

get_expected_income <- function(crop, land_ha, season, irrigation) {
  row <- CROP_DATA[[crop]]
  if (is.null(row)) return(0)
  acres <- to_acres_from_ha(land_ha)
  season_mod <- SEASON_MODIFIER[[season]] %||% 1
  irr_mod <- IRRIGATION_MODIFIER[[irrigation]] %||% 1
  gross <- row$yield_per_acre * acres * row$price_per_quintal * season_mod * irr_mod
  net <- gross * (1 - COST_FRACTION)
  round(net)
}

get_safe_loan_limit <- function(crop, land_ha, season, irrigation) {
  expected_income <- get_expected_income(crop, land_ha, season, irrigation)
  repayable <- expected_income * SAFE_REPAYMENT_SHARE
  term_years <- DEFAULT_TERM_MONTHS / 12
  denominator <- 1 + FLAT_RATE_PER_YEAR * term_years
  safe <- repayable / denominator
  round(max(0, safe))
}

get_repayment_feasibility <- function(crop, land_ha, season, irrigation, loan_requested) {
  requested <- as.numeric(loan_requested) %||% 0
  safe_limit <- get_safe_loan_limit(crop, land_ha, season, irrigation)
  expected_income <- get_expected_income(crop, land_ha, season, irrigation)

  if (expected_income <= 0) {
    return(list(feasibility = "risky", suggestedMonths = 18L))
  }

  ratio <- if (safe_limit > 0) requested / safe_limit else 2

  if (ratio <= 0.85) {
    return(list(feasibility = "affordable", suggestedMonths = 18L))
  }
  if (ratio <= 1.15) {
    return(list(feasibility = "tight", suggestedMonths = 24L))
  }
  list(feasibility = "risky", suggestedMonths = 24L)
}

get_rainfall_risk_score <- function(season, irrigation) {
  score <- 0
  if (identical(irrigation, "rainfed")) score <- score + 50
  else if (identical(irrigation, "mixed")) score <- score + 25
  if (identical(season, "kharif")) score <- score + 20
  min(100L, as.integer(score))
}

get_market_price_risk_score <- function(crop) {
  level <- CROP_PRICE_RISK[[crop]] %||% 2L
  if (level == 1L) 20L else if (level == 2L) 50L else 80L
}

get_income_burden_score <- function(crop, land_ha, season, irrigation, loan_requested) {
  expected <- get_expected_income(crop, land_ha, season, irrigation)
  if (expected <= 0) return(100L)
  requested <- as.numeric(loan_requested) %||% 0
  ratio <- requested / expected
  min(100L, as.integer(round(ratio * 50)))
}

get_risk_level <- function(crop, land_ha, season, irrigation, loan_requested) {
  rainfall <- get_rainfall_risk_score(season, irrigation)
  market <- get_market_price_risk_score(crop)
  burden <- get_income_burden_score(crop, land_ha, season, irrigation, loan_requested)
  combined <- (rainfall + market + burden) / 3

  if (combined <= 35) return("low")
  if (combined <= 65) return("medium")
  "high"
}

#' Full payload for LoanAdvisoryPage / OutputPanel / RiskExplanation / ActionGuidance
compute_loan_advice <- function(state, district, crop, land_ha, loan_requested, season, irrigation) {
  season <- if (is.null(season) || !nzchar(as.character(season))) "kharif" else as.character(season)
  irrigation <- if (is.null(irrigation) || !nzchar(as.character(irrigation))) {
    "borewell"
  } else {
    as.character(irrigation)
  }

  expected_income <- get_expected_income(crop, land_ha, season, irrigation)
  safe_loan_limit <- get_safe_loan_limit(crop, land_ha, season, irrigation)
  repay <- get_repayment_feasibility(crop, land_ha, season, irrigation, loan_requested)
  risk_level <- get_risk_level(crop, land_ha, season, irrigation, loan_requested)
  rainfall_score <- get_rainfall_risk_score(season, irrigation)
  market_score <- get_market_price_risk_score(crop)
  burden_score <- get_income_burden_score(crop, land_ha, season, irrigation, loan_requested)

  list(
    expectedIncome = expected_income,
    safeLoanLimit = safe_loan_limit,
    feasibility = repay$feasibility,
    suggestedMonths = repay$suggestedMonths,
    riskLevel = risk_level,
    rainfallScore = rainfall_score,
    marketScore = market_score,
    burdenScore = burden_score,
    state = as.character(state %||% ""),
    district = as.character(district %||% ""),
    crop = as.character(crop)
  )
}
