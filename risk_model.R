# Agrisaviour — regional risk model (aligned with src/riskAnalysis/*.js)
# Dependencies: base R only (jsonlite loaded by plumber).

# --- constants (same as JS) -------------------------------------------------

MSP_RUPEES_PER_QUINTAL <- c(
  Rice = 2060, Wheat = 2125, Cotton = 6620, Maize = 1962,
  Sugarcane = 340, Gram = 5440, Jute = 5050
)

EXPECTED_YIELD_TONS_PER_HA <- c(
  Rice = 3.2, Wheat = 3.4, Cotton = 0.55, Maize = 3.0,
  Sugarcane = 72, Gram = 1.1, Jute = 2.2
)

# --- FNV-1a 32-bit (same algorithm as JS hashString in simulatedData.js) -----

fnv1a_32 <- function(str) {
  bytes <- charToRaw(enc2utf8(as.character(str)))
  h <- 2166136261
  for (i in seq_along(bytes)) {
    h <- bitwXor(as.integer(h %% 4294967296), as.integer(bytes[i]))
    h <- (as.numeric(h) * 16777619) %% 4294967296
  }
  as.integer(h %% 4294967296)
}

estimate_farmer_count <- function(state, district) {
  h <- fnv1a_32(paste0(state, "|", district))
  12000L + (as.integer(abs(h)) %% 185000L)
}

# --- clamp / risk components -------------------------------------------------

clamp0to100 <- function(x) max(0, min(100, x))

rainfall_deviation_risk <- function(monthly_actual_mm, monthly_normal_mm) {
  ma <- as.numeric(monthly_actual_mm)
  mn <- as.numeric(monthly_normal_mm)
  n <- length(ma)
  if (n == 0L) return(0)
  sum <- 0
  for (i in seq_len(n)) {
    nn <- max(mn[i], 0.5)
    a <- ma[i]
    sum <- sum + abs(a - nn) / nn
  }
  mean_rel_dev <- sum / n
  clamp0to100(mean_rel_dev * 250)
}

price_risk_from_msp <- function(msp_rs_per_quintal, market_rs_per_quintal) {
  mp <- as.numeric(market_rs_per_quintal)
  if (length(mp) == 0L || msp_rs_per_quintal <= 0) return(0)
  sum <- 0
  for (p in mp) {
    if (p < msp_rs_per_quintal) {
      sum <- sum + (msp_rs_per_quintal - p) / msp_rs_per_quintal
    }
  }
  avg_shortfall <- sum / length(mp)
  clamp0to100(avg_shortfall * 100)
}

yield_risk_from_expected <- function(expected_tons_per_ha, actual_tons_per_ha) {
  ay <- as.numeric(actual_tons_per_ha)
  if (length(ay) == 0L || expected_tons_per_ha <= 0) return(0)
  sum <- 0
  for (a in ay) {
    sum <- sum + abs(a - expected_tons_per_ha) / expected_tons_per_ha
  }
  mean_rel_dev <- sum / length(ay)
  clamp0to100(mean_rel_dev * 100)
}

compute_final_risk_score <- function(rainfall_risk, yield_risk, price_risk) {
  clamp0to100(0.4 * rainfall_risk + 0.3 * yield_risk + 0.3 * price_risk)
}

affected_farmer_percent <- function(score) {
  s <- clamp0to100(score)
  if (s < 30) return(10)
  if (s <= 70) return(30 + ((s - 30) / 40) * 20)
  60 + ((s - 70) / 30) * 20
}

linear_regression <- function(xs, ys) {
  n <- min(length(xs), length(ys))
  if (n < 2L) return(list(a = if (length(ys) > 0) ys[[1]] else 0, b = 0))
  xs <- xs[seq_len(n)]
  ys <- ys[seq_len(n)]
  sum_x <- sum(xs)
  sum_y <- sum(ys)
  sum_xy <- sum(xs * ys)
  sum_xx <- sum(xs * xs)
  denom <- n * sum_xx - sum_x * sum_x
  if (abs(denom) < 1e-9) return(list(a = sum_y / n, b = 0))
  b <- (n * sum_xy - sum_x * sum_y) / denom
  a <- (sum_y - b * sum_x) / n
  list(a = a, b = b)
}

build_forecast <- function(last_values, horizon = 3L) {
  n <- length(last_values)
  if (n < 2L) return(numeric(0))
  take <- min(24L, n)
  slice <- last_values[seq.int(n - take + 1L, n)]
  xs <- seq_along(slice) - 1L
  fit <- linear_regression(xs, slice)
  out <- numeric(horizon)
  for (h in seq_len(horizon)) {
    x <- take - 1L + h
    out[[h]] <- fit$a + fit$b * x
  }
  out
}

month_label <- function(year_index, month_index) {
  d <- as.Date(sprintf("%d-%02d-01", 2020 + year_index, month_index + 1L))
  format(d, "%b %Y")
}

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1L && is.na(x))) y else x

# --- build dataset (same structure as JS buildSimulatedRegionalDataset) -----

build_regional_dataset <- function(state, district, crop, years) {
  years <- as.integer(years)
  if (!years %in% c(1L, 3L, 5L)) years <- 3L

  seed <- fnv1a_32(paste0(state, "|", district, "|", crop, "|", years))
  set.seed(seed %% 2147483647L)
  rng <- function() stats::runif(1)

  msp <- unname(MSP_RUPEES_PER_QUINTAL[crop]) %||% 2000
  expected_yield <- unname(EXPECTED_YIELD_TONS_PER_HA[crop]) %||% 2.5
  months <- years * 12L

  monthly_normal <- numeric(months)
  monthly_actual <- numeric(months)
  for (m in seq_len(months)) {
    phase <- ((m - 1L) %% 12L) / 12
    seasonal <- 45 + 95 * max(0, sin(2 * pi * (phase - 0.15)))^1.2
    n <- round(seasonal * (0.92 + 0.16 * rng()))
    monthly_normal[[m]] <- n
    shock <- 0.72 + 0.56 * rng()
    monthly_actual[[m]] <- max(5, round(n * shock))
  }

  market_prices <- numeric(months)
  drift <- 0
  for (i in seq_len(months)) {
    drift <- drift + (rng() - 0.48) * 0.02
    market_prices[[i]] <- round(msp * (0.88 + 0.22 * rng() + drift))
  }

  actual_yields <- numeric(years)
  for (y in seq_len(years)) {
    stress <- 0.78 + 0.44 * rng()
    actual_yields[[y]] <- round(expected_yield * stress, 2)
  }

  rainfall_series <- vector("list", months)
  price_series <- vector("list", months)
  for (m in seq_len(months)) {
    yi <- (m - 1L) %/% 12L
    mi <- (m - 1L) %% 12L
    lab <- month_label(yi, mi)
    rainfall_series[[m]] <- list(
      label = lab,
      monthIndex = m - 1L,
      normalMm = monthly_normal[[m]],
      actualMm = monthly_actual[[m]]
    )
    price_series[[m]] <- list(
      label = lab,
      monthIndex = m - 1L,
      msp = msp,
      marketRsPerQuintal = market_prices[[m]]
    )
  }

  yield_comparison <- vector("list", years)
  for (y in seq_len(years)) {
    yield_comparison[[y]] <- list(
      year = paste0("Year ", y),
      expectedTonsPerHa = expected_yield,
      actualTonsPerHa = actual_yields[[y]]
    )
  }

  total_farmers <- estimate_farmer_count(state, district)

  list(
    crop = crop,
    years = years,
    state = state,
    district = district,
    mspRsPerQuintal = msp,
    expectedYieldTonsPerHa = expected_yield,
    monthlyNormalMm = as.list(monthly_normal),
    monthlyActualMm = as.list(monthly_actual),
    marketRsPerQuintal = as.list(market_prices),
    actualYieldTonsPerHa = as.list(actual_yields),
    rainfallSeries = rainfall_series,
    priceSeries = price_series,
    yieldComparison = yield_comparison,
    totalFarmers = total_farmers
  )
}

# --- full analysis payload for API ------------------------------------------

compute_risk_analysis <- function(state, district, crop, years) {
  data <- build_regional_dataset(state, district, crop, years)

  ma <- unlist(data$monthlyActualMm)
  mn <- unlist(data$monthlyNormalMm)
  mp <- unlist(data$marketRsPerQuintal)
  ay <- unlist(data$actualYieldTonsPerHa)

  r_rain <- rainfall_deviation_risk(ma, mn)
  r_price <- price_risk_from_msp(data$mspRsPerQuintal, mp)
  r_yield <- yield_risk_from_expected(data$expectedYieldTonsPerHa, ay)
  score <- compute_final_risk_score(r_rain, r_yield, r_price)
  pct <- affected_farmer_percent(score)
  affected_count <- round((pct / 100) * data$totalFarmers)

  rain_fc <- build_forecast(ma, 3L)
  price_fc <- build_forecast(mp, 3L)

  list(
    data = data,
    rRain = r_rain,
    rYield = r_yield,
    rPrice = r_price,
    score = score,
    pctAffected = pct,
    affectedCount = affected_count,
    rainFc = as.list(rain_fc),
    priceFc = as.list(price_fc)
  )
}
