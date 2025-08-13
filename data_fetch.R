# --------- Fetchers (último dato para las tarjetas) ---------
fetch_banxico <- function() {
  codes <- c(
    CETES28 = "SF45470",
    TIIE28  = "SF43783",
    TasaRefMX = "SF61745",
    InflacionMX = "SP30578",
    InflacionSub = "SP74662",
    InflacionNoSub = "SP74665",
    TipoCambioFIX = "SF60653"
  )
  dat <- safe_try(getSeriesData(unname(codes), endDate = Sys.Date()))
  if (is.null(dat)) return(tibble())

  pull_one <- function(code, label, style) {
    df <- safe_try(getSerieDataFrame(dat, code)); if (is.null(df)) return(NULL)
    lv <- last_val(df)
    tibble(
      grupo = "México",
      etiqueta = label,
      codigo = code,
      valor = suppressWarnings(as.numeric(lv$value)),
      fecha = as.Date(lv$date),
      formato = style
    )
  }

  bind_rows(purrr::compact(list(
    pull_one(codes["CETES28"], "CETES 28 días", "pct"),
    pull_one(codes["TIIE28"],  "TIIE 28", "pct"),
    pull_one(codes["TasaRefMX"], "Tasa de referencia (MX)", "pct"),
    pull_one(codes["InflacionMX"], "Inflación general (MX)", "pct"),
    pull_one(codes["InflacionSub"], "Inflación subyacente (MX)", "pct"),
    pull_one(codes["InflacionNoSub"], "Inflación no subyacente (MX)", "pct"),
    pull_one(codes["TipoCambioFIX"], "Tipo de cambio liquidación (MXN/USD)", "plain")
  )))
}

fetch_fred <- function() {
  if (!nzchar(Sys.getenv("FRED_API_KEY"))) return(tibble())

  pull_cpi_yoy <- function() {
    df <- safe_try(fredr(
      series_id = "CPIAUCSL",
      observation_start = Sys.Date() - lubridate::years(2),
      observation_end   = Sys.Date()
    ))
    if (is.null(df) || !nrow(df)) return(NULL)
    df <- df %>% arrange(date) %>% mutate(yoy = (value/lag(value, 12) - 1) * 100)
    last_row <- df %>% filter(!is.na(yoy)) %>% slice_tail(n = 1)
    if (!nrow(last_row)) return(NULL)
    tibble(
      grupo   = "Estados Unidos",
      etiqueta= "Inflación general (USA)",
      codigo  = "CPIAUCSL_YoY",
      valor   = as.numeric(last_row$yoy),
      fecha   = as.Date(last_row$date),
      formato = "pct"
    )
  }

  pull_fred <- function(id, label, style) {
    df <- safe_try(fredr(series_id = id, observation_end = Sys.Date()))
    if (is.null(df) || !nrow(df)) return(NULL)
    lv <- last_val(dplyr::select(df, date, value))
    tibble(
      grupo = "Estados Unidos",
      etiqueta = label,
      codigo = id,
      valor = suppressWarnings(as.numeric(lv$value)),
      fecha = as.Date(lv$date),
      formato = style
    )
  }

  bind_rows(purrr::compact(list(
    pull_cpi_yoy(),
    pull_fred("DFEDTARU", "FED Funds (límite superior)", "pct"),
    pull_fred("SOFR",     "SOFR (overnight)",           "pct"),
    pull_fred("DTB4WK",   "T-Bill 1 mes",               "pct")
  )))
}

fetch_yahoo <- function() {
  op <- options(warn = -1); on.exit(options(op), add = TRUE)

  pull_yahoo <- function(ticker, label) {
    xt <- safe_try(quantmod::getSymbols(
      ticker, src = "yahoo", from = as.Date("2025-01-01"),
      to = Sys.Date(), periodicity = "daily", auto.assign = FALSE
    ))
    if (is.null(xt)) return(NULL)
    df <- data.frame(date = zoo::index(xt), coredata(xt))
    close_col <- grep("\\.Close$", names(df), value = TRUE)
    if (!length(close_col)) return(NULL)
    df <- df %>% dplyr::select(date, value = all_of(close_col))
    lv <- last_val(df)
    tibble(
      grupo = "Mercados",
      etiqueta = label,
      codigo = ticker,
      valor = suppressWarnings(as.numeric(lv$value)),
      fecha = as.Date(lv$date),
      formato = "index"
    )
  }

  bind_rows(purrr::compact(list(
    pull_yahoo("^GSPC", "S&P 500 (close)"),
    pull_yahoo("^DJI",  "Dow Jones (close)"),
    pull_yahoo("^IXIC", "NASDAQ (close)"),
    pull_yahoo("^VIX",  "VIX (nivel)"),
    pull_yahoo("^MXX",  "S&P/BMV IPC (close)")
  )))
}

fetch_all <- function() {
  bind_rows(fetch_banxico(), fetch_fred(), fetch_yahoo()) %>% arrange(grupo, etiqueta)
}

# --------- Históricos para el modal ---------
fetch_history <- function(code) {
  start <- as.Date("2000-01-01")
  if (grepl("^[A-Z]{2}\\d+$", code)) {                 
    dat <- safe_try(getSeriesData(code, startDate = start, endDate = Sys.Date()))
    if (is.null(dat)) return(tibble(date=as.Date(NA), value=NA_real_))
    df <- getSerieDataFrame(dat, code)
    df <- df %>% transmute(date = as.Date(date), value = suppressWarnings(as.numeric(value))) %>%
      filter(date >= start)
    return(df)
  } else if (identical(code, "CPIAUCSL_YoY")) {        
    df <- safe_try(fredr(
      series_id = "CPIAUCSL",
      observation_start = start - lubridate::years(1),
      observation_end   = Sys.Date()
    ))
    if (is.null(df) || !nrow(df)) return(tibble(date=as.Date(NA), value=NA_real_))
    df <- df %>% arrange(date) %>% mutate(value = (value/lag(value, 12) - 1) * 100) %>%
      filter(date >= start) %>% select(date, value)
    return(df)
  } else if (startsWith(code, "^")) {                 
    xt <- safe_try(quantmod::getSymbols(
      code, src = "yahoo", from = start, to = Sys.Date(),
      periodicity = "daily", auto.assign = FALSE
    ))
    if (is.null(xt)) return(tibble(date=as.Date(NA), value=NA_real_))
    df <- data.frame(date = zoo::index(xt), coredata(xt))
    close_col <- grep("\\.Close$", names(df), value = TRUE)
    df <- df[, c("date", close_col)]; names(df) <- c("date","value")
    df$date <- as.Date(df$date)
    return(df)
  } else {                                            
    df <- safe_try(fredr(
      series_id = code, observation_start = start, observation_end = Sys.Date()
    ))
    if (is.null(df) || !nrow(df)) return(tibble(date=as.Date(NA), value=NA_real_))
    return(dplyr::select(df, date, value))
  }
}