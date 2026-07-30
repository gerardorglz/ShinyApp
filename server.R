library(shiny)

# --------- Server ---------
server <- function(input, output, session) {
  start_year <- 2000

  # Snapshot diario: se re-consulta solo tras la frontera de las 06:00 (APP_TZ).
  # El timer se re-arma para dispararse justo después de la próxima 06:00, de modo
  # que una instancia viva se refresque sola sin depender de nuevas visitas.
  snapshot <- reactive({
    ms <- as.numeric(difftime(next_refresh_boundary(), Sys.time(), units = "secs")) * 1000 + 5000
    invalidateLater(ms, session)
    get_data_cached()
  })

  datos <- reactive(snapshot()$data)

  output$last_refresh <- renderUI({
    ts <- snapshot()$fetched_at
    fecha_txt <- if (is.null(ts) || is.na(ts)) "—"
                 else format(as.POSIXct(ts), "%d %b %Y, %H:%M", tz = APP_TZ)
    prox <- format(next_refresh_boundary(), "%d %b, %H:%M", tz = APP_TZ)
    tags$span(
      class = "refresh-badge",
      icon("clock"),
      HTML(glue(" Datos al <b>{fecha_txt}</b> · próxima actualización {prox} (CDMX)"))
    )
  })

  render_cards_layout <- function(df_group) {
    req(nrow(df_group) > 0)
    pick_icon <- function(lbl) {
      if (grepl("Tasa|TIIE|CETES|SOFR|Fed|T-Bill|T-bill|interanual|Inflaci", lbl, ignore.case = TRUE)) icon("percent")
      else if (grepl("Tipo de cambio", lbl, ignore.case = TRUE)) icon("dollar-sign")
      else if (grepl("VIX", lbl, ignore.case = TRUE)) icon("bolt")
      else icon("chart-line")
    }
    cards <- purrr::pmap(df_group, function(grupo, etiqueta, codigo, valor, valor_prev, fecha, formato) {
      wrap_click(
        card_metric(
          title = etiqueta,
          value = fmt_val(valor, formato),
          date  = nice_date(fecha),
          delta = delta_badge(valor, valor_prev, formato),
          icon  = pick_icon(etiqueta)
        ),
        code = codigo, etiqueta = etiqueta
      )
    })
    do.call(
      bslib::layout_column_wrap,
      c(list(width = "250px", gap = "1rem", heights_equal = "all"), cards)
    )
  }

  output$cards_mx <- renderUI({
    df <- datos() %>% dplyr::filter(grupo == "México")
    if (!nrow(df)) return(div(class = "empty-note", "Sin datos de Banxico por ahora."))
    render_cards_layout(df)
  })

  output$cards_us <- renderUI({
    df <- datos() %>% dplyr::filter(grupo == "Estados Unidos")
    if (!nrow(df)) return(div(class = "empty-note", "Sin datos de FRED por ahora."))
    render_cards_layout(df)
  })

  output$cards_mkts <- renderUI({
    df <- datos() %>% dplyr::filter(grupo == "Mercados")
    if (!nrow(df)) return(div(class = "empty-note", "Sin datos de mercados por ahora."))
    render_cards_layout(df)
  })

  # ---- Modal + control de rango de fechas ----
  clicked <- eventReactive(input$series_clicked, {
    meta <- isolate(datos()) %>% dplyr::filter(codigo == input$series_clicked) %>% dplyr::slice(1)
    df   <- fetch_history(meta$codigo[[1]])
    list(meta = meta, df = df)
  }, ignoreInit = TRUE)

  observeEvent(clicked(), {
    meta <- clicked()$meta
    df   <- clicked()$df

    min_d <- suppressWarnings(min(df$date, na.rm = TRUE))
    max_d <- suppressWarnings(max(df$date, na.rm = TRUE))
    def_start <- max(as.Date(sprintf("%s-01-01", start_year)), min_d)
    def_end   <- max_d

    showModal(modalDialog(
      title = tags$span(icon("chart-area"), glue(" {meta$etiqueta} — {meta$grupo}")),
      size = "l", easyClose = TRUE,
      footer = modalButton("Cerrar"),
      dateRangeInput(
        inputId = "date_range",
        label   = "Rango de fechas",
        start   = def_start, end = def_end,
        min     = min_d,     max = max_d,
        weekstart = 1, language = "es", separator = " a "
      ),
      plotlyOutput("modal_plot", height = "440px")
    ))
  })

  # --------- Gráfica interactiva con tooltips (plotly nativo) ---------
  # Se usa plot_ly() directo en lugar de ggplotly() porque ggplot2 >= 4.0
  # rompe la conversión de ggplotly ("subscript out of bounds").
  output$modal_plot <- renderPlotly({
    dat <- clicked(); req(dat)
    df  <- dat$df
    meta <- dat$meta
    formato <- meta$formato[[1]]
    serie_label <- meta$etiqueta[[1]]

    dr <- input$date_range
    if (!is.null(dr) && !any(is.na(dr))) {
      df <- df %>% dplyr::filter(date >= as.Date(dr[1]), date <= as.Date(dr[2]))
    } else {
      df <- df %>% dplyr::filter(date >= as.Date("2000-01-01"))
    }
    df <- df %>% dplyr::filter(!is.na(value))
    req(nrow(df) > 1)

    df <- df %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        value_num = as.numeric(value),
        text = paste0(format(as.Date(date), "%d %b %Y"), "<br>",
                      serie_label, ": ", fmt_hover(value_num, formato))
      )

    yaxis <- list(title = "", zeroline = FALSE, gridcolor = "#eef2f2")
    if (identical(formato, "pct")) {
      yaxis$ticksuffix <- " %"
    } else if (identical(formato, "currency")) {
      yaxis$tickprefix <- "$"; yaxis$tickformat <- ",.2f"
    } else {
      yaxis$tickformat <- ",.2f"
    }

    accent <- "#3fb0ac"
    plot_ly(
      df, x = ~date, y = ~value_num,
      type = "scatter", mode = "lines",
      line = list(color = accent, width = 2),
      fill = "tozeroy", fillcolor = "rgba(63,176,172,0.12)",
      text = ~text, hoverinfo = "text"
    ) %>%
      layout(
        hovermode = "x unified",
        xaxis = list(title = "", gridcolor = "#eef2f2"),
        yaxis = yaxis,
        margin = list(l = 10, r = 10, t = 10, b = 10),
        plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = list("select2d", "lasso2d"))
  })
}
