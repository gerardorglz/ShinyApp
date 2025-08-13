library(shiny)

# --------- Server ---------
server <- function(input, output, session) {
  start_year <- 2000

  datos <- reactive({
    invalidateLater(30 * 60 * 1000, session)
    fetch_all()
  })

  output$last_refresh <- renderText({
    glue("<b>Actualizado:</b> {format(Sys.time(), '%d %b %Y %H:%M')} (hora local)")
  })

  render_cards_layout <- function(df_group) {
    req(nrow(df_group) > 0)
    pick_icon <- function(lbl) {
      if (grepl("Tasa|TIIE|CETES|SOFR|Fed|T-Bill|T-bill|interanual", lbl, ignore.case = TRUE)) icon("percent")
      else if (grepl("Tipo de cambio", lbl, ignore.case = TRUE)) icon("dollar-sign")
      else if (grepl("VIX", lbl, ignore.case = TRUE)) icon("bolt")
      else icon("chart-line")
    }
    cards <- purrr::pmap(df_group, function(grupo, etiqueta, codigo, valor, fecha, formato) {
      wrap_click(
        card_metric(
          title = etiqueta,
          value = fmt_val(valor, formato),
          date  = nice_date(fecha),
          icon  = pick_icon(etiqueta)
        ),
        code = codigo, etiqueta = etiqueta
      )
    })
    do.call(
      bslib::layout_column_wrap,
      c(list(width = "260px", gap = "16px", heights_equal = "all"), cards)
    )
  }

  output$cards_mx <- renderUI({
    df <- datos() %>% dplyr::filter(grupo == "México")
    if (!nrow(df)) return(div("Sin datos de Banxico por ahora."))
    render_cards_layout(df)
  })

  output$cards_us <- renderUI({
    df <- datos() %>% dplyr::filter(grupo == "Estados Unidos")
    if (!nrow(df)) return(div("Sin datos de FRED por ahora."))
    render_cards_layout(df)
  })

  output$cards_mkts <- renderUI({
    df <- datos() %>% dplyr::filter(grupo == "Mercados")
    if (!nrow(df)) return(div("Sin datos de mercados por ahora."))
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
      title = glue("{meta$etiqueta} — {meta$grupo}"),
      size = "l", easyClose = TRUE,
      footer = modalButton("Cerrar"),
      dateRangeInput(
        inputId = "date_range",
        label   = "Rango de fechas",
        start   = def_start, end = def_end,
        min     = min_d,     max = max_d,
        weekstart = 1
      ),
      plotlyOutput("modal_plot", height = "440px")
    ))
  })

  # --------- Gráfica interactiva con tooltips (plotly) ---------
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
    req(nrow(df) > 1)

    df <- df %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        value_num = as.numeric(value),
        fecha_txt = format(as.Date(date), "%Y-%m-%d"),
        val_txt   = fmt_hover(value_num, formato),
        text      = paste0(fecha_txt, "<br>", serie_label, ": ", val_txt)
      )

    p <- ggplot(df, aes(x = as.Date(date), y = value_num, group = 1, text = text)) +
      geom_line(linewidth = .5) +
      labs(x = NULL, y = NULL) +
      theme_classic(base_size = 12) +
      theme(plot.title = element_text(face = "bold"))

    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "x") %>%
      config(displaylogo = FALSE)
  })
}