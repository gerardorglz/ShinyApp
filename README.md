# ShinyApp — Indicadores MX & USA

Aplicación Shiny que visualiza indicadores económicos clave de México y Estados Unidos, usando las APIs de <a href="https://www.banxico.org.mx/SieAPIRest/service/v1/">Banxico SIE</a>, <a href="https://fred.stlouisfed.org/docs/api/fred/">FRED</a> y <a href="https://www.quantmod.com">Yahoo Finance</a>. Cada tarjeta muestra el dato más reciente, su cambio respecto al periodo anterior (▲/▼) y la fecha del último dato. Al hacer clic en una tarjeta se abre el histórico interactivo de la variable, con datos desde el año 2000.

<a href="https://gerardorglz.shinyapps.io/indicadores-mx-usa/">Aquí</a> puedes ver la página publicada.

## Actualización de datos

Los datos se refrescan **una vez al día a las 06:00 (hora de México, `America/Mexico_City`)**:

- Al arrancar, la app cachea el snapshot de datos en `cache/snapshot.rds` con su timestamp.
- Solo vuelve a consultar las APIs cuando el caché es anterior a las últimas 06:00; el resto del día se sirve desde el caché (una consulta al día, no una por sesión).
- Una instancia en ejecución se auto-refresca con un temporizador que se dispara justo después de la siguiente 06:00.
- Si una API falla, se conserva el último caché válido en lugar de mostrar la app vacía.

La hora se controla con `APP_TZ` y `REFRESH_HOUR` en `global.R`.

### Refresco garantizado aunque nadie visite la app (opcional)

En shinyapps.io una instancia inactiva se apaga; el refresco a las 06:00 ocurre cuando llega la primera visita del día. Si quieres que los datos se actualicen **siempre** a las 06:00 sin depender de visitas, agrega un cron externo (p. ej. GitHub Actions) que regenere el snapshot y vuelva a desplegar la app con `rsconnect::deployApp()`, guardando `BANXICO_TOKEN`, `FRED_API_KEY` y las credenciales de shinyapps.io como *secrets*. Pídemelo y te dejo el workflow listo.

## Configuración local

Crea un archivo `.Renviron` en la raíz con tus llaves:

```
BANXICO_TOKEN=tu_token
FRED_API_KEY=tu_key
```

Luego ejecuta la app con `shiny::runApp()`.
