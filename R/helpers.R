# =====================================================================
# Funciones auxiliares de la plantilla.
# Las páginas .qmd leen curso.yml + modulos.yml + personas.yml
# y construyen el contenido; no hay que editar los .qmd por curso.
# =====================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

romano <- function(i) as.character(utils::as.roman(i))

# ---------------------------------------------------------------------
# Lectura y validación de la configuración
# ---------------------------------------------------------------------
leer_curso <- function(dir = ".") {
  cfg <- yaml::read_yaml(file.path(dir, "curso.yml"))
  cat_mod <- yaml::read_yaml(file.path(dir, "modulos.yml"))
  cat_per <- yaml::read_yaml(file.path(dir, "personas.yml"))

  faltan <- setdiff(cfg$modulos, names(cat_mod$modulos))
  if (length(faltan)) {
    stop("Módulos no definidos en modulos.yml: ", paste(faltan, collapse = ", "),
         "\nDisponibles: ", paste(names(cat_mod$modulos), collapse = ", "))
  }
  faltan <- setdiff(unlist(cfg$contactos), names(cat_per))
  if (length(faltan)) {
    stop("Contactos no definidos en personas.yml: ", paste(faltan, collapse = ", "))
  }

  ambito <- cfg$curso$ambito %||% "las localidades urbanas endémicas de México"

  cfg$modulos_det <- lapply(seq_along(cfg$modulos), function(i) {
    clave <- cfg$modulos[[i]]
    m <- cat_mod$modulos[[clave]]
    m$clave <- clave
    m$numero <- romano(i)
    if (!is.null(m$objetivo)) m$objetivo <- gsub("{ambito}", ambito, m$objetivo, fixed = TRUE)
    m$temas <- resolver_temas(m$temas, cfg$instructores[[clave]],
                              cat_mod$instructor_default)
    m
  })

  cfg$personas <- c(
    lapply(cfg$contactos, function(k) cat_per[[k]]),
    cfg$contactos_locales %||% list()
  )
  cfg
}

# Prioridad del instructor: curso.yml (por tema) > curso.yml (todo el
# módulo) > modulos.yml (por tema) > instructor_default
resolver_temas <- function(temas, override, default) {
  nombres <- vapply(temas, function(t) if (is.list(t)) t$tema else t, "", USE.NAMES = FALSE)
  base    <- vapply(temas, function(t) if (is.list(t)) t$instructor %||% default else default, "",
                    USE.NAMES = FALSE)
  if (is.character(override) && length(override) == 1) {
    base[] <- override
  } else if (is.list(override)) {
    for (n in intersect(names(override), nombres)) base[nombres == n] <- override[[n]]
  }
  data.frame(Tema = nombres, Instructor = base, stringsAsFactors = FALSE, row.names = NULL)
}

# ---------------------------------------------------------------------
# Componentes visuales
# ---------------------------------------------------------------------
tabla_temas <- function(df) {
  if (requireNamespace("reactable", quietly = TRUE)) {
    tema <- if (requireNamespace("reactablefmtr", quietly = TRUE)) reactablefmtr::flatly() else NULL
    reactable::reactable(df,
                         theme = tema,
                         defaultPageSize = nrow(df),
                         striped = TRUE, bordered = TRUE,
                         resizable = TRUE, wrap = TRUE,
                         columns = list(Tema = reactable::colDef(minWidth = 260),
                                        Instructor = reactable::colDef(minWidth = 200)))
  } else {
    knitr::kable(df)
  }
}

mapa_sede <- function(sede) {
  if (is.null(sede$lat) || is.null(sede$lon)) return(invisible(NULL))
  etiqueta <- sede$nombre %||% "Sede"
  if (requireNamespace("leaflet", quietly = TRUE)) {
    leaflet::leaflet(height = 380) |>
      leaflet::addTiles() |>
      leaflet::addMarkers(lng = sede$lon, lat = sede$lat, popup = etiqueta) |>
      leaflet::setView(lng = sede$lon, lat = sede$lat, zoom = 16)
  } else {
    d <- 0.004
    knitr::asis_output(sprintf(
      '<iframe width="100%%" height="380" style="border:0" src="https://www.openstreetmap.org/export/embed.html?bbox=%f,%f,%f,%f&layer=mapnik&marker=%f,%f"></iframe>',
      sede$lon - d, sede$lat - d, sede$lon + d, sede$lat + d, sede$lat, sede$lon))
  }
}

enlace <- function(texto, url) {
  if (is.null(url) || identical(url, "")) sprintf("%s *(enlace pendiente)*", texto)
  else sprintf("[%s](%s)", texto, url)
}

# ---------------------------------------------------------------------
# Generadores de markdown (usar en chunks con output: asis)
# ---------------------------------------------------------------------
md_objetivos <- function(cfg) {
  obj <- Filter(Negate(is.null), lapply(cfg$modulos_det, `[[`, "objetivo"))
  paste0(seq_along(obj), ".  ", unlist(obj), collapse = "\n\n")
}

md_lista <- function(x) paste0("-   ", unlist(x), collapse = "\n")

md_contacto <- function(p) {
  partes <- c(
    "------------------------------------------------------------------------\n",
    sprintf("## %s\n", p$nombre),
    if (!is.null(p$foto)) sprintf("![](%s){width=120px}\n", p$foto),
    if (!is.null(p$cargo)) sprintf("%s\n", p$cargo),
    if (!is.null(p[["telefono", exact = TRUE]])) sprintf("Teléfono personal: %s\n", p[["telefono", exact = TRUE]]),
    if (!is.null(p$telefono_oficina)) sprintf("Teléfono oficina: %s\n", p$telefono_oficina),
    if (!is.null(p$correo)) sprintf("Correo: %s\n",
                                    paste(sprintf("<%s>", unlist(p$correo)), collapse = ", ")),
    if (!is.null(p$enlaces)) paste(sprintf("%s: <%s>", names(p$enlaces), unlist(p$enlaces)),
                                   collapse = "\n\n")
  )
  paste(partes, collapse = "\n")
}

# Plantilla de un módulo del temario (markdown + chunk con la tabla).
# Se usa con knitr::knit_child para poder intercalar widgets.
temario_modulo_child <- function(i) {
  sprintf(
'
## Módulo `r cfg$modulos_det[[%1$d]]$numero`. `r cfg$modulos_det[[%1$d]]$titulo`

```{r}
#| echo: false
tabla_temas(cfg$modulos_det[[%1$d]]$temas)
```

`r md_presentaciones(cfg$modulos_det[[%1$d]])`
', i)
}

md_presentaciones <- function(m) {
  p <- m$presentaciones
  if (!length(p)) return("")
  paste0("**Presentaciones:** ",
         paste(vapply(p, function(x) enlace(x$texto, x$url), ""), collapse = " · "))
}

md_labs <- function(cfg) {
  bloques <- lapply(cfg$modulos_det, function(m) {
    lab <- m$laboratorio
    if (is.null(lab)) return(NULL)
    acts <- vapply(seq_along(lab$actividades), function(j) {
      pasos <- unlist(lab$actividades[[j]])
      paste0("### Actividad ", j, "\n\n",
             paste0(seq_along(pasos), ".  ", pasos, collapse = "\n"), "\n")
    }, "")
    paste0("## Laboratorio del Módulo ", m$numero, ". ", m$titulo, "\n\n",
           "**Objetivo.** ", lab$objetivo, "\n\n",
           paste(acts, collapse = "\n"))
  })
  paste(unlist(bloques), collapse = "\n\n")
}
