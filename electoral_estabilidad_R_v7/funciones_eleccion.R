# funciones_eleccion.R
# Versión 6
#
# Cambio principal respecto de v5:
# Permite declarar distritos específicos donde la columna de votos NO es "votos contados",
# sino "total final conocido". Esto corrige casos como:
#
#   Distrito 25:
#   votos reportados = 280000
#   avance = 1%
#
# Si D25 se declara como total conocido:
#   votos_contados = 280000 * 0.01 = 2800
#   votos_totales_estimados = 280000
#
# Para los otros distritos, si la opción general es "contados":
#   votos_totales_estimados = votos_contados / avance
#
# También agrega una validación opcional contra el total nacional de votos válidos.

paquetes_necesarios <- c("readxl", "dplyr", "ggplot2", "scales")
faltantes <- paquetes_necesarios[!vapply(paquetes_necesarios, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes) > 0) {
  stop("Instala estos paquetes antes de continuar: ", paste(faltantes, collapse = ", "))
}

normalizar_nombre <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("%", " pct ", x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

normalizar_id_distrito <- function(x) {
  z <- normalizar_nombre(x)
  z <- gsub("distrito_", "", z)
  z <- gsub("^d", "", z)
  z
}

parse_lista_distritos <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || trimws(x) == "") return(character(0))
  partes <- unlist(strsplit(as.character(x), "[,; ]+"))
  partes <- partes[nzchar(partes)]
  normalizar_id_distrito(partes)
}

limpiar_decimal <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("%", "", x)
  x <- gsub("\\s+", "", x)
  x[x %in% c("", "NA", "NaN", "-", "—", "NULL", "null")] <- NA_character_

  ambos <- grepl(",", x) & grepl("\\.", x)
  x[ambos] <- gsub("\\.", "", x[ambos])
  x[ambos] <- gsub(",", ".", x[ambos])

  solo_coma <- grepl(",", x) & !grepl("\\.", x)
  x[solo_coma] <- gsub(",", ".", x[solo_coma])

  puntos <- gregexpr("\\.", x)
  n_puntos <- vapply(
    puntos,
    function(z) {
      if (z[1] == -1) return(0L)
      as.integer(length(z))
    },
    integer(1)
  )
  muchos_puntos <- n_puntos > 1 & !grepl(",", x)
  x[muchos_puntos] <- gsub("\\.", "", x[muchos_puntos])

  suppressWarnings(as.numeric(x))
}

limpiar_votos <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\\s+", "", x)
  x <- gsub("\\.", "", x)
  x <- gsub(",", ".", x)
  x[x %in% c("", "NA", "NaN", "-", "—", "NULL", "null")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

normalizar_pct_col <- function(x) {
  z <- limpiar_decimal(x)
  finitos <- z[!is.na(z) & is.finite(z)]
  if (length(finitos) == 0) return(z)
  if (max(abs(finitos), na.rm = TRUE) > 1) z <- z / 100
  z
}

formato_pct <- function(x, decimales = 2) {
  ifelse(
    is.na(x) | !is.finite(x),
    "NA",
    paste0(formatC(100 * x, format = "f", digits = decimales), "%")
  )
}

formato_numero <- function(x, decimales = 0) {
  ifelse(
    is.na(x) | !is.finite(x),
    "NA",
    formatC(x, format = "f", digits = decimales, big.mark = ".", decimal.mark = ",")
  )
}

es_k <- function(x) {
  z <- normalizar_nombre(x)
  z %in% c("k", "pct_k", "k_pct", "porcentaje_k", "candidato_k", "votos_k_pct")
}

es_r <- function(x) {
  z <- normalizar_nombre(x)
  z %in% c("r", "pct_r", "r_pct", "porcentaje_r", "candidato_r", "votos_r_pct")
}

buscar_columna_en_header <- function(header, patrones) {
  h <- normalizar_nombre(header)
  for (pat in patrones) {
    hit <- which(grepl(pat, h))
    if (length(hit) > 0) return(hit[1])
  }
  NA_integer_
}

filtrar_nacional_valido <- function(out) {
  out <- out[complete.cases(out[, c("conteo_pct", "k_pct", "r_pct")]), , drop = FALSE]
  out <- out[out$conteo_pct >= 0 & out$conteo_pct <= 1, , drop = FALSE]
  out <- out[out$k_pct > 0 & out$r_pct > 0, , drop = FALSE]
  out <- out[(out$k_pct + out$r_pct) > 0.1, , drop = FALSE]

  if (nrow(out) == 0) return(out)

  suma <- out$k_pct + out$r_pct
  out$k_pct <- out$k_pct / suma
  out$r_pct <- out$r_pct / suma

  out <- out[order(out$conteo_pct), , drop = FALSE]
  rownames(out) <- NULL
  out
}

extraer_nacional_tidy <- function(raw) {
  max_filas <- min(nrow(raw), 15)

  for (i in seq_len(max_filas)) {
    header <- as.character(unlist(raw[i, ], use.names = FALSE))

    col_conteo <- buscar_columna_en_header(header, c("conteo", "contado", "avance"))
    col_k <- which(es_k(header))[1]
    col_r <- which(es_r(header))[1]

    if (!is.na(col_conteo) && !is.na(col_k) && !is.na(col_r)) {
      datos <- raw[(i + 1):nrow(raw), , drop = FALSE]

      out <- data.frame(
        conteo_pct = normalizar_pct_col(datos[[col_conteo]]),
        k_pct = normalizar_pct_col(datos[[col_k]]),
        r_pct = normalizar_pct_col(datos[[col_r]])
      )

      out <- filtrar_nacional_valido(out)
      if (nrow(out) > 0) return(out)
    }
  }

  NULL
}

extraer_nacional_ancho <- function(raw) {
  nc <- ncol(raw)

  k_pos <- which(apply(raw, 1, function(fila) any(es_k(fila))), arr.ind = TRUE)
  r_pos <- which(apply(raw, 1, function(fila) any(es_r(fila))), arr.ind = TRUE)

  if (length(k_pos) == 0 || length(r_pos) == 0) return(NULL)

  k_row <- k_pos[1]
  r_row <- r_pos[1]
  if (k_row == r_row) return(NULL)

  k_label_cols <- which(es_k(unlist(raw[k_row, ], use.names = FALSE)))
  r_label_cols <- which(es_r(unlist(raw[r_row, ], use.names = FALSE)))

  label_cols <- unique(c(k_label_cols, r_label_cols))
  posibles_columnas <- setdiff(seq_len(nc), label_cols)

  header_row <- max(1, min(k_row, r_row) - 1)

  out <- data.frame(
    conteo_pct = normalizar_pct_col(unlist(raw[header_row, posibles_columnas], use.names = FALSE)),
    k_pct = normalizar_pct_col(unlist(raw[k_row, posibles_columnas], use.names = FALSE)),
    r_pct = normalizar_pct_col(unlist(raw[r_row, posibles_columnas], use.names = FALSE))
  )

  out <- filtrar_nacional_valido(out)
  if (nrow(out) == 0) return(NULL)
  out
}

extraer_nacional <- function(raw) {
  if (!is.data.frame(raw)) stop("La hoja nacional no pudo leerse como tabla.")

  out <- extraer_nacional_tidy(raw)
  if (is.null(out) || nrow(out) == 0) out <- extraer_nacional_ancho(raw)

  if (is.null(out) || nrow(out) == 0) {
    stop(
      "No pude interpretar la hoja nacional. Usa columnas conteo_pct, k_pct y r_pct, ",
      "o una tabla horizontal donde una fila sea K, otra R y la fila superior tenga el avance de conteo."
    )
  }

  out
}

extraer_distritos_tidy <- function(raw, tipo_votos_distritos = "contados", distritos_total_conocido = character(0)) {
  tipo_votos_distritos <- match.arg(tipo_votos_distritos, c("contados", "totales"))

  max_filas <- min(nrow(raw), 25)

  for (i in seq_len(max_filas)) {
    header <- as.character(unlist(raw[i, ], use.names = FALSE))

    col_distrito <- buscar_columna_en_header(header, c("distrito", "circunscripcion", "territorio", "region", "zona"))
    col_k <- which(es_k(header))[1]
    col_r <- which(es_r(header))[1]

    col_votos_contados <- buscar_columna_en_header(header, c("votos.*contados", "contados.*votos", "total.*contado"))
    col_votos_total <- buscar_columna_en_header(header, c("total.*votos", "votos.*total", "votos_totales", "total_validos", "^total$"))
    col_conteo_pct <- buscar_columna_en_header(header, c("conteo", "pct.*contado", "contado", "avance"))

    col_votos_base <- if (tipo_votos_distritos == "contados") {
      if (!is.na(col_votos_contados)) col_votos_contados else col_votos_total
    } else {
      col_votos_total
    }

    if (!is.na(col_distrito) && !is.na(col_votos_base) && !is.na(col_k) && !is.na(col_r) && !is.na(col_conteo_pct)) {

      datos <- raw[(i + 1):nrow(raw), , drop = FALSE]

      votos_base <- limpiar_votos(datos[[col_votos_base]])
      conteo <- normalizar_pct_col(datos[[col_conteo_pct]])
      distrito <- as.character(datos[[col_distrito]])
      distrito_norm <- normalizar_id_distrito(distrito)

      total_conocido_por_excepcion <- distrito_norm %in% distritos_total_conocido
      fila_tipo <- ifelse(total_conocido_por_excepcion, "total_conocido", tipo_votos_distritos)

      out <- data.frame(
        distrito = distrito,
        distrito_norm = distrito_norm,
        votos_reportados = votos_base,
        votos_contados = ifelse(fila_tipo == "contados", votos_base, NA_real_),
        votos_totales_conocidos = ifelse(fila_tipo != "contados", votos_base, NA_real_),
        conteo_pct = conteo,
        k_pct = normalizar_pct_col(datos[[col_k]]),
        r_pct = normalizar_pct_col(datos[[col_r]]),
        tipo_votos_fila = fila_tipo,
        stringsAsFactors = FALSE
      )

      out <- out[!is.na(out$distrito) & out$distrito != "" &
                   !is.na(out$votos_reportados) &
                   !is.na(out$conteo_pct) &
                   !is.na(out$k_pct) & !is.na(out$r_pct) &
                   out$conteo_pct > 0 & out$conteo_pct <= 1 &
                   out$k_pct >= 0 & out$r_pct >= 0 &
                   (out$k_pct + out$r_pct) > 0.1, , drop = FALSE]

      if (nrow(out) > 0) return(out)
    }
  }

  NULL
}

extraer_distritos <- function(raw, tipo_votos_distritos = "contados", distritos_total_conocido = character(0)) {
  if (!is.data.frame(raw)) stop("La hoja distrital no pudo leerse como tabla.")

  out <- extraer_distritos_tidy(raw, tipo_votos_distritos = tipo_votos_distritos, distritos_total_conocido = distritos_total_conocido)

  if (is.null(out) || nrow(out) == 0) {
    stop(
      "No pude interpretar la hoja distrital. Debe contener columnas equivalentes a: ",
      "distrito, una columna de votos, k_pct, r_pct y conteo_pct."
    )
  }

  out
}

leer_datos <- function(
  archivo,
  hoja_nacional = 1,
  hoja_distritos = 2,
  tipo_votos_distritos = "contados",
  distritos_total_conocido = character(0)
) {
  nacional_raw <- readxl::read_excel(archivo, sheet = hoja_nacional, col_names = FALSE, .name_repair = "minimal")
  distritos_raw <- readxl::read_excel(archivo, sheet = hoja_distritos, col_names = FALSE, .name_repair = "minimal")

  list(
    nacional = extraer_nacional(nacional_raw),
    distritos = extraer_distritos(
      distritos_raw,
      tipo_votos_distritos = tipo_votos_distritos,
      distritos_total_conocido = distritos_total_conocido
    )
  )
}

preparar_distritos <- function(distritos) {
  d <- distritos

  d$votos_reportados <- limpiar_votos(d$votos_reportados)
  d$votos_contados <- limpiar_votos(d$votos_contados)
  d$votos_totales_conocidos <- limpiar_votos(d$votos_totales_conocidos)

  d$k_share <- d$k_pct
  d$r_share <- d$r_pct

  suma <- d$k_share + d$r_share
  d$k_share <- d$k_share / suma
  d$r_share <- d$r_share / suma

  d$votos_totales_estimados <- ifelse(
    !is.na(d$votos_totales_conocidos),
    d$votos_totales_conocidos,
    d$votos_contados / d$conteo_pct
  )

  d$votos_contados <- ifelse(
    is.na(d$votos_contados) & !is.na(d$votos_totales_conocidos),
    d$votos_totales_conocidos * d$conteo_pct,
    d$votos_contados
  )

  d <- d[complete.cases(d[, c("distrito", "votos_contados", "votos_totales_estimados", "k_share", "r_share")]), , drop = FALSE]

  if (nrow(d) == 0) stop("No quedaron distritos válidos después de limpiar los datos.")

  d$votos_totales_estimados <- pmax(d$votos_totales_estimados, d$votos_contados)
  d$votos_totales <- d$votos_totales_estimados

  d$votos_contados <- pmin(pmax(d$votos_contados, 0), d$votos_totales)
  d$avance <- d$votos_contados / d$votos_totales
  d$pendientes <- pmax(d$votos_totales - d$votos_contados, 0)

  d$k_actual <- d$votos_contados * d$k_share
  d$r_actual <- d$votos_contados * d$r_share

  d$k_pendiente_esperado <- d$pendientes * d$k_share
  d$r_pendiente_esperado <- d$pendientes * d$r_share

  d$k_final_estimado <- d$k_actual + d$k_pendiente_esperado
  d$r_final_estimado <- d$r_actual + d$r_pendiente_esperado

  d
}

calcular_nacional <- function(nacional) {
  x <- nacional[nrow(nacional), ]

  k <- x$k_pct
  r <- x$r_pct
  p <- x$conteo_pct

  if (any(is.na(c(k, r, p)))) stop("El último dato nacional completo contiene NA en conteo, K o R.")

  lider <- ifelse(k >= r, "K", "R")
  segundo <- ifelse(lider == "K", "R", "K")
  lider_pct <- max(k, r)
  segundo_pct <- min(k, r)

  d <- lider_pct - segundo_pct
  restante <- max(0, 1 - p)
  ventaja_sobre_total <- d * p

  irreversible <- ifelse(restante == 0, d > 0, ventaja_sobre_total > restante)
  conteo_min_irreversible <- ifelse(d > 0, 1 / (1 + d), 1)

  pct_necesario_segundo_pendiente <- ifelse(
    restante > 0,
    0.5 + ventaja_sobre_total / (2 * restante),
    Inf
  )

  data.frame(
    conteo_pct = p,
    lider = lider,
    segundo = segundo,
    lider_pct_contado = lider_pct,
    segundo_pct_contado = segundo_pct,
    diferencia_contada = d,
    ventaja_sobre_total = ventaja_sobre_total,
    pendiente_total = restante,
    matematicamente_irreversible = irreversible,
    conteo_min_irreversible = conteo_min_irreversible,
    pct_necesario_segundo_pendiente = pct_necesario_segundo_pendiente
  )
}

proyeccion_territorial <- function(distritos) {
  d <- preparar_distritos(distritos)

  total <- sum(d$votos_totales, na.rm = TRUE)
  total_contado <- sum(d$votos_contados, na.rm = TRUE)
  total_pendiente <- sum(d$pendientes, na.rm = TRUE)

  k_actual_total <- sum(d$k_actual, na.rm = TRUE)
  r_actual_total <- sum(d$r_actual, na.rm = TRUE)

  k_final <- sum(d$k_final_estimado, na.rm = TRUE)
  r_final <- sum(d$r_final_estimado, na.rm = TRUE)
  k_pend <- sum(d$k_pendiente_esperado, na.rm = TRUE)
  r_pend <- sum(d$r_pendiente_esperado, na.rm = TRUE)

  k_actual_pct <- k_actual_total / (k_actual_total + r_actual_total)
  r_actual_pct <- r_actual_total / (k_actual_total + r_actual_total)

  k_final_pct <- k_final / (k_final + r_final)
  r_final_pct <- r_final / (k_final + r_final)

  q_r_necesita <- ifelse(
    total_pendiente > 0,
    0.5 + (k_actual_total - r_actual_total) / (2 * total_pendiente),
    Inf
  )

  q_k_necesita <- ifelse(
    total_pendiente > 0,
    0.5 + (r_actual_total - k_actual_total) / (2 * total_pendiente),
    Inf
  )

  resumen <- data.frame(
    avance_ponderado_distrital = total_contado / total,
    votos_totales_estimados = total,
    votos_contados = total_contado,
    votos_pendientes_estimados = total_pendiente,
    k_actual_territorial_pct = k_actual_pct,
    r_actual_territorial_pct = r_actual_pct,
    k_final_estimado_pct = k_final_pct,
    r_final_estimado_pct = r_final_pct,
    ganador_final_estimado = ifelse(k_final_pct >= r_final_pct, "K", "R"),
    diferencia_final_estimado = abs(k_final - r_final) / (k_final + r_final),
    k_pendiente_esperado_pct = ifelse(total_pendiente > 0, k_pend / total_pendiente, NA_real_),
    r_pendiente_esperado_pct = ifelse(total_pendiente > 0, r_pend / total_pendiente, NA_real_),
    k_necesita_pendiente_territorial_pct = q_k_necesita,
    r_necesita_pendiente_territorial_pct = q_r_necesita
  )

  list(resumen = resumen, detalle = d)
}

validacion_total_nacional <- function(territorio, total_validos_nacional = NA_real_) {
  total_validos_nacional <- limpiar_votos(total_validos_nacional)

  if (is.na(total_validos_nacional) || total_validos_nacional <= 0) {
    return(data.frame(
      total_validos_nacional = NA_real_,
      total_distrital_estimado = territorio$resumen$votos_totales_estimados,
      diferencia_absoluta = NA_real_,
      diferencia_pct = NA_real_,
      advertencia = "Sin total nacional de votos válidos para validar."
    ))
  }

  total_dist <- territorio$resumen$votos_totales_estimados
  dif <- total_dist - total_validos_nacional
  dif_pct <- dif / total_validos_nacional

  advertencia <- if (abs(dif_pct) > 0.05) {
    "Advertencia: la suma de totales distritales estimados difiere en más de 5% del total nacional válido. Revisa si alguna fila usa votos contados como total, o al revés."
  } else {
    "Validación consistente: la suma distrital estimada está cerca del total nacional válido."
  }

  data.frame(
    total_validos_nacional = total_validos_nacional,
    total_distrital_estimado = total_dist,
    diferencia_absoluta = dif,
    diferencia_pct = dif_pct,
    advertencia = advertencia
  )
}

estabilidad_temporal <- function(nacional, ultimas_n = 5, umbral_cambio = 0.002) {
  if (nrow(nacional) < 2) {
    return(data.frame(
      observaciones_usadas = nrow(nacional),
      max_cambio_k = NA_real_,
      cambio_ultima_actualizacion_k = NA_real_,
      estable_temporalmente = NA
    ))
  }

  n <- min(ultimas_n, nrow(nacional))
  x <- tail(nacional, n)

  cambios <- abs(diff(x$k_pct))

  data.frame(
    observaciones_usadas = n,
    max_cambio_k = max(cambios, na.rm = TRUE),
    cambio_ultima_actualizacion_k = tail(cambios, 1),
    estable_temporalmente = max(cambios, na.rm = TRUE) <= umbral_cambio
  )
}

simulacion_montecarlo <- function(
  distritos,
  n_sim = 10000,
  n_efectivo_max = 5000,
  seed = 123
) {
  set.seed(seed)
  d <- preparar_distritos(distritos)

  k_actual_total <- sum(d$k_actual, na.rm = TRUE)
  r_actual_total <- sum(d$r_actual, na.rm = TRUE)
  total_pendiente <- sum(d$pendientes, na.rm = TRUE)

  sim_k_pend <- rep(0, n_sim)

  for (i in seq_len(nrow(d))) {
    u <- round(d$pendientes[i])
    if (is.na(u) || u <= 0) next

    n_eff <- min(round(d$votos_contados[i]), n_efectivo_max)
    if (is.na(n_eff) || n_eff <= 0) n_eff <- 10

    alpha <- d$k_share[i] * n_eff + 1
    beta <- d$r_share[i] * n_eff + 1

    theta <- stats::rbeta(n_sim, alpha, beta)
    sim_k_pend <- sim_k_pend + stats::rbinom(n_sim, size = u, prob = theta)
  }

  sim_k_final <- k_actual_total + sim_k_pend
  sim_r_final <- r_actual_total + (total_pendiente - sim_k_pend)

  total_final <- sim_k_final + sim_r_final
  k_pct <- sim_k_final / total_final
  r_pct <- sim_r_final / total_final

  resumen <- data.frame(
    n_sim = n_sim,
    prob_gana_k = mean(k_pct > r_pct),
    prob_gana_r = mean(r_pct > k_pct),
    k_pct_media = mean(k_pct),
    r_pct_media = mean(r_pct),
    k_pct_p025 = as.numeric(stats::quantile(k_pct, 0.025)),
    k_pct_p50 = as.numeric(stats::quantile(k_pct, 0.5)),
    k_pct_p975 = as.numeric(stats::quantile(k_pct, 0.975)),
    r_pct_p025 = as.numeric(stats::quantile(r_pct, 0.025)),
    r_pct_p50 = as.numeric(stats::quantile(r_pct, 0.5)),
    r_pct_p975 = as.numeric(stats::quantile(r_pct, 0.975))
  )

  muestras <- data.frame(
    k_pct = k_pct,
    r_pct = r_pct,
    ganador = ifelse(k_pct > r_pct, "K", "R")
  )

  list(resumen = resumen, muestras = muestras)
}

analizar_eleccion <- function(
  nacional,
  distritos,
  n_sim = 10000,
  n_efectivo_max = 5000,
  prob_corte = 0.995,
  margen_pendiente = 0.03,
  umbral_cambio_temporal = 0.002,
  ultimas_n = 5,
  seed = 123,
  total_validos_nacional = NA_real_
) {
  nat <- calcular_nacional(nacional)
  terr <- proyeccion_territorial(distritos)
  val <- validacion_total_nacional(terr, total_validos_nacional = total_validos_nacional)
  temp <- estabilidad_temporal(nacional, ultimas_n = ultimas_n, umbral_cambio = umbral_cambio_temporal)
  sim <- simulacion_montecarlo(distritos, n_sim = n_sim, n_efectivo_max = n_efectivo_max, seed = seed)

  lider <- nat$lider[1]
  segundo <- nat$segundo[1]

  prob_lider <- ifelse(lider == "K", sim$resumen$prob_gana_k, sim$resumen$prob_gana_r)

  pendiente_esperado_segundo <- ifelse(
    segundo == "K",
    terr$resumen$k_pendiente_esperado_pct,
    terr$resumen$r_pendiente_esperado_pct
  )

  brecha_segundo_nacional <- nat$pct_necesario_segundo_pendiente - pendiente_esperado_segundo

  pct_necesario_segundo_pendiente_territorial <- ifelse(
    segundo == "K",
    terr$resumen$k_necesita_pendiente_territorial_pct,
    terr$resumen$r_necesita_pendiente_territorial_pct
  )

  brecha_segundo_territorial <- pct_necesario_segundo_pendiente_territorial - pendiente_esperado_segundo

  ganador_final_estimado <- terr$resumen$ganador_final_estimado
  prob_ganador_final_estimado <- ifelse(
    ganador_final_estimado == "K",
    sim$resumen$prob_gana_k,
    sim$resumen$prob_gana_r
  )

  estadisticamente_estabilizado <- isTRUE(prob_ganador_final_estimado >= prob_corte) &&
    isTRUE(abs(brecha_segundo_territorial) >= margen_pendiente)

  estado <- if (isTRUE(nat$matematicamente_irreversible)) {
    "Matemáticamente irreversible"
  } else if (isTRUE(estadisticamente_estabilizado)) {
    paste0("Estadísticamente estabilizado para ", ganador_final_estimado, ", no matemáticamente irreversible")
  } else {
    paste0("Proyección territorial favorece a ", ganador_final_estimado, ", pero no cumple los criterios fijados de estabilización")
  }

  interpretacion <- paste0(
    "El líder nacional actual es ", lider, ". ",
    "El ganador proyectado territorialmente al 100% es ", ganador_final_estimado, ". ",
    "El segundo candidato nacional necesita ", formato_pct(nat$pct_necesario_segundo_pendiente),
    " de los votos pendientes según el umbral nacional agregado. ",
    "Pero, usando la base territorial, necesita ", formato_pct(pct_necesario_segundo_pendiente_territorial),
    " de los pendientes territoriales. ",
    "Según la proyección territorial, ese candidato recibiría aproximadamente ",
    formato_pct(pendiente_esperado_segundo),
    " de los votos pendientes. ",
    "La brecha territorial entre lo que necesita y lo esperado es ",
    formato_pct(brecha_segundo_territorial),
    ". La probabilidad simulada de victoria del ganador proyectado es ",
    formato_pct(prob_ganador_final_estimado),
    "."
  )

  diagnostico <- data.frame(
    estado = estado,
    lider = lider,
    segundo = segundo,
    ganador_final_estimado = ganador_final_estimado,
    prob_lider_nacional_actual = prob_lider,
    prob_ganador_final_estimado = prob_ganador_final_estimado,
    pct_necesario_segundo_pendiente_nacional = nat$pct_necesario_segundo_pendiente,
    pct_necesario_segundo_pendiente_territorial = pct_necesario_segundo_pendiente_territorial,
    pendiente_esperado_segundo = pendiente_esperado_segundo,
    brecha_segundo_nacional = brecha_segundo_nacional,
    brecha_segundo_territorial = brecha_segundo_territorial,
    interpretacion = interpretacion
  )

  list(
    diagnostico = diagnostico,
    nacional = nat,
    nacional_leida = nacional,
    territorio = terr,
    validacion = val,
    temporal = temp,
    simulacion = sim
  )
}
