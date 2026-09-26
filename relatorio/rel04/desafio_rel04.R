# DESAFIO FINAL — SCRIPT R INDEPENDENTE
# Execute na pasta do CSV. A função não usa objetos criados nas questões anteriores.
analisar_obras <- function(arquivo = "dados_engenharia_civil.csv",
                           pasta_saida = "saidas_desafio") {
  pacotes <- c("dplyr", "ggplot2", "plotly", "highcharter", "leaflet", "htmlwidgets", "scales")
  faltam <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltam)) stop("Instale os pacotes: ", paste(faltam, collapse = ", "))
  # Funções utilizadas explicitamente; qualquer RStudio pode executar este script.
  suppressPackageStartupMessages({
    library(dplyr); library(ggplot2); library(plotly)
    library(highcharter); library(leaflet)
  })
  fmt <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")
  pal <- c("Residencial" = "#2675A5", "Comercial" = "#CA863B",
           "Industrial" = "#6D65A8", "Infraestrutura" = "#298879")
  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

  # 1. INSPEÇÃO: a importação é verificada antes dos gráficos.
  base <- read.csv(arquivo, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  colunas <- c("id_obra", "tipo_obra", "regiao", "area_construida", "custo_total",
               "prazo_meses", "resistencia_mpa", "consumo_cimento", "indice_falhas", "satisfacao", "sustentavel")
  if (!all(colunas %in% names(base))) stop("O CSV não contém todas as colunas necessárias.")
  if (anyNA(base)) stop("Há ausências: investigue antes de interpretar os gráficos.")
  if (any(base$area_construida <= 0)) stop("A área deve ser positiva.")
  inspeção <- capture.output({
    print(head(base)); print(tail(base)); print(dim(base))
    str(base); dplyr::glimpse(base); print(summary(base))
    print(colSums(is.na(base)))
  })
  writeLines(inspeção, file.path(pasta_saida, "01_inspecao.txt"), useBytes = TRUE)
  resumo <- base |> group_by(tipo_obra) |>
    summarise(obras = n(), media = mean(custo_total), mediana = median(custo_total),
              aiq = IQR(custo_total), .groups = "drop") |> arrange(desc(media))
  write.csv2(resumo, file.path(pasta_saida, "02_resumo_tipo.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  base <- base |> mutate(tipo_obra = factor(tipo_obra, levels = resumo$tipo_obra),
                         custo_mi = custo_total/1e6)

  # 2. BASE R: distribuição de resistência. Classes iguais e média destacada.
  png(file.path(pasta_saida, "03_base_R.png"), width = 1440, height = 850, res = 150)
  par(mar = c(5, 5, 4, 1), las = 1, bty = "l")
  hist(base$resistencia_mpa, breaks = seq(floor(min(base$resistencia_mpa)/2.5)*2.5,
    ceiling(max(base$resistencia_mpa)/2.5)*2.5, by = 2.5),
    col = "#AACCDC", border = "white", main = "Distribuição da resistência",
    xlab = "Resistência (MPa)", ylab = "Número de obras")
  abline(v = mean(base$resistencia_mpa), col = "#298879", lwd = 2)
  legend("topright", "Média", col = "#298879", lwd = 2, bty = "n")
  dev.off()

  # 3. GGPLOT2 AUTORAL: pontos + retas, facetas e escala manual de cor.
  # Facetas em ordem de custo médio; escalas fixas permitem comparação direta.
  grafico_autoral <- ggplot(base, aes(area_construida, custo_mi, colour = tipo_obra)) +
    geom_point(alpha = .55, size = 1.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = .8) +
    facet_wrap(~tipo_obra, ncol = 2) + scale_colour_manual(values = pal) +
    scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Área e custo: tendências por tipologia", x = "Área (m²)", y = "Custo (R$ milhões)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"), plot.margin = margin(12,18,12,12))
  ggsave(file.path(pasta_saida, "04_ggplot_autoral.png"), grafico_autoral,
         width = 10, height = 7, dpi = 150, bg = "white")

  # 4. PLOTLY: identificação individual sem multiplicar rótulos sobre os pontos.
  b_hover <- base |> mutate(detalhe = paste0("Obra: ", id_obra, "<br>Tipo: ", tipo_obra,
    "<br>Região: ", regiao, "<br>Custo: R$ ", fmt(custo_total), "<br>Área: ", fmt(area_construida), " m²"))
  p <- ggplot(b_hover, aes(area_construida, custo_mi, colour = tipo_obra, text = detalhe)) +
    geom_point(alpha = .65) + scale_colour_manual(values = pal) +
    labs(x = "Área (m²)", y = "Custo (R$ milhões)", colour = "Tipo") +
    theme_minimal() + theme(legend.position = "bottom")
  interativo <- ggplotly(p, tooltip = "text", height = 550) |>
    layout(margin = list(l = 85, r = 25, b = 110, t = 40),
           legend = list(orientation = "h", x = 0, y = -.22)) |>
    config(displaylogo = FALSE, responsive = TRUE)
  htmlwidgets::saveWidget(interativo, file.path(pasta_saida, "05_plotly.html"), selfcontained = TRUE)

  # 5. HIGHCHARTER: maior custo médio primeiro; barras partem de zero.
  dados_hc <- resumo |> mutate(pos = row_number()-1, media_mi = media/1e6)
  executivo <- hchart(dados_hc, "column", hcaes(x = pos, y = media_mi), color = "#2675A5") |>
    hc_title(text = "Custo médio por tipo") |> hc_size(height = 480) |>
    hc_xAxis(categories = dados_hc$tipo_obra, title = list(text = NULL)) |>
    hc_yAxis(min = 0, title = list(text = "R$ milhões")) |>
    hc_legend(enabled = FALSE) |>
    hc_plotOptions(column = list(dataLabels = list(enabled = TRUE, format = "{point.y:.2f}"))) |>
    hc_tooltip(pointFormat = "Média: R$ {point.y:.2f} milhões")
  htmlwidgets::saveWidget(executivo, file.path(pasta_saida, "06_highcharter.html"), selfcontained = TRUE)

  # 6. LEAFLET: agregação regional, marcadores, popups e destaque explícito.
  capitais <- data.frame(regiao = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"),
    lat = c(-3.12,-12.97,-15.79,-23.55,-30.03), lon = c(-60.02,-38.50,-47.88,-46.63,-51.23))
  cont <- as.data.frame(table(base$regiao), stringsAsFactors = FALSE)
  names(cont) <- c("regiao", "n")
  geo <- left_join(capitais, cont, by = "regiao") |> arrange(desc(n))
  destaque <- geo |> filter(n == max(n))
  mapa <- leaflet(geo, width = "100%", height = 560,
                   options = leafletOptions(scrollWheelZoom = FALSE)) |>
    addProviderTiles(providers$Esri.WorldTopoMap) |>
    setView(lng = -52, lat = -15, zoom = 4) |>
    addMarkers(lng = ~lon, lat = ~lat,
      popup = ~paste0("<b>", regiao, "</b><br>", n, " obras<br>Referência regional ilustrativa.")) |>
    addCircles(data = destaque, lng = ~lon, lat = ~lat, radius = 150000,
      color = "#CA863B", fillOpacity = .2) |>
    addLegend(position = "bottomright", colors = "#CA863B",
      labels = paste0(destaque$regiao, ": ", destaque$n, " obras"), title = "Maior contagem")
  htmlwidgets::saveWidget(mapa, file.path(pasta_saida, "07_leaflet.html"), selfcontained = TRUE)

  # 7. INTERPRETAÇÕES E JUSTIFICATIVAS: sempre vinculadas aos gráficos exportados.
  leitura <- c(
    "DESAFIO FINAL — INTERPRETAÇÃO DOS GRÁFICOS",
    paste0("Base analisada: ", nrow(base), " obras e ", ncol(base)-1, " variáveis originais."),
    paste0("03 — Base R: resistência média ", fmt(mean(base$resistencia_mpa)),
      " MPa; mediana ", fmt(median(base$resistencia_mpa)), ". Histograma mostra concentração central; não é teste de normalidade nem de conformidade. Classes iguais evitam distorção e a linha destaca a média."),
    paste0("04 — ggplot2: correlação global área-custo = ", fmt(cor(base$area_construida, base$custo_total),3),
      ". A associação positiva permanece dentro das tipologias. Facetas com escalas fixas separam grupos; pontos e retas usam a mesma cor. Tipologia e escopo precisam acompanhar a interpretação de custo."),
    "05 — plotly: permite consultar obra, tipo, região, área e custo sem cobrir a nuvem de pontos. Obras com custos próximos podem ter áreas diferentes; hover apoia triagem, não diagnóstico de irregularidade.",
    paste0("06 — highcharter: ", resumo$tipo_obra[1], " tem o maior custo médio (R$ ", fmt(resumo$media[1]),
      "). A ordenação decrescente facilita o ranking; eixo iniciado em zero mantém a comparação proporcional. A média não substitui análise da dispersão."),
    paste0("07 — leaflet: ", geo$regiao[1], " reúne ", geo$n[1], " obras; ", tail(geo$regiao,1),
      " reúne ", tail(geo$n,1), ". Marcadores representam totais em referências regionais. Popups detalham contagens; o círculo apenas destaca a maior região, sem delimitar área das obras."),
    "Limites: amostra didática; associação não comprova causalidade. Não há coordenadas reais nem dados suficientes para aprovar concreto, inferir atraso ou estimar efeito da sustentabilidade."
  )
  writeLines(leitura, file.path(pasta_saida, "08_interpretacoes.txt"), useBytes = TRUE)
  writeLines(capture.output(sessionInfo()), file.path(pasta_saida, "09_ambiente.txt"))
  invisible(list(resumo = resumo, regioes = geo, interpretacoes = leitura,
                 arquivos = list.files(pasta_saida, full.names = TRUE)))
}

resultado_desafio <- analisar_obras()

