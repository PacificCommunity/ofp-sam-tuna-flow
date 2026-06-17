input_dir <- Sys.getenv("KFLOW_INPUT_DIR", "inputs")
out_dir <- Sys.getenv("KFLOW_OUT_DIR", "outputs")
plot_title <- Sys.getenv("PLOT_TITLE", "Tuna key quantities smoke check")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

package_status <- data.frame(
  package = c("mfclshiny", "mfclkit", "mfclrtmb"),
  available = vapply(c("mfclshiny", "mfclkit", "mfclrtmb"), requireNamespace, logical(1), quietly = TRUE),
  stringsAsFactors = FALSE
)
utils::write.csv(package_status, file.path(out_dir, "plot-package-status.csv"), row.names = FALSE)

depletion_files <- list.files(input_dir, pattern = "^depletion-smoke[.]csv$", recursive = TRUE, full.names = TRUE)
if (!length(depletion_files)) {
  stop("No depletion-smoke.csv files were found in upstream inputs.", call. = FALSE)
}

read_one <- function(file) {
  x <- utils::read.csv(file, stringsAsFactors = FALSE)
  x$source_file <- file
  x
}
depletion <- do.call(rbind, lapply(depletion_files, read_one))
depletion$year <- suppressWarnings(as.integer(depletion$year))
for (name in intersect(c("depletion", "spawning_potential", "recruitment", "fishing_mortality"), names(depletion))) {
  depletion[[name]] <- suppressWarnings(as.numeric(depletion[[name]]))
}
depletion <- depletion[is.finite(depletion$year) & is.finite(depletion$depletion), , drop = FALSE]
dedupe_columns <- setdiff(names(depletion), "source_file")
depletion <- depletion[!duplicated(depletion[dedupe_columns]), , drop = FALSE]
if (!"model_token" %in% names(depletion)) {
  depletion$model_token <- "model"
}
depletion$model_token[is.na(depletion$model_token) | !nzchar(depletion$model_token)] <- "model"
if (!"model_key" %in% names(depletion)) {
  depletion$model_key <- depletion$model_token
}
depletion$model_key[is.na(depletion$model_key) | !nzchar(depletion$model_key)] <- depletion$model_token[is.na(depletion$model_key) | !nzchar(depletion$model_key)]
if (!"change_token" %in% names(depletion)) {
  depletion$change_token <- depletion$model_token
}
depletion$change_token[is.na(depletion$change_token) | !nzchar(depletion$change_token)] <- depletion$model_token[is.na(depletion$change_token) | !nzchar(depletion$change_token)]
if (!"plot_label" %in% names(depletion)) {
  depletion$plot_label <- if ("model_token" %in% names(depletion)) depletion$model_token else depletion$model_key
}
if (!"report_label" %in% names(depletion)) {
  depletion$report_label <- depletion$plot_label
}
utils::write.csv(depletion, file.path(out_dir, "depletion-smoke-combined.csv"), row.names = FALSE)
utils::write.csv(depletion, file.path(out_dir, "key-quantities-combined.csv"), row.names = FALSE)

quantity_labels <- c(
  depletion = "Depletion",
  spawning_potential = "Spawning potential",
  recruitment = "Recruitment",
  fishing_mortality = "Fishing mortality"
)
quantity_cols <- intersect(names(quantity_labels), names(depletion))
quantity_long <- do.call(rbind, lapply(quantity_cols, function(quantity) {
  out <- depletion
  out$quantity <- quantity
  out$quantity_label <- unname(quantity_labels[[quantity]])
  out$value <- suppressWarnings(as.numeric(out[[quantity]]))
  out[is.finite(out$value), , drop = FALSE]
}))
if (is.null(quantity_long) || !nrow(quantity_long)) {
  stop("No finite key-quantity values were available for plotting.", call. = FALSE)
}
utils::write.csv(quantity_long, file.path(out_dir, "key-quantities-long.csv"), row.names = FALSE)

plot_file <- file.path(out_dir, "key-quantities-smoke.svg")
png_file <- file.path(out_dir, "key-quantities-smoke.png")
report_figure_dir <- file.path(out_dir, "report-figures")
dir.create(report_figure_dir, recursive = TRUE, showWarnings = FALSE)
mfclshiny_status <- "not_available"
mfclshiny_figure_dir <- report_figure_dir

if (isTRUE(package_status$available[package_status$package == "mfclshiny"]) &&
    "build_report_figures" %in% getNamespaceExports("mfclshiny")) {
  mfclshiny_status <- tryCatch({
    result <- mfclshiny::build_report_figures(
      data = depletion,
      output_dir = mfclshiny_figure_dir,
      title = plot_title,
      figure_basename = "key-quantities-smoke",
      formats = c("png", "pdf", "svg"),
      width = 12,
      height = 8,
      dpi = 300,
      build_payloads = FALSE,
      overwrite = TRUE,
      species_label = Sys.getenv("FLOW_SPECIES_LABEL", "selected stock"),
      assessment_year = Sys.getenv("FLOW_ASSESSMENT_YEAR", "")
    )
    svg_src <- file.path(mfclshiny_figure_dir, "key-quantities-smoke.svg")
    png_src <- file.path(mfclshiny_figure_dir, "key-quantities-smoke.png")
    if (file.exists(svg_src)) {
      invisible(file.copy(svg_src, plot_file, overwrite = TRUE))
    }
    if (file.exists(png_src)) {
      invisible(file.copy(png_src, png_file, overwrite = TRUE))
    }
    paste("report_figures_ok:", nrow(result$figures), "figures")
  }, error = function(e) {
    paste("report_figures_skipped:", conditionMessage(e))
  })
} else if (isTRUE(package_status$available[package_status$package == "mfclshiny"])) {
  mfclshiny_status <- tryCatch({
    model_roots <- unique(dirname(depletion_files))
    # This builds payloads only when real MFCL raw outputs are present. For the
    # smoke path it records that the mfclshiny batch hook was reached.
    mfclshiny::build_model_payloads(model_roots[[1]], recursive = TRUE, overwrite = FALSE)
    "payload_attempted"
  }, error = function(e) {
    paste("payload_skipped:", conditionMessage(e))
  })
}
writeLines(mfclshiny_status, file.path(out_dir, "mfclshiny-status.txt"))

if ((!file.exists(plot_file) || !file.exists(png_file)) && requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot2::ggplot(
    quantity_long,
    ggplot2::aes(
      x = year,
      y = value,
      colour = plot_label,
      group = interaction(plot_label, model_key, region, quantity)
    )
  ) +
    ggplot2::geom_line(linewidth = 0.7, alpha = 0.8) +
    ggplot2::geom_point(size = 1.25, alpha = 0.85) +
    ggplot2::facet_grid(ggplot2::vars(quantity_label), ggplot2::vars(region), scales = "free_y") +
    ggplot2::labs(
      title = plot_title,
      subtitle = "MFCL smoke key derived quantities from payload or RepOut extraction",
      x = NULL,
      y = "Value",
      colour = "Model"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.title.position = "plot"
    )
  ggplot2::ggsave(plot_file, p, width = 10, height = 7, units = "in")
  ggplot2::ggsave(png_file, p, width = 10, height = 7, units = "in", dpi = 160)
} else if (!file.exists(plot_file)) {
  grDevices::svg(plot_file, width = 10, height = 5)
  old <- graphics::par(mar = c(4, 5, 3, 1), mfrow = c(ceiling(length(quantity_cols) / 2), 2))
  on.exit(graphics::par(old), add = TRUE)
  tokens <- unique(quantity_long$model_token)
  cols <- grDevices::hcl.colors(max(length(tokens), 1L), "Dark 3")
  for (quantity in quantity_cols) {
    panel <- quantity_long[quantity_long$quantity == quantity, , drop = FALSE]
    avg <- stats::aggregate(value ~ year + model_token, panel, mean)
    graphics::plot(
      range(avg$year),
      range(avg$value, finite = TRUE),
      type = "n",
      xlab = "",
      ylab = "Value",
      main = quantity_labels[[quantity]]
    )
    for (i in seq_along(tokens)) {
      x <- avg[avg$model_token == tokens[[i]], ]
      if (!nrow(x)) next
      graphics::lines(x$year, x$value, col = cols[[i]], lwd = 2)
      graphics::points(x$year, x$value, col = cols[[i]], pch = 19)
    }
  }
  graphics::legend("bottomleft", legend = tokens, col = cols, lwd = 2, bty = "n")
  grDevices::dev.off()
}

if (!file.exists(png_file) && file.exists(plot_file) && requireNamespace("rsvg", quietly = TRUE)) {
  rsvg::rsvg_png(plot_file, png_file, width = 1600, height = 800)
}
if (file.exists(png_file)) {
  file.copy(png_file, file.path(report_figure_dir, "key-quantities-smoke.png"), overwrite = TRUE)
}
if (file.exists(plot_file)) {
  file.copy(plot_file, file.path(report_figure_dir, "key-quantities-smoke.svg"), overwrite = TRUE)
}

summary <- stats::aggregate(
  value ~ model_key + model_token + change_token + quantity + quantity_label,
  quantity_long,
  function(x) round(mean(x, na.rm = TRUE), 3)
)
names(summary)[names(summary) == "value"] <- "mean_value"
summary$plot_file <- basename(plot_file)
report_figures <- list.files(report_figure_dir, pattern = "[.]png$", recursive = TRUE, full.names = FALSE)
report_figures <- sort(file.path("report-figures", report_figures))
summary$report_figure <- if ("report-figures/key-quantities-smoke.png" %in% report_figures) {
  "report-figures/key-quantities-smoke.png"
} else if ("report-figures/depletion-smoke.png" %in% report_figures) {
  "report-figures/depletion-smoke.png"
} else if (length(report_figures)) {
  report_figures[[1]]
} else {
  ""
}
summary$report_figures <- paste(report_figures, collapse = ",")
utils::write.csv(summary, file.path(out_dir, "key-quantities-plot-summary.csv"), row.names = FALSE)
utils::write.csv(summary, file.path(out_dir, "depletion-plot-summary.csv"), row.names = FALSE)
