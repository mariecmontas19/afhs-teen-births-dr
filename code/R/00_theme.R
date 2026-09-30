# ============================================================================
# 00_theme.R — shared publication theme + palette for ALL figures (sourced after
# 00_config.R). One look across the paper/deck: clean typography, restrained grid,
# left-aligned bold titles, grey subtitles/captions, a cohesive accent palette, and
# colour-blind-safe hues. Use theme_pcua() for plots, theme_pcua_map() for maps.
# ============================================================================
suppressPackageStartupMessages(library(ggplot2))

# cohesive, colour-blind-friendly palette ------------------------------------
PCUA_COL <- list(
  ink    = "#1A1A2E",   # titles / text
  blue   = "#2166AC",   # Callaway-Sant'Anna (CS) / primary
  purple = "#762A83",   # CS + covariates
  green  = "#1B7837",   # triple-difference (DDD)
  orange = "#E08214",   # accent / second estimator
  red    = "#B2182B",   # emphasis / opening line
  teal   = "#35978F",   # tertiary
  gold   = "#F4B400",   # map markers (cool palettes)
  grey   = "#7A7A7A"
)
# discrete scales (qualitative) — for multi-series line plots
PCUA_QUAL <- c("#2166AC","#E08214","#1B7837","#762A83","#35978F","#B2182B","#9970AB","#80CDC1")

theme_pcua <- function(base_size = 13){
  theme_minimal(base_size = base_size) +
    theme(
      text             = element_text(color = PCUA_COL$ink),
      plot.title       = element_text(face = "bold", size = rel(1.12), color = PCUA_COL$ink,
                                       margin = margin(b = 2)),
      plot.subtitle    = element_text(size = rel(0.82), color = "grey35", margin = margin(b = 11)),
      plot.caption     = element_text(size = rel(0.60), color = "grey52", hjust = 0,
                                      margin = margin(t = 12)),
      axis.title       = element_text(size = rel(0.84), color = "grey25"),
      axis.title.x     = element_text(margin = margin(t = 7)),
      axis.title.y     = element_text(margin = margin(r = 7)),
      axis.text        = element_text(color = "grey30"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
      legend.position  = "top",
      legend.title     = element_text(size = rel(0.8)),
      legend.text      = element_text(size = rel(0.78)),
      legend.key.height = unit(10, "pt"),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin      = margin(15, 18, 11, 14)
    )
}

theme_pcua_map <- function(base_size = 13){
  theme_void(base_size = base_size) +
    theme(
      text          = element_text(color = PCUA_COL$ink),
      plot.title    = element_text(face = "bold", size = rel(1.10), color = PCUA_COL$ink,
                                   margin = margin(b = 2)),
      plot.subtitle = element_text(size = rel(0.80), color = "grey35", margin = margin(b = 6)),
      plot.caption  = element_text(size = rel(0.58), color = "grey52", hjust = 0, margin = margin(t = 8)),
      legend.title  = element_text(size = rel(0.74)),
      legend.text   = element_text(size = rel(0.72)),
      legend.position = "right",
      plot.title.position = "plot",
      plot.margin   = margin(12, 12, 10, 12)
    )
}

# event-study helper: dashed treatment line at -0.5 with a small label, faint
# post-period shading, zero line. Add AFTER the data layers, BEFORE theme_pcua().
es_guides <- function(label_y = Inf){
  list(
    annotate("rect", xmin = -0.5, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = "grey85", alpha = 0.18),
    geom_hline(yintercept = 0, color = "grey55", linewidth = 0.4),
    geom_vline(xintercept = -0.5, linetype = "22", color = "grey45", linewidth = 0.45),
    annotate("text", x = -0.5, y = label_y, label = "treatment  ", hjust = 1, vjust = 1.4,
             size = 3, color = "grey45", fontface = "italic")
  )
}

# Joint pre-trend test of the PLOTTED dynamic leads (default e in [-5,-2]), computed
# from the dynamic-aggregation influence functions. Works on BOTH a `did::aggte`
# (type="dynamic") object and a `triplediff` event-study aggregation — both expose
# $egt, $att.egt and $inf.function$dynamic.inf.func.e (an n_cluster x n_egt matrix).
# SE is the analytic clustered influence-function SE (= sqrt(sum((IF%*%w)^2))/n), the
# same convention `did`/`triplediff` use for point SEs. Returns avg lead, SE, two-sided
# p, and k = number of leads tested. The equal-weighted contrast tests "are the plotted
# pre-period leads jointly zero on average" — matching exactly what the event-study draws.
es_pretrend_p <- function(agg, lo = -5, hi = -2){
  IF  <- agg$inf.function$dynamic.inf.func.e
  egt <- agg$egt; att <- agg$att.egt
  pre <- which(egt >= lo & egt <= hi)
  if (is.null(IF) || length(pre) == 0) return(list(avg = NA_real_, se = NA_real_, p = NA_real_, k = 0L))
  n <- nrow(IF); w <- rep(0, length(egt)); w[pre] <- 1 / length(pre)
  avg <- sum(w * att); se <- sqrt(sum(as.numeric(IF %*% w)^2)) / n
  list(avg = avg, se = se, p = 2 * pnorm(-abs(avg / se)), k = length(pre))
}

# ggsave_pair(): writes the PNG exactly as before AND a vector PDF twin of the same
# size (JDE requires vector charts). cairo_pdf is unavailable on this machine (no
# XQuartz), so the PDF uses the base device and Ghostscript embeds the fonts.
ggsave_pair <- function(filename, plot = ggplot2::last_plot(), width, height, dpi = 200, ...) {
  ggplot2::ggsave(filename, plot, width = width, height = height, dpi = dpi, ...)
  pdf_f <- sub("\\.png$", ".pdf", filename)
  stopifnot(pdf_f != filename)
  ggplot2::ggsave(pdf_f, plot, width = width, height = height,
                  device = grDevices::pdf, useDingbats = FALSE)
  grDevices::embedFonts(pdf_f)
  invisible(pdf_f)
}
