# Handles generation of plots and outputs post-estimation
library(xpose.nlmixr2)
library(ggplot2)
library(gridExtra)

#' Generate standard Goodness-of-Fit plots and save as 6x6 PNGs
#'
#' @param fit The fitted nlmixr model
#' @param output_dir Directory to base the plots in
#' @param config The workflow configuration
generate_gof_plots <- function(fit, output_dir, config = NULL) {
  plot_dir <- file.path(output_dir, "plot")
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }
  
  # Create xpose database (optional, now unused but kept just in case)
  xp <- tryCatch({
    xpose_data_nlmixr(fit)
  }, error = function(e) {
    message("Warning: Failed to generate xpose data object (non-critical): ", e$message)
    return(NULL)
  })
  
  # --- Standard GOF plots (Custom NONMEM Style) ---
  tryCatch({
    fit_df <- as.data.frame(fit)
      
      # Ensure basic columns exist
      if (!"TIME" %in% names(fit_df) && "IDV" %in% names(fit_df)) {
        fit_df$TIME <- fit_df$IDV
      }
      
      # Theme for consistent look
      nm_theme <- theme_classic() + 
        theme(panel.grid.minor = element_blank(),
              strip.background = element_rect(fill = "white", color = "black"),
              legend.position = "bottom")

      # 1. DV vs PRED
      p1 <- ggplot(fit_df, aes(x = PRED, y = DV)) +
        geom_point(alpha = 0.5, color = "midnightblue") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
        geom_smooth(method = "loess", se = FALSE, color = "blue", formula = y ~ x) +
        labs(title = "DV vs population predictions (PRED)", x = "PRED", y = "DV") +
        nm_theme

      # 2. DV vs IPRED
      p2 <- NULL
      if ("IPRED" %in% names(fit_df)) {
        p2 <- ggplot(fit_df, aes(x = IPRED, y = DV)) +
          geom_point(alpha = 0.5, color = "midnightblue") +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
          geom_smooth(method = "loess", se = FALSE, color = "blue", formula = y ~ x) +
          labs(title = "DV vs individual predictions (IPRED)", x = "IPRED", y = "DV") +
          nm_theme
      }

      # 3. CWRES vs PRED/TIME/QQ
      p3 <- NULL; p4 <- NULL; p5 <- NULL
      if ("CWRES" %in% names(fit_df)) {
        p3 <- ggplot(fit_df, aes(x = PRED, y = CWRES)) +
          geom_point(alpha = 0.5, color = "midnightblue") +
          geom_hline(yintercept = 0) +
          geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
          geom_smooth(method = "loess", se = FALSE, color = "blue", formula = y ~ x) +
          labs(title = "CWRES vs PRED", x = "PRED", y = "CWRES") +
          nm_theme

        p4 <- ggplot(fit_df, aes(x = TIME, y = CWRES)) +
          geom_point(alpha = 0.5, color = "midnightblue") +
          geom_hline(yintercept = 0) +
          geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
          geom_smooth(method = "loess", se = FALSE, color = "blue", formula = y ~ x) +
          labs(title = "CWRES vs TIME", x = "TIME", y = "CWRES") +
          nm_theme
          
        p5 <- ggplot(fit_df, aes(sample = CWRES)) +
          stat_qq(color = "midnightblue") +
          stat_qq_line(color = "red") +
          labs(title = "QQ-plot of CWRES", x = "Theoretical Quantiles", y = "Sample Quantiles") +
          nm_theme
      }

      # Saving the newly built standard plots
      ggsave(file.path(plot_dir, "dv_vs_pred.png"), plot = p1, width = 6, height = 5, dpi = 150)
      if (!is.null(p2)) ggsave(file.path(plot_dir, "dv_vs_ipred.png"), plot = p2, width = 6, height = 5, dpi = 150)
      if (!is.null(p3)) ggsave(file.path(plot_dir, "cwres_vs_pred.png"), plot = p3, width = 6, height = 5, dpi = 150)
      if (!is.null(p4)) ggsave(file.path(plot_dir, "cwres_vs_idv.png"), plot = p4, width = 6, height = 5, dpi = 150)
      if (!is.null(p5)) ggsave(file.path(plot_dir, "cwres_qq.png"), plot = p5, width = 6, height = 5, dpi = 150)
      
    }, error = function(e) {
      message("Warning: Failed to generate custom GOF plots: ", e$message)
    })

    # --- ETA distribution histograms ---
    generate_eta_plots(xp, plot_dir, fit, config)

    # --- Individual subject plots (random sample of up to 9 subjects) ---
    generate_ind_plots(fit, plot_dir)

    message("Diagnostic plots (6x6 pngs) saved to: ", plot_dir)
}

#' Generate ETA distribution histograms and ETA vs. covariate plots
#'
#' @param xp An xpose_data object (unused now, kept for backward compatibility)
#' @param plot_dir Directory in which to save the PNGs
#' @param fit The fitted nlmixr model
#' @param config The workflow configuration
generate_eta_plots <- function(xp, plot_dir, fit, config = NULL) {
  tryCatch({
    fit_df <- as.data.frame(fit)
    
    # We only need one row per ID to look at ETAs
    cov_df <- fit_df[!duplicated(fit_df$ID), , drop = FALSE]
    eta_cols <- grep("^ETA[0-9]+$", names(cov_df), value = TRUE, ignore.case = TRUE)
    
    if (length(eta_cols) > 0) {
      # Manual eta distribution histogram
      plots_dist <- list()
      for (eta in eta_cols) {
        p_dist <- ggplot(cov_df, aes(x = .data[[eta]])) +
          geom_histogram(fill = "midnightblue", color = "white", alpha = 0.7, bins = 15) +
          geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
          labs(title = paste("Distribution of", eta), x = eta, y = "Count") +
          theme_bw()
        plots_dist[[(length(plots_dist) + 1)]] <- p_dist
      }
      
      p_eta_dist_combined <- do.call(gridExtra::arrangeGrob, c(plots_dist, ncol = min(3, length(plots_dist))))
      ggsave(
        filename = file.path(plot_dir, "eta_distribution.png"),
        plot     = p_eta_dist_combined,
        width    = 10, height = max(4, ceiling(length(plots_dist) / 3) * 3), units = "in", dpi = 300
      )
    } else {
      message("  Note: eta_distribution skipped (no ETAs found in fit)")
    }

    # Manual implementation of eta vs covariate plots
    covs <- if (!is.null(config) && !is.null(config$data$covariates)) config$data$covariates else character(0)

    
    if (length(covs) > 0) {
      fit_df <- as.data.frame(fit)
      
      # We only need one row per ID
      cov_df <- fit_df[!duplicated(fit_df$ID), , drop = FALSE]
      
      # Extract ETAs
      eta_cols <- grep("^ETA[0-9]+$", names(cov_df), value = TRUE, ignore.case = TRUE)
      
      if (length(eta_cols) > 0) {
        plots <- list()
        
        for (cov in covs) {
          if (cov %in% names(cov_df)) {
             for (eta in eta_cols) {
                # Determine if covariate is continuous or categorical
                is_num <- is.numeric(cov_df[[cov]]) && length(unique(na.omit(cov_df[[cov]]))) > 5
                
                if (is_num) {
                   p <- ggplot(cov_df, aes(x = .data[[cov]], y = .data[[eta]])) +
                     geom_point(alpha = 0.6, color = "midnightblue") +
                     geom_smooth(method = "loess", se = FALSE, color = "red") +
                     labs(x = cov, y = eta) +
                     theme_bw()
                } else {
                   # Treat as categorical/discrete
                   cov_df[[cov]] <- as.factor(cov_df[[cov]])
                   p <- ggplot(cov_df, aes(x = .data[[cov]], y = .data[[eta]])) +
                     geom_boxplot(fill = "lightblue", alpha = 0.7, outlier.shape = NA) +
                     geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
                     labs(x = cov, y = eta) +
                     theme_bw()
                }
                plots[[(length(plots) + 1)]] <- p
             }
          }
        }
        
        if (length(plots) > 0) {
          # Use gridExtra to arrange multiple plots
          p_eta_cov <- do.call(gridExtra::arrangeGrob, c(plots, ncol = min(3, length(plots))))
          ggsave(
            filename = file.path(plot_dir, "eta_vs_covariate.png"),
            plot     = p_eta_cov,
            width    = 10, height = max(4, ceiling(length(plots) / 3) * 3), units = "in", dpi = 300
          )
        } else {
          message("  Note: eta_vs_cov skipped (no covariate/eta pairs matched)")
        }
      } else {
        message("  Note: eta_vs_cov skipped (no ETAs found in fit)")
      }
    } else {
      message("  Note: eta_vs_cov skipped (no covariates declared in config)")
    }

    message("ETA diagnostic plots saved to: ", plot_dir)
  }, error = function(e) {
    message("Warning: Failed to generate ETA plots: ", e$message)
  })
}

#' Generate individual subject concentration-time plots 
#'
#' @param fit  The fitted nlmixr model object
#' @param plot_dir Directory in which to save the PNGs
#' @param n_subjects Maximum number of subjects to display (default NULL to plot all)
generate_ind_plots <- function(fit, plot_dir, n_subjects = NULL) {
  tryCatch({
    fit_df <- as.data.frame(fit)

    # Identify unique subjects
    all_ids  <- unique(fit_df$ID)
    if (!is.null(n_subjects) && length(all_ids) > n_subjects) {
      sampled  <- sort(sample(all_ids, n_subjects))
    } else {
      sampled  <- all_ids
    }
    
    # Split subjects into chunks of up to 9 per page
    chunk_size <- 9
    id_chunks <- split(sampled, ceiling(seq_along(sampled) / chunk_size))

    # Output a multi-page PDF in addition to PNGs
    pdf_path <- file.path(plot_dir, "individual_fits.pdf")
    pdf(pdf_path, width = 9, height = 9)

    for (i in seq_along(id_chunks)) {
      chunk_ids <- id_chunks[[i]]
      sub_df   <- fit_df[fit_df$ID %in% chunk_ids, ]

      # Build a ggplot panel: observed DV, IPRED, and PRED per subject
      p <- ggplot(sub_df, aes(x = TIME)) +
        geom_point(aes(y = DV),    colour = "black",  size = 1.5, na.rm = TRUE) +
        geom_line(aes(y = IPRED),  colour = "#E41A1C", linewidth = 0.8, na.rm = TRUE) +
        geom_line(aes(y = PRED),   colour = "#377EB8", linewidth = 0.8, linetype = "dashed", na.rm = TRUE) +
        facet_wrap(~ ID, scales = "free_y", ncol = 3, nrow = 3) +
        labs(
          title    = sprintf("Individual Fits (Page %d of %d)", i, length(id_chunks)),
          subtitle = "Black = DV  |  Red = IPRED  |  Blue dashed = PRED",
          x = "Time", y = "Concentration"
        ) +
        theme_bw(base_size = 10)
      
      # Print to PDF
      print(p)

      # Save PNG for each page
      if (length(id_chunks) == 1) {
        filename <- "individual_fits.png"
      } else {
        filename <- sprintf("individual_fits_page_%03d.png", i)
      }
      
      ggsave(
        filename = file.path(plot_dir, filename),
        plot     = p,
        width    = 9, height = 9, units = "in", dpi = 300
      )
    }
    dev.off()

    message(sprintf("Individual subject plots saved to: %s (total %d pages)", plot_dir, length(id_chunks)))
  }, error = function(e) {
    message("Warning: Failed to generate individual plots: ", e$message)
  })
}

#' Generate and save comprehensive summary to summary.txt
#'
#' Field names follow the nlmixr2 API documented at:
#' https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html
#'
#' Key mappings corrected from common mistakes:
#'   fit$objDF       — data frame with OBJF, AIC, BIC (not fit$objf / fit$aic)
#'   fit$parFixedDF  — fixed effects table (capital DF, not parFixedDf)
#'   fit$omega       — OMEGA covariance matrix
#'   fit$shrink      — shrinkage table including random effect RSE estimates
#'
#' @param fit The fitted nlmixr2 model object
#' @param output_dir Directory to save summary.txt
generate_parameter_table <- function(fit, output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  summary_path <- file.path(output_dir, "summary.txt")
  
  con <- file(summary_path, open = "wt")
  sink(con)
  on.exit({
    sink()
    close(con)
  })

  cat("=========================================\n")
  cat("        NLMIXR2 ESTIMATION SUMMARY       \n")
  cat("=========================================\n\n")

  # ── Objective function / information criteria ─────────────────────────────
  # fit$objDF is a data frame with columns OBJF, AIC, BIC (and others).
  # Scalar accessors fit$OBJF / fit$AIC / fit$BIC also work via nlmixr2's $
  # dispatch but are less reliable across versions — use objDF when present.
  cat("--- Model Fit Statistics ---\n")
  
  get_stat <- function(fit, prop_df, prop_direct) {
    tryCatch({
      v <- as.numeric(fit$objDF[[prop_df]][[1]])
      if (length(v) == 0) {
        if (prop_direct == "AIC") v <- as.numeric(AIC(fit))
        else if (prop_direct == "BIC") v <- as.numeric(BIC(fit))
        else v <- as.numeric(fit[[prop_direct]])
      }
      if (length(v) == 0) v <- NA_real_
      v[1]
    }, error = function(e) NA_real_)
  }

  objf_val <- get_stat(fit, "OBJF", "OBJF")
  aic_val  <- get_stat(fit, "AIC", "AIC")
  bic_val  <- get_stat(fit, "BIC", "BIC")

  cat(sprintf("Objective Function Value (OFV): %s\n",
              if (is.na(objf_val)) "N/A" else sprintf("%.4f", objf_val)))
  cat(sprintf("Akaike Information Criterion (AIC): %s\n",
              if (is.na(aic_val))  "N/A" else sprintf("%.4f", aic_val)))
  cat(sprintf("Bayesian Information Criterion (BIC): %s\n\n",
              if (is.na(bic_val))  "N/A" else sprintf("%.4f", bic_val)))

  # ── Fixed effects (THETAs) ────────────────────────────────────────────────
  # nlmixr2 API: fit$parFixedDF (capital DF)
  cat("--- Final Parameter Estimates (Fixed Effects) ---\n")
  pfe <- tryCatch(fit$parFixedDF, error = function(e) NULL)
  if (!is.null(pfe) && !is.null(nrow(pfe)) && nrow(pfe) > 0) {
    print(pfe)
  } else {
    cat("No fixed effects table returned.\n")
  }
  cat("\n")

  # ── Random effects (ETAs / OMEGA) ─────────────────────────────────────────
  # fit$omega — the estimated OMEGA covariance matrix
  # fit$shrink — shrinkage table; also contains eta RSE estimates
  cat("--- Final Estimates (Random Effects) ---\n")
  omega_mat <- tryCatch(fit$omega, error = function(e) NULL)
  if (!is.null(omega_mat)) {
    cat("OMEGA Matrix (variance-covariance):\n")
    print(omega_mat)
    cat("\n")
  } else {
    cat("OMEGA matrix not available.\n\n")
  }

  shrink_tbl <- tryCatch(fit$shrink, error = function(e) NULL)
  if (!is.null(shrink_tbl)) {
    cat("Shrinkage / RSE Summary:\n")
    print(shrink_tbl)
    cat("\n")
  }

  message("Detailed summary saved to: ", summary_path)
}

#' Generate and save individual predictions to CSV
#'
#' @param fit The fitted nlmixr model
#' @param output_dir Directory to save CSV
generate_predictions_table <- function(fit, output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  csv_path <- file.path(output_dir, "individual_predictions.csv")
  
  # The fit object in nlmixr can be directly coerced to a data frame
  # which contains ID, TIME, DV, PRED, IPRED, CWRES, IWRES, ETAs, and individual parameter estimates
  tryCatch({
    fit_df <- as.data.frame(fit)
    write.csv(fit_df, csv_path, row.names = FALSE)
    message("Individual predictions saved to: ", csv_path)
  }, error = function(e) {
    message("Warning: Failed to generate individual predictions CSV: ", e$message)
  })
}

#' Generate a fallback summary.txt for failed estimation
#'
#' @param output_dir Directory to save summary.txt
#' @param error_message Complete error message to print
generate_failed_summary <- function(output_dir, error_message) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  summary_path <- file.path(output_dir, "summary.txt")
  con <- file(summary_path, open = "wt")
  sink(con)
  on.exit({
    sink()
    close(con)
  })
  
  cat("=========================================\n")
  cat("        NLMIXR ESTIMATION SUMMARY        \n")
  cat("=========================================\n\n")
  
  cat("=========================================\n")
  cat("              RUN FAILED                 \n")
  cat("=========================================\n\n")
  cat(sprintf("Error Message: %s\n\n", error_message))
  
  cat("--- Model Fit Statistics ---\n")
  cat("Objective Function Value (OFV): 0\n")
  cat("Akaike Information Criterion (AIC): 0\n")
  cat("Bayesian Information Criterion (BIC): 0\n\n")
  
  cat("--- Final Parameter Estimates (Fixed Effects) ---\n")
  cat("0 (Run Failed)\n\n")
  
  cat("--- Final Vector/Matrix Estimates (Random Effects) ---\n")
  cat("OMEGA Matrix:\n0 (Run Failed)\n\n")
  cat("Random Effects Estimates (with RSE):\n0 (Run Failed)\n")
  
  message("Failed run generated fallback summary.txt at: ", summary_path)
}
