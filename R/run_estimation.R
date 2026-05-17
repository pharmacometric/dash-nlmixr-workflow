# Configures and runs the actual nlmixr2 estimation engines.
# Control objects are built using the method-specific constructors
# (saemControl, foceiControl, etc.) so that nlmixr2 receives the exact
# type it expects rather than a plain list.
library(nlmixr2)

#' Run nlmixr2 estimation with the provided configuration
#'
#' @param model_ui  The nlmixr2 model function built by build_nlmixr_model().
#' @param data      The prepared data frame (NONMEM/RxODE compatible columns).
#' @param est_options A list of estimation options parsed from the JSON config.
#'   Recognised keys: method, print_iterations, maxeval (FOCEi), n_burn (SAEM),
#'   n_em (SAEM), n_iter (SAEM alias for n_em).
#' @return The fitted nlmixr2 model object.
run_nlmixr_estimation <- function(model_ui, data, est_options = list()) {

  # ── 1. Resolve estimation method ─────────────────────────────────────────
  method <- if (!is.null(est_options$method)) est_options$method else "FOCE"
  method_upper <- toupper(trimws(method))

  if (method_upper %in% c("FOCE", "FOCEI")) {
    est_method <- "focei"
  } else if (method_upper == "SAEM") {
    est_method <- "saem"
  } else if (method_upper == "NLME") {
    est_method <- "nlme"
  } else if (method_upper %in% c("POSTHOC", "FOCE-POSTHOC")) {
    est_method <- "posthoc"
  } else {
    est_method <- tolower(method)
  }

  print_n <- if (!is.null(est_options$print_iterations))
    as.integer(est_options$print_iterations) else 0L

  # ── 2. Build method-specific control object ───────────────────────────────
  # nlmixr2 dispatches internally on the class of the control object, so a
  # plain list() can silently discard settings or cause type errors.
  # Always use the dedicated constructors: saemControl(), foceiControl(), etc.
  control_obj <- tryCatch({

    if (est_method == "saem") {
      # saemControl key options: print, n.burn, n.em, seed
      saem_args <- list(print = print_n)
      # n_burn overrides SAEM burn-in iterations
      if (!is.null(est_options$n_burn))
        saem_args$n.burn <- as.integer(est_options$n_burn)
      # n_iter / n_em overrides SAEM EM iterations
      n_em <- est_options$n_em %||% est_options$n_iter
      if (!is.null(n_em))
        saem_args$n.em <- as.integer(n_em)

      do.call(saemControl, saem_args)

    } else if (est_method == "focei") {
      # foceiControl key options: print, maxeval, covMethod
      focei_args <- list(print = print_n)
      if (!is.null(est_options$maxeval))
        focei_args$maxeval <- as.integer(est_options$maxeval)

      do.call(foceiControl, focei_args)

    } else {
      # For nlme, posthoc, and other methods fall back to an empty list —
      # nlmixr2 will apply its own defaults.
      list()
    }

  }, error = function(e) {
    message("  Warning: Could not build typed control object for '",
            est_method, "': ", e$message,
            ". Falling back to list().")
    list()
  })

  # ── 3. Log the execution plan ─────────────────────────────────────────────
  message(sprintf("  Estimation method : %s", est_method))
  message(sprintf("  Control object    : %s", class(control_obj)[1]))

  # ── 4. Execute the fit, add cwres ────────────────────────────────────────────────────
  fit <- tryCatch(
    addCwres(nlmixr2(model_ui, data, est = est_method, control = control_obj)),
    error = function(e) stop("nlmixr2 estimation failed: ", e$message)
  )

  return(fit)
}

# Minimal null-coalescing helper (base R has no %||%)
`%||%` <- function(a, b) if (!is.null(a)) a else b
