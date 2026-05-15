# Dynamically constructs the nlmixr2 model function using the parsed JSON config.
# Supports NONMEM ADVAN1/2/3/4/10/11/12 model types, inferring the correct
# compartmental ODEs, IPRED expression, and residual error model automatically.
library(nlmixr2)

# =============================================================================
# ADVAN → nlmixr2 ODE specification table
# =============================================================================

#' Return compartment ODEs, the IPRED line, and a human-readable note for a
#' given NONMEM ADVAN model type.
#'
#' Returns NULL for unrecognised types; the caller falls back to JSON-supplied
#' differential_equations in that case.
#'
#' @param model_type Character string, e.g. "ADVAN2" or "advan4".
#' @return Named list with elements \code{note}, \code{odes} (character vector),
#'   and \code{ipred} (single character), or NULL.
get_advan_ode_spec <- function(model_type) {
  mt <- toupper(trimws(as.character(model_type)))

  specs <- list(

    # ------------------------------------------------------------------
    # ADVAN1 — 1-compartment IV bolus (linear elimination)
    # Parameters: CL (clearance), V (central volume)
    # ------------------------------------------------------------------
    ADVAN1 = list(
      note  = "1-compartment IV bolus",
      odes  = c(
        "d/dt(central) <- -(CL / V) * central"
      ),
      ipred = "IPRED <- central / V"
    ),

    # ------------------------------------------------------------------
    # ADVAN2 — 1-compartment, first-order absorption (oral / SC)
    # Parameters: CL, V, KA (absorption rate constant)
    # ------------------------------------------------------------------
    ADVAN2 = list(
      note  = "1-compartment first-order absorption",
      odes  = c(
        "d/dt(depot)   <- -KA * depot",
        "d/dt(central) <- KA * depot - (CL / V) * central"
      ),
      ipred = "IPRED <- central / V"
    ),

    # ------------------------------------------------------------------
    # ADVAN3 — 2-compartment IV bolus
    # Parameters: CL (central clearance), V1 (central volume),
    #             Q  (inter-compartmental clearance), V2 (peripheral volume)
    # ------------------------------------------------------------------
    ADVAN3 = list(
      note  = "2-compartment IV bolus",
      odes  = c(
        "d/dt(central)    <- -((CL + Q) / V1) * central + (Q / V2) * peripheral",
        "d/dt(peripheral) <-   (Q / V1) * central        - (Q / V2) * peripheral"
      ),
      ipred = "IPRED <- central / V1"
    ),

    # ------------------------------------------------------------------
    # ADVAN4 — 2-compartment, first-order absorption (oral / SC)
    # Parameters: CL, V2 (central volume), Q, V3 (peripheral volume), KA
    # Note: NONMEM ADVAN4 labels the central compartment as #2 (V2) and
    #       the peripheral as #3 (V3) to reserve #1 for the depot.
    # ------------------------------------------------------------------
    ADVAN4 = list(
      note  = "2-compartment first-order absorption",
      odes  = c(
        "d/dt(depot)      <- -KA * depot",
        "d/dt(central)    <- KA * depot - ((CL + Q) / V2) * central + (Q / V3) * peripheral",
        "d/dt(peripheral) <- (Q / V2) * central - (Q / V3) * peripheral"
      ),
      ipred = "IPRED <- central / V2"
    ),

    # ------------------------------------------------------------------
    # ADVAN10 — 1-compartment Michaelis-Menten (non-linear) elimination
    # Parameters: VM (maximum elimination rate), KM (Michaelis constant),
    #             V  (central volume)
    # ------------------------------------------------------------------
    ADVAN10 = list(
      note  = "1-compartment Michaelis-Menten elimination",
      odes  = c(
        "d/dt(central) <- -(VM * (central / V)) / (KM + (central / V))"
      ),
      ipred = "IPRED <- central / V"
    ),

    # ------------------------------------------------------------------
    # ADVAN11 — 3-compartment IV bolus
    # Parameters: CL, V1, Q2, V2, Q3, V3
    # ------------------------------------------------------------------
    ADVAN11 = list(
      note  = "3-compartment IV bolus",
      odes  = c(
        "d/dt(central)      <- -((CL + Q2 + Q3) / V1) * central + (Q2 / V2) * peripheral1 + (Q3 / V3) * peripheral2",
        "d/dt(peripheral1)  <-   (Q2 / V1) * central - (Q2 / V2) * peripheral1",
        "d/dt(peripheral2)  <-   (Q3 / V1) * central - (Q3 / V3) * peripheral2"
      ),
      ipred = "IPRED <- central / V1"
    ),

    # ------------------------------------------------------------------
    # ADVAN12 — 3-compartment, first-order absorption (oral / SC)
    # Parameters: CL, V2, Q3, V3, Q4, V4, KA
    # NONMEM convention: depot=#1, central=#2, peripheral1=#3, peripheral2=#4
    # ------------------------------------------------------------------
    ADVAN12 = list(
      note  = "3-compartment first-order absorption",
      odes  = c(
        "d/dt(depot)        <- -KA * depot",
        "d/dt(central)      <- KA * depot - ((CL + Q3 + Q4) / V2) * central + (Q3 / V3) * peripheral1 + (Q4 / V4) * peripheral2",
        "d/dt(peripheral1)  <-  (Q3 / V2) * central - (Q3 / V3) * peripheral1",
        "d/dt(peripheral2)  <-  (Q4 / V2) * central - (Q4 / V4) * peripheral2"
      ),
      ipred = "IPRED <- central / V2"
    )
  )

  specs[[mt]]   # returns NULL automatically for unknown keys
}


# =============================================================================
# NONMEM → nlmixr2 syntax translation
# =============================================================================

#' Translate a vector of NONMEM code lines to nlmixr2-compatible R syntax.
#'
#' Handles: parenthesised THETA/ETA/EPS/A references, NONMEM built-in
#' functions (EXP, LOG, SQRT, ABS, INT, LOG10), and bare = assignments
#' (skipping ==, !=, <=, >=).  Strips inline ; comments and blank lines.
#'
#' @param lines Character vector of raw NONMEM code strings.
#' @return Filtered, translated character vector.
translate_nonmem_syntax <- function(lines) {
  lines <- as.character(lines)
  translated <- sapply(lines, function(x) {

    # Strip inline NONMEM comments
    x <- sub(";.*$", "", x)
    x <- trimws(x)
    if (nchar(x) == 0) return("")

    # Parenthesised population parameter references → plain names
    x <- gsub("THETA\\(([0-9]+)\\)", "THETA\\1", x, perl = TRUE)
    x <- gsub("ETA\\(([0-9]+)\\)",   "ETA\\1",   x, perl = TRUE)
    x <- gsub("EPS\\(([0-9]+)\\)",   "eps(\\1)", x, perl = TRUE)
    x <- gsub("A\\(([0-9]+)\\)",     "A\\1",     x, perl = TRUE)

    # Bare EPS without parentheses (e.g. EPS1 from phikl-parse)
    x <- gsub("\\bEPS([0-9]+)\\b", "eps(\\1)", x, perl = TRUE)

    # NONMEM built-in functions → R equivalents (word-boundary aware)
    x <- gsub("\\bEXP\\(",   "exp(",         x)
    x <- gsub("\\bLOG10\\(", "log10(",       x)
    x <- gsub("\\bLOG\\(",   "log(",         x)
    x <- gsub("\\bSQRT\\(",  "sqrt(",        x)
    x <- gsub("\\bABS\\(",   "abs(",         x)
    x <- gsub("\\bINT\\(",   "as.integer(",  x)
    x <- gsub("\\bMOD\\(",   "%%",           x)

    # Assignment: bare = → <-  (perl lookaround skips ==, !=, <=, >=)
    x <- gsub("(?<![=!<>])=(?!=)", " <- ", x, perl = TRUE)

    x
  }, USE.NAMES = FALSE)

  # Drop empty lines produced by blank/comment-only input
  translated[nchar(trimws(translated)) > 0]
}


# =============================================================================
# Error model helpers
# =============================================================================

#' Infer the residual error structure from NONMEM error_code lines.
#'
#' Detects whether the error model is additive, proportional, or combined by
#' scanning for patterns where EPS appears multiplied by IPRED/F (proportional)
#' or as a bare additive term (additive).
#'
#' @param error_lines Character vector of raw NONMEM error_code lines.
#' @return One of "add", "prop", or "combined".
infer_error_type <- function(error_lines) {
  txt <- paste(as.character(error_lines), collapse = " ")

  # Proportional: EPS multiplied by IPRED or F
  has_prop <- grepl(
    "(IPRED|F)\\s*\\*\\s*EPS|EPS\\s*\\*\\s*(IPRED|F)",
    txt, ignore.case = TRUE, perl = TRUE
  )
  # Additive: EPS appears as a simple sum term (not next to a multiplication)
  has_add <- grepl(
    "\\+\\s*EPS(\\([0-9]+\\)|[0-9]+)(?!\\s*\\*)|EPS(\\([0-9]+\\)|[0-9]+)\\s*$",
    txt, ignore.case = TRUE, perl = TRUE
  )

  if (has_prop && has_add) return("combined")
  if (has_prop)            return("prop")
  return("add")
}

#' Build the ini residual-error lines and the Y formula for the model block.
#'
#' Converts NONMEM SIGMA variances to SDs for nlmixr2's ini declarations, and
#' produces the appropriate \code{Y ~} expression for the model block.
#'
#' @param sigma_vals List of numeric SIGMA initial values (variances).
#' @param error_type One of "add", "prop", or "combined".
#' @return Named list: \code{ini_lines} (character vector),
#'   \code{model_expr} (single character string).
build_error_spec <- function(sigma_vals, error_type) {

  safe_sd <- function(v) sqrt(max(as.numeric(v), 1e-10))

  # NOTE: The endpoint variable in the ~ expression MUST be a variable that is
  # computed in the model block (e.g. IPRED <- central / V).  Using 'Y' here
  # causes nlmixr2 to error "endpoint 'Y' is not defined in the model" because
  # Y is never assigned anywhere.  IPRED is always computed by the ODE spec.

  if (error_type == "prop") {
    sd1 <- safe_sd(sigma_vals[[1]])
    return(list(
      ini_lines  = sprintf("prop.err <- %f", sd1),
      model_expr = "IPRED ~ prop(prop.err)"
    ))
  }

  if (error_type == "add") {
    sd1 <- safe_sd(sigma_vals[[1]])
    return(list(
      ini_lines  = sprintf("add.err <- %f", sd1),
      model_expr = "IPRED ~ add(add.err)"
    ))
  }

  # combined
  sd_add  <- safe_sd(if (length(sigma_vals) >= 1) sigma_vals[[1]] else 0.01)
  sd_prop <- safe_sd(if (length(sigma_vals) >= 2) sigma_vals[[2]] else sigma_vals[[1]])
  list(
    ini_lines  = c(
      sprintf("add.err  <- %f", sd_add),
      sprintf("prop.err <- %f", sd_prop)
    ),
    model_expr = "IPRED ~ add(add.err) + prop(prop.err)"
  )
}


# =============================================================================
# Main builder
# =============================================================================

#' Build the UI function for nlmixr2 from a parsed JSON configuration.
#'
#' Dispatches on \code{parsed_config$model$model_type} (ADVAN1–ADVAN12) to
#' generate the correct compartmental ODEs and IPRED expression.  Falls back to
#' any \code{differential_equations} block in the JSON for custom / unrecognised
#' model types.  Error model type (additive / proportional / combined) is
#' inferred automatically from \code{error_code}.
#'
#' @param parsed_config The list returned by \code{parse_json_config()}.
#' @return A parsed nlmixr2 model function ready to pass to \code{nlmixr2()}.
build_nlmixr_model <- function(parsed_config) {

  model_type <- parsed_config$model$model_type

  # ── 1. ini block: fixed-effect parameters (THETAs) ───────────────────────
  ini_lines <- c()
  thetas <- parsed_config$parameters$theta
  original_theta_names <- names(thetas)
  
  tv_theta_names <- sapply(original_theta_names, function(nm) {
    if (grepl("^THETA[0-9]+$", nm, ignore.case = TRUE)) {
      nm
    } else if (!grepl("^tv", nm, ignore.case = TRUE)) {
      paste0("tv", nm)
    } else {
      nm
    }
  }, USE.NAMES = FALSE)
  tv_names_map <- setNames(tv_theta_names, original_theta_names)

  for (i in seq_along(original_theta_names)) {
    orig <- original_theta_names[[i]]
    tv_nm <- tv_theta_names[[i]]
    ini_lines <- c(ini_lines, sprintf("%s <- %f", tv_nm, as.numeric(thetas[[orig]])))
  }

  # ── 2. ini block: random effects (ETAs / OMEGAs) ─────────────────────────
  omegas <- parsed_config$parameters$omega
  omega_entries <- list()
  named_omegas <- list()

  for (nm in names(omegas)) {
    val <- as.numeric(unlist(omegas[[nm]]))
    is_pure_index <- grepl("^[0-9_., ]+$", nm)
    idx <- as.integer(regmatches(nm, gregexpr("[0-9]+", nm))[[1]])
    
    if (is_pure_index && length(val) == 1) {
      if (length(idx) == 2) {
        omega_entries[[length(omega_entries) + 1]] <-
          list(row = idx[1], col = idx[2], val = val)
      } else if (length(idx) == 1) {
        omega_entries[[length(omega_entries) + 1]] <-
          list(row = idx[1], col = idx[1], val = val)
      }
    } else {
      # Handle named parameters (e.g., "eta.Cl", "eta.cl + eta.vc") or array values
      name_key <- nm
      if (is_pure_index && length(idx) == 1) {
        name_key <- paste0("ETA", idx[1])
      }
      named_omegas[[name_key]] <- val
    }
  }

  diag_idx <- sort(unique(vapply(
    Filter(function(x) x$row == x$col, omega_entries),
    function(x) x$row,
    integer(1)
  )))

  for (i in diag_idx) {
    diag_val_list <- Filter(function(x) x$row == i && x$col == i, omega_entries)
    diag_val <- if (length(diag_val_list) > 0) diag_val_list[[1]]$val else 0
    
    off_diag <- Filter(
      function(x) (x$row == i && x$col < i) || (x$col == i && x$row > i),
      omega_entries
    )
    if (length(off_diag) == 0) {
      ini_lines <- c(ini_lines, sprintf("ETA%d ~ %f", i, diag_val))
    } else {
      row_vals <- sapply(seq_len(i), function(j) {
        elem <- Filter(function(x) (x$row == i && x$col == j) || (x$row == j && x$col == i), omega_entries)
        if (length(elem) > 0) elem[[1]]$val else 0
      })
      ini_lines <- c(ini_lines,
        sprintf("ETA%d ~ c(%s)", i, paste(row_vals, collapse = ", "))
      )
    }
  }

  for (nm in names(named_omegas)) {
    val <- named_omegas[[nm]]
    if (length(val) == 1) {
      ini_lines <- c(ini_lines, sprintf("%s ~ %f", nm, val))
    } else {
      ini_lines <- c(ini_lines, sprintf("%s ~ c(%s)", nm, paste(val, collapse = ", ")))
    }
  }

  # ── 3. Error model: infer type, build ini lines + Y formula ──────────────
  error_lines_raw <- if (!is.null(parsed_config$model$error_code))
    unlist(parsed_config$model$error_code) else character(0)
  sigma_vals      <- if (!is.null(parsed_config$parameters$sigma))
    parsed_config$parameters$sigma else list(0.01)

  error_type <- infer_error_type(error_lines_raw)
  err_spec   <- build_error_spec(sigma_vals, error_type)
  ini_lines  <- c(ini_lines, err_spec$ini_lines)

  message(sprintf("  Residual error model: %s  [Y ~ expression: %s]",
                  error_type, err_spec$model_expr))

  # ── 4. model block: PK parameter assignments ──────────────────────────────
  pk_raw   <- if (!is.null(parsed_config$model$pk_code))
    unlist(parsed_config$model$pk_code) else character(0)
  pk_lines <- translate_nonmem_syntax(pk_raw)

  # Remove any IPRED / Y / F redefinitions from pk_code — these are handled
  # by the ODE spec and error spec below.
  pk_lines <- pk_lines[!grepl(
    "^\\s*(IPRED|Y|F)\\s*<-", pk_lines, perl = TRUE
  )]

  # ── 4a. Theta name resolution ─────────────────────────────────────────────
  # phikl-parse may give theta keys as meaningful PK names (CL, V2, KA …)
  # instead of positional labels (THETA1, THETA2 …).
  #
  # When keys ARE meaningful, the ini block will contain:
  #   CL <- 0.15 ;  V2 <- 8.0 ;  KA <- 1.0 …
  # but the NONMEM pk_code still references THETA(1), THETA(2), … which after
  # translate_nonmem_syntax become THETA1, THETA2, … — not present in the ini
  # block.  nlmixr2 then throws:
  #   "parameter(s) were in the ini block but not in the model block: CL, V2 …"
  #
  # Fix: build a positional map (THETA1 → CL, THETA2 → V2, …) and substitute.
  theta_names <- original_theta_names
  uses_positional_theta_names <- all(
    grepl("^THETA[0-9]+$", theta_names, ignore.case = TRUE)
  )

  if (!uses_positional_theta_names && length(pk_lines) > 0) {
    # Replace every THETAn with the corresponding LHS variable if it exists in theta_names,
    # otherwise fallback to positional mapping to avoid breaking other parsers.
    available_thetas <- theta_names
    
    for (theta_idx in 1:200) { 
      target_theta_regex <- paste0("\\bTHETA", theta_idx, "\\b")
      if (any(grepl(target_theta_regex, pk_lines))) {
        lines_with_theta <- pk_lines[grepl(target_theta_regex, pk_lines)]
        # Extract LHS: "CL <- TVCL * THETA1" -> "CL"
        lhs_vars <- unique(trimws(gsub("<-.*", "", lines_with_theta)))
        matched_name <- intersect(lhs_vars, available_thetas)
        
        rep_orig <- NULL
        if (length(matched_name) >= 1) {
          rep_orig <- matched_name[1]
          available_thetas <- setdiff(available_thetas, rep_orig) # don't assign it again
        } else if (theta_idx <= length(theta_names)) {
          rep_orig <- theta_names[[theta_idx]]
          available_thetas <- setdiff(available_thetas, rep_orig)
        }
        
        if (!is.null(rep_orig)) {
          rep_tv <- tv_names_map[[rep_orig]]
          pk_lines <- gsub(target_theta_regex, rep_tv, pk_lines, perl = TRUE)
        }
      }
    }
  }

  # ── 4b. Fallback: ensure every named ini-theta appears in model block ──────
  # If pk_code is missing or doesn't assign some theta-named parameters, nlmixr2
  # raises "in ini block but not in model block".  Generate passthrough
  # assignments (CL <- CL) so each population parameter is at least referenced.
  if (!uses_positional_theta_names) {
    # Extract LHS names of all current pk assignments
    lhs_pattern <- "^\\s*([A-Za-z][A-Za-z0-9_.]*?)\\s*<-"
    defined_in_pk <- unique(
      regmatches(pk_lines, regexpr(lhs_pattern, pk_lines, perl = TRUE))
    )
    defined_in_pk <- trimws(sub("<-", "", defined_in_pk))

    missing_params <- setdiff(theta_names, defined_in_pk)
    if (length(missing_params) > 0) {
      message("  Auto-generating passthrough assignments for: ",
              paste(missing_params, collapse = ", "))
      # CL <- tvCL  means "individual CL equals the population estimate from ini"
      passthrough <- sapply(missing_params, function(nm) {
        sprintf("%s <- %s", nm, tv_names_map[[nm]])
      }, USE.NAMES = FALSE)
      pk_lines <- c(pk_lines, passthrough)
    }
  }

  # ── 5. model block: compartmental ODEs and IPRED ──────────────────────────
  ode_spec <- get_advan_ode_spec(model_type)

  if (!is.null(ode_spec)) {
    ode_lines  <- ode_spec$odes
    ipred_line <- ode_spec$ipred
    message(sprintf("  Model type %s (%s): using auto-generated ODEs.",
                    model_type, ode_spec$note))
  } else {
    # Fall back: use explicit differential_equations from the JSON (if any)
    json_odes <- if (!is.null(parsed_config$model$differential_equations))
      unlist(parsed_config$model$differential_equations) else character(0)
    ode_lines  <- translate_nonmem_syntax(json_odes)

    # Try to recover an IPRED line from the JSON error_code block
    ipred_candidates <- grep("^\\s*IPRED\\s*=",
                             error_lines_raw, value = TRUE)
    ipred_line <- if (length(ipred_candidates) > 0)
      translate_nonmem_syntax(ipred_candidates[[1]])
    else
      "IPRED <- F   # fallback: replace with correct concentration expression"

    if (is.null(model_type) || nchar(trimws(as.character(model_type))) == 0) {
      message("  Warning: model_type not specified in JSON. ",
              "Using JSON-supplied differential_equations (if provided).")
    } else {
      message(sprintf(
        "  Warning: model_type '%s' not recognised. ",
        model_type
      ), "Using JSON-supplied differential_equations.")
    }
  }

  # ── 6. Assemble the full model block ─────────────────────────────────────
  model_lines <- c(
    pk_lines,            # PK parameter calculations  (CL <- THETA1 * exp(ETA1), etc.)
    ode_lines,           # Compartmental ODEs         (d/dt(...) <- ...)
    ipred_line,          # Predicted concentration    (IPRED <- central / V)
    err_spec$model_expr  # Residual error formula     (Y ~ prop(prop.err))
  )
  print(model_lines);
  # ── 7. Build and parse the nlmixr2 function string ───────────────────────
  model_code_str <- sprintf(
    paste0(
      "function() {\n",
      "  ini({\n",
      "    %s\n",
      "  })\n",
      "  model({\n",
      "    %s\n",
      "  })\n",
      "}"
    ),
    paste(ini_lines,   collapse = "\n    "),
    paste(model_lines, collapse = "\n    ")
  )
  print(model_code_str)
  message(model_code_str)
  model_func <- eval(parse(text = model_code_str))
  return(model_func)
}
