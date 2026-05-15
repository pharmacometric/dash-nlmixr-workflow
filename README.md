# nlmixrWorkflow

## Scope
`nlmixrWorkflow` is a structured and highly automated R package/pipeline intended to facilitate the execution of `nlmixr2` models based strictly on structured JSON configuration files.

The primary use case is to ingest a **JSON configuration file**—produced by tools like `phikl-parse` that translate NONMEM control streams—and use it to construct, execute, and report on an `nlmixr2` modeling pipeline. This allows organizations to seamless transition from NONMEM to `nlmixr` environments with fully reproducible execution artifacts.

Example data  acop.csv taken from Ron Keizer - https://github.com/ronkeizer/nonmem_examples

## Architecture and Structure
To ensure a well-organized workflow, the package is split into modular R scripts that separate concerns across the modeling lifecycle:

- `main.R`: The primary entry point orchestrating the end-to-end workflow. Handles runtime logging and coordinates the steps below.
- `R/parse_config.R`: Reads, validates, and interprets the JSON configuration file ensuring standard schema blocks (`model`, `data`, `parameters`, `estimation`) are present.
- `R/build_model.R`: Metaprograms the parsed JSON components into a functional `nlmixr2` unified UI function (the `ini` block parameters and `model` block equations).
- `R/run_estimation.R`: Maps the estimation engine settings (e.g., SAEM, FOCEi) and executes the fit against the dataset using `nlmixr2()`.
- `R/generate_reports.R`: Produces standardized Goodness-of-Fit (GOF) PNG plots (saved to `output_dir/plot/`) and extracts statistical metrics (OFV, AIC, BIC) and parameter estimates (Fixed/Random Effects and RSE) to a comprehensive `summary.txt`.

## Getting Started

### 1. Preparation
Ensure your environment has the necessary libraries: `jsonlite`, `nlmixr2`, `xpose.nlmixr2`, and `ggplot2`.
You will need a translated JSON configuration and the associated CSV dataset.

### 2. Generating the JSON Configuration (The Typical Workflow)
A typical workflow starts with an existing NONMEM control stream (e.g., `.mod` or `.ctl`). You use the `phikl-parse` tool to parse this control stream into the structured `.json` format that `nlmixrWorkflow` expects.

For example, if you have a NONMEM control stream named `run001.mod`, you can generate the JSON file using the `phikl-parse` CLI:

```bash
# Basic conversion
phikl-parse -i run001.mod -o run001.json

# You can also override the dataset or estimation method during parsing:
phikl-parse -i run001.mod -o run001.json --set-dataset path/to/dataset.csv --set-method SAEM
```

Once you have generated the `run001.json` file, you can combine it with your dataset to execute the workflow using either Option A (R Console) or Option B (Terminal CLI) as shown below.

### 3. The Configuration File
The workflow expects a JSON file output mimicking the `phikl-parse` schema. 
An example snippet of the expected structure:

```json
{
  "model": {
    "model_type": "ADVAN2",
    "pk_code": [
      "CL = THETA1 * exp(ETA1)",
      "V = THETA2 * exp(ETA2)"
    ],
    "error_code": [
      "IPRED = F",
      "Y = IPRED + IPRED * EPS1"
    ]
  },
  "data": {
    "file": "warfarin.csv"
  },
  "parameters": {
    "theta": { "THETA1": 0.15, "THETA2": 8.0 },
    "omega": { "OMEGA1_1": 0.09, "OMEGA2_2": 0.04 },
    "sigma": [0.01]
  },
  "estimation": {
    "method": "SAEM",
    "print_iterations": 100
  }
}
```

### 4. Usage & Initiating the Workflow

**Option A: R Console / Scripting**

To initiate a run interactively, source the `main.R` file and call the `run_pipeline` function.

```R
# Source the main entry script
source("nlmixrworkflow/main.R")

# Execute the pipeline by calling run_pipeline
fitted_model <- run_pipeline(
  config_path = "path/to/translated_model.json", 
  data_path = "path/to/warfarin.csv", # optional, overrides JSON data path 
  output_dir = "run_results_001"
)
```

**Option B: Terminal (CLI)**

You can execute the entire workflow from the terminal via the `Rscript` command using the `main_cli.R` script. The script accepts the target directory path as an argument to set up the work area for file outputs.

```bash
# Usage: Rscript main_cli.R <config_path> <output_dir> [data_path]
Rscript nlmixrworkflow/main_cli.R \
  path/to/translated_model.json \
  output_work_area \
  path/to/optional_dataset_override.csv
```

## Run Outputs

For every executed run, the pipeline generates the following artifacts in your specified `output_dir`:

1. **Verbose Execution Log (`run_[datetime]_[random_alphanumeric].log`)**: 
   A uniquely generated log file containing header metadata, continuous console stdout/messages, any warnings or errors thrown during estimation, and the total execution elapsed time. A robust `sink()` handler ensures logs are gracefully written even in the event of pipeline interruption.

2. **Diagnostic Plots (`/plot/`)**: 
   A dedicated subdirectory containing comprehensive standard Goodness-of-Fit (GOF) plots (Custom NONMEM Style) generated directly through `ggplot2` for maximum reliability:
   - `dv_vs_ipred.png` and `dv_vs_pred.png` (DV against Individual/Population Predictions)
   - `cwres_vs_pred.png`, `cwres_vs_idv.png`, and `cwres_qq.png` (CWRES Conditional Weighted Residuals)
   - `absval_cwres_vs_pred.png` and `absval_cwres_vs_idv.png`
   - `eta_distribution.png`: Arranged faceted histograms displaying the distribution of each random effect (ETA).
   - `eta_vs_covariate.png`: Grid-arranged plots of ETAs against covariates declared dynamically in the JSON config (`config$data$covariates`). Handles both continuous (Loess smooth) and categorical covariates (Boxplots with jitter).
   - `individual_fits.pdf` & `individual_fits_page_XXX.png`: Scalable, paginated concentration-time plots grouped by subject (max 9 subjects per page), featuring DV, IPRED, and PRED profiles for both graphical review (PDF) and fast inclusion in reports (PNGs).

3. **Analysis Summary (`summary.txt`)**: 
   A highly readable text file report containing:
   - Global fit statistics (OFV, AIC, BIC) dynamically parsed directly from the `NLMIXR2` structured object.
   - Final Fixed Effects parameter estimates (THETAs)
   - Random Effect Matrices and estimates (OMEGA / SIGMA) with Relative Standard Error (RSE) data when available.
   *Note: If the estimation fails (or fails covariance/run structure errors out), a fast fallback `summary.txt` is automatically generated indicating the failure gracefully.*

4. **Individual Predictions Table (`individual_predictions.csv`)**: 
   A CSV format table output equivalent to a NONMEM `$TABLE` consisting of row-level prediction metrics. Contains `ID`, `TIME`, `DV`, `PRED`, `IPRED`, `CWRES`, `IWRES`, conditional estimates for `ETA`s, and individual posthoc parameter estimates (`CL`, `V`, `KA`, etc.).

## How it works under the hood

**1. Config Parsing & Validation (`parse_config.R`)**
The workflow dynamically extracts JSON representations of the control stream (PK/PRED equations, error structures, THETAs/OMEGAs/SIGMAs) and confirms the presence of datasets/covariates.

**2. Model Compilation (`build_model.R`)**
Constructs an `nlmixr2` UI string by programmatically writing the `ini` block (combining fixed and random effect priors) and `model` block. It handles index replacing and error expression mappings.

**3. Estimation (`run_estimation.R`)**
Re-maps estimation methods (SAEM, FOCE-I) and dynamically unwraps estimation control parameters.

**4. Report Generation (`generate_reports.R`)**
To maximize reliability and robustness across varied model structures, the unified `fit` object is collapsed into heavily processed native data frames. The graphical module utilizes `gridExtra` and custom-built `ggplot2` calls to produce robust Nonmem-style diagnostic outputs rather than relying on strict, occasionally brittle wrappers, handling covariates, IPRED routing, and multi-page layouts automatically.
