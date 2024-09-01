library(targets)
library(tarchetypes)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(data_summary) to view the results.

# Define custom functions and other global objects.
# This is where you write
# source("R/preprocessing.R")
# source("R/figures.R")
devtools::load_all()
# if you keep your functions in external scripts.
summarize_data <- function(dataset) {
  summary(dataset)
}

# Set target-specific options such as packages:
tar_option_set(packages = "utils") # nolint

# End this file with a list of target objects.
list(

  ## Input
  tar_target(data_raw, import_data("./data-raw/yemen_cholera_cases.csv")),
  tar_target(shp_raw, import_shape("./data-raw/yemen_shp/yem_admbnda_adm1_govyem_cso_20191002.shp")),

  # Preprocessing
  tar_target(ycc, preprocess_ycc(data_raw, shp_raw)),
  tar_target(yem_shp, preprocess_shp(shp_raw)),
  tar_target(cases, preprocess_split(ycc, "cases")),
  tar_target(deaths, preprocess_split(ycc, "deaths")),
  tar_target(cfr_abs, preprocess_split(ycc, "cfr_abs")),
  tar_target(attack_abs, preprocess_split(ycc, "attack_abs")),

  tar_target(ycc_lag, preprocess_ycc_lags(ycc) |>
                        cholera_status()),
  tar_target(cases_4w, preprocess_split(ycc_lag, "cases_4w")),
  tar_target(deaths_4w, preprocess_split(ycc_lag, "deaths_4w")),
  tar_target(cfr_4w, preprocess_split(ycc_lag, "cfr_4w")),
  tar_target(attack_4w, preprocess_split(ycc_lag, "attack_4w")),
  tar_target(status, preprocess_split(ycc_lag, "status")),

  tar_target(yccwider, join_wider(cases, deaths,cfr_abs,attack_abs)),
  tar_target(yccwider_lag, join_wider(cases_4w, deaths_4w,
                                      cfr_4w, attack_4w, status)),
  tar_target(yemen, yem_shp |> preprocess_join(yccwider)),
  tar_target(yemen_4w, yem_shp |>  preprocess_join(yccwider_lag)),

  ## figures
  tar_target(fig_1b_1, make_fig_1b(
    ycc_lag, cases, color = "blue",
    title = "Cholera cases per week")
  ),
  tar_target(fig_1b_2, make_fig_1b(
    ycc_lag, attack_abs, color = "green",
    title = "Cholera Attack rate (x1000) per week")
  ),
  tar_target(fig_1b_3, make_fig_1b(
    ycc_lag, deaths, color = "black",
    title = "Cholera deaths per week")
  ),
  tar_target(fig_1b_4, make_fig_1b(
    ycc_lag, cfr_abs, color = "orange",
    title = "Cholera Case Fatality Rate per week")
  ),

  tar_target(fig_1c_1, make_fig_1c(
    ycc_lag, cases, color = "blue",
    title = "Cholera cases per week")
    ),
  tar_target(fig_1c_2, make_fig_1c(
    ycc_lag, attack_abs, color = "green",
    title = "Cholera Attack rate (x1000) per week")
  ),
  tar_target(fig_1c_3, make_fig_1c(
    ycc_lag, deaths, color = "black",
    title = "Cholera deaths per week")
  ),
  tar_target(fig_1c_4, make_fig_1c(
    ycc_lag, cfr_abs, color = "orange",
    title = "Cholera Case Fatality Rate per week")
  ),

  tar_target(fig_1d_1, make_fig_1d(
    yemen_4w,
    `status-2017-23`,
    "Cholera status in early June")
    ),

  tar_target(fig_1d_2, make_fig_1d(
    yemen_4w,
    `status-2017-35`,
    "Cholera status in early September")
  ),

  tar_target(fig_1d_3, make_fig_1d(
    yemen_4w,
    `status-2017-51`,
    "Cholera status in early December")
  ),

  tar_target(fig_1e_1, make_fig_1ef(
    ycc_lag, cases_4w, color = "blue",
    title = "Distribution of new Cholera cases")
  ),
  tar_target(fig_1e_2, make_fig_1ef(
    ycc_lag, attack_4w, color = "green",
    title = "Distribution of Cholera Attack rates")
  ),
  tar_target(fig_1f_1, make_fig_1ef(
    ycc_lag, deaths_4w, color = "black",
    title = "Distribution of new Cholera deaths")
  ),
  tar_target(fig_1f_2, make_fig_1ef(
    ycc_lag, cfr_4w, color = "orange",
    title = "Distribution of Cholera CFR")
  ),

  tar_target(fig_2_a, make_fig_2ab(
    ycc_lag, cases_4w, deaths_4w,
    title = "Correlation between monthly Cholera cases and deaths")
  ),
  tar_target(fig_2_b, make_fig_2ab(
    ycc_lag, cases_4w, cfr_4w,
    title = "Correlation between monthly Cholera cases and CFR")
  ),
  tar_target(fig_2c_1, make_fig_2c(
    ycc_lag, status, cases_4w,
    "Status", "New cases (past month(",
    "Correlation between Configuration Status and new cases")
  ),
  tar_target(fig_2c_2, make_fig_2c(
    ycc_lag, status, deaths_4w,
    "Status", "New deaths (past month)",
    "Correlation between Configuration Status and new deaths")
  ),
  tar_target(fig_2c_3, make_fig_2c(
    ycc_lag, status, attack_4w,
    "Status", "Attack rate (past month)",
    "Correlation between Configuration Status and attack rate")
  ),
  tar_target(fig_2c_4, make_fig_2c(
    ycc_lag, status, cfr_4w,
    "Status", "CFR (past month)",
    "Correlation between Configuration Status and CFR")
  ),


  ## Render reports
  tar_render(thesis, "reports/report.Rmd"),

  ## Save files
  tar_target(adj_list_out,
             dput(adj_list(yem_shp),
                  file = "./data-raw/adj_list.txt",
                  control = "niceNames"))
)
