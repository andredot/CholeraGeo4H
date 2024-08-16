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
source("R/functions.R")
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
  tar_target(ycc, preprocess_ycc(data_raw)),
  tar_target(data_summary, summarize_data(ycc)),
  tar_target(yem_shp, preprocess_shp(shp_raw)),
  tar_target(cases, preprocess_split(ycc, "cases")),
  tar_target(deaths, preprocess_split(ycc, "deaths")),
  tar_target(cfr_abs, preprocess_split(ycc, "cfr_abs")),
  tar_target(attack_abs, preprocess_split(ycc, "attack_abs")),
  tar_target(yccwider, join_wider(cases, deaths,cfr_abs,attack_abs)),
  tar_target(yemen, preprocess_join(yem_shp, yccwider)),

  ## Save files
  tar_target(adj_list_out,
             dput(adj_list(yem_shp),
                  file = "./data-raw/adj_list.txt",
                  control = "niceNames"))
)
