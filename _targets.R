library(targets)
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
  colMeans(dataset)
}

# Set target-specific options such as packages:
tar_option_set(packages = "utils") # nolint

# End this file with a list of target objects.
list(
  tar_target(data_raw, import_data("./CholeraGeo4H/data-raw/yemen_cholera_cases.csv")),
  tar_target(ycc, preprocess_ycc(data_raw)),
  tar_target(data_summary, summarize_data(ycc)) # Call your custom functions.
)
