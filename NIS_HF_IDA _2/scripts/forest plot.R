library(tidyverse)
library(forester)
library(extrafont)

# Prepare tidy data frame for forest plot
dta <- tibble(
  Outcome = c("HFpEF (Reference)", "HFrEF"),
  Estimate = c(1, 13750),
  CI.Lower = c(1, 11168),
  CI.Upper = c(1, 16332)
)

# Rename left side column for clarity
left_side_df <- dta %>% select(Outcome) %>% rename(`Outcome` = Outcome)

# Load custom font on Windows (make sure installed)
windowsFonts("IBM Plex Sans" = windowsFont("IBM Plex Sans"))

# Generate forest plot
fp <- forester(
  left_side_data = left_side_df,
  estimate = dta$Estimate,
  ci_low = dta$CI.Lower,
  ci_high = dta$CI.Upper,
  display = FALSE,
  estimate_precision = 2,
  arrows = TRUE,
  estimate_col_name = "*Adjusted Odds Ratio (95% CI)",
  xlim = c(-18000, 18000),
  arrow_labels = c("Favors HFpEF", "Favors HFrEF"),
  null_line_at = 0,
  ggplot_width = 40,
  font_family = "IBM Plex Sans",
  file_path = "./plots/charge.jpeg"
)



















#
#
# # Load libraries
# library(tidyverse)
# library(forester)
# library(extrafont)
#
# # Import data
# dta <- readxl::read_excel("data/length_of_stay.xlsx")
# dta <- dta |> mutate(
#   Estimate = as.numeric(Estimate),
#   CI.Lower = as.numeric(CI.Lower),
#   CI.Upper = as.numeric(CI.Upper)
# )
#
# # Import custom font
# windowsFonts("IBM Plex Sans" = windowsFont("IBM Plex Sans"))
#
#
# # Modify 'Endpoint' column: Indent subgroup if there is a number in the 'Placebo' column
# # dta <- dta %>%
# #   mutate(Endpoint HFpEF = if_else(is.na(Placebo), Endpoint HFpEF, paste0("   ", Endpoint HFpEF))) %>%
# #   mutate(Endpoint HFpEF = replace(Endpoint HFpEF, 1, "Patient preferred outcome"))
#
# # Create forest plot table
# forester(
#   left_side_data = dta[,1],
#   estimate = dta$Estimate,
#   ci_low = dta$CI.Lower,
#   ci_high = dta$CI.Upper,
#   display = FALSE,
#   estimate_precision = 2,
#   arrows = TRUE,
#   estimate_col_name = "*Beta (95% CI)",
#   xlim = c(-2.5,2.5),
#   arrow_labels = c("Favors Other BMI category", "Favors Normal Weight"),
#   null_line_at = 0,
#   ggplot_width = 40,
#   font_family = "IBM Plex Sans",
#   file_path = "./plots/LOS.tiff"
# )
