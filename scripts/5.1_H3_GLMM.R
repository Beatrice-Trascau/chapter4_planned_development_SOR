##----------------------------------------------------------------------------##
# PAPER 4: PLANNED DEVELOPMENT AREA AND SPECIES OCCURRENCE RECORDS
# 5.2_H3_GLMM
# This script contains code to test Hypothesis 3: Urban and near-urban polygons
# will have more SOR than other planned development types
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Load polygon all data
model_data_raw <- readRDS(here("data", "derived_data", "h2_polygon_buffer_data.rds"))

# 2. PREPARE DATA FOR MODELING -------------------------------------------------

# Only use development polygons
model_data <- model_data_raw |>
  filter(polygon_type == "Development",
         english_categories != "Ports") |>
  mutate(area_km2 = area_m2_numeric / 1e6,
         log_area_km2 = log(area_km2),
         english_categories = factor(english_categories),
         land_cover_name = factor(land_cover_name),
         kommune_factor = factor(kommune))

# Remove rows with NAs in model variables
model_data <- model_data |>
  filter(!is.na(n_occurrences), !is.na(log_area_km2),
         !is.na(english_categories), !is.na(land_cover_name),
         !is.na(kommune_factor))

# Quick summary of the data
cat("Development polygons:", nrow(model_data), "\n") #129881
cat("Municipalities:", n_distinct(model_data$kommune_factor), "\n") #353
cat("Development categories:", nlevels(model_data$english_categories), "\n") #9
cat("\nPolygons per development category:\n")
print(table(model_data$english_categories))
# Combined   Commercial      Defense       Mining Recreational  Residential       Retail     Services      Tourism 
#     4266         9662           25         4436        55702        44009          900         5568         5313
cat("\nOccurrence count summary:\n")
print(summary(model_data$n_occurrences))
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000     0.000     0.000     2.491     0.000 22318.000 
cat("Proportion of zero-occurrence polygons:",
    round(mean(model_data$n_occurrences == 0), 3), "\n") #0.909 

# 3. FIT MODELS ----------------------------------------------------------------

## 3.1. Negative binomial with offset  -----------------------------------------

# Fit model
h3_nb <- glmmTMB(n_occurrences ~ english_categories + land_cover_name +
                   offset(log_area_km2) + (1 | kommune_factor),
                 data = model_data,
                 family = nbinom2)

# Save model
save(h3_nb, file = here::here("data", "models", "h3_nb.RData"))

## 3.2. Zero infalted no interaction -------------------------------------------

# Fit model
h3_zinb <- glmmTMB(n_occurrences ~ english_categories + land_cover_name +
                     offset(log_area_km2) + (1 | kommune_factor),
                   data = model_data,
                   family = nbinom2,
                   ziformula = ~ log_area_km2 + english_categories)

# Save model
save(h3_zinb, file = here::here("data", "models", "h3_zinb.RData"))

# Compare models
AICtab(h3_nb, h3_zinb, base = TRUE)
#         AIC      dAIC     df
# h3_zinb 138625.4      0.0 27
# h3_nb   142278.5   3653.1 17

# Keep the best model
best_model_h3 <- h3_zinb

# 4. MODEL SUMMARY AND DIAGNOSTICS ---------------------------------------------

## 4.1. Model summary ----------------------------------------------------------

# Get the model summary
print(summary(best_model_h3))

# Check convergence
if (best_model_h3$sdr$pdHess) {
  cat("\nH3 model converged successfully\n")
} else {
  cat("\nWarning: H3 model may not have converged\n")
}

# Create simple coefficient table
coef_table_h3 <- broom.mixed::tidy(best_model_h3,
                                   effects = "fixed", conf.int = TRUE) |>
  mutate(Estimate  = round(estimate, 3),
         SE        = round(std.error, 3),
         `z value` = round(statistic, 2),
         `p value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))) |>
  select(Term = term, Estimate, SE, `z value`, `p value`)

# Save as CSV
write.csv(coef_table_h3,
          here("figures", "Table_H3_model_coefficients.csv"),
          row.names = FALSE)

## 4.2. Model diagnostics with DHARMa ------------------------------------------

# Simulate residuals
sim_residuals_h3 <- simulateResiduals(fittedModel = best_model_h3, n = 1000)

# Create diagnostic plots
png(filename = here("figures", "Figure_H3_diagnostics.png"),
    width = 12, height = 8, units = "in", res = 300)
plot(sim_residuals_h3)
dev.off()

# Test for dispersion
print(testDispersion(sim_residuals_h3))

# Test for zero-inflation
print(testZeroInflation(sim_residuals_h3))

# Test outliers
print(testOutliers(sim_residuals_h3))

## 4.4. Extract random effects -------------------------------------------------

# Get random effect variance
random_effects_h3 <- VarCorr(best_model_h3)
print(random_effects_h3)
# Conditional model:
#   Groups         Name        Std.Dev.
# kommune_factor (Intercept) 1.3889  

# Calculate ICC (intraclass correlation coefficient)
# Proportion of variance explained by municipality
re_var_h3 <- as.numeric(random_effects_h3$cond$kommune_factor[1])
cat("Random effect variance (kommune):", round(re_var_h3, 4), "\n") #1.9289

# 5. HYPOTHESIS TESTING --------------------------------------------------------

cat("\n=== HYPOTHESIS 3 TESTING ===\n")
cat("H3: development types differ in occurrence density; urban/near-urban types\n")
cat("    are expected to hold more records per km2 than other types.\n\n")

# Estimate occurrence density per km2by development category
# set the offset to 0 
emm_category <- emmeans(best_model_h3, ~ english_categories,
                        offset = 0, type = "response")
cat("Estimated occurrence density (records per km2) by development category:\n")
print(summary(emm_category))
# english_categories response    SE  df asymp.LCL asymp.UCL
# Combined                261  31.0 Inf       207       329
# Commercial              231  23.1 Inf       190       281
# Defense                 127  90.9 Inf        31       517
# Mining                  144  18.9 Inf       112       187
# Recreational            219  21.5 Inf       180       265
# Residential             278  25.8 Inf       232       334
# Retail                  505 118.0 Inf       320       799
# Services                332  37.3 Inf       266       414
# Tourism                 451  54.4 Inf       356       572

# Get pairwise comparisons between development categories
pairwise_category <- contrast(emm_category, method = "pairwise",
                              adjust = "tukey", type = "response")
cat("\nAll pairwise comparisons of development categories (density ratios):\n")
print(summary(pairwise_category, infer = TRUE))

# Save the category means and the pairwise comparisons
write.csv(as.data.frame(summary(emm_category)),
          here("figures", "Table_H3_density_by_category.csv"),
          row.names = FALSE)
write.csv(as.data.frame(summary(pairwise_category, infer = TRUE)),
          here("figures", "Table_H3_pairwise_category_comparisons.csv"),
          row.names = FALSE)
saveRDS(list(density_by_category = emm_category,
             pairwise           = pairwise_category),
        here("data", "models", "h3_category_inference.rds"))

# 6. EXTRACT AND PLOT PREDICTIONS ----------------------------------------------


## 6.1. Estimate density by development category -------------------------------

# Check where each development type sits at 
cat_df <- as.data.frame(summary(emm_category))
rate_col <- grep("rate|response|emmean|prob", names(cat_df), value = TRUE)[1]
lo_col   <- grep("LCL|lower", names(cat_df), value = TRUE)[1]
hi_col   <- grep("UCL|upper", names(cat_df), value = TRUE)[1]
cat_df <- cat_df |>
  rename(density = all_of(rate_col), conf.low = all_of(lo_col), conf.high = all_of(hi_col))

# Plot figure
(fig_h3_category <- ggplot(cat_df,
                           aes(x = reorder(english_categories, density), y = density)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                    colour = "#5E3C99", linewidth = 0.8, size = 0.6) +
    coord_flip() +
    labs(x = "Development Category",
         y = expression(paste("Estimated Occurrence Density (records per km"^2, ")"))) +
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)))

# Save figures to file
ggsave(filename = here("figures", "Figure_H3_density_by_category.png"),
       plot = fig_h3_category, width = 10, height = 7, dpi = 600)
ggsave(filename = here("figures", "Figure_H3_density_by_category.pdf"),
       plot = fig_h3_category, width = 10, height = 7, dpi = 600)

## 6.2.Pairwise density ratios  ------------------------------------------------

# Get all pairwise ratios on a log scale (use 1 as a reference line to mean no difference)
pw_df <- as.data.frame(summary(pairwise_category, infer = TRUE))
ratio_col <- grep("ratio|estimate", names(pw_df), value = TRUE)[1]
plo_col   <- grep("LCL|lower", names(pw_df), value = TRUE)[1]
phi_col   <- grep("UCL|upper", names(pw_df), value = TRUE)[1]
pw_df <- pw_df |>
  rename(ratio = all_of(ratio_col), conf.low = all_of(plo_col), conf.high = all_of(phi_col)) |>
  mutate(significant = ifelse(p.value < 0.05, "Yes", "No"))

# Create prediction plot
(fig_h3_pairwise <- ggplot(pw_df,
                          aes(x = ratio, y = reorder(contrast, ratio),
                              colour = significant)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.6) +
  scale_x_log10() +
  scale_colour_manual(values = c("No" = "grey65", "Yes" = "#5E3C99"),
                      name = "Significant (p < 0.05)") +
  labs(x = "Density Ratio (log scale)", y = "Development Category Comparison") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 11),
        legend.position = "bottom"))

# Save figure
ggsave(filename = here("figures", "Figure_H3_pairwise_category_ratios.png"),
       plot = fig_h3_pairwise, width = 12, height = 10, dpi = 600)
ggsave(filename = here("figures", "Figure_H3_pairwise_category_ratios.pdf"),
       plot = fig_h3_pairwise, width = 12, height = 10, dpi = 600)

# END OF SCRIPT ----------------------------------------------------------------