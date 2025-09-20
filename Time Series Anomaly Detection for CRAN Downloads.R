## R Script: Complete Time Series Anomaly Detection for CRAN Downloads
## Author: Enhanced by Claude (Originally by Vrajesh Shah)
## Description: Comprehensive analysis of package download trends with anomaly detection,
##              parameter sensitivity analysis, result interpretation, and business insights
## Inspiration: https://business-science.github.io/anomalize/
## Date: September 2025

# ============================================================================
# SETUP AND CONFIGURATION
# ============================================================================

# Clear environment for clean start
rm(list = ls())

# Set random seed for reproducibility
set.seed(42)

# Function to install and load packages
install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      cat("Installing package:", pkg, "\n")
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Load essential packages with error handling
required_packages <- c("tidyverse", "anomalize", "ggplot2", "lubridate", 
                       "gridExtra", "scales", "knitr", "DT")

tryCatch({
  install_and_load(required_packages)
  cat("✓ All packages loaded successfully!\n\n")
}, error = function(e) {
  cat("Error loading packages:", e$message, "\n")
  stop("Please install required packages manually")
})

# Display session information
cat("=== SESSION INFORMATION ===\n")
sessionInfo()
cat("\n" %>% rep(2) %>% cat())

# ============================================================================
# DATA LOADING AND VALIDATION
# ============================================================================

cat("=== DATA LOADING AND VALIDATION ===\n")

# Load built-in CRAN download data with validation
tryCatch({
  data("tidyverse_cran_downloads")
  
  # Validate data structure
  if (!exists("tidyverse_cran_downloads")) {
    stop("Dataset 'tidyverse_cran_downloads' not found")
  }
  
  # Check data quality
  cat("Dataset loaded successfully!\n")
  cat("Dimensions:", dim(tidyverse_cran_downloads), "\n")
  cat("Date range:", min(tidyverse_cran_downloads$date), "to", max(tidyverse_cran_downloads$date), "\n")
  cat("Packages included:", length(unique(tidyverse_cran_downloads$package)), "\n")
  cat("Package names:", paste(unique(tidyverse_cran_downloads$package), collapse = ", "), "\n")
  
  # Check for missing values
  missing_summary <- tidyverse_cran_downloads %>%
    summarise_all(~sum(is.na(.))) %>%
    gather(key = "column", value = "missing_count") %>%
    filter(missing_count > 0)
  
  if (nrow(missing_summary) > 0) {
    cat("Missing values found:\n")
    print(missing_summary)
  } else {
    cat("✓ No missing values detected\n")
  }
  
}, error = function(e) {
  cat("Error loading data:", e$message, "\n")
  stop("Cannot proceed without valid data")
})

cat("\n")

# ============================================================================
# EXPLORATORY DATA ANALYSIS
# ============================================================================

cat("=== EXPLORATORY DATA ANALYSIS ===\n")

# Basic statistics
download_stats <- tidyverse_cran_downloads %>%
  group_by(package) %>%
  summarise(
    total_days = n(),
    total_downloads = sum(count),
    avg_daily_downloads = mean(count),
    median_daily_downloads = median(count),
    max_downloads = max(count),
    min_downloads = min(count),
    std_dev = sd(count),
    cv = sd(count) / mean(count),  # Coefficient of variation
    .groups = 'drop'
  ) %>%
  arrange(desc(total_downloads))

cat("Download Statistics by Package:\n")
print(download_stats)

# Overall time series plot
overall_plot <- tidyverse_cran_downloads %>%
  ggplot(aes(x = date, y = count, color = package)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~package, scales = "free_y", ncol = 3) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10)) +
  labs(title = "CRAN Download Trends by Package",
       subtitle = "Daily download counts over time",
       x = "Date", y = "Downloads") +
  scale_y_continuous(labels = scales::comma)

print(overall_plot)

cat("\n")

# ============================================================================
# ANOMALY DETECTION - OVERVIEW ANALYSIS
# ============================================================================

cat("=== ANOMALY DETECTION - OVERVIEW ANALYSIS ===\n")

# Detect anomalies across all packages with progress tracking
cat("Performing anomaly detection across all packages...\n")

all_anomalies_detected <- tidyverse_cran_downloads %>%
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  time_recompose()

# Create comprehensive anomaly visualization
anomaly_overview_plot <- all_anomalies_detected %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5) +
  theme_minimal() +
  theme(strip.text = element_text(size = 9)) +
  labs(title = "Anomaly Detection Results - All Packages",
       subtitle = "Red points indicate detected anomalies")

print(anomaly_overview_plot)

# Extract and analyze anomalies
anomalies_summary <- all_anomalies_detected %>%
  filter(anomaly == "Yes")

cat("Total anomalies detected across all packages:", nrow(anomalies_summary), "\n")

# Anomaly statistics by package
anomaly_stats <- anomalies_summary %>%
  group_by(package) %>%
  summarise(
    anomaly_count = n(),
    avg_anomaly_magnitude = mean(abs(remainder)),
    max_anomaly_magnitude = max(abs(remainder)),
    first_anomaly = min(date),
    last_anomaly = max(date),
    .groups = 'drop'
  ) %>%
  arrange(desc(anomaly_count))

cat("\nAnomaly Statistics by Package:\n")
print(anomaly_stats)

# Calculate anomaly rates
anomaly_rates <- download_stats %>%
  left_join(anomaly_stats, by = "package") %>%
  mutate(
    anomaly_count = replace_na(anomaly_count, 0),
    anomaly_rate = anomaly_count / total_days * 100
  ) %>%
  arrange(desc(anomaly_rate))

cat("\nAnomaly Rates by Package:\n")
print(anomaly_rates %>% select(package, anomaly_count, total_days, anomaly_rate))

cat("\n")

# ============================================================================
# PARAMETER SENSITIVITY ANALYSIS
# ============================================================================

cat("=== PARAMETER SENSITIVITY ANALYSIS ===\n")

# Test different alpha values for sensitivity analysis
alpha_values <- c(0.01, 0.05, 0.1, 0.15, 0.2)
max_anoms_values <- c(0.1, 0.15, 0.2, 0.25, 0.3)

cat("Testing different parameter combinations...\n")

# Function to test parameter sensitivity
test_parameters <- function(data, alpha_vals, max_anoms_vals) {
  results <- data.frame()
  
  for (alpha in alpha_vals) {
    for (max_anoms in max_anoms_vals) {
      tryCatch({
        anomaly_count <- data %>%
          time_decompose(count) %>%
          anomalize(remainder, method = "iqr", alpha = alpha, max_anoms = max_anoms) %>%
          time_recompose() %>%
          filter(anomaly == "Yes") %>%
          nrow()
        
        results <- rbind(results, data.frame(
          alpha = alpha,
          max_anoms = max_anoms,
          anomaly_count = anomaly_count
        ))
      }, error = function(e) {
        # Skip if combination causes error
      })
    }
  }
  return(results)
}

# Test parameters on lubridate package (example)
lubridate_data <- tidyverse_cran_downloads %>% 
  filter(package == "lubridate") %>%
  ungroup()

sensitivity_results <- test_parameters(lubridate_data, alpha_values, max_anoms_values)

cat("Parameter sensitivity results for 'lubridate':\n")
print(sensitivity_results)

# Visualize parameter sensitivity
sensitivity_plot <- sensitivity_results %>%
  ggplot(aes(x = factor(alpha), y = factor(max_anoms), fill = anomaly_count)) +
  geom_tile() +
  geom_text(aes(label = anomaly_count), color = "white", size = 3) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Parameter Sensitivity Analysis - Lubridate",
       subtitle = "Number of anomalies detected for different parameter combinations",
       x = "Alpha (Significance Level)", 
       y = "Max Anomalies (%)",
       fill = "Anomaly\nCount")

print(sensitivity_plot)

cat("\n")

# ============================================================================
# DETAILED PACKAGE ANALYSIS
# ============================================================================

cat("=== DETAILED PACKAGE ANALYSIS ===\n")

# Analyze top 3 packages with most anomalies
top_packages <- head(anomaly_stats$package, 3)
cat("Performing detailed analysis on top anomaly packages:", paste(top_packages, collapse = ", "), "\n")

# Function for detailed package analysis
analyze_package <- function(pkg_name, data) {
  cat("\n--- Analyzing", pkg_name, "---\n")
  
  pkg_data <- data %>% 
    filter(package == pkg_name) %>%
    ungroup()
  
  # Basic statistics
  pkg_stats <- pkg_data %>%
    summarise(
      total_days = n(),
      avg_downloads = round(mean(count)),
      median_downloads = round(median(count)),
      max_downloads = max(count),
      min_downloads = min(count),
      std_dev = round(sd(count)),
      date_range = paste(min(date), "to", max(date))
    )
  
  cat("Package Statistics:\n")
  print(pkg_stats)
  
  # Perform anomaly detection
  pkg_anomalies <- pkg_data %>%
    time_decompose(count, method = "stl", frequency = "auto", trend = "auto") %>%
    anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
    time_recompose()
  
  # Anomaly summary
  pkg_anomaly_summary <- pkg_anomalies %>%
    filter(anomaly == "Yes") %>%
    arrange(desc(abs(remainder)))
  
  cat("Anomalies detected:", nrow(pkg_anomaly_summary), "\n")
  
  if (nrow(pkg_anomaly_summary) > 0) {
    cat("Top 3 most extreme anomalies:\n")
    print(pkg_anomaly_summary %>% 
            select(date, observed, remainder, anomaly) %>%
            head(3))
  }
  
  # Create detailed plots
  # 1. Decomposition plot
  decomp_plot <- pkg_anomalies %>%
    plot_anomaly_decomposition() +
    ggtitle(paste(pkg_name, "Downloads: Anomaly Decomposition")) +
    theme_minimal()
  
  # 2. Anomaly plot with bounds
  anomaly_plot <- pkg_anomalies %>%
    plot_anomalies(time_recomposed = TRUE) +
    ggtitle(paste(pkg_name, "Downloads: Anomalies with Confidence Bounds")) +
    theme_minimal()
  
  # Print plots
  print(decomp_plot)
  print(anomaly_plot)
  
  return(list(
    stats = pkg_stats,
    anomalies = pkg_anomaly_summary,
    plots = list(decomposition = decomp_plot, anomaly = anomaly_plot)
  ))
}

# Analyze each top package
package_analyses <- list()
for (pkg in top_packages) {
  package_analyses[[pkg]] <- analyze_package(pkg, tidyverse_cran_downloads)
}

cat("\n")

# ============================================================================
# BUSINESS INSIGHTS AND INTERPRETATION
# ============================================================================

cat("=== BUSINESS INSIGHTS AND INTERPRETATION ===\n")

# Seasonal patterns analysis
seasonal_analysis <- all_anomalies_detected %>%
  mutate(
    month = month(date, label = TRUE),
    day_of_week = wday(date, label = TRUE),
    is_weekend = wday(date) %in% c(1, 7)
  ) %>%
  group_by(package, month) %>%
  summarise(
    avg_downloads = mean(observed),
    anomaly_rate = mean(anomaly == "Yes") * 100,
    .groups = 'drop'
  )

cat("Seasonal patterns by month:\n")
seasonal_summary <- seasonal_analysis %>%
  group_by(month) %>%
  summarise(
    avg_downloads_all = mean(avg_downloads),
    avg_anomaly_rate = mean(anomaly_rate),
    .groups = 'drop'
  )
print(seasonal_summary)

# Weekend vs weekday analysis
weekend_analysis <- all_anomalies_detected %>%
  mutate(is_weekend = wday(date) %in% c(1, 7)) %>%
  group_by(package, is_weekend) %>%
  summarise(
    avg_downloads = mean(observed),
    anomaly_rate = mean(anomaly == "Yes") * 100,
    .groups = 'drop'
  )

cat("\nWeekend vs Weekday patterns:\n")
weekend_summary <- weekend_analysis %>%
  group_by(is_weekend) %>%
  summarise(
    avg_downloads_all = mean(avg_downloads),
    avg_anomaly_rate = mean(anomaly_rate),
    .groups = 'drop'
  ) %>%
  mutate(period = ifelse(is_weekend, "Weekend", "Weekday")) %>%
  select(period, avg_downloads_all, avg_anomaly_rate)
print(weekend_summary)

# ============================================================================
# ANOMALY CLASSIFICATION AND VALIDATION
# ============================================================================

cat("\n=== ANOMALY CLASSIFICATION AND VALIDATION ===\n")

# Classify anomalies by type and magnitude
anomaly_classification <- anomalies_summary %>%
  mutate(
    anomaly_type = case_when(
      remainder > 0 ~ "Positive Spike",
      remainder < 0 ~ "Negative Dip",
      TRUE ~ "Unknown"
    ),
    magnitude_category = case_when(
      abs(remainder) < quantile(abs(remainder), 0.33) ~ "Low",
      abs(remainder) < quantile(abs(remainder), 0.66) ~ "Medium",
      TRUE ~ "High"
    )
  )

# Summary by type and magnitude
classification_summary <- anomaly_classification %>%
  group_by(package, anomaly_type, magnitude_category) %>%
  summarise(
    count = n(),
    avg_magnitude = mean(abs(remainder)),
    .groups = 'drop'
  ) %>%
  arrange(package, desc(count))

cat("Anomaly classification summary:\n")
print(classification_summary)

# ============================================================================
# RECOMMENDATIONS AND NEXT STEPS
# ============================================================================

cat("\n=== RECOMMENDATIONS AND NEXT STEPS ===\n")

# Generate recommendations based on findings
recommendations <- list(
  "Data Quality" = c(
    "Monitor packages with high anomaly rates for potential data quality issues",
    "Investigate negative spikes as they may indicate service disruptions",
    "Set up automated alerts for anomalies exceeding 3 standard deviations"
  ),
  "Business Intelligence" = c(
    "Positive spikes may indicate viral adoption or major releases - investigate causes",
    "Weekend patterns suggest different usage behaviors worth exploring",
    "Seasonal trends can inform release timing and marketing strategies"
  ),
  "Technical Monitoring" = c(
    "Implement real-time anomaly detection for critical packages",
    "Consider different detection methods for different package types",
    "Validate anomalies with external events (releases, conferences, holidays)"
  )
)

cat("KEY RECOMMENDATIONS:\n")
for (category in names(recommendations)) {
  cat("\n", category, ":\n")
  for (i in seq_along(recommendations[[category]])) {
    cat(paste0("  ", i, ". ", recommendations[[category]][i], "\n"))
  }
}

# ============================================================================
# FINAL SUMMARY AND EXPORT
# ============================================================================

cat("\n=== FINAL SUMMARY ===\n")

final_summary <- list(
  total_packages_analyzed = length(unique(tidyverse_cran_downloads$package)),
  total_anomalies_detected = nrow(anomalies_summary),
  date_range = paste(min(tidyverse_cran_downloads$date), "to", max(tidyverse_cran_downloads$date)),
  top_anomaly_package = anomaly_stats$package[1],
  avg_anomaly_rate = round(mean(anomaly_rates$anomaly_rate, na.rm = TRUE), 2),
  parameters_used = "STL decomposition, IQR method, alpha=0.05, max_anoms=0.2"
)

cat("ANALYSIS SUMMARY:\n")
for (item in names(final_summary)) {
  cat(paste0("• ", gsub("_", " ", str_to_title(item)), ": ", final_summary[[item]], "\n"))
}

# Optional: Save results to files
save_results <- FALSE  # Set to TRUE to save files

if (save_results) {
  tryCatch({
    # Save main results
    write_csv(anomaly_stats, "anomaly_statistics.csv")
    write_csv(anomalies_summary, "detected_anomalies.csv")
    write_csv(download_stats, "download_statistics.csv")
    
    # Save plots (requires specific plot objects)
    ggsave("overall_anomalies.png", anomaly_overview_plot, width = 12, height = 8)
    ggsave("parameter_sensitivity.png", sensitivity_plot, width = 8, height = 6)
    
    cat("\n✓ Results saved to files successfully!\n")
  }, error = function(e) {
    cat("\nWarning: Could not save files -", e$message, "\n")
  })
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Script executed successfully with comprehensive anomaly detection and analysis!\n")
cat("Generated", length(dev.list()), "plots and", nrow(anomalies_summary), "anomaly records.\n")
cat("Review the insights above and consider implementing the recommendations.\n")


