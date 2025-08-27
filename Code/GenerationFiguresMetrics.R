# Load necessary libraries
#' @import dplyr
#' @import ggplot2
#' @import tidyr


PIFolder <- projectConfiguration$outputFolder
# Species vector
species_vector <- species.vec

# Correction for same species from/to in fitted
Correct_fitted <- function (fitted_data,total_data){
  fitted_data_corrected <- data.frame()
  for (i in 1:length(species_vector)){
    fitted_data_species <- fitted_data %>%
      filter(specieTo != species_vector[i]) %>%
      mutate(specieFrom = species_vector[i])
    fitted_data_corrected <- rbind(fitted_data_corrected, fitted_data_species)

  }
  fitted_data_corrected <- fitted_data_corrected%>%
    semi_join(total_data, by = c("specieTo", "specieFrom", "compoundID"))
  return(fitted_data_corrected)
}

# Function for computing the improvement
compute_improvement <- function(data, metric_columns, base_extrapolation = "Naive") {
  base_data <- data %>% filter(Extrapolation == base_extrapolation)
  base_data_fitted <- data %>% filter(Extrapolation == "Fitted")
  base_data_fitted_corrected <- Correct_fitted(base_data_fitted,data)

  other_data <- data %>% filter(Extrapolation != base_extrapolation)


  if (base_extrapolation != "Fitted") {
    other_data <- other_data %>% filter(Extrapolation != "Fitted")
    other_data <- rbind(other_data, base_data_fitted_corrected)
  }


  improvement_list <- list()

  # Iterate over each metric column to compute improvement
  for (metric in metric_columns) {
    # Merge the datasets on compoundID, specieTo, specieFrom, and DataSet
    if (base_extrapolation != "Fitted") {
      merged_data <- merge(other_data, base_data, by = c("compoundID", "specieTo", "specieFrom", "DataSet"), suffixes = c("_Other", paste0("_", base_extrapolation)))
    } else {
      merged_data <- merge(other_data, base_data, by = c("compoundID", "specieTo", "DataSet"), suffixes = c("_Other", paste0("_", base_extrapolation)))
    }
    # Compute the improvement metric
    improvement_col_name <- paste0("Improvement_", metric)
    merged_data <- merged_data %>%
      mutate(!!improvement_col_name := (get(paste0(metric, "_", base_extrapolation)) - get(paste0(metric, "_Other"))) / get(paste0(metric, "_", base_extrapolation)))

    if (base_extrapolation != "Fitted") {
      result <- merged_data %>%
        select(compoundID, specieTo, specieFrom, DataSet, Extrapolation_Other, all_of(improvement_col_name))
    } else {
      result <- merged_data %>%
        select(compoundID, specieTo, specieFrom_Other, DataSet, Extrapolation_Other, all_of(improvement_col_name)) %>%
        rename("specieFrom" = "specieFrom_Other")
    }
    improvement_list[[metric]] <- result
  }

  # Combine improvements into a single data frame
  improvements <- Reduce(function(x, y) full_join(x, y, by = c("compoundID", "specieTo", "specieFrom", "DataSet", "Extrapolation_Other")), improvement_list)
  return(improvements)
}

# Read the complete table of metrics
metrics.dtf <- read.csv(file = file.path(PIFolder, "ExtrapolationResults", "metrics_summary.csv"))

# Add AUCfit and AUC ratio fit as new columns
metrics.dtf <- metrics.dtf %>% group_by(DataSet) %>% mutate(AUCfit = AUCsim[.data[["Extrapolation"]] == "Fitted"], .after = "AUCsim")
metrics.dtf <- metrics.dtf %>% mutate(AUCfitratio = AUCsim / AUCfit, .after = "AUCratio")

# Set the factors for Extrapolation
metrics.dtf$Extrapolation <- factor(metrics.dtf$Extrapolation, levels = c("Naive", "FU", "KP", "SP", "FU_KP", "SP_FU", "SP_KP", "SP_FU_KP", "Fitted"))

# Define the list of metric columns
metric_columns <- c("Metric", "RMSE", "RMSLE", "foldCmax", "foldtend", "foldmean", "foldCmaxlog", "foldtendlog", "foldmeanlog")

# Compute improvements directly on metrics.dtf
improvements_dtf <- compute_improvement(metrics.dtf, metric_columns)
improvements_fitted_dtf <- compute_improvement(metrics.dtf, metric_columns, base_extrapolation = "fitted")
# Function to create species-specific plots
species_plots <- function(data, species, y, alias) {
  ggplot(data, aes(x = specieFrom, y = !!sym(y), fill = Extrapolation)) +
    geom_boxplot(outlier.shape = NA) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    coord_cartesian(ylim = c(0, 4)) +
    geom_hline(yintercept = 0.5, linetype = "dotdash", color = "black") +
    geom_hline(yintercept = 2, linetype = "dotdash", color = "black") +
    geom_hline(yintercept = 1, linetype = "dotted", color = "black") +
    labs(title = paste(alias, "to predict", species, "data"),
         x = "Specie From", y = alias)
}

# Look at AUC ratio with data or fit as ref
for (auc in c("AUCratio", "AUCfitratio")) {
  alias <- c("AUCratio" = "AUC ratio (predicted vs observed)", "AUCfitratio" = "AUC ratio (predicted vs fitted)")[auc]

  ggplot(metrics.dtf, aes(x = Extrapolation, y = !!sym(auc), fill = Extrapolation)) +
    geom_boxplot(outlier.shape = NA) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    coord_cartesian(ylim = c(0, 4)) +
    geom_hline(yintercept = 0.5, linetype = "dotdash", color = "black") +
    geom_hline(yintercept = 2, linetype = "dotdash", color = "black") +
    geom_hline(yintercept = 1, linetype = "dotted", color = "black") +
    labs(title = paste(alias, "per Extrapolation"), x = "Extrapolation", y = alias)

  ggsave(file = file.path(PIFolder, "ExtrapolationResults", "Plots", paste0(auc, ".png")), width = 8, height = 6, dpi = 300)

  # Boxplot for AUC ratio per compound
  ggplot(metrics.dtf, aes(x = Extrapolation , y = !!sym(auc), fill = Extrapolation)) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(~compoundID) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    coord_cartesian(ylim = c(0, 4)) +
    geom_hline(yintercept = 0.5, linetype = "dotdash", color = "black") +
    geom_hline(yintercept = 2, linetype = "dotdash", color = "black") +
    geom_hline(yintercept = 1, linetype = "dotted", color = "black") +
    labs(title = paste(alias, "per Extrapolation and compounds"),
         x = "Extrapolation",
         y = alias)

  ggsave(file = file.path(PIFolder,"ExtrapolationResults","Plots",paste0(auc, "_compounds.png")), width = 8, height = 6, dpi = 300)

  # AUC ratio boxplot with different species
  ggplot(metrics.dtf, aes(x = specieFrom, y = !!sym(auc), fill = Extrapolation)) +
    geom_boxplot(outlier.shape = NA) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = c(0, 4)) +
    geom_hline(yintercept = 0.5, linetype = "dotdash", color = "black") +
    geom_hline(yintercept = 2, linetype = "dotdash", color = "black") +
    geom_hline(yintercept = 1, linetype = "dotted", color = "black") +
    labs(title = paste(alias, "grouped by reference species"), x = "Extrapolation", y = alias)

  ggsave(file = file.path(PIFolder,"ExtrapolationResults","Plots", paste0(auc, "_species.png")), width = 8, height = 6, dpi = 300)

  # Loop through species vector and generate plots
  for (speciesID in species_vector) {
    species_data <- metrics.dtf %>%
      filter(specieTo == speciesID,
             specieFrom != speciesID)

    ggsave(file = file.path(PIFolder,"ExtrapolationResults","Plots", paste0(auc, "_", speciesID, ".png")),
           plot = species_plots(species_data, speciesID, y = auc, alias = alias), width = 8, height = 6, dpi = 300)
  }
}

# Compute improvements directly on metrics.dtf for Naive and fitted as ref
for (reference in c("Naive", "Fitted")) {
  improvements_dtf <- compute_improvement(metrics.dtf, metric_columns, base_extrapolation = reference)

  long_data <- improvements_dtf %>%
    pivot_longer(cols = starts_with("Improvement"), names_to = "Metric", values_to = "Improvement") %>%
    rename(Extrapolation = Extrapolation_Other)

  # Filtered data for boxplots
  improvement_metric_data <- long_data %>%
    filter(Metric == "Improvement_Metric")

  Improvement_RMSLE_data <- long_data %>%
    filter(Metric == "Improvement_RMSLE")

  # Improvement RMSLE boxplot
  ggplot(Improvement_RMSLE_data, aes(x = Extrapolation, y = Improvement, fill = Extrapolation)) +
    geom_boxplot(outlier.shape = NA) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    coord_cartesian(ylim = c(-4,1)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(title = paste0("Improvement RMSLE (vs ", reference, ") by Extrapolation"), x = "Extrapolation", y = paste0("Improvement of RMSLE (vs ", reference, ""))

  ggsave(file = file.path(PIFolder,"ExtrapolationResults","Plots", paste0("Improv_RMSLE_vs_", reference, ".png")), width = 8, height = 6, dpi = 300)

  # Improvement Metric boxplot by compound
  ggplot(improvement_metric_data, aes(x = Extrapolation, y = Improvement, fill = Extrapolation)) +
    geom_boxplot(outlier.shape = NA) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    facet_wrap(~compoundID) +
    coord_cartesian(ylim = c(-5, 1)) +
    labs(title = paste0("Improvement Metric (vs ", reference, ")by Extrapolation and compound"),
         x = "Extrapolation", y = paste0("Improvement of Metric (vs ", reference, ")"))

  ggsave(file = file.path(PIFolder,"ExtrapolationResults","Plots", paste0("Improv_metric_vs_", reference, "_compound.png")), width = 9, height = 6, dpi = 300)

  # Improvement Metric boxplot
  ggplot(improvement_metric_data, aes(x = Extrapolation, y = Improvement, fill = Extrapolation)) +
    geom_boxplot(outlier.shape = NA) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    coord_cartesian(ylim = c(-4,1)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(title = paste0("Improvement Metric (vs ", reference, ") by Extrapolation"),
         x = "Extrapolation", y = paste0("Improvement (vs ", reference, ")"))

  ggsave(file = file.path(PIFolder,"ExtrapolationResults","Plots",paste0("Improv_metric_vs_", reference, ".png")), width = 8, height = 6, dpi = 300)
}

improvements_dtf <- compute_improvement(metrics.dtf, metric_columns, base_extrapolation = "Naive")
improvements_dtf_fitted <- compute_improvement(metrics.dtf, metric_columns, base_extrapolation = "Fitted")

long_data <- improvements_dtf %>%
  pivot_longer(cols = starts_with("Improvement"), names_to = "Metric", values_to = "Improvement") %>%
  rename(Extrapolation = Extrapolation_Other)

# Filter data for comparison with "Fitted"
comparison_extrapolations <- c("FU", "KP", "SP", "FU_KP", "SP_FU", "SP_KP", "SP_FU_KP")
identification_AUC_better <- data.frame()
for (extrapolation in comparison_extrapolations) {
  improvement_metric_extrapolation <- long_data %>%
    filter(Extrapolation %in% extrapolation,
           Metric == "Improvement_Metric")
  improvement_metric_fitted <- long_data %>%
    filter(Extrapolation == "Fitted",
           Metric == "Improvement_Metric",
           DataSet %in% improvement_metric_extrapolation$DataSet)
  Comparison_improvement_metric_Extrapolation <- rbind(improvement_metric_extrapolation, improvement_metric_fitted)
  ggplot(Comparison_improvement_metric_Extrapolation, aes(x = Extrapolation, y = Improvement, fill = Extrapolation)) +
    geom_boxplot(outlier.shape = NA) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = c(-4,1)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(title = paste("Improvement Metric by Extrapolation and compound, with", extrapolation, "as reference"),
         x = "Extrapolation", y = "Improvement of Metric")
  ggsave(file = file.path(PIFolder,"ExtrapolationResults","Plots",paste0("Extrapolations_Improv_metric_",extrapolation,".png")), width = 8, height = 6, dpi = 300)



  # Same but for metric.dtf and AUC
  AUC_extrapolation <- metrics.dtf %>%
    filter(Extrapolation %in% extrapolation) %>%
    select(DataSet, Extrapolation, AUCratio,specieTo,specieFrom,compoundID)
  AUC_fitted <- metrics.dtf %>%
    filter(Extrapolation == "Fitted",
           DataSet %in% AUC_extrapolation$DataSet) %>%
    select(DataSet, Extrapolation, AUCratio,specieTo,specieFrom,compoundID)

  AUC_fitted <- Correct_fitted(AUC_fitted, AUC_extrapolation)

  Comparison_AUC_Extrapolation <- rbind(AUC_extrapolation, AUC_fitted)

  identification_AUC_better_extrapolation <- merge(AUC_extrapolation, AUC_fitted, by = c("DataSet", "compoundID", "specieTo", "specieFrom"), all = TRUE, suffixes = c("_extrapolation", "_fitted")) %>%
    filter(abs(AUCratio_extrapolation-1) < abs(AUCratio_fitted - 1)) %>%
    select(DataSet, compoundID, specieTo, specieFrom, Extrapolation_extrapolation  ,AUCratio_extrapolation, AUCratio_fitted)
  identification_AUC_better <- rbind(identification_AUC_better, identification_AUC_better_extrapolation)
  ggplot(Comparison_AUC_Extrapolation, aes(x = Extrapolation, y = AUCratio, fill = Extrapolation)) +
    geom_boxplot(outlier.shape = NA) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    coord_cartesian(ylim = c(0, 4)) +
    geom_hline(yintercept = 0.5, linetype = "dotdash", color = "black") +
    geom_hline(yintercept = 2, linetype = "dotdash", color = "black") +
    geom_hline(yintercept = 1, linetype = "dotted", color = "black") +
    labs(title = paste("AUC ratio selected on", extrapolation),
         x = "Extrapolation", y = "AUC ratio value")
  ggsave(file = file.path(PIFolder,"ExtrapolationResults", "Plots", paste0("Extrapolations_AUCratio_", extrapolation, ".png")), width = 8, height = 6, dpi = 300)

}
write.csv(identification_AUC_better, file = file.path(PIFolder,"ExtrapolationResults", paste0("AUC_better_extrapolation.csv")), row.names = FALSE)


