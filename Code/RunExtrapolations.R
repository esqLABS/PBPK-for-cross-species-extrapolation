source("Code/utils/utilities-project.R")
source("ExtractInVivoDataAdmins.R")

#' @import dplyr
#' @import ggplot2
runExtrapolations <- function(projectConfiguration, inVivoData, saveSimulations = TRUE, yAxisScale = "log") {
  projectConfigExtrapol <- projectConfiguration$clone()
  projectConfigExtrapol$scenarioDefinitionFile <- "ScenariosExtrapolation.xlsx"
  projectConfigExtrapol$paramsFile <- file.path(
    projectConfigExtrapol$paramsFolder,
    "ModelParametersExtrapolation.xlsx"
  )

  projectConfigFitted <- projectConfiguration$clone()
  projectConfigFitted$scenarioDefinitionFile <- "ScenariosFitted.xlsx"
  projectConfigFitted$modelFolder <- file.path(projectConfigFitted$outputFolder, "PIResults", "Simulations")

  scenariosConfiguration <- readScenarioConfigurationFromExcel(
    projectConfiguration = projectConfigExtrapol
  )

  # set export config and create directory
  export_config <- createEsqlabsExportConfiguration(projectConfigExtrapol)
  export_config$path <- file.path(export_config$path, "ExtrapolationResults")

  if (!dir.exists(export_config$path)) {
    dir.create(export_config$path)
    dir.create(file.path(export_config$path, "Plots"))

  }

  adminsAllDF <- extractInVivoDataAdmins(inVivoData)

  # Update scenario config to follow admin and set outputPaths:
  scenarioConfigList <- list()
  scenarioList <- list()
  for (scenarioConfigIdx in seq_along(scenariosConfiguration)) {
    scenarioNameSplit <- str_split(names(scenariosConfiguration)[scenarioConfigIdx], pattern = "_", simplify = TRUE)
    compoundID <- scenarioNameSplit[1]
    specieToID <- scenarioNameSplit[4]
    specieFromID <- scenarioNameSplit[2]
    extrapol <- paste(scenarioNameSplit[5:length(scenarioNameSplit)], collapse = "_")

    adminSpecieCompound <- adminsAllDF %>% filter(.data[["Compound"]] == compoundID & .data[["Specie"]] == specieToID)

    # create scenario and sim for each admin
    for (adminIdx in seq_len(nrow(adminSpecieCompound))) {
      admin <- paste(adminSpecieCompound[adminIdx, c("Specie", "Compound", "Route", "Dose")], collapse = "_")
      admin <- paste(specieFromID, "to", admin, extrapol, sep = "_")

      # Rename scenarioPC
      scenarioConfigList[[admin]] <- scenariosConfiguration[[scenarioConfigIdx]]$clone()
      scenarioConfigList[[admin]]$scenarioName <- admin

      timeMax <- as.numeric(adminSpecieCompound[adminIdx, "TMax"])
      timeUnit <- as.character(adminSpecieCompound[adminIdx, "TUnit"])
      timeRes <- case_when(timeUnit == "h" ~ 60,
        timeUnit == "day" ~ 24,
        .default = 1
      )

      scenarioConfigList[[admin]]$simulationTime <- paste(0, timeMax, timeRes,
        sep = ","
      )
      scenarioConfigList[[admin]]$simulationTimeUnit <- timeUnit

      # Get output from all data sets
      outputPaths <- data.frame(
        Organs = unlist(adminSpecieCompound[adminIdx, "Organ"]),
        Compartments = unlist(adminSpecieCompound[adminIdx, "Compartment"])
      )

      outputPaths <- outputPaths %>%
        rowwise() %>%
        mutate(Paths = createOutputPath(.data[["Organs"]], .data[["Compartments"]]))

      scenarioConfigList[[admin]]$outputPaths <- outputPaths$Paths

      # create scenario to have access to sim
      scenarioList <- c(scenarioList, createScenarios(scenarioConfigList[admin]))
      sim <- scenarioList[[admin]]$simulation

      # get Application param and add
      routeStr <- adminSpecieCompound[adminIdx, "Route"]
      doseStr <- adminSpecieCompound[adminIdx, "Dose"]

      setAdministration(doseStr, routeStr, sim)
      setParameterValuesByPath(
        parameterPaths = "Compound-Total Hepatic Clearance-InVitroTotalHepaticClearance|Blood/Plasma concentration ratio", # nolint: line_length_linter
        values = 1,
        simulation = sim
      )
      setParameterValuesByPath(
        parameterPaths = "Organism|Liver|Periportal|Intracellular|Undefined Liver|Concentration",
        values = 1,
        simulation = sim
      )
    }
  }

  scenariosConfigurationFitted <- readScenarioConfigurationFromExcel(
    projectConfiguration = projectConfigFitted
  )
  scenarioFitted <- createScenarios(scenariosConfigurationFitted)

  results <- runScenarios(c(scenarioList, scenarioFitted))

  if (isTRUE(saveSimulations)) {
    # Save all extrapolations as pkml
    if (!dir.exists(file.path(export_config$path, "Simulations"))) {
      dir.create(file.path(export_config$path, "Simulations"))
    }

    for (resultIdx in seq_along(results)) {
      saveSimulation(
        simulation = results[[resultIdx]]$simulation,
        filePath = file.path(
          export_config$path,
          "Simulations",
          gsub("/", "per", paste0(names(results)[resultIdx], ".pkml"))
        )
      )
    }
  }

  # Loop for DataCombined
  metrics.dtf <- data.frame()

  # Get unique combination of Compound/Species
  allPlots <- adminsAllDF %>%
    ungroup() %>%
    select(.data[["Compound"]], .data[["Specie"]]) %>%
    unique() %>%
    rowwise() %>%
    mutate(Name = paste(.data[["Specie"]], .data[["Compound"]], sep = "_"))

  plots <- vector("list", nrow(allPlots))
  names(plots) <- allPlots$Name

  for (adminIdx in seq_len(nrow(adminsAllDF))) {
    compoundID <- adminsAllDF %>%
      ungroup() %>%
      slice(adminIdx) %>%
      pull("Compound")
    speciesTo <- adminsAllDF %>%
      ungroup() %>%
      slice(adminIdx) %>%
      pull("Specie")
    route <- adminsAllDF %>%
      ungroup() %>%
      slice(adminIdx) %>%
      pull("Route")
    dose <- adminsAllDF %>%
      ungroup() %>%
      slice(adminIdx) %>%
      pull("Dose")

    admin <- paste(speciesTo, compoundID, route, dose, sep = "_")
    scenariosToKeep <- grep(pattern = admin, c(names(scenarioList), names(scenarioFitted)))
    if (length(scenariosToKeep) > 0) {
    for (outputsIdx in seq_along(adminsAllDF[adminIdx, "Organ"])) {
      organ <- adminsAllDF[adminIdx, "Organ"][[outputsIdx]]
      compartment <- adminsAllDF[adminIdx, "Compartment"][[outputsIdx]]

      datasets <- inVivoData[sapply(inVivoData, function(dataset) {
        if (is.null(dataset$metaData$Organ) | is.null(dataset$metaData$Compartment)) {
          res <- FALSE
        } else {
          res <- dataset$metaData$Molecule == compoundID &
            dataset$metaData$Species == speciesTo &
            dataset$metaData$Route == route &
            dataset$metaData$Dose == dose &
            gsub(" ", "", dataset$metaData$Organ) == organ &
            dataset$metaData$Compartment == compartment
        }
        return(res)
      })]

      outputPaths <- createOutputPath(organ = organ, compartment = compartment)

      myDataCombined <- DataCombined$new()

      for (resIdx in scenariosToKeep) {
        if (!is.null(results[[resIdx]]$results)) {
          myDataCombined$addSimulationResults(
            names = names(results)[resIdx],
            simulationResults = results[[resIdx]]$results,
            quantitiesOrPaths = outputPaths
          )
          mol_weight <- getParameter("Compound|Molecular weight", results[[resIdx]]$simulation)
        }
      }

      for (dataset in datasets) {
        StudyName <- dataset$metaData$`Study Id`

        dataset$molWeight <- toUnit(
          ospDimensions$`Molecular weight`,
          values = mol_weight$value,
          targetUnit = "g/mol",
          sourceUnit = mol_weight$unit
        )

        myDataCombined$addDataSets(
          dataSets = dataset,
          groups = StudyName
        )
      }

      myDataCombinedPlot <- myDataCombined
      myDataCombinedPlot <- convertUnits(myDataCombinedPlot, xUnit = "h", yUnit = "µg/L")
      myDataCombinedPlot <- myDataCombinedPlot %>%
        mutate(
          # Extract 'species_from' as the first term before the first underscore
          species_from = str_extract(.data[["name"]], "^[^_]+"),
          # Extract 'species_to' as the word after the second underscore
          species_to = str_extract(.data[["name"]], "(?<=_to_)[^_]+"),
          # Extract 'extrapolation' using a pattern match towards the end of the string
          extrapolation = str_extract(.data[["name"]], "(Naive|SP|FU|KP|FU_KP|SP_KP|SP_FU|SP_FU_KP|Fitted)$"),
          extrapolation = ifelse(is.na(.data[["extrapolation"]]), "obs", .data[["extrapolation"]])
        )

      metric <- calculateMetric(myDataCombinedPlot)
      metric <- metric %>% mutate(
        compoundID = compoundID,
        specieTo = speciesTo,
        specieFrom = str_split(.data[["Simulation"]], pattern = "_", simplify = TRUE, n = 2)[, 1],
        Extrapolation = str_split(.data[["Simulation"]], pattern = "_", simplify = TRUE, n = 7)[, 7],
        StudyID = str_split(DataSet, pattern = "_", simplify = TRUE, n = 2)[, 1]
      )

      # plot of myDataCombinedPlot Naive versus others extrapolation
      plot <- ggplot(
        data = myDataCombinedPlot %>%
          filter(
            .data[["dataType"]] == "simulated",
            !(.data[["extrapolation"]] %in% c("Naive", "obs", "Fitted"))
          ),
        mapping = aes(x = xValues, y = yValues)
      ) +
        # Observed data points (larger)
        geom_point(
          data = myDataCombinedPlot %>%
            filter(.data[["dataType"]] == "observed") %>%
            select(-extrapolation),
          aes(shape = `Study Id`),
          size = 3,
          alpha = 1
        ) +
        # Naive extrapolation lines
        geom_line(
          data = myDataCombinedPlot %>%
            filter(
              .data[["dataType"]] == "simulated",
              extrapolation == "Naive"
            ) %>%
            select(-extrapolation),
          aes(linetype = "Naive", color = species_from),
          size = 1.2
        ) +

        # Extrapolated lines with increased line width
        geom_line(
          aes(linetype = "Extrapolated", color = species_from),
          size = 1.2
        ) +
        facet_grid(~ extrapolation, drop = TRUE) +
        scale_color_brewer(palette = "Set2") +
        ggnewscale::new_scale_color() +
        # Fitted lines
        geom_line(
          data = myDataCombinedPlot %>%
            filter(
              .data[["dataType"]] == "simulated",
              extrapolation == "Fitted"
            ) %>%
            select(-extrapolation),
          aes(color = "Fitted"),
          size = 1.2,
          alpha = 0.8
        ) +
        scale_colour_manual(values = c("Fitted" = "mediumpurple1")) +
        scale_linetype_manual(values = c("Naive" = "solid", "Extrapolated" = "dashed")) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
        theme_minimal(base_size = 14) + # increased base size for texts
        labs(
          title = paste("Compound:", compoundID, "; Species:", speciesTo, "; Organ:", organ, "; Compartment:", compartment),
          x = "Time (h)", y = "Concentration (µg/l)"
        ) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1), # rotated labels
          panel.grid.major = element_line(color = "grey85"), # lighter grid lines
          panel.grid.minor = element_blank()
         )# +
        # ggtitle(paste(route, dose, organ, compartment))


      if (yAxisScale == "log") {
        plot <- plot + scale_y_log10()
      }

      # for multiple datasets add total
      if (length(unique(metric$StudyID)) > 1) {
        tableMetrics <- lapply(c("Metric", "RMSE", "RMSLE"), function(m) {
          metric %>%
            select(specieFrom, StudyID, Extrapolation, all_of(m)) %>%
            group_by(specieFrom, Extrapolation) %>%
            group_modify(~ bind_rows(., summarize_at(., .vars = m, ~ sqrt(sum(.^2))))) %>%
            mutate(StudyID = coalesce(StudyID, "Total")) %>%
            tidyr::spread(Extrapolation, m)
        })
      } else {
        tableMetrics <- lapply(c("Metric", "RMSE", "RMSLE"), function(m) {
          metric %>%
            select(specieFrom, StudyID, Extrapolation, all_of(m)) %>%
            tidyr::spread(Extrapolation, m)
        })
      }
      # move Fitted results
      tableMetrics <- lapply(tableMetrics, function(table) {
        if ("Fitted" %in% colnames(table)) {
          table <- table %>%
            group_by(StudyID) %>%
            mutate("Fitted" = .data[["Fitted"]][.data[["specieFrom"]] == speciesTo]) %>%
            filter(specieFrom != speciesTo)
        }
        return(table)
      })

      names(tableMetrics) <- c("Metric", "RMSE", "RMSLE")
      tableGrobs <- lapply(seq_len(length(tableMetrics)), function(i) {
        cols <- ifelse(tableMetrics[[i]] < tableMetrics[[i]]$Naive, "lightgreen", NA)
        table <- gridExtra::tableGrob(
          d = tableMetrics[[i]],
          theme = gridExtra::ttheme_default(base_size = 8, core = list(bg_params = list(fill = cols)))
        )
        title <- grid::textGrob(names(tableMetrics)[i])
        table <- gtable::gtable_add_rows(
          table,
          heights = grid::grobHeight(title) + unit(0.5, "line"), pos = 0
        )
        table <- gtable::gtable_add_grob(
          table, list(title),
          t = 1, l = 1, r = ncol(table)
        )
      })

      tablePlot <- gridExtra::arrangeGrob(grobs = tableGrobs, ncol = 1)

      if (is.null(plots[[paste(speciesTo, compoundID, sep = "_")]])) {
        plots[[paste(speciesTo, compoundID, sep = "_")]] <- list(
          gridExtra::arrangeGrob(grobs = list(ggplotGrob(plot), tablePlot), ncol = 2)
        )
      } else {
        plots[[paste(speciesTo, compoundID, sep = "_")]] <- c(
          plots[[paste(speciesTo, compoundID, sep = "_")]],
          list(gridExtra::arrangeGrob(grobs = list(ggplotGrob(plot), tablePlot), ncol = 2))
        )
      }
      metrics.dtf <- rbind(metrics.dtf, metric)
    }
  }
  }
  plots <- plots %>% purrr::discard(is.null)
  sapply(seq_len(length(plots)), function(i) {
    ggsave(
      filename = file.path(export_config$path, "Plots", paste0(names(plots)[i], ".jpg")),
      plot = gridExtra::grid.arrange(grobs = plots[[i]], ncol = 1),
      width = 20,
      height = 7 * length(plots[[i]])
    )
  })

  write.csv(file = file.path(export_config$path, "metrics_summary.csv"), metrics.dtf, row.names = FALSE)

  # do we need to aggregate metric for multiple studies corresponding to the same extrapolation?
  metrics.dtf$Extrapolation <- factor(
    x = metrics.dtf$Extrapolation,
    levels = c("Naive", "FU", "KP", "SP", "FU_KP", "SP_FU", "SP_KP", "SP_FU_KP", "Fitted")
  )

  # aggregate
  metrics_agg <- metrics.dtf %>%
    group_by(Extrapolation, compoundID, specieTo, specieFrom) %>%
    summarize_at(.vars = c("Metric", "RMSE", "RMSLE",
                           "foldCmax", "foldtend", "foldmean",
                           "foldCmaxlog", "foldtendlog", "foldmeanlog",
                           "AUCobs", "AUCsim", "AUCratio"),
                 .funs = ~ mean(.)) %>%
    arrange(compoundID)

  write.csv(file = file.path(export_config$path, "metrics_aggregated.csv"), metrics_agg, row.names = FALSE)
}


# function that takes as entry a myDC_df and returns the metric
#' @importFrom rlang .data
#' @import dplyr
calculateMetric <- function(myDC_df) {
  observed <- myDC_df %>%
    filter(dataType == "observed") %>%
    mutate(yValues = ifelse(.data$yValues == 0, 0.00001, .data$yValues))
  observed_time <- observed %>% pull(xValues)

  # remove subjectID from name
  observed$name <- apply(str_split_fixed(observed$name, "_", n = 7)[, -3], 1, paste, collapse = "_")

  simulated <- myDC_df %>% filter(dataType == "simulated")
  simulated <- simulated %>%
    group_by(name) %>%
    tidyr::complete(xValues = observed_time) %>%
    arrange(xValues, .by_group = TRUE) %>%
    mutate(yValues = approx(xValues, yValues, xValues)$y) %>%
    filter(xValues %in% observed_time)

  # add corresponding observed value
  comparison <- left_join(simulated, observed, by = "xValues") %>% select(name.x, name.y, xValues, yValues.x, yValues.y)
  # calculate the metric
  comparison <- comparison %>%
    summarise(
      metric = sqrt(mean((yValues.y - yValues.x)^2 / yValues.y^2)),
      RMSE = sqrt(mean((yValues.y - yValues.x)^2)),
      RMSLE = sqrt(mean((log10(yValues.y) - log10(yValues.x))^2)),
      foldCmax = ospsuite.utils::foldSafe(pmax(yValues.y, yValues.x), pmin(yValues.y, yValues.x))[[1]],
      foldtend = tail(ospsuite.utils::foldSafe(pmax(yValues.y, yValues.x), pmin(yValues.y, yValues.x)), n = 1),
      foldmean = mean(ospsuite.utils::foldSafe(pmax(yValues.y, yValues.x), pmin(yValues.y, yValues.x))),
      foldCmaxlog = ospsuite.utils::foldSafe(
        pmax(log10(yValues.y), log10(yValues.x)),
        pmin(log10(yValues.y), log10(yValues.x))
      )[[1]],
      foldtendlog = tail(
        ospsuite.utils::foldSafe(
          pmax(log10(yValues.y), log10(yValues.x)),
          pmin(log10(yValues.y), log10(yValues.x))
        ),
        n = 1
      ),
      foldmeanlog = mean(
        ospsuite.utils::foldSafe(
          pmax(log10(yValues.y), log10(yValues.x)),
          pmin(log10(yValues.y), log10(yValues.x))
        )
      ),
      AUCobs = pracma::trapz(xValues, yValues.y),
      AUCsim = pracma::trapz(xValues, yValues.x),
      AUCratio = pracma::trapz(xValues, yValues.x) / pracma::trapz(xValues, yValues.y),
      .by = c(name.x, name.y)
    ) %>%
    rename(Simulation = name.x, DataSet = name.y, Metric = metric)

  return(comparison)
}
