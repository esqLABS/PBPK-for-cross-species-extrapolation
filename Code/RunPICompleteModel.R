# load useful functions
source("Code/utils/utilities-project.R")
source("ExtractInVivoDataAdmins.R")

#' @import dplyr
runPICompleteModel <- function(
    drugs,
    species,
    inVivoData,
    projectConfiguration,
    renallyCleared = "Acyclovir",
    piScaling = "log",
    piTargetFunctionType = "m3",
    piAlgorithm = "BOBYQA",
    savePlots = "pdf") {
  # Get outputfolder from project config
  projectConfiguration$paramsFolder <- file.path(projectConfiguration$outputFolder, "Parameters")
  projectConfiguration$paramsFile <- "ModelParameters.xlsx"
  projectConfiguration$scenarioDefinitionFile <- "ScenariosPI.xlsx"
  piOutputFolder <- file.path(projectConfiguration$outputFolder, "PIResults")

  # Code
  scenariosConfiguration <- readScenarioConfigurationFromExcel(
    projectConfiguration = projectConfiguration
  )

  # get admin from inVivoData
  adminsAllDF <- extractInVivoDataAdmins(inVivoData)

  # Do PI for each compound specie combination
  for (compoundID in drugs) {
    for (speciesID in species) {
      print(paste(compoundID, speciesID, sep = "_"))

      # subset scenarios for specific compound and species
      scenariosToKeep <- str_detect(names(scenariosConfiguration), compoundID) &
        str_detect(names(scenariosConfiguration), speciesID)

      scenariosConfigPC <- scenariosConfiguration[scenariosToKeep]
      adminSpecieCompound <- adminsAllDF %>%
        filter(.data[["Compound"]] == compoundID & .data[["Specie"]] == speciesID)

      # get dataset to be used for species/compound
      dataToKeep <- vapply(
        X = inVivoData,
        FUN = function(dataset) {
          if (is.null(dataset$metaData$Organ) |
            is.null(dataset$metaData$Compartment)) {
            res <- FALSE
          } else {
            res <- dataset$metaData$Molecule == compoundID &
              dataset$metaData$Species == speciesID &
              gsub(" ", "", dataset$metaData$Organ) %in% unlist(adminSpecieCompound$Organ) &
              dataset$metaData$Compartment %in% unlist(adminSpecieCompound$Compartment)
          }
          return(res)
        },
        FUN.VALUE = logical(1L)
      )

      # initialize results per species & compound
      outputsPI <- list()

      for (scenarioPC in scenariosConfigPC) {
        partCoeff <- gsub(
          pattern = "PC",
          replacement = "",
          x = str_split(scenarioPC$modelFile, "_",
            simplify = TRUE
          )[3],
          fixed = TRUE
        )
        print(partCoeff)

        # initialize output mapping and sim lists
        outputMappings <- list()
        studySimulations <- list()
        lipophilicityParametersList <- list()
        clearanceParametersList <- list()
        GFRParametersList <- list()

        # create scenario and sim for each admin
        for (adminIdx in seq_len(nrow(adminSpecieCompound))) {
          admin <- paste(
            adminSpecieCompound[
              adminIdx,
              c("Specie", "Compound", "Route", "Dose")
            ],
            collapse = "_"
          )

          # Rename scenarioPC and clone (to apply admins)
          scenarioConfig <- list()
          scenarioConfig[[admin]] <- scenarioPC$clone()
          scenarioConfig[[admin]]$scenarioName <- admin

          # Get and apply simulation times to scenario config
          timeMax <- as.numeric(adminSpecieCompound[adminIdx, "TMax"])
          timeUnit <- as.character(adminSpecieCompound[adminIdx, "TUnit"])
          timeRes <- case_when(timeUnit == "h" ~ 60,
            timeUnit == "day" ~ 24,
            .default = 1
          )

          scenarioConfig[[admin]]$simulationTime <- paste(0, timeMax, timeRes, sep = ",")
          scenarioConfig[[admin]]$simulationTimeUnit <- timeUnit

          # Get output from all data sets and add to sim
          outputPaths <- data.frame(
            Organs = unlist(adminSpecieCompound[adminIdx, "Organ"]),
            Compartments = unlist(adminSpecieCompound[adminIdx, "Compartment"])
          )

          outputPaths <- outputPaths %>%
            rowwise() %>%
            mutate(Paths = createOutputPath(.data[["Organs"]], .data[["Compartments"]]))

          scenarioConfig[[admin]]$outputPaths <- outputPaths$Paths

          # create scenarios to have access to sim
          scenario <- createScenarios(scenarioConfig)
          sim <- scenario[[admin]]$simulation

          # get Application param and set
          routeStr <- as.character(adminSpecieCompound[adminIdx, "Route"])
          doseStr <- as.character(adminSpecieCompound[adminIdx, "Dose"])

          setAdministration(doseStr, routeStr, sim)

          # Set Blood/Plasma ratio to 1 to decouple lipo and clearance, and enzyme liver to 1 umol/l
          setParameterValuesByPath(
            parameterPaths = "Compound-Total Hepatic Clearance-InVitroTotalHepaticClearance|Blood/Plasma concentration ratio", # nolint : line_length_linter
            values = 1,
            units = "",
            simulation = sim
          )
          setParameterValuesByPath(
            parameterPaths = "Organism|Liver|Periportal|Intracellular|Undefined Liver|Concentration",
            values = 1,
            units = "umol/l",
            simulation = sim
          )

          # apply mol weigth to data if not given the xls
          for (studyIdx in which(dataToKeep)) {
            if (is.null(inVivoData[[studyIdx]]$molWeight)) {
              inVivoData[[studyIdx]]$molWeight <- toUnit(
                quantityOrDimension = ospDimensions$`Molecular weight`,
                values = getParameter("Compound|Molecular weight", sim)$value,
                targetUnit = "g/mol",
                sourceUnit = getParameter("Compound|Molecular weight", sim)$unit
              )
            }
          }

          # get study organs
          dataMetaData <- do.call(
            rbind,
            lapply(
              inVivoData[dataToKeep],
              function(l) {
                data.frame(
                  Route = l$metaData$Route,
                  Dose = l$metaData$Dose,
                  Organ = gsub(" ", "", l$metaData$Organ),
                  Compartment = l$metaData$Compartment
                )
              }
            )
          )

          # map simulation and datasets
          for (pathIdx in seq_len(nrow(outputPaths))) {
            outputMapping <- PIOutputMapping$new(
              quantity = getQuantity(
                path = sim$outputSelections$allOutputs[[pathIdx]]$path,
                container = sim
              )
            )
            outputMapping$addObservedDataSets(
              data = inVivoData[
                which(dataToKeep)[
                  dataMetaData$Route == as.character(adminSpecieCompound[adminIdx, "Route"]) &
                    dataMetaData$Dose == as.character(adminSpecieCompound[adminIdx, "Dose"]) &
                    dataMetaData$Organ == outputPaths$Organs[pathIdx] &
                    dataMetaData$Compartment == outputPaths$Compartments[pathIdx]
                ]
              ]
            )

            # PI scaling
            outputMapping$scaling <- piScaling

            adminQuant <- paste(
              admin,
              outputPaths$Organs[pathIdx],
              outputPaths$Compartments[pathIdx]
            )
            # add output mappping and sim to list
            studySimulations[[adminQuant]] <- sim
            outputMappings[[adminQuant]] <- outputMapping
          }

          # Set parameters to fit
          lipophilicityParametersList <- c(
            lipophilicityParametersList,
            ospsuite::getParameter(
              path = "Compound|Lipophilicity",
              container = sim
            ),
            ospsuite::getParameter(
              path = "Compound-Total Hepatic Clearance-InVitroTotalHepaticClearance|Lipophilicity (experiment)",
              container = sim
            )
          )
          clearanceParametersList <- c(
            clearanceParametersList,
            ospsuite::getParameter(
              path = "Compound-Total Hepatic Clearance-InVitroTotalHepaticClearance|Plasma clearance",
              container = sim
            )
          )
          GFRParametersList <- c(
            GFRParametersList,
            ospsuite::getParameter(
              path = "Neighborhoods|Kidney_pls_Kidney_ur|Compound|Glomerular Filtration-RenalClearanceGFR|GFR fraction",
              container = sim
            )
          )
        }

        if (length(studySimulations) > 0) {
          piConfiguration <- PIConfiguration$new()
          piConfiguration$targetFunctionType <- piTargetFunctionType
          piConfiguration$algorithm <- piAlgorithm

          # add Lipo to fit
          parameterLipophilicity <- PIParameters$new(
            parameters = c(lipophilicityParametersList)
          )
          # add clearance to fit
          parameterClearance <- PIParameters$new(
            parameters = c(clearanceParametersList)
          )
          # add GFR to fit
          parameterGFR <- PIParameters$new(
            parameters = c(GFRParametersList)
          )

          # set range (need to set as argument)
          parameterLipophilicity$minValue <- parameterLipophilicity$startValue - 2
          parameterLipophilicity$maxValue <- parameterLipophilicity$startValue + 2
          parameterClearance$minValue <- 0.1 * parameterClearance$startValue
          parameterClearance$maxValue <- 10 * parameterClearance$startValue

          parameterGFR$minValue <- 0.1 * parameterGFR$startValue
          parameterGFR$maxValue <- 10 * parameterGFR$startValue

          if (compoundID %in% renallyCleared) {
            parametersToFit <- c(Lipo = parameterLipophilicity, GFR = parameterGFR)
          } else {
            parametersToFit <- c(Lipo = parameterLipophilicity, PlasmaClearance = parameterClearance)
          }

          task <- ParameterIdentification$new(
            simulations = studySimulations,
            parameters = parametersToFit,
            outputMappings = outputMappings,
            configuration = piConfiguration
          )

          # Run PI
          results <- task$run()

          # save plots
          if (!is.null(savePlots)) {
            plots <- task$plotResults()

            # in names(studySimulations) we replace  / by _ to use for file path
            name_plot <- gsub("/", "_", names(studySimulations))
            # add study name as title
            plots <- sapply(seq_along(plots), function(i) {
              plots[[i]] + patchwork::plot_annotation(names(studySimulations)[i])
            })

            if ("png" %in% savePlots) {
              sapply(seq_along(plots), function(i) {
                ggsave(
                  filename = file.path(
                    piOutputFolder,
                    "Plots",
                    paste0(paste(speciesID, compoundID, partCoeff, name_plot[i], sep = "_"), ".png")
                  ),
                  plot = plots[[i]],
                  width = 7,
                  height = 7 * 0.618,
                  dpi = 250
                )
              })
            }

            if ("pdf" %in% savePlots) {
              cairo_pdf(
                filename = file.path(
                  piOutputFolder,
                  "Plots",
                  paste0(paste(speciesID, compoundID, partCoeff, sep = "_"), ".pdf")
                ),
                onefile = TRUE
              )
              print(plots)
              dev.off()
            }
          }

          # store results to list
          startParam <- vapply(
            X = parametersToFit,
            FUN = function(l) {
              l$startValue
            },
            FUN.VALUE = numeric(1L)
          )
          objBefore <- task$.__enclos_env__$private$.targetFunction(startParam)$model
          objAfter <- task$.__enclos_env__$private$.targetFunction(results$par)$model

          outputPIPC <- tibble::tibble_row(
            Compound = compoundID,
            Species = speciesID,
            PartitionCoef = partCoeff,
            objectiveBefore = objBefore,
            objectiveAfter = objAfter
          )

          for (i in seq_along(parametersToFit)) {
            outputPIPC <- bind_cols(
              outputPIPC,
              tibble::tibble_row(
                parametersToFit[[i]]$startValue,
                results$par[[i]],
                results$lwr[[i]],
                results$upr[[i]],
                results$cv[[i]],
                .name_repair = ~ paste0(
                  c("initial", "estimated", "lower", "upper", "variation"),
                  names(parametersToFit)[i]
                )
              )
            )
          }

          # Add specific clearance
          outputPIPC <- bind_cols(
            outputPIPC,
            estimatedSpecifiClearance = getParameter(
              path = "Compound-Total Hepatic Clearance-InVitroTotalHepaticClearance|Specific clearance",
              container = task$simulations[[1]]
            )$value
          )

          # add to compound/ species outputs
          outputsPI <- bind_rows(outputsPI, outputPIPC)

          # save fitted simulations
          for (i in seq_len(length(studySimulations))) {
            saveSimulation(
              simulation = studySimulations[[i]],
              filePath = file.path(
                piOutputFolder,
                "Simulations",
                paste0(paste(partCoeff, gsub("/", "per", names(studySimulations)[i]), sep = "_"), ".pkml")
              )
            )
          }
        }
      }

      # write output results to file
      if (length(outputsPI) > 0) {
        write.csv(
          x = outputsPI,
          file = file.path(
            piOutputFolder,
            paste0(speciesID, "_", compoundID, ".csv")
          ),
          row.names = FALSE
        )
      }

      ospsuite::clearMemory()
    }
  }
  return(invisible(NULL))
}
