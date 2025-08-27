# This script is about generating the different excel files for the scenario
#' @import dplyr
#' @importFrom stringr str_detec
createExtrapolationScenarios <- function(
    projectConfiguration,
    drugs,
    species,
    compoundDataPath,
    extrapolations = c("Naive", "SP", "FU", "KP", "SP_FU", "SP_KP", "FU_KP", "SP_FU_KP", "Fitted")) {
  # check extrapolations given
  if (!all(extrapolations %in% c("Naive", "SP", "FU", "KP", "SP_FU", "SP_KP", "FU_KP", "SP_FU_KP", "Fitted"))) {
    stop("Extrapolation supported are: Naive, SP, FU, KP, SP_FU, SP_KP, FU_KP, SP_FU_KP, Fitted")
  }

  # get folder with PI results
  piResultsFolder <- file.path(projectConfiguration$outputFolder, "PIResults")

  # create scenarios
  scenariosCombinations <- expand.grid(drugs, species, species, extrapolations)
  colnames(scenariosCombinations) <- c("Drug", "Species_From", "Species_To", "Extrapolation")

  scenariosCombinations$DrugShortName <- names(drugs)[match(scenariosCombinations$Drug, drugs)]

  # Create table of combinations between species, drugs and extrapolation
  if ( "Fitted" %in% extrapolations) {
    scenariosCombinationsFitted <- scenariosCombinations %>%
      filter(
        (.data[["Species_From"]] == .data[["Species_To"]] & .data[["Extrapolation"]] == "Fitted")
      )
    scenariosTableFitted <- scenariosCombinationsFitted %>%
      rowwise() %>%
      do(constructScenario(.$Drug, .$DrugShortName, .$Species_From, .$Species_To, .$Extrapolation, piResultsFolder))

    scenariosListFitted <- NULL
    scenariosListFitted[["Scenarios"]] <- scenariosTableFitted
    scenariosListFitted[["OutputPaths"]] <- data.frame()

    write_xlsx(
      x = scenariosListFitted,
      path = file.path(projectConfiguration$outputFolder, "Parameters", "ScenariosFitted.xlsx")
    )
  }

  if (sum(extrapolations != "Fitted") > 0) {
    scenariosCombinationsExtrapolation <- scenariosCombinations %>%
      filter(
        (.data[["Species_From"]] != .data[["Species_To"]] & .data[["Extrapolation"]] != "Fitted")
      )

    # read in vitro for extrapolation
    inVitroLiverMic <- readxl::read_excel(compoundDataPath, sheet = "InVitroLM")
    inVitoLiverHep <- readxl::read_excel(compoundDataPath, sheet = "InVitroHep")

    # We apply the function to the ScenarioCombination.dfr
    scenariosTableExtrapolation <- scenariosCombinationsExtrapolation %>%
      rowwise() %>%
      do(constructScenario(.$Drug, .$DrugShortName, .$Species_From, .$Species_To, .$Extrapolation, piResultsFolder))

    # We then write the table by using readxl package
    scenariosListExtrapolation <- NULL
    scenariosListExtrapolation[["Scenarios"]] <- scenariosTableExtrapolation
    scenariosListExtrapolation[["OutputPaths"]] <- data.frame()

    # load file with Model Parameter
    sheetNames <- readxl::excel_sheets(
      path = file.path(projectConfiguration$outputFolder, "Parameters", "ModelParameters.xlsx")
    )
    modelParams <- lapply(
      X = seq_along(sheetNames),
      FUN = function(i) {
        readxl::read_xlsx(
          path = file.path(projectConfiguration$outputFolder, "Parameters", "ModelParameters.xlsx"),
          sheet = i
        )
      }
    )
    names(modelParams) <- sheetNames

    for (drugIdx in seq_along(drugs)) {
      drug <- drugs[drugIdx]
      shortDrug <- names(drugs)[drugIdx]
      for (specie in species) {
        resultsPath <- file.path(piResultsFolder, paste0(paste(specie, drug, sep = "_"), ".csv"))
        if (file.exists(resultsPath)) {
          piResults <- read.csv(file = resultsPath)
          best <- which.min(piResults$objectiveAfter)

          # Updates global sheet related to species/compound (lipophilicity)
          sheetToUpdate <- paste(shortDrug, specie, "global", sep = "_")
          modelParams[[sheetToUpdate]] <- modelParams[[sheetToUpdate]] %>%
            mutate(
              Value = ifelse(.data[["Parameter Name"]] %in% c("Lipophilicity", "Lipophilicity (experiment)"),
                piResults$estimatedLipo[best],
                .data[["Value"]]
              )
            )

          # update KP sheet
          sheetToUpdate <- paste(shortDrug, specie, "KP", sep = "_")
          if ("estimatedGFR" %in% colnames(piResults)) {
            modelParams[[sheetToUpdate]] <- modelParams[[sheetToUpdate]] %>%
              mutate(
                "Value" = case_when(
                  .data[["Parameter Name"]] == "GFR fraction" ~ piResults[best, "estimatedGFR"],
                  .default = .data[["Value"]]
                )
              )
          }

          # change plasma clearance for Specific clearance
          modelParams[[sheetToUpdate]] <- modelParams[[sheetToUpdate]] %>%
            mutate(
              "Parameter Name" = case_when(
                .data[["Parameter Name"]] == "Plasma clearance" ~ "Specific clearance",
                .default = .data[["Parameter Name"]]
              ),
              "Value" = case_when(
                .data[["Parameter Name"]] == "Specific clearance" ~ piResults$estimatedSpecifiClearance[best],
                .default = .data[["Value"]]
              ),
              "Units" = case_when(
                .data[["Parameter Name"]] == "Specific clearance" ~ "1/min",
                .default = .data[["Units"]]
              )
            )

          # Calculate scaling factor based on species from to species to if we have in vitro on both species.
          scalingFacMic <- inVitroLiverMic %>%
            filter(.data[["CompoundId"]] == drug) %>%
            select(-.data[["CompoundId"]], -.data[["Units"]])
          scalingFacMic <- scalingFacMic %>% mutate_all(.funs = ~ .x / unname(scalingFacMic %>% pull(specie)))
          scalingFacHep <- inVitoLiverHep %>%
            filter(.data[["CompoundId"]] == drug) %>%
            select(-.data[["CompoundId"]], -.data[["Units"]])
          scalingFacHep <- scalingFacHep %>% mutate_all(.funs = ~ .x / unname(scalingFacHep %>% pull(specie)))

          for (specieTo in colnames(scalingFacMic)) {
            if (specieTo != specie) {
              sheetToAdd <- paste(shortDrug, specie, "To", specieTo, "KP", sep = "_")

              # get scaling factor and update
              if (!is.na(scalingFacMic[specieTo]) || !is.na(scalingFacHep[specieTo])) {
                scalingFactor <- mean(c(scalingFacMic %>% pull(specieTo), scalingFacHep %>% pull(specieTo)), na.rm = TRUE)
                modelParams[[sheetToAdd]] <- modelParams[[sheetToUpdate]] %>%
                  mutate(
                    "Value" = case_when(
                      .data[["Parameter Name"]] == "Specific clearance" ~ scalingFactor * piResults$estimatedSpecifiClearance[best], # nolint : line_length_linter
                      .default = .data[["Value"]]
                    )
                  )
              } else {
                scenariosListExtrapolation[["Scenarios"]] <- scenariosListExtrapolation[["Scenarios"]] %>%
                  filter(str_detect(.data[["ModelParameterSheets"]], sheetToAdd, negate = TRUE))
              }
            }
          }
        } else {
          scenariosListExtrapolation[["Scenarios"]] <- scenariosListExtrapolation[["Scenarios"]] %>%
            filter(str_detect(.data[["Scenario_name"]], paste(drug, ".*", specie, sep = ""), negate = TRUE))
        }
      }
    }

    # overwrite ModelParameter sheet with estimated values
    write_xlsx(
      x = modelParams,
      path = file.path(projectConfiguration$outputFolder, "Parameters", "ModelParametersExtrapolation.xlsx")
    )
    write_xlsx(
      x = scenariosListExtrapolation,
      path = file.path(projectConfiguration$outputFolder, "Parameters", "ScenariosExtrapolation.xlsx")
    )
  }
}

## Creation of the Scenarios.xlsx file ##
#' @import dplyr
#' @importFrom stringr str_split_i str_detect
constructScenario <- function(drug, shortDrug, specieFrom, specieTo, extrapolation, piResultsFolder = NULL) {
  if (!is.null(piResultsFolder)) {
    resultFilePath <- file.path(
      piResultsFolder,
      paste0(paste(specieFrom, drug, sep = "_"), ".csv")
    )
    if (file.exists(resultFilePath)) {
      piResults <- read.csv(file = resultFilePath)
      partitionCoeff <- piResults$PartitionCoef[which.min(piResults$objectiveAfter)]
    } else {
      return(data.frame())
    }
  }

  if (isTRUE(extrapolation == "Fitted")) {
    file_dir <- file.path(piResultsFolder, "Simulations")

    # Read the list of filenames from the specified directory
    filenames <- list.files(path = file_dir, pattern = "\\.pkml$", full.names = FALSE) %>%
      as_tibble() %>%
      rename("filename" = "value")
    modelFiles <- filenames %>%
      filter(str_detect(.data[["filename"]], as.character(drug))) %>%
      filter(str_detect(.data[["filename"]], as.character(specieTo))) %>%
      filter(str_detect(.data[["filename"]], partitionCoeff)) %>%
      mutate(
        admin = gsub(
          "perkg.*",
          "/kg",
          paste(
            str_split_i(.data[["filename"]], pattern = "_", i = 4),
            str_split_i(.data[["filename"]], pattern = "_", i = 5),
            sep = "_"
          )
        )
      )

    if (length(modelFiles) != 0) {
      scenarios <- modelFiles %>%
        transmute(Scenario_name = paste(specieFrom, "to", specieTo, drug, admin, extrapolation, sep = "_"),
                  IndividualId = "",
                  PopulationId = "",
                  ReadPopulationFromCSV = FALSE,
                  ModelParameterSheets = "",
                  ApplicationProtocol = "",
                  SimulationTime = "",
                  SimulationTimeUnit = "",
                  SteadyState = "",
                  SteadyStateTime = "",
                  SteadyStateTimeUnit = "",
                  ModelFile = .data[["filename"]],
                  OutputPathsIds = "")
    } else {
      scenarios <- data.frame()
    }
  } else {
    # Àlways use specie from for global
    modelParamsSheet <- paste(shortDrug, specieFrom, "global", sep = "_")

    # Use correct Fu sheet
    if (str_detect(extrapolation, "FU")) {
      modelParamsSheet <- c(modelParamsSheet, paste(shortDrug, specieTo, "FU", sep = "_"))
    } else {
      modelParamsSheet <- c(modelParamsSheet, paste(shortDrug, specieFrom, "FU", sep = "_"))
    }

    # Use correct KP sheet
    if (str_detect(extrapolation, "KP")) {
      modelParamsSheet <- c(modelParamsSheet, paste(shortDrug, specieFrom, "To", specieTo, "KP", sep = "_"))
    } else {
      modelParamsSheet <- c(modelParamsSheet, paste(shortDrug, specieFrom, "KP", sep = "_"))
    }

    modelParamsSheet <- paste(modelParamsSheet, collapse = ",")

    # choose sim based on PI results, default use PKSim Part coeff
    # If we have "SP" in the Extrapolation, then the individualID is the Species_to,
    # otherwise it is the Species_From
    if (str_detect(extrapolation, "SP")) {
      IndividualId <- specieTo
    } else {
      IndividualId <- specieFrom
    }

    modelFiles <- paste0(
      "AllSpecies/Sim_Compound_PC",
      partitionCoeff,
      "_CPPKSimStandard_", IndividualId, ".pkml"
    )

    scenarios <- data.frame(
      Scenario_name = paste(drug, specieFrom, "To", specieTo, extrapolation, sep = "_"),
      IndividualId = "",
      PopulationId = "",
      ReadPopulationFromCSV = FALSE,
      ModelParameterSheets = modelParamsSheet,
      ApplicationProtocol = "",
      SimulationTime = "",
      SimulationTimeUnit = "",
      SteadyState = "",
      SteadyStateTime = "",
      SteadyStateTimeUnit = "",
      ModelFile = modelFiles,
      OutputPathsIds = ""
    )
  }
  return(scenarios)
}
