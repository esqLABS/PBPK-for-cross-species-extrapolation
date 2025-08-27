#' @import dplyr
createPIScenarios <- function(drugs, species, partitionCoeff, cellularPerm, outputFolder) {
  scenarioCombination <- expand.grid(drugs, species, partitionCoeff, cellularPerm)
  colnames(scenarioCombination) <- c(
    "Drug", "Specie", "PartitionCoefficient", "CellularPermeability"
  )
  scenarioCombination$DrugShortName <- names(drugs)[match(scenarioCombination$Drug, drugs)]

  scenariosDF <- scenarioCombination %>%
    transmute(
      Scenario_name = paste(
        .data[["Drug"]],
        .data[["Specie"]],
        .data[["PartitionCoefficient"]],
        .data[["CellularPermeability"]],
        "Complete",
        sep = "_"
      ),
      IndividualId = "",
      PopulationId = "",
      ReadPopulationFromCSV = FALSE,
      ModelParameterSheets = paste(
        paste(.data[["DrugShortName"]], .data[["Specie"]], "global", sep = "_"),
        paste(.data[["DrugShortName"]], .data[["Specie"]], "KP", sep = "_"),
        paste(.data[["DrugShortName"]], .data[["Specie"]], "FU", sep = "_"),
        sep = ","
      ),
      ApplicationProtocol = "",
      SimulationTime = "",
      SimulationTimeUnit = "",
      SteadyState = "",
      SteadyStateTime = "",
      SteadyStateTimeUnit = "",
      ModelFile = paste0(
        "AllSpecies/Sim_Compound",
        "_PC", .data[["PartitionCoefficient"]],
        "_CP", .data[["CellularPermeability"]],
        "_", .data[["Specie"]],
        ".pkml"
      ),
      OutputPathsIds = ""
    )

  scenarioList <- NULL
  scenarioList[["Scenarios"]] <- scenariosDF
  scenarioList[["OutputPaths"]] <- data.frame()
  write_xlsx(scenarioList, file.path(outputFolder, "Parameters", "ScenariosPI.xlsx"))

  return(invisible(NULL))
}
