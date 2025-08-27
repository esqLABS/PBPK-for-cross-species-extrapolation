## to install all packages (run once)
renv::restore()

## Load all libraries in main script
library(esqlabsR)
library(ospsuite.parameteridentification)
library(ggplot2)
library(dplyr)
library(tidyr)
library(writexl)
library(stringr)


# Init PKSim
ospsuite::initPKSim(pksimFolderPath = "PKSim")

# Define the project configuration
projectConfiguration <- createDefaultProjectConfiguration(
  path = "ProjectConfiguration.xlsx"
)

runTime <- format(Sys.time(), "Run-%Y%m%d-%H%M")

# Set output folder for PI and create
piOutputFolder <- file.path(
  projectConfiguration$outputFolder,
  runTime
)

if (!dir.exists(piOutputFolder)) {
  dir.create(file.path(piOutputFolder, "PIResults", "Plots"), recursive = TRUE)
  dir.create(file.path(piOutputFolder, "PIResults", "Simulations"), recursive = TRUE)
  dir.create(file.path(piOutputFolder, "Parameters"), recursive = TRUE)
}

projectConfiguration$paramsFolder <- file.path(piOutputFolder, "Parameters")
projectConfiguration$outputFolder <- piOutputFolder

# Set drugs and partition coefficient to do
species.vec <- c("Mouse", "Rat", "Rabbit")
drugs.vec <- c(
  "Para" = "Paracetamol",
  "Caf" = "Caffeine",
  "Theo" = "Theophylline",
  "Acv" = "Acyclovir",
  "Olti" = "Oltipraz",
  "Dasa" = "Dasatinib",
  "Zev" = "Zevaquenabant",
  "Dpt" = "Deoxypodophyllotoxin",
  "Ulo" = "Ulotaront"
)
if (is.null(names(drugs.vec))) {
  names(drugs.vec) <- drugs.vec
}

renallyCleared <- "Acyclovir"
partitionCoeff.vec <- c("PKSimStandard", "RR", "Schmitt", "PT", "Berezhkovskiy")
cellularPerm <- "PKSimStandard"

# Define dataset to use
datasetPath <- file.path("Data", "Rodent_PBPK_TimeValuesData.xlsx")
compoundPath <- file.path("Data", "CompoundData.xlsx")

# Create model parameterSheet for drugs
source("Code/CreateModelParameterSheet.R")
createModelParameterSheet(
  path = compoundPath,
  drugs = drugs.vec,
  species = species.vec,
  projectConfiguration = projectConfiguration
)

# load invivo data
importerConfiguration <- loadDataImporterConfiguration(
  configurationFilePath = projectConfiguration$dataImporterConfigurationFile
)

inVivoData <- loadDataSetsFromExcel(
  xlsFilePath = datasetPath,
  importerConfigurationOrPath = importerConfiguration,
  importAllSheets = TRUE
)

# copy in vivo data to results folder
fs::file_copy(
  path = datasetPath,
  new_path = file.path(projectConfiguration$outputFolder, "Data")
)

# Create scenario for complete models
source("Code/CreatePIScenarios.R")
createPIScenarios(
  drugs = drugs.vec,
  species = species.vec,
  partitionCoeff = partitionCoeff.vec,
  cellularPerm = cellularPerm,
  outputFolder = piOutputFolder
)

# Define the project configuration
source("Code/RunPICompleteModel.R")
runPICompleteModel(
  drugs = drugs.vec,
  species = species.vec,
  inVivoData = inVivoData,
  projectConfiguration = projectConfiguration,
  renallyCleared = renallyCleared,
  savePlots = c("pdf", "png")
)

# Create scenarios for extrapolation
source("Code/CreateExtrapolationScenarios.R")
createExtrapolationScenarios(
  projectConfiguration = projectConfiguration,
  drugs = drugs.vec,
  species = species.vec,
  compoundDataPath = compoundPath
)

# Run extrapolations
source("Code/RunExtrapolations.R")
runExtrapolations(
  projectConfiguration = projectConfiguration,
  inVivoData = inVivoData,
  saveSimulations = TRUE
)

# Create Figures based on metrics
source("Code/GenerationFiguresMetrics.R")

# Generate Report
quarto::quarto_render(
  input = "Code/ReportGeneration.qmd",
  execute_params = list(
    "folder" = projectConfiguration$outputFolder,
    "datasetPath" = datasetPath,
    "compounds_vec" = drugs.vec,
    "renallyCleared" = renallyCleared
  ),
  output_file = paste0("Report_", runTime, ".html")
)

# Need move quarto report to right location (using relative path for images)
file.rename(
  from = paste0("Report_", runTime, ".html"),
  to = file.path(projectConfiguration$outputFolder, paste0("Report_", runTime, ".html"))
)
