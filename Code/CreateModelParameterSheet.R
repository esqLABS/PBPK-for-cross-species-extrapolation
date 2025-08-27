#' @import dplyr
createModelParameterSheet <- function(path, drugs, species, projectConfiguration, copyDataToOutpuFolder = TRUE) {
  # If required copy compound data to output folder location
  if (isTRUE(copyDataToOutpuFolder)) {
    if (!dir.exists(file.path(projectConfiguration$outputFolder, "Data"))) {
      dir.create(file.path(projectConfiguration$outputFolder, "Data"))
    }
    fs::file_copy(path = path, new_path = file.path(projectConfiguration$outputFolder, "Data", "CompoundData.xlsx"))
  }

  fractionUnbound <- readxl::read_excel(path, sheet = "FractionUnbound")
  hepClearance <- readxl::read_excel(path, sheet = "HepaticClearance")
  renClearance <- readxl::read_excel(path, sheet = "GFRFraction")
  drugParameters <- readxl::read_excel(path, sheet = "CompoundParam")

  # make sure pKaType are characters
  drugParameters <- drugParameters %>%
    mutate(
      across(.cols = starts_with("pKaType"), .fns = as.character)
    )

  # Convert: Acidic -1, Base +1, Neutral 0
  drugParameters <- drugParameters %>%
    mutate(
      pKa1 = case_match(.data[["pKa1"]], NA ~ 0, .default = .data[["pKa1"]]),
      pKa2 = case_match(.data[["pKa2"]], NA ~ 0, .default = .data[["pKa2"]]),
      pKa3 = case_match(.data[["pKa3"]], NA ~ 0, .default = .data[["pKa3"]]),
      pKaType1 = case_match(.data[["pKaType1"]], "Acidic" ~ -1, "Basic" ~ 1, .default = 0),
      pKaType2 = case_match(.data[["pKaType2"]], "Acidic" ~ -1, "Basic" ~ 1, .default = 0),
      pKaType3 = case_match(.data[["pKaType3"]], "Acidic" ~ -1, "Basic" ~ 1, .default = 0)
    )


  # list param paths and matching columns
  mapping <- data.frame(
    path = c(
      "Compound|Molecular weight",
      "Compound|Lipophilicity",
      "Compound-Total Hepatic Clearance-InVitroTotalHepaticClearance|Lipophilicity (experiment)",
      "Compound|Solubility at reference pH",
      paste(rep("Compound", 6), paste(c("pKa value", "Compound type"), rep(0:2, each = 2)), sep = "|"),
      paste0("Compound|",c("F","Cl", "I", "Br"))
    ),
    colnames = c(
      "MolecularWeight",
      rep("Lipophilicity", 2),
      "Solubility",
      paste0(c("pKa", "pKaType"), rep(1:3, each = 2)),
      c("F","Cl", "I", "Br")
    )
  )

  mapping <- mapping %>%
    mutate(
      container = stringr::str_split(pattern = stringr::fixed("|"), path, simplify = TRUE)[, 1],
      par = stringr::str_split(pattern = stringr::fixed("|"), path, simplify = TRUE)[, 2],
      .before = "colnames"
    )

  ## Creation of the ModelParameter file ##
  modelParameterSheetList <- list()

  for (drugIdx in seq_along(drugs)) {
    drug <- drugs[drugIdx]
    drugShort <- names(drugs)[drugIdx]
    for (specie in species) {
      # create global sheet
      modelParameterSheetDF <- data.frame(
        "Container Path" = mapping$container,
        "Parameter Name" = mapping$par,
        "Value" = t(drugParameters[drugParameters[["CompoundId"]] == drug, mapping$colnames]),
        "Units" = c(
          t(drugParameters[drugParameters[["CompoundId"]] == drug, paste(mapping$colnames[1:4], "Units")]),
          rep("", 10)
        ),
        check.names = FALSE
      )

      modelParameterSheetList[[paste(drugShort, specie, "global", sep = "_")]] <- modelParameterSheetDF

      # create KP sheet
      modelParameterSheetDF <- data.frame(
        "Container Path" = c(
          "Compound-Total Hepatic Clearance-InVitroTotalHepaticClearance",
          "Neighborhoods|Kidney_pls_Kidney_ur|Compound|Glomerular Filtration-RenalClearanceGFR"
        ),
        "Parameter Name" = c("Plasma clearance", "GFR fraction"),
        "Value" = c(
          hepClearance %>% filter(.data[["CompoundId"]] == drug) %>% pull(specie),
          renClearance %>% filter(.data[["CompoundId"]] == drug) %>% pull(specie)
        ),
        "Units" = c(
          hepClearance %>% filter(.data[["CompoundId"]] == drug) %>% pull("Units"),
          renClearance %>% filter(.data[["CompoundId"]] == drug) %>% pull("Units")
        ),
        check.names = FALSE
      )
      modelParameterSheetList[[paste(drugShort, specie, "KP", sep = "_")]] <- modelParameterSheetDF

      # create FU sheet
      modelParameterSheetDF <- data.frame(
        "Container Path" = c("Compound", "Compound-Total Hepatic Clearance-InVitroTotalHepaticClearance"),
        "Parameter Name" = c("Fraction unbound (plasma, reference value)", "Fraction unbound (experiment)"),
        "Value" = rep(fractionUnbound %>% filter(.data[["CompoundId"]] == drug) %>% pull(specie), 2),
        "Units" = rep(fractionUnbound %>% filter(.data[["CompoundId"]] == drug) %>% pull("Units"), 2),
        check.names = FALSE
      )
      modelParameterSheetList[[paste(drugShort, specie, "FU", sep = "_")]] <- modelParameterSheetDF
    }
  }

  writexl::write_xlsx(
    x = modelParameterSheetList,
    path = file.path(projectConfiguration$outputFolder, "Parameters", "ModelParameters.xlsx")
  )

  return(invisible(NULL))
}
