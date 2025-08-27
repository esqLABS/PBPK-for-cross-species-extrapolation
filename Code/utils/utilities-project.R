#' function to create OutputPath from Organ and Compartment (found in dataset
#' xls files)
#' Not vectorised only works for a single organ and compartment
#' @param organ Name of the organ component in simulation
#' @param compartment Name of the compartment with the organ in the simulation
#' @param compoudName Name of the compound in the simulation (default to
#' "Compound")
#' @return A string corresponding to the output path.
#' @examples
#' createOutputPath("ArterialBlood", "Plasma")
#' createOutputPath("PeripheralVenousBlood", "Plasma")
createOutputPath <- function(organ, compartment, compoundName = "Compound") {
  if (organ == "PeripheralVenousBlood") {
    outputPath <- paste("Organism", organ, compoundName, compartment,
                        sep = "|")
    if (compartment %in% c("Plasma", "Plasma Unbound", "Whole Blood")) {
      outputPath <- paste(outputPath, "(Peripheral Venous Blood)",
                          sep = " ")
    }
  } else {
    if (compartment == "Plasma Unbound") {
      outputPath <- paste("Organism", organ, "Plasma", compoundName,
                          compartment, sep = "|")
    } else {
      outputPath <- paste("Organism", organ, compartment, compoundName,
                          "Concentration in container", sep = "|")
    }
  }
  return(outputPath)
}

#' function to get and set all administration parameters based on Route and Dose
#' (found in dataset xls files)
#' Not vectorised only works for a single routeStr and doseStr
#' @param doseStr String corresponding to the Dose column in the dataset.
#' Must be in the form of "dose doseUnit", e.g. "1 mg/kg" or "10 mg".
#' @param routeStr String correspondinf to the Route column in the dataset.
#' Must be either "IVBolus" or for IV infusion in the form of
#' "IVInfusion duration durationUnit", e.g. "IVInfusion 10 min"
#' @param sim simulation object to which you want to apply the admin
#' @return Nothing but simulation values are set to the correct admin
#' @importFrom stringr str_split
setAdministration <- function(doseStr, routeStr, sim) {
  dose <- as.numeric(stringr::str_split(doseStr, " ", simplify = TRUE)[1])
  doseUnit <- stringr::str_split(doseStr, " ", simplify = TRUE)[2]
  routeStr <- stringr::str_split(string = routeStr, pattern = " ", simplify = TRUE)

  doseType <- routeStr[1]
  # if bolus set infusion duration to 1 min, otherwise use what is given in
  # routeStr
  doseInfusionDuration <- switch(
    doseType,
    IVBolus = 1,
    IVInfusion = as.numeric(routeStr[2])
  )
  doseInfusionDurationUnit <- switch(
    doseType,
    IVBolus = "min",
    IVInfusion = routeStr[3]
  )
  if (is.null(doseInfusionDuration)) {
    stop("Wrong Route supplyed. Should be on of 'IVBolus' or ",
         "'IVInfusion'. You supplied '", doseType, "'.")
  }
  # convert dose to DosePerBodyWeight if needed
  if (doseUnit %in% c(ospUnits$Mass, ospUnits$Amount)) {
    dose <- dose / getParameter("Organism|Weight", sim)$value
    doseUnit <- paste(doseUnit, getParameter("Organism|Weight", sim)$unit,
                      sep = "/")
  }

  # convert dose given in amount (mol/kg) to mass (mg/kg)
  if (doseUnit %in% paste0(c("", "m", "u", "n", "p"), "mol/kg")) {
    dose <- toUnit(
      quantityOrDimension = "Amount",
      value = dose,
      sourceUnit = stringr::str_split(doseUnit, fixed("/"), simplify = TRUE)[1],
      targetUnit = "mg",
      molWeight = getParameter("Compound|Molecular weight", sim)$value
    )
    doseUnit <- "mg/kg"
  }

  # Check that dose unit is compatible
  if (!(doseUnit %in% c(ospUnits$`Dose per body weight`))) {
    stop("Incompatible dose unit supplied.")
  }

  # convert to base unit
  dose <- toBaseUnit(
    quantityOrDimension = ospDimensions$`Dose per body weight`,
    values = dose,
    unit = doseUnit
  )
  doseInfusionDuration <- toBaseUnit(
    quantityOrDimension = ospDimensions$Time,
    values = doseInfusionDuration,
    unit = doseInfusionDurationUnit
  )

  # set values to sim
  setParameterValuesByPath(
    parameterPaths = c(
      "Applications|Admin|Application_1|ProtocolSchemaItem|DosePerBodyWeight",
      "Applications|Admin|Application_2|ProtocolSchemaItem|DosePerBodyWeight",
      "Applications|Admin|Application_2|ProtocolSchemaItem|Infusion time"
    ),
    values = c(
      ifelse(doseType == "IVBolus", dose, 0),
      ifelse(doseType == "IVInfusion", dose, 0),
      doseInfusionDuration
    ),
    simulation = sim
  )
  return()
}
