#' @import dplyr
extractInVivoDataAdmins <- function(
    inVivoData,
    fittedOrgans = c("PeripheralVenousBlood", "VenousBlood", "ArterialBlood")) {
  # get all admin first to avoid multiple identical simulations
  adminsAllDF <- do.call(
    rbind,
    lapply(
      inVivoData,
      function(l) {
        if (!is.null(l$metaData$Organ) & !is.null(l$metaData$Compartment)) {
          data.frame(
            Compound = l$metaData$Molecule,
            Specie = l$metaData$Species,
            Route = l$metaData$Route,
            Dose = l$metaData$Dose,
            Organ = gsub(" ", "", l$metaData$Organ),
            Compartment = l$metaData$Compartment,
            TMax = max(l$xValues),
            TUnit = l$xUnit
          )
        }
      }
    )
  )

  # keep only data set with blood Organ
  adminsAllDF <- adminsAllDF %>% filter(.data[["Organ"]] %in% fittedOrgans)

  # convert to tMax to min (in case not all datasets are in the same unit)
  adminsAllDF <- adminsAllDF %>%
    rowwise() %>%
    mutate(TMaxMin = ospsuite::toUnit("Time", .data[["TMax"]], "min", .data[["TUnit"]]))

  # keep overall tMax for single admin
  adminsAllDF <- adminsAllDF %>%
    group_by(
      .data[["Compound"]],
      .data[["Specie"]],
      .data[["Route"]],
      .data[["Dose"]],
      .data[["Organ"]],
      .data[["Compartment"]]
    ) %>%
    summarise(
      TMax = ifelse(length(unique(.data[["TUnit"]])) == 1, max(.data[["TMax"]]), .data[["max(TMaxMin)"]]),
      TUnit = ifelse(length(unique(.data[["TUnit"]])) == 1, .data[["TUnit"]], "min")
    )

  # group compartementa per admin and re do the tmax aggregation
  adminsAllDF <- adminsAllDF %>%
    group_by(.data[["Compound"]], .data[["Specie"]], .data[["Route"]], .data[["Dose"]]) %>%
    summarise(
      TMax = ifelse(length(unique(.data[["TUnit"]])) == 1, max(.data[["TMax"]]), max(.data[["TMaxMin"]])),
      TUnit = ifelse(length(unique(.data[["TUnit"]])) == 1, .data[["TUnit"]], "min"),
      Organ = list(.data[["Organ"]]),
      Compartment = list(.data[["Compartment"]])
    )

  return(adminsAllDF)
}
