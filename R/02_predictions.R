source("R/00_libraries.R")


# load model --------------------------------------------------------------

model_names <- readxl::excel_sheets(
  "data/Data for modelling - 2015-Qtr 3 2022.xlsx"
)

for (population_type in model_names) {

  model <- readRDS(
    paste0("outputs/models/",
           population_type,
           ".rds")
  )



  # raw  data -----------------------------------------------------

  raw_populations <- readxl::read_excel(
    "data/Population Estimates by LGD , MDM17 quintile , age and sex - TO SEND.xlsx",
    sheet = "2001-2026",
    range = "A1:F16729"
  )

  bank_holidays <- readxl::read_excel(
    "data/Public-Bank Holidays 2015-2023.xlsx"
  )


  # data manipulation -------------------------------------------------------

  bank_holidays <- bank_holidays |>
    mutate(
      Date = gsub("Mach", "March", Date),
      bhs = lubridate::dmy(paste0(Date, Year))
    ) |>
    pull(bhs)




  prediction_weekly_pops <- raw_populations |>
    # renaming variables
    rename(
      areacode = "LGD2014",
      deprivation_quintile = "MDM17_quintile",
      age_group = "age_band",
      population = "population Estimate/projection"
    ) |>
    convert_annual_to_weekly_populations(
      from_date = as.Date("2020-03-21"),
      to_date = as.Date("2023-12-29"),
      holidays = bank_holidays
    ) |>
    tidyr::complete(
      areacode,
      deprivation_quintile,
      sex,
      age_group,
      week_ending,
      fill = list(denominator = 0)
    ) |>
    group_by(
      areacode,
      deprivation_quintile,
      sex,
      age_group
    ) |>
    filter(
      max(denominator) != 0
    ) |>
    ungroup()

  if (population_type != "All Persons") {
    prediction_weekly_pops <- prediction_weekly_pops |>
      group_by(
        areacode,
        sex,
        age_group,
        week_ending
      ) |>
      summarise(
        denominator = sum(denominator),
        .groups = "drop"
      ) |>
      mutate(
        deprivation_quintile = "All"
      )

    deprivation_quintiles <- "All"

  } else {
    deprivation_quintiles <- 1:5
  }


  # build prediction matrix -------------------------------------------------


  # build table of data to predict from
  predictors <- build_prediction_dates(
    areacode = unique(prediction_weekly_pops$areacode),
    age_group = unique(model$data$age_group),
    deprivation_quintile = deprivation_quintiles,
    from_date = as.Date("2020-03-21"),
    to_date = as.Date("2023-12-29"),
    holidays = bank_holidays,
    denominators = prediction_weekly_pops
  )

  # apply model to the table of data
  predictions <- predict(
    model,
    newdata = predictors,
    type = "response")


  # store predictions -------------------------------------------------------

  dispersion_parameter <- readLines(
    paste0(
      "outputs/dispersion parameters/",
      population_type,
      ".txt")
  ) |>
    as.numeric()

  # reduce table down to component parts and add prediction intervals
  predictors |>
    mutate(expected_deaths = predictions) |>
    select(
      week_ending, areacode, sex, age_group, deprivation_quintile, expected_deaths
    ) |>
    # add_prediction_intervals(dispersion_parameter) |>
    write.csv(
      paste0(
        "outputs/predictions/",
        population_type,
        ".csv"),
      row.names = FALSE
    )

}
