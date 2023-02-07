source("R/00_libraries.R")


# raw data ----------------------------------------------------------------

raw_populations <- readxl::read_excel(
  "data/Population Estimates by LGD , MDM17 quintile , age and sex - TO SEND.xlsx",
  sheet = "2001-2026",
  range = "A1:F16729"
)

sheet_names <- readxl::excel_sheets(
  "data/Data for modelling - 2015-Qtr 3 2022.xlsx"
)

for (population_type in sheet_names) {
  # population_type <- sheet_names[15]

  raw_registered_deaths <- readxl::read_excel(
    "data/Data for modelling - 2015-Qtr 3 2022.xlsx",
    sheet = population_type
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

  baseline_weekly_pops <- raw_populations |>
    # renaming variables
    rename(
      areacode = "LGD2014",
      deprivation_quintile = "MDM17_quintile",
      age_group = "age_band",
      population = "population Estimate/projection"
    ) |>
    convert_annual_to_weekly_populations(
      from_date = as.Date("2015-01-03"),
      to_date = as.Date("2019-12-27"),
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
    baseline_weekly_pops <- baseline_weekly_pops |>
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

    raw_registered_deaths <- raw_registered_deaths |>
      mutate(
        `MDM Quintile` = "All"
      )
  }

  # Fill in the weeks for the missing deaths
  baseline_complete_deaths <- raw_registered_deaths |>
    # renaming variables
    rename(
      areacode = "Local Government District",
      deprivation_quintile = "MDM Quintile",
      week_ending = "Date",
      age_group = "Age_Group",
      registered_deaths = "Number of Deaths"
    ) |>
    filter(
      between(week_ending,
              as.Date("2015-01-09"),
              as.Date("2019-12-27"))
    ) |>
    tidyr::complete(
      areacode,
      deprivation_quintile,
      sex,
      age_group = c("0-24", "25-49", "50-64", "65-74", "75-84", "85+"),
      week_ending,
      fill = list(registered_deaths = 0)
    )

  # Convert field to correct classes
  baseline_weekly_pops <- baseline_weekly_pops |>
    mutate(
      across(
        c(areacode, sex, age_group, deprivation_quintile),
        .fns = \(x) factor(x)
      ),
      week_ending = as.Date(week_ending)
    )

  baseline_complete_deaths <- baseline_complete_deaths |>
    mutate(
      across(
        c(areacode, sex, age_group, deprivation_quintile),
        .fns = \(x) factor(x)
      ),
      week_ending = as.Date(week_ending)
    )


  # Combine deaths and populations
  baseline_data <- baseline_complete_deaths |>
    right_join(
      baseline_weekly_pops,
      by = c("areacode", "deprivation_quintile", "sex", "age_group", "week_ending"))

  # Create the date dependent variables
  from_date <- as.Date("2015-01-03")
  to_date <- as.Date("2019-12-27")

  # create date-dependent variables
  date_dependent_variables <- create_date_dependent_variables(
    from_date = from_date,
    to_date = to_date,
    holidays = bank_holidays
  )

  # Add on predictor variables
  baseline_data <- baseline_data |>
    left_join(date_dependent_variables,
              by = "week_ending")

  # perform modelling -------------------------------------------------------

  cat(paste(
    "building model",
    population_type,
    "\n"))

  start_time <- Sys.time()

  if (population_type == "All Persons") {
    model <- glm("registered_deaths ~ offset(log(denominator)) +
                           sex:age_group +
                           areacode +
                           deprivation_quintile:age_group +
                           years_from_20161231:deprivation_quintile +
                           years_from_20161231:age_group +
                           years_from_20161231 +
                           month1:age_group + month2:age_group +
                           month3:age_group + month4:age_group +
                           month5:age_group + month6:age_group +
                           month7:age_group + month8:age_group +
                           month9:age_group + month10:age_group +
                           month11:age_group + month12:age_group +
                           easter_pre + easter_post_1 + easter_post_2 +
                           wk_nearest_BH + wk_next_nearest_BH +
                           wk_sat_to_mon_xmas + wk_post_sat_to_mon_xmas + wk2_post_sat_to_mon_xmas +
                           consecutive_bh + wk_after_tue_to_thu_xmas + wk2_after_tue_to_thu_xmas",
                 family = quasipoisson,
                 data = baseline_data)

  } else {
    model <- glm("registered_deaths ~ offset(log(denominator)) +
                           sex:age_group +
                           areacode +
                           years_from_20161231:age_group +
                           years_from_20161231 +
                           month1:age_group + month2:age_group +
                           month3:age_group + month4:age_group +
                           month5:age_group + month6:age_group +
                           month7:age_group + month8:age_group +
                           month9:age_group + month10:age_group +
                           month11:age_group + month12:age_group +
                           easter_pre + easter_post_1 + easter_post_2 +
                           wk_nearest_BH + wk_next_nearest_BH +
                           wk_sat_to_mon_xmas + wk_post_sat_to_mon_xmas + wk2_post_sat_to_mon_xmas +
                           consecutive_bh + wk_after_tue_to_thu_xmas + wk2_after_tue_to_thu_xmas",
                 family = quasipoisson,
                 data = baseline_data)

  }


  dispersion_parameter <- summary(model)$dispersion
  cat(
    paste(
      "Finished:",
      Sys.time() - start_time,
      "\n"
    )
  )


  # store outputs -----------------------------------------------------------

  saveRDS(
    model,
    paste0("outputs/models/",
           population_type,
           ".rds")
  )

  writeLines(
    as.character(dispersion_parameter),
    paste0("outputs/dispersion parameters/",
           population_type,
           ".txt")
  )
}

