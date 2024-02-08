df <- tidycensus::get_acs(
    geography = "tract",
    state = "MA",
    variables = c("B25003_001", 	
                  "B25003_002",
                  "B25003_003"),
    year = 2021,
    output = "wide"
  ) |>
  dplyr::mutate(
    county = stringr::str_extract(NAME, "(?<=, )[A-Za-z ]+(?= County)"),
    pct_rent_occ = B25003_003E / B25003_001E * 100,
    pct_own_occ = B25003_002E / B25003_001E * 100
  ) |>
  dplyr::select(GEOID, NAME, county, pct_rent_occ, pct_own_occ) |>
  tidyr::drop_na() |>
  readr::write_csv("l02_demo.csv")

df |>
  dplyr::group_by(county) |>
  dplyr::summarize(
    tracts = dplyr::n()
  ) |>
  dplyr::arrange(
    desc(tracts)
  ) |>
  readr::write_csv("l02_demo_grouped.csv")