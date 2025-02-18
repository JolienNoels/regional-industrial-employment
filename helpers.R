
# List of 40 OECD countries -----------------------------------------------
oecd_countries <- c(
  "AUS", "AUT", "BEL", "BGR",
  "CAN", "CHE", "CHL", "COL", "CZE", "CRI",
  "DNK", "DEU", "EST", "ESP",
  "FIN", "FRA", "GRC", "GBR", "HUN",
  "ISL", "ISR", "IRL", "ITA",
  "JPN", "KOR", "LVA", "LTU", "LUX",
  "MEX", "NLD", "NZL", "NOR", "POL", "PRT",
  "SVK", "SVN", "SWE", "TUR", "ROU", "USA"
)


# tibble to identify macro regions ----------------------------------------
regions <- tibble::tribble(
  ~continent,    ~iso3,
  "Asia-Pacific",    "AUS",
  "Asia-Pacific",    "JPN",
  "Asia-Pacific",    "KOR",
  "Asia-Pacific",    "NZL",
  "North Amercia",    "CAN",
  "North Amercia",    "USA",
  "Central and South America",    "MEX",
  "Central and South America",    "CHL",
  "Central and South America",    "COL",
  "Central and South America",    "CRI",
  "Europe",    "AUT",
  "Europe",    "BEL",
  "Europe",    "BGR",
  "Europe",    "CHE",
  "Europe",    "CZE",
  "Europe",    "DEU",
  "Europe",    "DNK",
  "Europe",    "ESP",
  "Europe",    "EST",
  "Europe",    "FIN",
  "Europe",    "FRA",
  "Europe",    "GBR",
  "Europe",    "GRC",
  "Europe",    "HUN",
  "Europe",    "IRL",
  "Europe",    "ISL",
  "Europe",    "ITA",
  "Europe",    "LTU",
  "Europe",    "LUX",
  "Europe",    "LVA",
  "Europe",    "NLD",
  "Europe",    "NOR",
  "Europe",    "POL",
  "Europe",    "PRT",
  "Europe",    "ROU",
  "Europe",    "SVK",
  "Europe",    "SVN",
  "Europe",    "SWE",
  "Europe",    "TUR",
  "Other OECD",    "ISR"
)




