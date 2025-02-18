dir.create("data-raw")
dir.create("data")
dir.create("figures")

source("helpers.R")

for (i in oecd_countries) {
  
  path <- paste0(here::here("data-raw"), "/", i)
  dir.create(path)
  
}

data <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("iso3", "indicator", "values", "source"))
# writexl::write_xlsx(data, path(here("data-raw"),"data.xlsx"))