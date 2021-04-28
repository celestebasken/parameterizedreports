library(fivethirtyeight)
library(tidyverse)
library(rmarkdown)

render(input = "Report.rmd")

state <- bad_drivers$state

reports <- tibble(
  input = "Report.rmd",
  output_file = stringr::str_c("output/", state, "-driving.html"),
  params = map(state, ~list(state = .))
)

reports %>%
  pwalk(render)

reports %>%
  pwalk(rmarkdown::render, input = "02_rmarkdown-factsheets/demonstration.Rmd")