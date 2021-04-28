library(fivethirtyeight)
library(tidyverse)
library(rmarkdown)

render(input = "NEReport.rmd")

state <- historicCurrentStates$state

reports <- tibble(
  input = "NEReport.rmd",
  output_file = stringr::str_c("NEoutput/", state, "-comparison.html"),
  params = map(state, ~list(state = .))
)

reports %>%
  pwalk(render)