library(fivethirtyeight)
library(tidyverse)
library(rmarkdown)

render(input = "evtEcoProtect/EcoEvtReport.Rmd")

ECO_NAME <- evt_combine$ECO_NAME

reports <- tibble(
  input = "evtEcoProtect/EcoEvtReport.Rmd",
  output_file = stringr::str_c("ouput2/", ECO_NAME, "-test1.html"),
  params = map(ECO_NAME, ~list(ECO_NAME = .))
)

reports %>%
  pwalk(render)