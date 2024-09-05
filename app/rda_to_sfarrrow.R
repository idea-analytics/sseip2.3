# Load RDAs and save as arrow or sfarrow files, for performance reasons

library(arrow)
library(sfarrow)
library(tidyverse)


# Load RDAs
working_dir <- here::here("app")
load(glue::glue("{working_dir}/sf_dot_density_mvp.rda"))
load(glue::glue("{working_dir}/sf_isochrones_idea_mvp.rda"))
load(glue::glue("{working_dir}/sf_schools_idea_mvp.rda"))
load(glue::glue("{working_dir}/sf_students_idea_mvp.rda"))

# Save as arrow files
st_write_feather(sf_dot_density_mvp, glue::glue("{working_dir}/sf_dot_density_mvp.feather"))
st_write_feather(sf_isochrones_idea_mvp, glue::glue("{working_dir}/sf_isochrones_idea_mvp.feather"))
st_write_feather(sf_schools_idea_mvp, glue::glue("{working_dir}/sf_schools_idea_mvp.feather"))
st_write_feather(sf_students_idea_mvp, glue::glue("{working_dir}/sf_students_idea_mvp.feather"))
