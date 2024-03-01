library(tidyverse)
library(here)
library(vroom)
library(janitor)

fix_data <- function(vctr){
  vctr|>
    str_remove_all(",")|>
    as.numeric()
}

raw <- vroom::vroom(here("data", "raw_cip_noc_attain.csv"), skip= 12, n_max = 3481) #garbage before and after data
colnames(raw)[1] <- "Field of Study" #missing column name
colnames(raw)[2] <- "Highest attainment"
raw <- raw[-1,] #garbage in first row

raw|>
  remove_empty(c("cols"))|>
  fill(`Field of Study`, .direction = "down")|>
  mutate(across(-c("Field of Study", "Highest attainment"), fix_data))|>
  unite("Education", "Field of Study", "Highest attainment", sep=": ")|>
  pivot_longer(cols=-Education)|>
  mutate(NOC=paste0("#",str_sub(name, 1, 5)))|>
  select(-name)|>
#  pivot_wider(id_cols = "Education", names_from = NOC, values_from = value)|>
  write_csv(here("out","edu_noc.csv"))

read_csv(here("data","Clustering NOCs based on ONET skills.csv"))|>
  select(NOC, new_cluster)|>
  separate(NOC, into=c("NOC","NOC Description"), sep=": ")|>
  mutate(NOC=paste0("#",NOC))|>
  write_csv(here("out","skill_clusters.csv"))




#
#
# |>
#   adorn_totals()|> #going to filter out NOCs with a total count of less than 1000 below
#   column_to_rownames("Education")
#
# cip_noc$total <-  rowSums(cip_noc) #going to filter out Education with total count of less than 1000 below
#
# cip_noc <- cip_noc|>
#   rownames_to_column("Education")
#

