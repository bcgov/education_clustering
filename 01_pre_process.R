library(tidyverse)
library(here)
library(vroom)
library(janitor)
library(corrr)

#constants
margin_greater_than <- 1000 #only keep rows and columns that sum to more than this

fix_data <- function(vctr){
  vctr|>
    str_remove_all(",")|>
    as.numeric()
}

raw <- vroom::vroom(here("data", "raw_cip_noc_attain.csv"), skip= 12, n_max = 3481) #garbage before and after data
colnames(raw)[1] <- "Field of Study" #missing column name
colnames(raw)[2] <- "Highest attainment"
raw <- raw[-1,] #garbage in first row

edu_noc <- raw|>
  remove_empty(c("cols"))|>
  fill(`Field of Study`, .direction = "down")|>
  mutate(across(-c("Field of Study", "Highest attainment"), fix_data))|>
  unite("Education", "Field of Study", "Highest attainment", sep=": ")|>
  adorn_totals("both")

#here we do some filtering to limit the sparsity of the data---------------
keep_columns <- as.vector(edu_noc[edu_noc$Education=="Total",]>margin_greater_than)
edu_noc <- edu_noc[edu_noc$Total>margin_greater_than, keep_columns]|>
  select(-Total)|>
  filter(Education!="Total")|>
  pivot_longer(cols=-Education, names_to = "NOC", values_to = "value")

#write to disk------------------------------
write_csv(edu_noc, here("out","edu_noc.csv"))

spear_cor <- edu_noc|>
  pivot_wider(names_from = Education, values_from = value)|>
  column_to_rownames("NOC")|>
  correlate(method=c("spearman"))|>
  shave()|>
  stretch(na.rm=TRUE)|>
  rename(spearman_cor=r)

pearson_cor <- edu_noc|>
  pivot_wider(names_from = Education, values_from = value)|>
  column_to_rownames("NOC")|>
  correlate(method=c("pearson"))|>
  shave()|>
  stretch(na.rm=TRUE)|>
  rename(pearson_cor=r)

all_cor <- full_join(spear_cor, pearson_cor)|>
  separate(x, into=c("cip1","highest1"), sep=": ")|>
  separate(y, into=c("cip2","highest2"), sep=": ")|>
  arrange(desc(pearson_cor))

write_csv(all_cor, here("out", "all_cor.csv"))


# might not need stuff below--------------------
# read_csv(here("data","Clustering NOCs based on ONET skills.csv"))|>
#   select(NOC, new_cluster)|>
#   separate(NOC, into=c("NOC","NOC Description"), sep=": ")|>
#   mutate(NOC=paste0("#",NOC))|>
#   write_csv(here("out","skill_clusters.csv"))





