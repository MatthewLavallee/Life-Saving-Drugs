library(data.table)
library(here)
#devtools::install_github("jjchern/ahrf@v0.0.1")
library(ahrf)
#df<-ahrf::ahrf_state
df<-ahrf::ahrf_county
dir.create(here("trunk","raw","AHRF"),showWarnings = F)
fwrite(df,here("trunk","raw","AHRF","ahrf.txt"))
