#######################################################################
#                     loop script
#######################################################################

source("2- Charting Script.r")
c=read_csv("totalJobs_v14.csv")%>%
  filter(year==2013)%>%
  select(countyfips)

for (i in c){
  assign(jobsForecast(i), x)
  ggsave(paste0("chart",i,".png"), x)
}