#######################################################################
#                     loop script
#######################################################################

source("2- Charting Script.r")
c=read_csv("J:/Estimates/Admin/AppDevelopment/EconForecastCharts/totalJobs_v14.csv")%>%
  filter(year==2013)%>%
  select(countyfips)

path="J:/Estimates/Admin/AppDevelopment/EconForecastCharts/Charts"
func=function(i){

}
for (i in c){
  jobsForecast(i)
  ggsave(paste0("chart",i,".png"), path=path)
}