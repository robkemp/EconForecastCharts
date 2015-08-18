library(shiny)
library(shinydashboard)
library(shinythemes)
library(readxl)
library(car)
library(dplyr)
library(readr)


c=read_excel("FIPSandRegion.xls")%>%
  rename(regionnum=PMRegion)%>%
  mutate(countyfips=as.numeric(Fips))%>%
  filter(countyfips!=1 , countyfips!=0 , countyfips!=5 ,countyfips!=13 ,countyfips!=14 ,countyfips!=31 ,countyfips!=35 ,countyfips!=59)

r=read_csv("totalJobsReg_v14.csv")%>%
  filter(year==2013)%>%
  select(regionnumber)

dashboardPage( skin="black",
  dashboardHeader(title= "SDO Forecast Review"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Region", tabName = "region", icon = icon("signal", lib = "glyphicon")),
      selectInput("Rnum","Region Number:",
                  choices=r$regionnum),
      menuItem("County", tabName = "county", icon = icon("signal", lib = "glyphicon")),
      selectInput("county","County:",
                  choices=unique(c$Name)),
      
#       menuItem("Growth Rates", tabName = "growth", icon = icon("signal", lib = "glyphicon"))
      menuItem("Population Methodology (PDF)", icon = icon("file-code-o"), 
         href = "http://www.colorado.gov/cs/Satellite?blobcol=urldata&blobheadername1=Content-Disposition&blobheadername2=Content-Type&blobheadervalue1=inline%3B+filename%3D%22Forecasts+Methodolofy.pdf%22&blobheadervalue2=application%2Fpdf&blobkey=id&blobtable=MungoBlobs&blobwhere=1251731969473&ssbinary=true"),
      menuItem("Population Methodology (PDF)", icon = icon("file-code-o"), 
         href = "http://www.colorado.gov/cs/Satellite?blobcol=urldata&blobheadername1=Content-Disposition&blobheadername2=Content-Type&blobheadervalue1=inline%3B+filename%3D%22Forecasts+Methodolofy.pdf%22&blobheadervalue2=application%2Fpdf&blobkey=id&blobtable=MungoBlobs&blobwhere=1251731969473&ssbinary=true")

    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "region",
              fluidRow(box(title="Total Job Forecast - Region",
                           plotOutput("Rplot")),
                       box(title="Total Population Forecast- Region",
                           plotOutput("RplotPop")))
      )),
    tabItems(
      tabItem(tabName = "county",
              fluidRow(box(title="Total Job Forecast",
                           plotOutput("plot")),
                       box(title="Total Population Forecast",
                           plotOutput("plotPop")))
      )
    )
#     tabItems(
#       tabItem(tabName = "growth",
#               fluidRow(tabBox(title="5-Year Growth Rates",
#                               tabPanel("County", ),
#                               tabPanel("Region", )
#                            ))
#       )
#     )
  )
)

