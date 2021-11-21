#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Import libraries
library(shiny)
library(xts)
library(ggplot2)
library(forecast)
library(TTR)
library(dplyr)
library(stringr)

#Set Johns Hopkins github data urls
COVIDRecovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
COVIDDeaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
COVIDConfirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
COVIDUSConfirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
COVIDUSDeaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"

#Import raw data into data tables
Confirmed <- read.csv(url(COVIDConfirmed),check.names=FALSE)
Deaths <- read.csv(url(COVIDDeaths),check.names=FALSE)
Recovered <- read.csv(url(COVIDRecovered),check.names=FALSE)
USConfirmed <- read.csv(url(COVIDUSConfirmed),check.names=FALSE)
USDeaths <- read.csv(url(COVIDUSDeaths),check.names=FALSE)

CBSA <- read.csv("CBSA_2020.csv")

CBSA$FIPS <- as.numeric(paste0(str_pad(CBSA$FIPS.State.Code,2,pad="0"),
                               str_pad(CBSA$FIPS.County.Code, 3, pad = "0")))

CBSAConf.df <- aggregate(. ~ CBSA.Title, merge(CBSA,USConfirmed), sum)[,-c(2:18)]
CBSADeath.df <- aggregate(. ~ CBSA.Title, merge(CBSA,USDeaths), sum)[,-c(2:19)]

CBSAcovidplot <- function(CBSA,plconf = TRUE,pldeath=TRUE){
    CBSA_conf <- t(CBSAConf.df)
    CBSA_death <- t(CBSADeath.df)
    colnames(CBSA_death) <- CBSA_death[1,]
    colnames(CBSA_conf) <- CBSA_conf[1,]
    CBSA_conf_df <- as.data.frame(CBSA_conf[-1,])
    CBSA_conf_df$Date <- as.Date(rownames(CBSA_conf_df), format = "%m/%e/%y")
    CBSA_conf_df <- CBSA_conf_df[,c(ncol(CBSA_conf_df),1:(ncol(CBSA_conf_df)-1))]
    CBSA_conf_df[,-1] <- lapply(CBSA_conf_df[,-1], function(x) {as.numeric(as.character(x))})
    CBSA_conf_xts <- xts(CBSA_conf_df[,-1], order.by=CBSA_conf_df[,1])
    CBSA_conf_diff <- diff(CBSA_conf_xts)
    CBSA_death_df <- as.data.frame(CBSA_death[-1,])
    CBSA_death_df$Date <- as.Date(rownames(CBSA_death_df), format = "%m/%e/%y")
    CBSA_death_df <- CBSA_death_df[,c(ncol(CBSA_death_df),1:(ncol(CBSA_death_df)-1))]
    CBSA_death_df[,-1] <- lapply(CBSA_death_df[,-1], function(x) {as.numeric(as.character(x))})
    CBSA_death_xts <- xts(CBSA_death_df[,-1], order.by=CBSA_death_df[,1])
    CBSA_death_diff <- diff(CBSA_death_xts)
    i = which(colnames(CBSA_death_diff)==CBSA)
    c = CBSA_conf_diff[,i]
    d = CBSA_death_diff[,i]
    cn = colnames(c)
    dt = index(tail(CBSA_conf_diff,1))
    if (plconf == FALSE) {
        p = ggplot() +
            geom_line(data = CBSA_death_diff, aes(x=index(CBSA_death_diff),y=d, color = "Deaths"), na.rm=TRUE) +
            geom_line(data = CBSA_death_diff, aes(x=index(CBSA_death_diff),y=SMA(d,14), color = "Deaths - 14 Day MA"), na.rm=TRUE) +
            theme(legend.title=element_blank()) +
            labs(title=paste0("COVID-19 Metropolitan Areas\n",cn,"\nDeath Daily Change - \n",dt),
                 x ="Date", y = "Change", subtitle="Source: https://github.com/CSSEGISandData/COVID-19")
        print(p)
    } else if (pldeath == FALSE) {
        p = ggplot() + geom_line(data = CBSA_conf_diff, aes(x=index(CBSA_conf_diff),y=c, color = "Confirmed"), na.rm=TRUE) +
            geom_line(data = CBSA_conf_diff, aes(x=index(CBSA_conf_diff),y=SMA(c,14), color = "Confirmed - 14 Day MA"), na.rm=TRUE) +
            theme(legend.title=element_blank()) +
            labs(title=paste0("COVID-19 Metropolitan Areas\n",cn,"\nConfirmed Cases Daily Change - \n",dt),
                 x ="Date", y = "Change", subtitle="Source: https://github.com/CSSEGISandData/COVID-19")
        print(p)
    } else {
        p = ggplot() + geom_line(data = CBSA_conf_diff, aes(x=index(CBSA_conf_diff),y=c, color = "Confirmed"), na.rm=TRUE) +
            geom_line(data = CBSA_conf_diff, aes(x=index(CBSA_conf_diff),y=SMA(c,14), color = "Confirmed - 14 Day MA"), na.rm=TRUE) +
            geom_line(data = CBSA_death_diff, aes(x=index(CBSA_death_diff),y=d, color = "Deaths"), na.rm=TRUE) +
            geom_line(data = CBSA_death_diff, aes(x=index(CBSA_death_diff),y=SMA(d,14), color = "Deaths - 14 Day MA"), na.rm=TRUE) +
            theme(legend.title=element_blank()) +
            labs(title=paste0("COVID-19 Metropolitan Areas\n",cn,"\nConfirmed and Death Daily Change - \n",dt),
                 x ="Date", y = "Change", subtitle="Source: https://github.com/CSSEGISandData/COVID-19")
        print(p)
    }                
}

statecovid <- function(state){
    d = USDeaths[USDeaths$Province_State == state,]
    c = USConfirmed[USConfirmed$Province_State == state,]
    d.t = t(d[-c(1,2,3,4,5,6,7,8,9,10,11)])
    c.t = t(c[-c(1,2,3,4,5,6,7,8,9,10,11)])
    colnames(d.t) = d$Admin2
    colnames(c.t) = c$Admin2
    d.df = as.data.frame(d.t[-1,])
    d.df$Date = as.Date(rownames(d.df), format = "%m/%e/%y")
    d.df = d.df[,c(ncol(d.df),1:(ncol(d.df)-1))]
    c.df = as.data.frame(c.t[-1,])
    c.df$Date = as.Date(rownames(c.df), format = "%m/%e/%y")
    c.df = c.df[,c(ncol(c.df),1:(ncol(c.df)-1))]
    d.xts = xts(d.df[,-1], order.by=d.df[,1])
    d.xts = window(d.xts, start = "2020-03-10", end = Sys.Date())
    c.xts = xts(c.df[,-1], order.by=c.df[,1])
    c.xts = window(c.xts, start = "2020-03-10", end = Sys.Date())
    d.diff = diff(d.xts)
    c.diff = diff(c.xts)
    covidlist = list(d.xts,d.diff,c.xts,c.diff)
    names(covidlist) = c("Deaths.xts","Deaths.diff","Confirmed.xts","Confirmed.diff")
    return(covidlist)
}

uscovidplot <- function(state,plconf = TRUE,pldeath=TRUE){
    USDeathAgg = t(aggregate(. ~ Province_State, USDeaths, sum)[-c(2,3,4,5,6,7,8,9,10,11,12)])
    USConfAgg = t(aggregate(. ~ Province_State, USConfirmed, sum)[-c(2,3,4,5,6,7,8,9,10,11)])
    USConfAgg_df = as.data.frame(USConfAgg)[-1,]
    colnames(USConfAgg_df) = USConfAgg[1,]
    USConfAgg_df$Date = as.Date(rownames(USConfAgg_df), format = "%m/%e/%y")
    USConfAgg_df = USConfAgg_df[,c(ncol(USConfAgg_df),1:(ncol(USConfAgg_df)-1))]
    USConfAgg_df[,-1] = lapply(USConfAgg_df[,-1], function(x) {as.numeric(as.character(x))})
    USConfAgg_xts = xts(USConfAgg_df[,-1], order.by=USConfAgg_df[,1])
    USDeathAgg_df = as.data.frame(USDeathAgg)[-1,]
    colnames(USDeathAgg_df) = USDeathAgg[1,]
    USDeathAgg_df$Date = as.Date(rownames(USDeathAgg_df), format = "%m/%e/%y")
    USDeathAgg_df = USDeathAgg_df[,c(ncol(USDeathAgg_df),1:(ncol(USDeathAgg_df)-1))]
    USDeathAgg_df[,-1] = lapply(USDeathAgg_df[,-1], function(x) {as.numeric(as.character(x))})
    USDeathAgg_xts = xts(USDeathAgg_df[,-1], order.by=USDeathAgg_df[,1])
    USDeaths_diff = diff(USDeathAgg_xts)
    USConf_diff = diff(USConfAgg_xts)
    i = which(colnames(USConf_diff)==state)
    c = USConf_diff[,i]
    d = USDeaths_diff[,i]
    cn = colnames(c)
    dt = index(tail(USDeaths_diff,1))
    if (plconf == FALSE) {
        p = ggplot() +
            geom_line(data = USDeaths_diff, aes(x=index(USDeaths_diff),y=d, color = "Deaths"), na.rm=TRUE) +
            geom_line(data = USDeaths_diff, aes(x=index(USDeaths_diff),y=SMA(d,14), color = "Deaths - 14 Day MA"), na.rm=TRUE) +
            theme(legend.title=element_blank()) +
            labs(title=paste0("COVID-19 US \n",cn," Death Daily Change - \n",dt),
                 x ="Date", y = "Change", subtitle="Source: https://github.com/CSSEGISandData/COVID-19")
        print(p)
    } else if (pldeath == FALSE) {
        p = ggplot() + geom_line(data = USConf_diff, aes(x=index(USConf_diff),y=c, color = "Confirmed"), na.rm=TRUE) +
            geom_line(data = USConf_diff, aes(x=index(USConf_diff),y=SMA(c,14), color = "Confirmed - 14 Day MA"), na.rm=TRUE) +
            theme(legend.title=element_blank()) +
            labs(title=paste0("COVID-19 US \n",cn," Confirmed Cases Daily Change - \n",dt),
                 x ="Date", y = "Change", subtitle="Source: https://github.com/CSSEGISandData/COVID-19")
        print(p)
    } else {
        p = ggplot() + geom_line(data = USConf_diff, aes(x=index(USConf_diff),y=c, color = "Confirmed"), na.rm=TRUE) +
            geom_line(data = USConf_diff, aes(x=index(USConf_diff),y=SMA(c,14), color = "Confirmed - 14 Day MA"), na.rm=TRUE) +
            geom_line(data = USDeaths_diff, aes(x=index(USDeaths_diff),y=d, color = "Deaths"), na.rm=TRUE) +
            geom_line(data = USDeaths_diff, aes(x=index(USDeaths_diff),y=SMA(d,14), color = "Deaths - 14 Day MA"), na.rm=TRUE) +
            theme(legend.title=element_blank()) +
            labs(title=paste0("COVID-19 US \n",cn," Confirmed Cases and Death Daily Change - \n",dt),
                 x ="Date", y = "Change", subtitle="Source: https://github.com/CSSEGISandData/COVID-19")
        print(p)
    }                          
}

nationcovidplot <- function(country,plconf = TRUE,pldeath=TRUE){
    colnames(Confirmed)[2] <- c("Country")
    colnames(Deaths)[2] <- c("Country")
    World_conf <- t(aggregate(. ~ Country, Confirmed[-c(1,3,4)], sum))
    World_death <- t(aggregate(. ~ Country, Deaths[-c(1,3,4)], sum))
    colnames(World_death) <- World_death[1,]
    colnames(World_conf) <- World_conf[1,]
    World_conf_df <- as.data.frame(World_conf[-1,])
    World_conf_df$Date <- as.Date(rownames(World_conf_df), format = "%m/%e/%y")
    World_conf_df <- World_conf_df[,c(ncol(World_conf_df),1:(ncol(World_conf_df)-1))]
    World_conf_df[,-1] <- lapply(World_conf_df[,-1], function(x) {as.numeric(as.character(x))})
    World_conf_xts <- xts(World_conf_df[,-1], order.by=World_conf_df[,1])
    World_conf_diff <- diff(World_conf_xts)
    World_death_df <- as.data.frame(World_death[-1,])
    World_death_df$Date <- as.Date(rownames(World_death_df), format = "%m/%e/%y")
    World_death_df <- World_death_df[,c(ncol(World_death_df),1:(ncol(World_death_df)-1))]
    World_death_df[,-1] <- lapply(World_death_df[,-1], function(x) {as.numeric(as.character(x))})
    World_death_xts <- xts(World_death_df[,-1], order.by=World_death_df[,1])
    World_death_diff <- diff(World_death_xts)
    i = which(colnames(World_conf_diff)==country)
    c = World_conf_diff[,i]
    d = World_death_diff[,i]
    cn = colnames(c)
    dt = index(tail(World_conf_diff,1))
    if (plconf == FALSE) {
        p = ggplot() +
            geom_line(data = World_death_diff, aes(x=index(World_death_diff),y=d, color = "Deaths"), na.rm=TRUE) +
            geom_line(data = World_death_diff, aes(x=index(World_death_diff),y=SMA(d,14), color = "Deaths - 14 Day MA"), na.rm=TRUE) +
            theme(legend.title=element_blank()) +
            labs(title=paste0("COVID-19 World \n",cn," Death Daily Change - \n",dt),
                 x ="Date", y = "Change", subtitle="Source: https://github.com/CSSEGISandData/COVID-19")
        print(p)
    } else if (pldeath == FALSE) {
        p = ggplot() + geom_line(data = World_conf_diff, aes(x=index(World_conf_diff),y=c, color = "Confirmed"), na.rm=TRUE) +
            geom_line(data = World_conf_diff, aes(x=index(World_conf_diff),y=SMA(c,14), color = "Confirmed - 14 Day MA"), na.rm=TRUE) +
            theme(legend.title=element_blank()) +
            labs(title=paste0("COVID-19 World \n",cn," Confirmed Cases Daily Change - \n",dt),
                 x ="Date", y = "Change", subtitle="Source: https://github.com/CSSEGISandData/COVID-19")
        print(p)
    } else {
        p = ggplot() + geom_line(data = World_conf_diff, aes(x=index(World_conf_diff),y=c, color = "Confirmed"), na.rm=TRUE) +
            geom_line(data = World_conf_diff, aes(x=index(World_conf_diff),y=SMA(c,14), color = "Confirmed - 14 Day MA"), na.rm=TRUE) +
            geom_line(data = World_death_diff, aes(x=index(World_death_diff),y=d, color = "Deaths"), na.rm=TRUE) +
            geom_line(data = World_death_diff, aes(x=index(World_death_diff),y=SMA(d,14), color = "Deaths - 14 Day MA"), na.rm=TRUE) +
            theme(legend.title=element_blank()) +
            labs(title=paste0("COVID-19 World \n",cn," Confirmed and Death Daily Change - \n",dt),
                 x ="Date", y = "Change", subtitle="Source: https://github.com/CSSEGISandData/COVID-19")
        print(p)
    }                
}

countyplot <- function(state,county,plconf = TRUE,pldeath=TRUE){
    slist = statecovid(state)
    d.xts = slist$Deaths.xts
    d.diff = slist$Deaths.diff
    c.xts = slist$Confirmed.xts
    c.diff = slist$Confirmed.diff
    id = which(colnames(d.xts)==county)
    idd = which(colnames(d.diff)==county)
    ic = which(colnames(c.xts)==county)
    icd = which(colnames(c.diff)==county)
    c = c.diff[,icd]
    d = d.diff[,idd]
    cn = colnames(c)
    dt = index(tail(d.diff,1))
    if (plconf == FALSE) {
        p = ggplot() +
            geom_line(data = d.diff, aes(x=index(d.diff),y=d, color = "Deaths"), na.rm=TRUE) +
            geom_line(data = d.diff, aes(x=index(d.diff),y=SMA(d,14), color = "Deaths - 14 Day MA"), na.rm=TRUE) +
            theme(legend.title=element_blank()) +
            labs(title=paste0("COVID-19 - ",state, "\n",cn, " County \n","Confirmed Cases and Death Daily Change - \n",dt),
                 x ="Date", y = "Change", subtitle="Source: https://github.com/CSSEGISandData/COVID-19")
        print(p)
    } else if (pldeath == FALSE) {
        p = ggplot() + geom_line(data = c.diff, aes(x=index(c.diff),y=c, color = "Confirmed"), na.rm=TRUE) +
            geom_line(data = c.diff, aes(x=index(c.diff),y=SMA(c,14), color = "Confirmed - 14 Day MA"), na.rm=TRUE) +
            theme(legend.title=element_blank()) +
            labs(title=paste0("COVID-19 - ",state, "\n",cn, " County \n","Confirmed Cases and Death Daily Change - \n",dt),
                 x ="Date", y = "Change", subtitle="Source: https://github.com/CSSEGISandData/COVID-19")
        print(p)
    } else {
        p = ggplot() + geom_line(data = c.diff, aes(x=index(c.diff),y=c, color = "Confirmed"), na.rm=TRUE) +
            geom_line(data = c.diff, aes(x=index(c.diff),y=SMA(c,14), color = "Confirmed - 14 Day MA"), na.rm=TRUE) +
            geom_line(data = d.diff, aes(x=index(d.diff),y=d, color = "Deaths"), na.rm=TRUE) +
            geom_line(data = d.diff, aes(x=index(d.diff),y=SMA(d,14), color = "Deaths - 14 Day MA"), na.rm=TRUE) +
            theme(legend.title=element_blank()) +
            labs(title=paste0("COVID-19 - ",state, "\n",cn, " County \n","Confirmed Cases and Death Daily Change - \n",dt),
                 x ="Date", y = "Change", subtitle="Source: https://github.com/CSSEGISandData/COVID-19")
        print(p)
    }
}

# Define UI for application that draws a histogram
ui <- fluidPage(h1("COVID-19 Daily Rate of Change"),
                fluidRow(column(checkboxInput("cases", "Show Cases", TRUE),width=2),
                         column(checkboxInput("deaths", "Show Deaths", TRUE),width=2)),
    fluidRow(tabsetPanel(
    tabPanel("World COVID-19",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "country", label = "Select Country", choices = countrynames <- sort(unique(Confirmed[,2])))
                 ),
                 # Show a plot of the generated distribution
                 mainPanel(
                     plotOutput("wgraph"),
                     tags$style(type="text/css",
                                ".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }"
                     )
                 )
             )
    ),
    tabPanel("US States COVID-19",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "usstate", label = "Select State", choices = statenames <- sort(unique(USConfirmed$Province_State)))
                 ),
                 # Show a plot of the generated distribution
                 mainPanel(
                     plotOutput("ussgraph"),
                     tags$style(type="text/css",
                                ".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }"
                     )
                 )
             )
    ),
    tabPanel("US Metro Areas COVID-19",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "uscbsa", label = "Select Metro Area", choices = cbsanames <- sort(unique(CBSAConf.df$CBSA.Title)))
                 ),
                 # Show a plot of the generated distribution
                 mainPanel(
                     plotOutput("cbsagraph"),
                     tags$style(type="text/css",
                                ".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }"
                     )
                 )
             )
    ),
    tabPanel("US Counties COVID-19",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "state", label = "Select State", choices = statenames <- sort(unique(USConfirmed$Province_State))),
                     selectInput(inputId = "counties", label = "Select County", choices = NULL)
                     ),
                 mainPanel(
                     plotOutput("uscgraph"),
                     tags$style(type="text/css",
                      ".shiny-output-error { visibility: hidden; }",
                      ".shiny-output-error:before { visibility: hidden; }"
                      )
                     )
                 )
             )
)
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    choices_counties <- reactive({
        choices_counties <- sort(unique(USConfirmed[USConfirmed$Province_State == input$state,]$Admin2)) 
    })
    
    observe({
        updateSelectInput(session = session, inputId = "counties", choices = choices_counties())
    })

    output$wgraph <- renderPlot({
        nationcovidplot(input$country,input$cases,input$deaths)
    })
    output$ussgraph <- renderPlot({
        uscovidplot(input$usstate,input$cases,input$deaths)
    })
    output$cbsagraph <- renderPlot({
        CBSAcovidplot(input$uscbsa,input$cases,input$deaths)
    })
    output$uscgraph <- renderPlot({
        countyplot(input$state,input$counties,input$cases,input$deaths)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
