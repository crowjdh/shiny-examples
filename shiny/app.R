# library(gganimate)
# sessionInfo() # 1.0.7
# 
# require(devtools)
# install_version("gganimate", version = "0.1.1", repos = "http://cran.us.r-project.org")
# getwd()

library(spdplyr)
library(bit64)
library(data.table)
library(dplyr)
library(ggplot2)
library(shiny)
library(readxl)
library(dplyr)
library(shinyWidgets)
library(extrafont)
library(shinydashboard)
library(leaflet)
library(png)
library(showtext)
# library(rsconnect)
library(ggmap)
library(tidyr)
library(raster)
library(rgeos)
library(maptools)
library(devtools)
library(gganimate)
library(tidyverse)
library(showtext)
library(rgdal)
library(geosphere)
library(leaflet)
library(RColorBrewer)
# library(rsconnect)

# rsconnect::setAccountInfo(name='minha', token='B614D14DAF5D24F08272324925C3B366', secret='yGxOc0kvA0iHfpRpRXmHUoomxSX3sAymIfCQ/Vva')
# rsconnect::deployApp("/Users/seungyeon-jeong/shinyapp", lint = "FALSE")

# setwd('/Users/seungyeon-jeong/shinyapp')

# flow <- read.csv("TC_EN_TRNSFRN_ENTRPRS_INFO.csv", encoding='utf-8', fileEncoding = 'cp949') 
# head(flow)
# states_xy <- read.csv("latlong.csv", encoding='utf-8', fileEncoding = 'cp949')

# flow <- dplyr::select(flow, 'STDR_YM', 'TRNSFRN_CTPRVN_NM', 'MVT_CTPRVN_NM')
# flow <- flow[!flow$TRNSFRN_CTPRVN_NM %in% c('03', '32', '39', '41'),]
# flow <- flow[!flow$MVT_CTPRVN_NM %in% c('|구', '41', '42'),]
# flow <- flow[flow['TRNSFRN_CTPRVN_NM'] != flow['MVT_CTPRVN_NM'],]

# flow$STDR_YM <- substring(flow$STDR_YM,1,4)

# df <- flow %>% group_by(STDR_YM, TRNSFRN_CTPRVN_NM, MVT_CTPRVN_NM) %>%
#   summarize(counts = n()) %>%
#   ungroup()

# df <- df %>%
#   left_join(states_xy, by = c('MVT_CTPRVN_NM' = 'name')) %>%
#   left_join(states_xy, by = c('TRNSFRN_CTPRVN_NM' = 'name'))

# df <- df[c(1,3,2,4,6,5,8,7)]

# df$longitude.x <- as.numeric(as.character(df$longitude.x))
# df$longitude.x <- as.numeric(as.character(df$longitude.x))

# df$latitude.y <- as.numeric(as.character(df$latitude.y))
# df$longitude.y <- as.numeric(as.character(df$longitude.y))

# ############ 연도 필터링
# #df <- df %>%
# #  filter(STDR_YM == '2020')

# # head(df)

# flows <- gcIntermediate(df[,5:6], df[,7:8], 
#                         n=100, sp = TRUE, addStartEnd = TRUE)

# flows$counts <- df$counts

# flows$origins <- df$MVT_CTPRVN_NM

# flows$destinations <- df$TRNSFRN_CTPRVN_NM
# flows$STDR_YM <- df$STDR_YM

# hover <- paste0(flows$origins, " to ", 
#                 flows$destinations, ': ', 
#                 as.character(flows$counts))

# pal <- colorFactor(brewer.pal(4, 'Set2'), flows$origins)


load("flows.RData")
load("hover.RData")
load("pal.RData")



# map <- leaflet() %>%
#   addProviderTiles('CartoDB.Positron') %>%
#   addPolylines(data = flows, weight = ~counts/10, label = hover, 
#                group = ~origins, color = ~pal(origins)) %>%
#   addLayersControl(overlayGroups = unique(flows$origins), 
#                    options = layersControlOptions(collapsed = FALSE))
# 
# map

font_add_google("Nanum Gothic", "nanumgothic")
# font_add_google("Poor Story", "poorstory")
# font_add_google('Nanum Pen Script', 'nanumpen')

showtext_auto()
showtext_opts(dpi = 60)

g <- ggplot(data=mtcars, aes(x=disp, y=mpg)) + geom_point()
g + annotate(geom='text', x=275, y=28, size=15, family='nanumgothic', label='지역별 기업 재무현황')
g + labs(title = "지역별 기업 평균 매출 현황, ")

library(devtools)

# 패키지를 로드합니다.
# install_github("plgrmr/readAny", force = TRUE)
# library(readAny)

# 사용법은 read.table 과 100% 똑같습니다.
jj <- read.csv("TC_EN_SELNG_PROFIT_GNRL_CPR.csv", header = TRUE, encoding='utf-8', fileEncoding = 'cp949')
jaemu <- jj
# jaemu <- fread("TC_EN_SELNG_PROFIT_GNRL_CPR.csv", encoding = 'UTF-8')
# View(jaemu)
jaemu1 <- jaemu[jaemu$IEM_NM == "매출액",]
jaemu2 <- jaemu1[, -c(4,5,6,9,11,14,16,17,18,19)]

sales <- jaemu2 %>% group_by(SIGNGU_NM, STDR_YEAR) %>% summarise(mean_sales = mean(TOTAMT))

# for(i in 1:12) {
#   sales$SIGNGU_NM[i] <- "세종특별자치시"
# }

# head(sales, 11)

korea <- shapefile("TL_SCCO_SIG.shp", encoding = "UTF-8")
korea <- fortify(korea, region='SIG_CD')
head(korea)

code <- read.csv("시군구 코드.csv", encoding='utf-8', fileEncoding = 'cp949')
code <- rename(code, "SIGNGU_NM" = "Sigun")
# View(code)

finalsales <- merge(x = sales, y = code, by = 'SIGNGU_NM', all = TRUE)
# finalsales


final <- merge(korea, finalsales, by='id')
# head(final, 100)

final$sales <- as.numeric(final$mean_sales)
# str(final)
final$Average_sales <- log1p(final$sales)
# View(final)

# o <- ggplot(data = final, aes(frame = STDR_YEAR)) +geom_polygon(data=final, aes(x=long, y=lat, group=group, fill=Average_sales)) +
#   theme_void() +
#   guides(fill = guide_colorbar(title.position = "top")) +
#   scale_fill_gradient(low='white', high='#004ea2') +
#   labs(title = "지역별 기업 평균 매출 현황, ") +
#   labs(caption = "Map by JINMINHA, @Handong University\nsource: KED 주요 재무현황 (전체 법인기업)") +
#   theme(plot.title = element_text(family = 'nanumgothic', hjust = 0.415, vjust = 0.4, size=35)) +
#   theme(plot.caption = element_text(family = 'nanumgothic', hjust = 0, color="gray40", size=15)) +
#   theme( legend.position = c(.5, .08), 
#          legend.direction = "horizontal", 
#          legend.title.align = 0,
#          legend.key.size = unit(1.3, "cm"),
#          legend.title=element_text(size=17), 
#          legend.text=element_text(size=13) )
# 
# 
# gganimate(o, title_frame = T, ani.width=900, ani.height=800, dpi=800, interval = 1)


################################# 매출 시각화 ##########################################

debt <- jaemu[jaemu$IEM_NM == "부채총계",]
debt1 <- debt[, -c(4,5,6,9,11,14,16,17,18,19)]

debts1 <- debt1 %>% group_by(SIGNGU_NM, STDR_YEAR) %>% summarise(mean_debt = mean(TOTAMT))

# for(i in 1:12) {
#   debts1$SIGNGU_NM[i] <- "세종특별자치시"
# }

head(debts1, 11)

korea <- shapefile("TL_SCCO_SIG.shp")
korea <- fortify(korea, region='SIG_CD')
# head(korea)
# 
code <- read.csv("시군구 코드.csv", encoding='utf-8', fileEncoding = 'cp949')
code <- rename(code, "SIGNGU_NM" = "Sigun")
# View(code)

finaldebts <- merge(x = debts1, y = code, by = 'SIGNGU_NM', all = TRUE)
finaldebts


final1 <- merge(korea, finaldebts, by='id')
head(final1, 100)

final1$sales <- as.numeric(final1$mean_debt)
# str(final1)
final1$Average_debts <- log1p(final1$sales)
# View(final1)

# q <- ggplot(data = final1, aes(frame = STDR_YEAR)) +geom_polygon(data=final1, aes(x=long, y=lat, group=group, fill=Average_debts)) +
#   theme_void() +
#   guides(fill = guide_colorbar(title.position = "top")) +
#   scale_fill_gradient(low = "yellow", high = "red",
#                       breaks = c(12, 13, 14, 15, 16, 18))+
#   labs(title = "지역별 기업 평균 부채 현황, ") +
#   labs(caption = "Map by JINMINHA, @Handong University\nsource: KED 주요 재무현황 (전체 법인기업)") +
#   theme(plot.title = element_text(hjust = 0.415, vjust = 0.4, size=35)) +
#   theme(plot.caption = element_text(hjust = 0, color="gray40", size=15)) +
#   theme( legend.position = c(.5, .08), 
#          legend.direction = "horizontal", 
#          legend.title.align = 0,
#          legend.key.size = unit(1.3, "cm"),
#          legend.title=element_text(size=17), 
#          legend.text=element_text(size=13) )
# 
# 
# gganimate(q, title_frame = T, ani.width=1200, ani.height=1100, dpi=800, interval = 1)


ui <- dashboardPage(skin = 'yellow',
                    dashboardHeader(title = "기업 인프라 구축 제안을 위한 지역별 기업 생태계 시각화",
                                    titleWidth = 700),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("전국 기업 현황", tabName = "a", icon = icon("list-alt")),
                        menuItem("개요", tabName = "c", icon = icon("check-square")),
                        menuItem("전입전출현황", tabName = "b", icon = icon("th")),
                        menuItem("신용평가등급", tabName = "e", icon = icon("bar-chart-o")),
                        menuItem("재무현황", tabName = "d", icon = icon("table")),
                        menuItem("기업 전입전출현황", tabName = "f", icon = icon("map"))
                      )
                    ),
                    
                    dashboardBody(
                      
                      tags$head(
                        tags$style(
                          HTML(
                            '.main-header .logo {
                            font-family: "Georgia", Times, "Times New Roman", serif;
                            font-weight: bold;
                            font-size: 26px;}
                            
                            body {
                            font-family: "Georgia", Times, "Times New Roman", serif;
                            font-weight: bold;
                            font-size: 16px;}'))),
                      
                      tabItems(
                        tabItem(tabName = "b",
                                fluidPage(
                                  titlePanel("기업별 전입전출현황")
                                )),
                        
                        tabItem(tabName = "f",
                                fluidPage(
                                  radioButtons("yearInput", "year",
                                               c("2010" = "2010",
                                                 "2011" = "2011",
                                                 "2012" = "2012",
                                                 "2013" = "2013",
                                                 "2014" = "2014",
                                                 "2015" = "2015",
                                                 "2016" = "2016",
                                                 "2017" = "2017",
                                                 "2018" = "2018",
                                                 "2019" = "2019",
                                                 "2020" = "2020")),
                                  leafletOutput("mymap", width="100%", height="1000px")
                                )),
                        
                        tabItem(tabName = "c",
                                fluidPage(
                                  valueBox(
                                    value = tags$p("90k", style = "font-size: 150%;"),
                                    subtitle = tags$p("전국 기업 개수", style = "font-size: 200%;"),
                                    icon = icon("info-circle"),
                                    color = "blue"
                                  ),
                                  
                                  valueBox(
                                    value = tags$p("120k", style = "font-size: 150%;"),
                                    subtitle = tags$p("기업 평균 종업원 수", style = "font-size: 200%;"),
                                    icon = icon("users"),
                                    color = "aqua"
                                  ),
                                  
                                  valueBox(
                                    value = tags$p("90k", style = "font-size: 150%;"),
                                    subtitle = tags$p("기업 평균 휴폐업 수", style = "font-size: 200%;"),
                                    icon = icon("gbp"),
                                    color = "teal"
                                  )
                                  
                                )),
                        
                        
                        tabItem(tabName = "a",
                                
                                fluidPage(
                                  #titlePanel("지역별 기업 재무현황"),
                                  #br(),
                                  #br(),
                                  
                                  box(width = 5, title = tags$p("지역별 매출 지도", style = "font-size: 120%; font-weight: bold"), status = "info", solidHeader = TRUE, 
                                      imageOutput("plot1", height = 800, width = 500)),
                                  box(width = 5, title = tags$p("지역별 부채 지도", style = "font-size: 120%; font-weight: bold"), status = "danger", solidHeader = TRUE,
                                      imageOutput("plot2", height = 800, width = 500))
                                )),
                        
                        tabItem(tabName = "d",
                                fluidPage(
                                  titlePanel("지역별 재무현황")
                                )),
                        
                        tabItem(tabName = "e",
                                h2("지역별 KED 신용등급 현황"),
                                fluidRow(
                                  
                                  
                                  box(title = "Inputs", status = "warning", solidHeader = TRUE,
                                      dateRangeInput("dateInput", "Date", 
                                                     start = "2015-01-01", 
                                                     end = '2020-11-30'),
                                      selectInput("worktypeInput", 
                                                  "업종",
                                                  choices=c("제조업", "건설업"),
                                                  selected = c("제조업", "농기계")),
                                      br(),
                                      checkboxGroupInput("detailworktypeInput", 
                                                         "세부업종",
                                                         choices=c("식료품 제조업", "음료 제조업"),
                                                         selected = c("식료품 제조업", "음료 제조업")),
                                      br()),
                                  
                                  
                                  
                                  
                                  
                                  fluidRow(
                                    box(title = "Bar chart", status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                                        plotOutput("stplot"))))
                                
                        ))))




server <- function(input, output) {
  getPage<-function() {
    return(includeHTML("bicycle.html"))
  }
  
  output$inc<-renderUI({getPage()})
  
  output$plot1 <- renderImage({
    outfile <- tempfile(fileext='.gif')
    
    o <- ggplot(data = final, aes(frame = STDR_YEAR)) +geom_polygon(data=final, aes(x=long, y=lat, group=group, fill=Average_sales)) +
      theme_void() +
      guides(fill = guide_colorbar(title.position = "top")) +
      scale_fill_gradient(low='white', high='#004ea2') +
      labs(title = "지역별 기업 평균 매출 현황, ") +
      labs(caption = "Map by JINMINHA, @Handong University\nsource: KED 주요 재무현황 (전체 법인기업)") +
      theme(plot.title = element_text(family = 'nanumgothic', hjust = 0.415, vjust = 0.4, size=35)) +
      theme(plot.caption = element_text(family = 'nanumgothic', hjust = 0, color="gray40", size=15)) +
      theme( legend.position = c(.5, .08), 
             legend.direction = "horizontal", 
             legend.title.align = 0,
             legend.key.size = unit(1.3, "cm"),
             legend.title=element_text(size=17), 
             legend.text=element_text(size=13) )
    
    
    animate(o, "outfile.gif", title_frame = T, ani.width=800, ani.height=800, dpi=800, interval = 1)
    
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  
  output$plot2 <- renderImage({
    outfile1 <- tempfile(fileext='.gif')
    
    q <- ggplot(data = final1, aes(frame = STDR_YEAR)) +geom_polygon(data=final1, aes(x=long, y=lat, group=group, fill=Average_debts)) +
      theme_void() +
      guides(fill = guide_colorbar(title.position = "top")) +
      scale_fill_gradient(low = "yellow", high = "red",
                          breaks = c(12, 13, 14, 15, 16, 18))+
      labs(title = "지역별 기업 평균 부채 현황, ") +
      labs(caption = "Map by JINMINHA, @Handong University\nsource: KED 주요 재무현황 (전체 법인기업)") +
      theme(plot.title = element_text(family = 'nanumgothic', hjust = 0.415, vjust = 0.4, size=35)) +
      theme(plot.caption = element_text(family = 'nanumgothic', hjust = 0, color="gray40", size=15)) +
      theme( legend.position = c(.5, .08), 
             legend.direction = "horizontal", 
             legend.title.align = 0,
             legend.key.size = unit(1.3, "cm"),
             legend.title=element_text(size=17), 
             legend.text=element_text(size=13) )
    
    
    gg_animate(q, "outfile1.gif", title_frame = T, width=800, height=800, dpi=800, interval = 1)
    # animate(q, "outfile1.gif", title_frame = T, width=800, height=800, dpi=800, interval = 1)
    
    
    list(src = "outfile1.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  output$mymap <- renderLeaflet({
    
    filtered <- flows %>% filter(STDR_YM %in% input$yearInput)
    
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      addPolylines(data = filtered, weight = ~counts/10, label = hover, 
                   group = ~origins, color = ~pal(origins)) %>%
      addLayersControl(overlayGroups = unique(filtered$origins), 
                       options = layersControlOptions(collapsed = FALSE))
  })
  
  
  
}

shinyApp(ui, server)
