library(spdplyr)
library(bit64)
library(data.table)
library(dplyr)
library(ggplot2)
library(shiny)
library(flexdashboard)
library(readxl)
library(dplyr)
library(shinyWidgets)
library(rvest)
library(extrafont)
library(shinydashboard)
library(leaflet)
library(png)
library(showtext)
library(rsconnect)
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
library(babynames)
library(hrbrthemes)
library(viridis)
library(gifski)
library(png)
library(scales)
library(plotly)
library(RColorBrewer)
# devtools::install_github("thomasp85/gganimate", ref="v0.1.1")
require(devtools)
# devtools::install_github("dgrtwo/gganimate")
# rsconnect::setAccountInfo(name='minha', token='B614D14DAF5D24F08272324925C3B366', secret='yGxOc0kvA0iHfpRpRXmHUoomxSX3sAymIfCQ/Vva')


# library(rsconnect)
# rsconnect::deployApp('C:/Users/user/Documents/shinyapp')

# setwd('/Users/seungyeon-jeong/shinyapp')

############################### 전입전출현황 그래프 ###########################################

# data <- read.csv('TC_EN_TRNSFRN_ENTRPRS_INFO.csv', encoding = 'utf-8', fileEncoding = 'cp949')
# 
# data <- data[!data$TRNSFRN_CTPRVN_NM %in% c('03', '32', '39', '41'),]
# data <- data[!data$MVT_CTPRVN_NM %in% c('|구', '41', '42'),]
# data <- select(data, 'STDR_YM', 'TRNSFRN_CTPRVN_NM', 'MVT_CTPRVN_NM', 'INDUTY_LCLAS_CODE', 'INDUTY_LCLAS_NM')
# data <- data[data['TRNSFRN_CTPRVN_NM'] != data['MVT_CTPRVN_NM'],]
# data$STDR_YM <- substring(data$STDR_YM, 1, 4)
# 
# TRNSFRN_count <- data %>% group_by(STDR_YM, TRNSFRN_CTPRVN_NM) %>% tally()
# MVT_count <- data %>% group_by(STDR_YM, MVT_CTPRVN_NM) %>% tally()
# 
# TRNSFRN_count <- TRNSFRN_count %>%
#   group_by(STDR_YM) %>%
#   mutate(rank = min_rank(-n) * 1,
#          Value_rel = n/n[rank==1],
#          Value_lbl = paste0(" ",n)) %>%
#   filter(rank <=17) %>%
#   ungroup()
# 
# MVT_count <- MVT_count %>%
#   group_by(STDR_YM) %>%
#   mutate(rank = min_rank(-n) * 1,
#          Value_rel = n/n[rank==1],
#          Value_lbl = paste0(" ",n)) %>%
#   filter(rank <=17) %>%
#   ungroup()
# 
# load("/Users/seungyeon-jeong/TRNSFRN_count.RData")
# load("/Users/seungyeon-jeong/MVT_count.RData")

############################## 신용평가등급  ######################################

# KED<-fread('TC_EN_AREA_CRISIS_INFO.csv')
# 
# KED$STDR_YM_f<-as.factor(KED$STDR_YM)
# KED$CREDT_GRAD_NM<-as.factor(KED$CREDT_GRAD_NM)
# 
# ### 전처리 
# KED2<-(KED %>% group_by(STDR_YM_f,CTPRVN_NM,CREDT_GRAD_NM) %>% summarise(cnt_지역KED=n())) %>%
#   data.frame()
# KED3<-data.frame(STDR_YM_f=rep(unique(KED$STDR_YM_f),each=17*23),
#                  CTPRVN_NM=rep(sort(unique(KED$CTPRVN_NM)),each=23,130),
#                  CREDT_GRAD_NM=rep(sort(unique(KED$CREDT_GRAD_NM)),130*17),cnt_지역KED=0)
# KED4<-merge(KED3,KED2,
#             by=c('STDR_YM_f','CTPRVN_NM','CREDT_GRAD_NM'),all.x=T)
# KED4<-KED4[,-4] %>% rename(cnt_지역KED=cnt_지역KED.y)
# KED4$cnt_지역KED[which(is.na(KED4$cnt_지역KED))]<-0
# 
# KED4<-KED4 %>% group_by(STDR_YM_f,CTPRVN_NM) %>% rename(DATE=STDR_YM_f) %>%
#   mutate(cnt_지역=sum(cnt_지역KED),ratio=round(cnt_지역KED/cnt_지역*100,3))
# 
# KED4_abcd<-KED4 %>% mutate(CREDT_ABCD=ifelse(str_detect(CREDT_GRAD_NM,'A'),'A',
#                                              ifelse(str_detect(CREDT_GRAD_NM,'B'),'B',
#                                                     ifelse(str_detect(CREDT_GRAD_NM,'C'),'C',
#                                                            ifelse(CREDT_GRAD_NM=='D','D','NR'))))) %>%
#   group_by(DATE,CTPRVN_NM,CREDT_ABCD) %>% 
#   summarise(cnt_지역ABCD=sum(cnt_지역KED),ratio_ABCD=cnt_지역ABCD/cnt_지역*100) %>%
#   unique()
# 
# load()
load('KED4_abcd.RData')

############################## 조기경보 등급 ######################################

# data3<-fread("TC_EN_CREDT_EW.csv")
# # head(data3)
# # sum(is.na(data3))
# # str(data3)
# unique(data3$STDR_DE)
# data3$STDR_DE_f<-as.factor(data3$STDR_DE)
# 
# # 데이터 전처리 
# d4<-(data3 %>% group_by(STDR_DE_f,CTPRVN_NM,EW_GRAD_NM) %>% summarise(cnt_지역등급=n())) %>%
#   group_by(STDR_DE_f,CTPRVN_NM) %>% 
#   mutate(cnt_지역=sum(cnt_지역등급),ratio=round(cnt_지역등급/cnt_지역*100,3)) %>%
#   rename(DATE=STDR_DE_f) %>%
#   data.frame() #%>% filter(EW_GRAD_NM=='정상') #%>% dplyr::select(STDR_DE_f,CTPRVN_NM,ratio)
# 
# head(d4)
# 
# # 지역별 조기경보등급 변화 
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(17)

load('d4.RData')

############################### 전입전출현황 지도 ###########################################

# flow <- read.csv("TC_EN_TRNSFRN_ENTRPRS_INFO.csv", encoding = 'utf-8', fileEncoding = 'cp949') 
# head(flow)
# states_xy <- read.csv("latlong.csv", encoding = 'utf-8', fileEncoding = 'cp949')
# 
# flow <- dplyr::select(flow, 'STDR_YM', 'TRNSFRN_CTPRVN_NM', 'MVT_CTPRVN_NM', 'INDUTY_LCLAS_NM', 'PRCSS_ENTRPRS_SE')
# flow <- flow[!flow$TRNSFRN_CTPRVN_NM %in% c('03', '32', '39', '41'),]
# flow <- flow[!flow$MVT_CTPRVN_NM %in% c('|구', '41', '42'),]
# flow <- flow[flow['TRNSFRN_CTPRVN_NM'] != flow['MVT_CTPRVN_NM'],]
# 
# flow$STDR_YM <- substring(flow$STDR_YM,1,4)
# 
# df <- flow %>% group_by(STDR_YM, TRNSFRN_CTPRVN_NM, MVT_CTPRVN_NM, INDUTY_LCLAS_NM, PRCSS_ENTRPRS_SE) %>%
#   summarize(counts = n()) %>%
#   ungroup()
# 
# # head(df)
# 
# df <- df %>%
#   left_join(states_xy, by = c('MVT_CTPRVN_NM' = 'name')) %>%
#   left_join(states_xy, by = c('TRNSFRN_CTPRVN_NM' = 'name'))
# 
# df <- df[c(1,3,2,4,5,6,8,7,10,9)]
# 
# df$longitude.x <- as.numeric(as.character(df$longitude.x))
# df$longitude.x <- as.numeric(as.character(df$longitude.x))
# 
# df$latitude.y <- as.numeric(as.character(df$latitude.y))
# df$longitude.y <- as.numeric(as.character(df$longitude.y))
# 
# flows <- gcIntermediate(df[,7:8], df[,9:10], 
#                         n=10, sp = TRUE, addStartEnd = TRUE)
# 
# flows$counts <- df$counts
# flows$origins <- df$MVT_CTPRVN_NM
# 
# flows$destinations <- df$TRNSFRN_CTPRVN_NM
# flows$STDR_YM <- df$STDR_YM
# flows$INDUTY_LCLAS_NM <- df$INDUTY_LCLAS_NM
# flows$PRCSS_ENTRPRS_SE <- df$PRCSS_ENTRPRS_SE

# hover <- paste0(flows$origins, " to ", 
#                 flows$destinations, ': ', 
#                 as.character(flows$counts))

load("flows.RData")
pal <- colorFactor(brewer.pal(4, 'Set2'), flows$origins)

# View(flows)

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

# g <- ggplot(data=mtcars, aes(x=disp, y=mpg)) + geom_point()
# g + annotate(geom='text', x=275, y=28, size=15, family='nanumgothic', label='지역별 기업 재무현황')
# g + labs(title = "지역별 기업 평균 매출 현황, ")


################################# 재무현황 (매출) ######################################

# jaemu <- fread("TC_EN_SELNG_PROFIT_GNRL_CPR.csv")
# View(jaemu)
# jaemu1 <- jaemu[jaemu$IEM_NM == "매출액"]
# jaemu2 <- jaemu1[, -c(4,5,6,9,11,14,16,17,18,19)]
# 
# sales <- jaemu2 %>% group_by(SIGNGU_NM, STDR_YEAR) %>% summarise(mean_sales = mean(TOTAMT))
# 
# for(i in 1:12) {
#   sales$SIGNGU_NM[i] <- "세종특별자치시"
# }
# 
# head(sales, 11)
# 
# korea <- shapefile("TL_SCCO_SIG.shp")
# korea <- fortify(korea, region='SIG_CD')
# head(korea)
# 
# code <- read.csv("시군구 코드.csv")
# code <- rename(code, "SIGNGU_NM" = "Sigun")
# View(code)
# 
# finalsales <- merge(x = sales, y = code, by = 'SIGNGU_NM', all = TRUE)
# finalsales
# 
# 
# final <- merge(korea, finalsales, by='id')
# head(final, 100)
# 
# final$sales <- as.numeric(final$mean_sales)
# str(final)
# final$Average_sales <- log1p(final$sales)
# View(final)
# 
# # write.csv(final, "final.csv")
# 
# 
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
# gg_animate(o, title_frame = T, ani.width=800, ani.height=800, dpi=800, interval = 1)
# 
# 
# animate(o, height = 800, width = 800, fps = 20, duration = 20, start_pause = 10, end_pause = 20,  rewind = T)
# ??animate
# 
# if(!require(installr)) {
#   install.packages("installr"); require(installr)}
# 
# update.packages ()

################################# 재무현황 (부채) ######################################

# debt <- jaemu[jaemu$IEM_NM == "부채총계"]
# debt1 <- debt[, -c(4,5,6,9,11,14,16,17,18,19)]
# 
# debts1 <- debt1 %>% group_by(SIGNGU_NM, STDR_YEAR) %>% summarise(mean_debt = mean(TOTAMT))
# 
# for(i in 1:12) {
#   debts1$SIGNGU_NM[i] <- "세종특별자치시"
# }
# 
# head(debts1, 11)
# 
# korea <- shapefile("TL_SCCO_SIG.shp")
# korea <- fortify(korea, region='SIG_CD')
# # head(korea)
# # 
# code <- read.csv("시군구 코드.csv")
# code <- rename(code, "SIGNGU_NM" = "Sigun")
# # View(code)
# 
# finaldebts <- merge(x = debts1, y = code, by = 'SIGNGU_NM', all = TRUE)
# finaldebts
# 
# 
# final1 <- merge(korea, finaldebts, by='id')
# head(final1, 100)
# 
# final1$sales <- as.numeric(final1$mean_debt)
# str(final1)
# final1$Average_debts <- log1p(final1$sales)
# View(final1)
# 
# write.csv(final1, "final1.csv")
# 
# q <- ggplot(data = final1, aes(frame = STDR_YEAR)) +geom_polygon(data=final1, aes(x=long, y=lat, group=group, fill=Average_debts)) +
#   theme_void() +
#   guides(fill = guide_colorbar(title.position = "top")) +
#   scale_fill_gradient(low = "yellow", high = "red",
#                        breaks = c(12, 13, 14, 15, 16, 18))+
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
# gg_animate(q, title_frame = T, ani.width=800, ani.height=800, dpi=800, interval = 1)


################################# 샤이니앱  ######################################

ui <- dashboardPage(skin = 'black',
  dashboardHeader(title = "기업 인프라 구축 제안을 위한 지역별 기업 생태계 시각화",
                  titleWidth = 700),
  dashboardSidebar(
    sidebarMenu(
      menuItem("개요", tabName = "c", icon = icon("dashboard")),
      menuItem("재무현황", tabName = "d", icon = icon("map")),
      menuItem("신용평가등급", tabName = "e", icon = icon("bar-chart-o")),
      menuItem("조기경보등급", tabName = "a", icon = icon("pie-chart")),
      menuItem("전입전출현황", tabName = "b", icon = icon("map-marker")),
      menuItem("회생파산현황", tabName = "f", icon = icon("line-chart"))
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
              fluidRow(
                div(style = "font-size: 18px;",
                box(width = 3, title = tags$p("INPUTS", style = "font-size: 120%; font-weight: bold"), status = "warning", solidHeader = TRUE,
                    
                    sliderInput("yearInput", "연도",
                                min = 2008, max = 2020,
                                value = c(2020, 2020)),
                    
                    br(),
                    checkboxGroupInput("companysizeInput",
                                       "기업 크기", 
                                       choices=c("대기업", "중기업", "중견기업", "소기업", "미분류", "판단제외"),
                                       selected = c("대기업", "중기업", "중견기업", "소기업", "미분류", "판단제외"),
                                       inline = F),
                    br(),
                    checkboxGroupInput("worktypeInput",
                                       "업종", 
                                       choices=c("가구 내 고용활동 및 달리 분류되지 않은 자가소비 생산활동",
                                                 "건설업", "공공행정; 국방 및 사회보장 행정", "광업",
                                                 "교육 서비스업", "국제 및 외국기관", "금융 및 보험업",
                                                 "농업; 임업 및 어업", "도매 및 소매업", "보건업 및 사회복지 서비스업",
                                                 "부동산업", "사업시설 관리; 사업 지원 및 임대 서비스업",
                                                 "수도; 하수 및 폐기물 처리; 원료 재생업", "숙박 및 음식점업",
                                                 "예술; 스포츠 및 여가관련 서비스업", "운수 및 창고업",
                                                 "전기; 가스; 증기 및 공기조절 공급업", "전문; 과학 및 기술 서비스업",
                                                 "정보통신업", "제조업", "협회 및 단체; 수리 및 기타 개인 서비스업"
                                       ),
                                       selected = c("가구 내 고용활동 및 달리 분류되지 않은 자가소비 생산활동",
                                                    "건설업", "공공행정; 국방 및 사회보장 행정", "광업",
                                                    "교육 서비스업", "국제 및 외국기관", "금융 및 보험업",
                                                    "농업; 임업 및 어업", "도매 및 소매업", "보건업 및 사회복지 서비스업",
                                                    "부동산업", "사업시설 관리; 사업 지원 및 임대 서비스업",
                                                    "수도; 하수 및 폐기물 처리; 원료 재생업", "숙박 및 음식점업",
                                                    "예술; 스포츠 및 여가관련 서비스업", "운수 및 창고업",
                                                    "전기; 가스; 증기 및 공기조절 공급업", "전문; 과학 및 기술 서비스업",
                                                    "정보통신업", "제조업", "협회 및 단체; 수리 및 기타 개인 서비스업"),
                                       inline = F))),
                fluidRow(
                  box(width = 8, height="100%", title = tags$p("지역별 전입전출현황 지도", style = "font-size: 120%; font-weight: bold,"), status = "info", solidHeader = TRUE,
                      leafletOutput("mymap", width="100%", height="1050px"))
                ),
                
                fluidRow(
                  box(width = 9, title = tags$p("지역별 전입전출현황 시계열 그래프", style = "font-size: 120%; font-weight: bold"), status = "danger", solidHeader = TRUE,
                      plotlyOutput("plot7", height = 600, width = "100%"))
                  
                  
                )
                
              )
              ),
      
      tabItem(tabName = "f"
              
              ),
      
      tabItem(tabName = "c",
              fluidPage(
                box(width = 2, title = tags$p("INPUTS", style = "font-size: 120%; font-weight: bold"), status = "warning", solidHeader = TRUE,
                    
                    div(style = "font-size: 22px;",
                        
                        checkboxGroupInput("regionInput", "지역",
                                           choices=c("강원", "경기", "경남", "경북", "광주", "대구", "대전", "부산", "서울",
                                                     "세종", "울산", "인천", "전남", "전북", "제주", "충남", "충북"),
                                           selected = c("강원", "경기", "경남", "경북", "광주", "대구", "대전", "부산", "서울",
                                                        "세종", "울산", "인천", "전남", "전북", "제주", "충남", "충북"),
                                           inline = F)
                        
                    )),
                # valueBox(width = 3,
                #   value = tags$p("90k", style = "font-size: 150%;"),
                #   subtitle = tags$p("기업 수", style = "font-size: 200%;"),
                #   icon = icon("info-circle"),
                #   color = "blue"
                # ),
                
                # valueBox(width = 3,
                #   value = tags$p("120k", style = "font-size: 150%;"),
                #   subtitle = tags$p("평균 종업원 수", style = "font-size: 200%;"),
                #   icon = icon("users"),
                #   color = "aqua"
                # ),
                # 
                # valueBox(width = 3,
                #   value = tags$p("90k", style = "font-size: 150%;"),
                #   subtitle = tags$p("휴폐업 기업 수", style = "font-size: 200%;"),
                #   icon = icon("gbp"),
                #   color = "teal"
                # ),
                box(flexdashboard::gaugeOutput("plt1"),width=3,title="Gauge Graph"),
                box(flexdashboard::gaugeOutput("plt2"),width=3,title="Gauge Graph"),
                box(flexdashboard::gaugeOutput("plt3"),width=3,title="Gauge Graph")
                
                )),
      
      tabItem(tabName = "a",
              fluidRow(
                box(width = 2, height = "672px",title = tags$p("INPUTS",style = "font-size: 120%; font-weight: bold"), status = "warning", solidHeader = TRUE,
                    
                    div(style = "font-size: 22px;",
                        
                        radioButtons("dangergradeInput", "조기경보등급",
                                     c("관심" = "관심",
                                       "관찰1" = "관찰1",
                                       "관찰2" = "관찰2",
                                       "관찰3" = "관찰3",
                                       "정상" = "정상",
                                       "휴업" = "휴업",
                                       "폐업" = "폐업",
                                       "부도" = "부도"))
                    )),
                fluidRow(
                  box(width = 9, title = tags$p("지역별 조기경보등급 시계열 그래프", style = "font-size: 120%; font-weight: bold"), status = "danger", solidHeader = TRUE,
                      plotlyOutput("plot3", height = 600, width = "100%"))
                  
                  
                )),
              fluidRow(
                box(width = 2, title = tags$p("INPUTS", style = "font-size: 120%; font-weight: bold"), status = "warning", solidHeader = TRUE,
                    
                    div(style = "font-size: 22px;",
                        
                        radioButtons("regionInput", "지역",
                                     c("강원" = "강원",
                                       "경기" = "경기",
                                       "경남" = "경남",
                                       "경북" = "경북",
                                       "광주" = "광주",
                                       "대구" = "대구",
                                       "대전" = "대전",
                                       "부산" = "부산",
                                       "서울" = "서울",
                                       "세종" = "세종",
                                       "울산" = "울산",
                                       "인천" = "인천",
                                       "전남" = "전남",
                                       "전북" = "전북",
                                       "제주" = "제주",
                                       "충남" = "충남",
                                       "충북" = "충북"
                                       ))
                    )),
                fluidRow(
                  box(width = 9, title = tags$p("지역별 조기경보등급 비율 시계열 그래프", style = "font-size: 120%; font-weight: bold"), status = "danger", solidHeader = TRUE,
                      plotlyOutput("plot4", height = 662, width = "100%"))
                  
                  
                ))),
      
      tabItem(tabName = "d",
              fluidPage(
                
                # box(width = 5, title = tags$p("지역별 매출 지도", style = "font-size: 120%; font-weight: bold"), status = "success", solidHeader = TRUE, 
                #     imageOutput("plot1", height = 800, width = 500)),
                # box(width = 5, title = tags$p("지역별 부채 지도", style = "font-size: 120%; font-weight: bold"), status = "danger", solidHeader = TRUE,
                #     imageOutput("plot2", height = 800, width = 500))
              )),
      
      tabItem(tabName = "e",
              fluidRow(
                box(width = 2, height = "672px",title = tags$p("INPUTS",style = "font-size: 120%; font-weight: bold"), status = "warning", solidHeader = TRUE,
                    
                    div(style = "font-size: 22px;",
                        
                        radioButtons("creditgradeInput", "신용평가등급",
                                     c("A" = "A",
                                       "B" = "B",
                                       "C" = "C",
                                       "D" = "D",
                                       "NR" = "NR"))
                    )),
                fluidRow(
                  box(width = 9, title = tags$p("지역별 신용평가등급 시계열 그래프", style = "font-size: 120%; font-weight: bold"), status = "success", solidHeader = TRUE,
                      plotlyOutput("plot5", height = 600, width = "100%"))
                  
                )),
              fluidRow(
                box(width = 2, title = tags$p("INPUTS", style = "font-size: 120%; font-weight: bold"), status = "warning", solidHeader = TRUE,
                    
                    div(style = "font-size: 22px;",
                        
                        radioButtons("regionaInput", "지역",
                                     c("강원" = "강원",
                                       "경기" = "경기",
                                       "경남" = "경남",
                                       "경북" = "경북",
                                       "광주" = "광주",
                                       "대구" = "대구",
                                       "대전" = "대전",
                                       "부산" = "부산",
                                       "서울" = "서울",
                                       "세종" = "세종",
                                       "울산" = "울산",
                                       "인천" = "인천",
                                       "전남" = "전남",
                                       "전북" = "전북",
                                       "제주" = "제주",
                                       "충남" = "충남",
                                       "충북" = "충북"
                                     ))
                    )),
                fluidRow(
                  box(width = 9, title = tags$p("지역별 신용평가등급 비율 시계열 그래프", style = "font-size: 120%; font-weight: bold"), status = "success", solidHeader = TRUE,
                      plotlyOutput("plot6", height = 662, width = "100%"))
                  
                  
                ))
              
      ))))



server <- function(input, output, session) {
  
  
  # output$plot1 <- renderImage({
  #   outfile <- tempfile(fileext='.gif')
  #   
  #   
  #   o <- ggplot(data = final, aes(frame = STDR_YEAR)) +geom_polygon(data=final, aes(x=long, y=lat, group=group, fill=Average_sales)) +
  #     theme_void() +
  #     guides(fill = guide_colorbar(title.position = "top")) +
  #     scale_fill_gradient(low='white', high='#004ea2') +
  #     labs(title = "지역별 기업 평균 매출 현황, ") +
  #     labs(caption = "Map by JINMINHA, @Handong University\nsource: KED 주요 재무현황 (전체 법인기업)") +
  #     theme(plot.title = element_text(family = 'nanumgothic', hjust = 0.415, vjust = 0.4, size=35)) +
  #     theme(plot.caption = element_text(family = 'nanumgothic', hjust = 0, color="gray40", size=15)) +
  #     theme( legend.position = c(.5, .08), 
  #            legend.direction = "horizontal", 
  #            legend.title.align = 0,
  #            legend.key.size = unit(1.3, "cm"),
  #            legend.title=element_text(size=17), 
  #            legend.text=element_text(size=13) )
  #   
  #   
  #   gg_animate(o, "outfile.gif", title_frame = T, ani.width=800, ani.height=800, dpi=800, interval = 1)
  #   
  #   # Return a list containing the filename
  #   list(src = "outfile.gif",
  #        contentType = 'image/gif'
  #        # width = 400,
  #        # height = 300,
  #        # alt = "This is alternate text"
  #   )}, deleteFile = TRUE)
  # 
  # 
  # output$plot2 <- renderImage({
  #   outfile1 <- tempfile(fileext='.gif')
  #   
  #   q <- ggplot(data = final1, aes(frame = STDR_YEAR)) +geom_polygon(data=final1, aes(x=long, y=lat, group=group, fill=Average_debts)) +
  #     theme_void() +
  #     guides(fill = guide_colorbar(title.position = "top")) +
  #     scale_fill_gradient(low = "yellow", high = "red",
  #                         breaks = c(12, 13, 14, 15, 16, 18))+
  #     labs(title = "지역별 기업 평균 부채 현황, ") +
  #     labs(caption = "Map by JINMINHA, @Handong University\nsource: KED 주요 재무현황 (전체 법인기업)") +
  #     theme(plot.title = element_text(family = 'nanumgothic', hjust = 0.415, vjust = 0.4, size=35)) +
  #     theme(plot.caption = element_text(family = 'nanumgothic', hjust = 0, color="gray40", size=15)) +
  #     theme( legend.position = c(.5, .08), 
  #            legend.direction = "horizontal", 
  #            legend.title.align = 0,
  #            legend.key.size = unit(1.3, "cm"),
  #            legend.title=element_text(size=17), 
  #            legend.text=element_text(size=13) )
  #   
  #   
  #   gg_animate(q, "outfile1.gif", title_frame = T, ani.width=800, ani.height=800, dpi=800, interval = 1)
  #   
  #   
  #   list(src = "outfile1.gif",
  #        contentType = 'image/gif'
  #        # width = 400,
  #        # height = 300,
  #        # alt = "This is alternate text"
  #   )}, deleteFile = TRUE)
  
  output$plot3 <- renderPlotly({
    
    filtered2 <- d4 %>% filter(EW_GRAD_NM %in% input$dangergradeInput)

    d4_정상_plot<- filtered2 %>% plot_ly(x=~CTPRVN_NM) %>%
      add_trace(y=~ratio, type='bar',frame=~DATE,textfont=list(size=20),
                colors = mycolors,
                color=~CTPRVN_NM,text = ~ratio,
                hovertemplate = paste('%{x}', '<br>정상:', '%{y}', '%<br>'),
                texttemplate = '%{y:.3r}%',
                textposition = "outside") %>%
      animation_slider(
        currentvalue = list(prefix = "DATE : ", font = list(color="red"))
      ) %>%
      layout(
        xaxis = list(title = "지역"),
        yaxis = list(title = "조기경보등급비율(%)"),
        showlegend = F
      )

    d4_정상_plot
  })
  
  output$plot4 <- renderPlotly({
    
    filtered3 <- d4 %>% filter(CTPRVN_NM %in% input$regionInput)
    
    filtered3 %>%
      plot_ly(labels = ~EW_GRAD_NM, values = ~ratio, type = 'pie',
              frame=~DATE,sort=F,rotation=0,direction='clockwise',opacity=0.8,
              textfont=list(size=18,color='black'),#textinfo='label+percent',
              marker=list(colors = c('#8dd3c7','#ffffb3','#bebada','#fb8072',
                                     '#fdb462','#80b1d3','#b3de69','#fccde5')), hole=0.5,
              title = list(text=paste(input$regionInput, " 조기경보등급 비율<br>시계열 그래프",sep=''),
                           font=list(family='Nanum Gothic',size=15))) %>%
      animation_slider(
        currentvalue = list(prefix = "DATE ", font = list(color="red"))
      ) %>% animation_opts(transition=500)
  })

  
  output$plot5 <- renderPlotly({
    req(input$creditgradeInput)
    
    filtered4 <- KED4_abcd %>% filter(CREDT_ABCD %in% input$creditgradeInput)
  
    
    filtered4 %>% plot_ly(x=~CTPRVN_NM) %>% 
      add_trace(y=~ratio_ABCD, type='bar',frame=~DATE,textfont=list(size=20),
                colors = mycolors,
                color=~CTPRVN_NM,text = ~ratio_ABCD,
                hovertemplate = paste(input$creditgradeInput,'등급<br>', '%{y:.4r}', '%<br>'),
                texttemplate = '%{y:.2r}%',
                textposition = "outside") %>%
      layout(
        title = list(text=paste("지역별 KED신용등급(",input$creditgradeInput,") 시계열 그래프",sep=''),
                     yanchor='top',font=list(family='Nanum Gothic',size=15)),
        xaxis = list(title = "지역"),
        yaxis = list(title = "KED신용등급비율(%)"),
        showlegend = F
      ) %>%
      animation_slider(
        currentvalue = list(prefix = "DATE : ", font = list(color="red"))
      )
  })
  
  output$plot6 <- renderPlotly({
    
    filtered5 <- KED4_abcd %>% filter(CTPRVN_NM %in% input$regionaInput)
    
    filtered5 %>%
      plot_ly(labels = ~CREDT_ABCD, values = ~ratio_ABCD, type = 'pie',
              frame=~DATE,sort=F,rotation=0,direction='clockwise',opacity=0.9,name=input$regionaInput,
              #hoverinfo = 'label+value+text',
              hovertemplate = paste('%{label}등급<br>', '%{value:.4r}', '%<br>'),
              textfont=list(size=c(15),color='black'),textinfo='label+percent',
              marker=list(colors = c('#ACC3FB','#C3FE96','#FDFA99','#FBC893','#FB8383'),
                          line = list(width=0))
              #hole=0.5,
      ) %>%
      layout(title = list(text=paste(input$regionaInput," KED신용등급 비율 시계열 그래프",sep=''),
                          font=list(family='Nanum Gothic',size=18),x=0.53,y=0.95),
             margin=list(t=120),
             legend=list(x=0.8)) %>%
      animation_slider(
        currentvalue = list(prefix = "DATE ", font = list(color="red"))
      ) %>% animation_opts(transition=500)
  })
  
  
  
  
  output$mymap <- renderLeaflet({
    
    
    filtered <- flows %>% filter(STDR_YM %in% input$yearInput,
                                 INDUTY_LCLAS_NM %in% input$worktypeInput,
                                 PRCSS_ENTRPRS_SE %in% input$companysizeInput) %>%
                                group_by(origins, destinations) %>%
                                mutate(n=sum(counts))
    
    hover <- paste0(filtered$origins, " to ", 
                    filtered$destinations, ': ', 
                    as.character(filtered$n))
                                 
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      addPolylines(data = filtered, weight = ~n/10, label = hover, 
                   group = ~origins, color = ~pal(origins)) %>%
      addLayersControl(overlayGroups = unique(filtered$origins), 
                       options = layersControlOptions(collapsed = FALSE))
  })
  
  output$plt1 <- flexdashboard::renderGauge({
    gauge(81, min = 0, max = 100, symbol = '%', label = paste("Test Label"),gaugeSectors(
      success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
    ))
    
  })
  
  output$plt2 <- flexdashboard::renderGauge({
    gauge(50, min = 0, max = 100, symbol = '%', label = paste("Test Label"),gaugeSectors(
      success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
    ))
    
  })
  
  output$plt3 <- flexdashboard::renderGauge({
    gauge(30, min = 0, max = 100, symbol = '%', label = paste("Test Label"),gaugeSectors(
      success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
    ))
    
  })
  
  
  output$plot7 <- renderPlotly({
    
    m <- ggplot(MVT_count, aes(rank, group = MVT_CTPRVN_NM,
                               fill = as.factor(MVT_CTPRVN_NM), color = as.factor(MVT_CTPRVN_NM))) + 
      geom_tile(aes(y = n/2,
                    height = n,
                    width = 0.9), alpha = 0.8, color = NA) +
      transition_time(as.integer(STDR_YM)) +
      coord_flip(clip = "off", expand = FALSE) +
      guides(color = FALSE, fill = FALSE) +
      geom_text(aes(y = 0, label = paste(MVT_CTPRVN_NM, " ")), vjust = 0.2, hjust = 1) +
      geom_text(aes(y=n,label = Value_lbl, hjust=0), size=7) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_reverse() +
      guides(color = FALSE, fill = FALSE) +
      theme(panel.background = element_rect(fill='white', colour='white'),
            plot.title = element_text(hjust = 0, size = 27),
            axis.title = element_text(size = 20),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x=element_text(size = 17),
            axis.text.y=element_blank(),
            plot.margin = margin(1,1,1,2, "cm"))+
      labs(title='전출 기업 수, {frame_time}\n', x = "", y = '전출 기업 수',
           caption = "source: KED 전출기업현황")+
      ease_aes('cubic-in-out')
    
    
    animate(m, duration = 10, fps = 20, rendere=gifski_renderer(loop=TRUE),  width= 600, height=600) 
    
    
  })
}

# devtools::install_github("thomasp85/gganimate", ref="v0.1.1")
shinyApp(ui, server)
