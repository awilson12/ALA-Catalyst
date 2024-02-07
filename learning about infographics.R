require(waffle)
require(extrafont)
require(showtext)
require(tidyverse)
require(hrbrthemes)
require(echarts4r)
require(devtools)
require(echarts4r.assets)
#remotes::install_github("JohnCoene/echarts4r.assets")

showtext_auto()

df22 <- data.frame(
  x = sort(LETTERS[1:5], decreasing = TRUE),
  y = sort(sample(20:80,5))
)


frame.all<-data.frame(risks=c(risk.student.inhale,risk.student.face,risk.student.total,
                              risk.teacher.inhale,risk.teacher.face,risk.teacher.total),
                      type=rep(c(rep("Inhalation",length(risk.student.inhale)),rep("Ingestion",length(risk.student.face)),rep("Total",length(risk.student.total))),2),
                      person=c(rep("Student",length(c(risk.student.inhale,risk.student.face,risk.student.total))),
                               rep("Teacher",length(c(risk.teacher.inhale,risk.teacher.face,risk.teacher.total)))))

type<-c("Inhalation","Ingestion","Total")
person<-c("Student","Teacher")
type.all<-rep(NA,6)
person.all<-rep(NA,6)
risk<-rep(NA,6)

for (i in 1:3){
  for (j in 1:2){
    if (i==1 & j==1){
      risk<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$risks[frame.all$person==person[j]]])
      type.all<-type[i]
      person.all<-person[j]
    }else{
      risktemp<-mean(frame.all$risks[frame.all$type==type[i] & frame.all$risks[frame.all$person==person[j]]])
      typetemp<-type[i]
      persontemp<-person[j]
      risk<-c(risk,risktemp)
      type.all<-c(type.all,typetemp)
      person.all<-c(person.all,persontemp)
    }
    
  }
}

df22<-data.frame(y=round(risk*100000,digits=0),type.all,x=person.all)
df22teacher<-df22[type.all=="Total",]
df22teacher<-subset(df22teacher,select=-c(type.all))


df22teacher %>% 
  e_charts(x) %>% 
  e_pictorial(y, symbol = ea_icons("user"), 
              symbolRepeat = TRUE,
              symbolSize = c(15, 15)) %>% 
  e_theme("westeros") %>%
  e_title("Number of Infections per 100,000 People") %>% 
  e_flip_coords() %>%
  # Hide Legend
  e_legend(show = TRUE) %>%
  # Remove Gridlines
  e_x_axis(splitLine=list(show = TRUE)) %>%
  e_y_axis(splitLine=list(show = TRUE)) %>%
  # Format Label
  e_labels(fontSize = 16, fontWeight ='bold', position = "right",offset=c(10,0))


