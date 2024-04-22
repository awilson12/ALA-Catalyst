library(readr)
data <- read_csv("C:/Users/amwilson2/Box/Proposals/Funded/American Lung Association Catalyst/ALA_prelim_2024_April.csv")

require(ggplot2)
require(ggpubr)

table(data$role[!is.na(data$role)])
length(data$record_id)

table(data$asthma)

table(data$years)


  strategy1<-c(rep("Hand sanitizer in classroom",length(data$strategies___1[data$strategies___1==1 & data$grade___1==1])),
              rep("Cleaning desks and other classroom surfaces",length(data$strategies___2[data$strategies___2==1 & data$grade___1==1])),
              rep("Wearing a mask",length(data$strategies___3[data$strategies___3==1 & data$grade___1==1])),
              rep("Increasing fresh air with open windows and/or doors",length(data$strategies___4[data$strategies___4==1 & data$grade___1==1])),
              rep("Using portable air purifiers or filters",length(data$strategies___5[data$strategies___5==1 & data$grade___1==1])),
              rep("Social distancing in the classroom",length(data$strategies___6[data$strategies___6==1 & data$grade___1==1])),
              rep("Other",length(data$strategies___7[data$strategies___7==1 & data$grade___1==1])))
  
  strategy2<-c(rep("Hand sanitizer in classroom",length(data$strategies___1[data$strategies___1==1 & data$grade___2==1])),
               rep("Cleaning desks and other classroom surfaces",length(data$strategies___2[data$strategies___2==1 & data$grade___2==1])),
               rep("Wearing a mask",length(data$strategies___3[data$strategies___3==1 & data$grade___2==1])),
               rep("Increasing fresh air with open windows and/or doors",length(data$strategies___4[data$strategies___4==1 & data$grade___2==1])),
               rep("Using portable air purifiers or filters",length(data$strategies___5[data$strategies___5==1 & data$grade___2==1])),
               rep("Social distancing in the classroom",length(data$strategies___6[data$strategies___6==1 & data$grade___2==1])),
               rep("Other",length(data$strategies___7[data$strategies___7==1 & data$grade___2==1])))
  
  strategy3<-c(rep("Hand sanitizer in classroom",length(data$strategies___1[data$strategies___1==1 & data$grade___3==1])),
               rep("Cleaning desks and other classroom surfaces",length(data$strategies___2[data$strategies___2==1 & data$grade___3==1])),
               rep("Wearing a mask",length(data$strategies___3[data$strategies___3==1 & data$grade___3==1])),
               rep("Increasing fresh air with open windows and/or doors",length(data$strategies___4[data$strategies___4==1 & data$grade___3==1])),
               rep("Using portable air purifiers or filters",length(data$strategies___5[data$strategies___5==1 & data$grade___3==1])),
               rep("Social distancing in the classroom",length(data$strategies___6[data$strategies___6==1 & data$grade___3==1])),
               rep("Other",length(data$strategies___7[data$strategies___7==1 & data$grade___3==1])))
  
  strategy4<-c(rep("Hand sanitizer in classroom",length(data$strategies___1[data$strategies___1==1 & data$grade___4==1])),
               rep("Cleaning desks and other classroom surfaces",length(data$strategies___2[data$strategies___2==1 & data$grade___4==1])),
               rep("Wearing a mask",length(data$strategies___3[data$strategies___3==1 & data$grade___4==1])),
               rep("Increasing fresh air with open windows and/or doors",length(data$strategies___4[data$strategies___4==1 & data$grade___4==1])),
               rep("Using portable air purifiers or filters",length(data$strategies___5[data$strategies___5==1 & data$grade___4==1])),
               rep("Social distancing in the classroom",length(data$strategies___6[data$strategies___6==1 & data$grade___4==1])),
               rep("Other",length(data$strategies___7[data$strategies___7==1 & data$grade___4==1])))
  
  strategy5<-c(rep("Hand sanitizer in classroom",length(data$strategies___1[data$strategies___1==1 & data$grade___5==1])),
               rep("Cleaning desks and other classroom surfaces",length(data$strategies___2[data$strategies___2==1 & data$grade___5==1])),
               rep("Wearing a mask",length(data$strategies___3[data$strategies___3==1 & data$grade___5==1])),
               rep("Increasing fresh air with open windows and/or doors",length(data$strategies___4[data$strategies___4==1 & data$grade___5==1])),
               rep("Using portable air purifiers or filters",length(data$strategies___5[data$strategies___5==1 & data$grade___5==1])),
               rep("Social distancing in the classroom",length(data$strategies___6[data$strategies___6==1 & data$grade___5==1])),
               rep("Other",length(data$strategies___7[data$strategies___7==1 & data$grade___5==1])))
  
  strategy6<-c(rep("Hand sanitizer in classroom",length(data$strategies___1[data$strategies___1==1 & data$grade___6==1])),
               rep("Cleaning desks and other classroom surfaces",length(data$strategies___2[data$strategies___2==1 & data$grade___6==1])),
               rep("Wearing a mask",length(data$strategies___3[data$strategies___3==1 & data$grade___6==1])),
               rep("Increasing fresh air with open windows and/or doors",length(data$strategies___4[data$strategies___4==1 & data$grade___6==1])),
               rep("Using portable air purifiers or filters",length(data$strategies___5[data$strategies___5==1 & data$grade___6==1])),
               rep("Social distancing in the classroom",length(data$strategies___6[data$strategies___6==1 & data$grade___6==1])),
               rep("Other",length(data$strategies___7[data$strategies___7==1 & data$grade___6==1])))
  
  strategy7<-c(rep("Hand sanitizer in classroom",length(data$strategies___1[data$strategies___1==1 & data$grade___7==1])),
               rep("Cleaning desks and other classroom surfaces",length(data$strategies___2[data$strategies___2==1 & data$grade___7==1])),
               rep("Wearing a mask",length(data$strategies___3[data$strategies___3==1 & data$grade___7==1])),
               rep("Increasing fresh air with open windows and/or doors",length(data$strategies___4[data$strategies___4==1 & data$grade___7==1])),
               rep("Using portable air purifiers or filters",length(data$strategies___5[data$strategies___5==1 & data$grade___7==1])),
               rep("Social distancing in the classroom",length(data$strategies___6[data$strategies___6==1 & data$grade___7==1])),
               rep("Other",length(data$strategies___7[data$strategies___7==1 & data$grade___7==1])))



frame.all<-data.frame(strategy=c(strategy1,strategy2,strategy3,strategy4,strategy5,strategy6,strategy7),
                      grades=c(rep("Kindergarten",length(strategy1)),rep("1st Grade",length(strategy2)),
                               rep("2nd Grade",length(strategy3)),rep("3rd Grade",length(strategy4)),
                               rep("4th Grade",length(strategy5)),rep("5th Grade",length(strategy6)),
                               rep("Specials",length(strategy7))))

frame.all$grades<-ordered(frame.all$grades, levels=c("Kindergarten","1st Grade","2nd Grade",
                                                     "3rd Grade","4th Grade","5th Grade","Specials"))

ggplot(frame.all)+geom_bar(aes(x=grades,fill=strategy))+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Count")+
  scale_fill_discrete(name="Intervention Strategies")+
  theme_pubclean()+
  theme(legend.position="right")

table(data$strategies___1[data$role==1])
table(data$strategies___2[data$role==1])
table(data$strategies___3[data$role==1])
table(data$strategies___4[data$role==1])
table(data$strategies___5[data$role==1])
table(data$strategies___6[data$role==1])
table(data$strategies___7[data$role==1])

table(data$barriers___1[data$role==1])
table(data$barriers___2[data$role==1])
table(data$barriers___3[data$role==1])
table(data$barriers___4[data$role==1])
table(data$barriers___5[data$role==1])
table(data$barriers___6[data$role==1])
table(data$barriers___7[data$role==1])

table(data$spread___1[data$role==2|data$role==3])
table(data$spread___2[data$role==2|data$role==3])
table(data$spread___3[data$role==2|data$role==3])
table(data$spread___4[data$role==2|data$role==3])
table(data$spread___5[data$role==2|data$role==3])
table(data$spread___6[data$role==2|data$role==3])
table(data$spread___7[data$role==2|data$role==3])

table(data$barriers_admin___1[data$role==2|data$role==3])
table(data$barriers_admin___2[data$role==2|data$role==3])
table(data$barriers_admin___3[data$role==2|data$role==3])
table(data$barriers_admin___4[data$role==2|data$role==3])
table(data$barriers_admin___5[data$role==2|data$role==3])
table(data$barriers_admin___6[data$role==2|data$role==3])
table(data$barriers_admin___7[data$role==2|data$role==3])
table(data$barriers_admin___8[data$role==2|data$role==3])
table(data$barriers_admin___9[data$role==2|data$role==3])
table(data$barriers_admin___10[data$role==2|data$role==3])
table(data$barriers_admin___11[data$role==2|data$role==3])

table(data$ach___1[data$role==4])
table(data$ach___2[data$role==4])
table(data$ach___3[data$role==4])
table(data$ach___4[data$role==4])

table(data$filter___1[data$role==4])
table(data$filter___2[data$role==4])
table(data$filter___3[data$role==4])
table(data$filter___4[data$role==4])
table(data$filter___5[data$role==4])
table(data$filter___6[data$role==4])
table(data$filter___7[data$role==4])
table(data$filter___8[data$role==4])
table(data$filter___9[data$role==4])
table(data$filter___10[data$role==4])
table(data$filter___11[data$role==4])
table(data$filter___12[data$role==4])
table(data$filter___13[data$role==4])
table(data$filter___14[data$role==4])
table(data$filter___15[data$role==4])

table(data$challenges_vent___1[data$role==4])
table(data$challenges_vent___2[data$role==4])
table(data$challenges_vent___3[data$role==4])
table(data$challenges_vent___4[data$role==4])
table(data$challenges_vent___5[data$role==4])

table(data$school_type)
