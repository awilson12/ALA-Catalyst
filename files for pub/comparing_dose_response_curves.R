iterations<-1000
dose<-10^runif(iterations,-5,10)

  k<-rtriangle(iterations,a=0.484,b=1,c=1)

  risk.rhino.total<-1-exp(-dose*k)
  

  k<-0.054
  
  risk.covid.total<-1-exp(-dose*k)
  

  alpha.children<-exp(-1.24)
  N50.children<-exp(5.6)
  
 risk.flu.total<-1-(1+(dose*(2^(1/alpha.children)-1)/N50.children))^-alpha.children
 
tempframe<-data.frame(risk=c(risk.rhino.total,risk.covid.total,risk.flu.total),
                      pathogen=c(rep("rhino",iterations),rep("covid",iterations),rep("flu",iterations)),
                      dose=dose)

windows()
ggplot(tempframe)+geom_point(aes(x=dose,y=risk,color=pathogen))+
  scale_x_continuous(trans="log10")
