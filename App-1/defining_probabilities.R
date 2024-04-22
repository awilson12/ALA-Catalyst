

#Row 1 (room air)-------------------------
#if(openwindowsdoors=="Yes"){
  if(pathogen=="COVID-19"){
    lambdas.1<-c(lambda.1.2[i], lambda.1.3, lambda.1.4[i],lambda.1.5[i],0,
                 0,lambda.1.8,lambda.1.9,lambda.1.10[i])  
  }else{
    lambdas.1<-c(lambda.1.2[i], lambda.1.3, lambda.1.4[i],lambda.1.5[i],0,
                 0,lambda.1.8,lambda.1.9,lambda.1.10)
  }
  
#}else{
#  if(pathogen=="COVID-19"){
#    lambdas.1<-c(lambda.1.2, lambda.1.3, lambda.1.4[i],lambda.1.5[i],0,
#                 0,lambda.1.8,lambda.1.9,lambda.1.10[i])  
#  }else{
#    lambdas.1<-c(lambda.1.2, lambda.1.3, lambda.1.4[i],lambda.1.5[i],0,
#                 0,lambda.1.8,lambda.1.9,lambda.1.10)
#  }
  
#}



lambda.1.T<-sum(lambdas.1)

P.1.1<-exp(-lambda.1.T)

P.1<-(1-P.1.1)*lambdas.1/lambda.1.T

P.1.total<-c(P.1.1,P.1)

#Row 2 (exhaust)------------------------
P.2.total<-c(0,1,rep(0,8))

#Row 3 (surfaces)------------------------

lambdas.3<-c(0,0,0,0,0,0,lambda.3.8[i],lambda.3.9[i],lambda.3.10)
lambda.3.T<-sum(lambdas.3)
P.3.3<-exp(-lambda.3.T)
P.3<-(1-P.3.3)*(lambdas.3/lambda.3.T)
P.3.total<-c(P.3[1:2],P.3.3,P.3[3:9])

#Row 4 (student respiratory tracts)------------------------

P.4.total<-c(0,0,0,1,0,0,0,0,0,0)

#Row 5 (teacher respiratory tract)------------------------

P.5.total<-c(0,0,0,0,1,0,0,0,0,0)

#Row 6 (student mucosal membranes)------------------------

P.6.total<-c(0,0,0,0,0,1,0,0,0,0)

#Row 7 (teacher mucosal membranes)------------------------

P.7.total<-c(0,0,0,0,0,0,1,0,0,0)

#Row 8 (student hands)------------------------

lambdas.8<-c(0,0,lambda.8.3[i],0,0,lambda.8.6[i],0,0,lambda.8.10[i])
lambda.8.T<-sum(lambdas.8)
P.8.8<-exp(-lambda.8.T)
P.8<-(1-P.8.8)*(lambdas.8/lambda.8.T)
P.8.total<-c(P.8[1:7],P.8.8,P.8[8:9])

#Row 9 (teacher hands)------------------------

lambdas.9<-c(0,0,lambda.9.3[i],0,0,0,lambda.9.7[i],0,lambda.9.10[i])
lambda.9.T<-sum(lambdas.9)
P.9.9<-exp(-lambda.9.T)
P.9<-(1-P.9.9)*(lambdas.9/lambda.9.T)
P.9.total<-c(P.9[1:8],P.9.9,P.9[9])


#Row 10 (inactivation)-----------------------
P.10.total<-c(rep(0,9),1)


matrix.all<-matrix(nrow=10,ncol=10)
matrix.all[1,]<-P.1.total
matrix.all[2,]<-P.2.total
matrix.all[3,]<-P.3.total
matrix.all[4,]<-P.4.total
matrix.all[5,]<-P.5.total
matrix.all[6,]<-P.6.total
matrix.all[7,]<-P.7.total
matrix.all[8,]<-P.8.total
matrix.all[9,]<-P.9.total
matrix.all[10,]<-P.10.total
Ptemp<-matrix.all

