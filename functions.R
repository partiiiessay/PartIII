#' I declare that this code is work done as part of the Part III Examination. I have read and
#' understood both the University¡¯s statement on the Definition of Academic Misconduct 
#' and the Faculty Guidelines on Plagiarism and Academic Misconduct and have abided by them.
#' This is the result of my own work, and except where explicitly stated otherwise, only
#' includes material undertaken since the publication of the list of essay titles, and includes
#' nothing which was performed in collaboration. No part of this code has been submitted, or
#' is concurrently being submitted, for any degree, diploma or similar qualification at any
#' university or similar institution.
#'
#' take the East of England data as an example
filePath <- "https://raw.githubusercontent.com/partiiiessay/PartIII/master/"
fileName <- "EastEngland.csv"
EE <- read.csv(paste0(filePath, fileName)) 
head(EE) # check that the dataframe has been successfully created
EE<-EE[EE$count!=0,]

require(psych)
n=sum(EE[,5])
newdat2<-matrix(NA,n,4,byrow=T)
newdat2<-c()
for(i in 1:nrow(EE)){
  newdat2<-rbind(matrix(EE[i,1:4],EE[i,5],4,byrow=T),newdat2)
}
dim(newdat2)
newdat2 # list all known cases in the contingency table individually
write.csv(newdat2,"newdat.csv")
dat<-read.table("newdat.csv",header = T,sep=",")
colnames(dat)
dat<-dat[,-1]
colnames(dat)<-c("L1","L2","L3","L4")

fa.parallel(dat,fa="pc",fm="ml",n.iter=100,show.legend = T) # the parallel analysis scree plot shows that 3 PCs need to be retained

pc<-principal(dat,nfactors = 3,scores = T,rotate = "none") # only consider PCs without the post-processings
k<-abs(pc$weights);k  # only focus on the effect, not the direction

MPC1=k[1,1]*dat$L1+k[2,1]*dat$L2+k[3,1]*dat$L3+k[4,1]*dat$L4
MPC2=k[1,2]*dat$L1+k[2,2]*dat$L2+k[3,2]*dat$L3+k[4,2]*dat$L4
MPC3=k[1,3]*dat$L1+k[2,3]*dat$L2+k[3,3]*dat$L3+k[4,3]*dat$L4
FA<-MPC1+MPC2+MPC3
fit.f<-glm(FA~MPC1+MPC2+MPC3+MPC1:MPC2+MPC1:MPC3+MPC2:MPC3,family = quasipoisson(link = "log")) # use quasipossion to avoid overdispersion
summary(fit.f)
p<-predict(fit.f,data.frame(MPC1=0,MPC2=0,MPC3=0),type="response");p
n/p # point estimate

p<-predict(fit.f,data.frame(MPC1=0,MPC2=0,MPC3=0),type="response",se.fit=T);p 
ci95_high<-n/(p$fit-p$se.fit*qt(1-0.05/2,n-1))
ci95_low<-n/(p$fit+p$se.fit*qt(1-0.05/2,n-1))
ci80_high<-n/(p$fit-p$se.fit*qt(1-0.2/2,n-1))
ci80_low<-n/(p$fit+p$se.fit*qt(1-0.2/2,n-1))
result <- data.frame(ci95_low, ci80_low, ci80_high, ci95_high)
names(result) <- c("95% Lower","80% Lower","80% Upper","95% Upper")
require(magrittr)
result %>% knitr::kable(digits=0) # confidence intervals
