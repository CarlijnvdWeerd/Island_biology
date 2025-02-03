### File for new package installations 
install.packages("renv")
renv::init()
renv::restore()


install.packages('ape',dependencies = TRUE)
install.packages('phytools',dependencies = TRUE)
install.packages('DAISIE',dependencies = TRUE)
install.packages('DAISIEprep',dependencies = TRUE)
install.packages("paran")
install.packages("splits", repos="http://R-Forge.R-project.org")
install.packages("tidyverse")
install.packages("phytools")
install.packages("maps")
install.packages("ggplot2")

library(ape)
library(phytools)
library(splits)
library(ggplot2)

cyanistes <- read.nexus('C:/Users/carli/_projects/Island Bio/Cyanistes.tre')
cyanistes

cyanistes$tip.label
plot(cyanistes)
plot(cyanistes,cex=0.4)
plot(ladderize(cyanistes),cex=0.4,no.margin=TRUE)
is.ultrametric(cyanistes)

plot(ladderize(cyanistes),cex=0.4,no.margin=TRUE)
nodelabels(cex=0.3,frame='circle',bg='grey')

plot(ladderize(cyanistes),type='fan',cex=0.3,no.margin=TRUE,)
subtreeplot(ladderize(cyanistes),cex=0.4)

cyanistes_ingroup<-drop.tip(cyanistes,c("Parus_major_major_KP759174.1",
                                        "Parus_major_DQ792786.1", 
                                        "Parus_major_DQ792787.1",
                                        "Parus_major_EU167009.1", 
                                        "Parus_major_KJ456375.1"))
par(mfrow=c(1,1))
plot(ladderize(cyanistes_ingroup),cex=0.4,no.margin=TRUE)

island_data<-read.csv("C:/Users/carli/_projects/Island Bio/Cyanistes_distribution.csv",header=T)
View(island_data)

island_d<-as.data.frame(island_data$Distribution)
taxa<-as.data.frame(island_data$Species)
islands<-as.data.frame(island_d[match(cyanistes$tip.label,taxa[,1]),])
islands<-t(islands)
islands<-as.character(islands)
names(islands)<-cyanistes$tip.label

set.seed(3)
cyanistes_simmap<-make.simmap(cyanistes,islands,model="ER",nsim=1000)

pd<-summary(cyanistes_simmap,plot=FALSE)

par(oma=c(0,0,0,0))
cols<-setNames(palette()[1:length(unique(islands))],sort(unique(islands)))
plot(cyanistes_simmap[[1]],cols,fsize=0.8)
add.simmap.legend(colors=cols,prompt=FALSE,x=0.9*par()$usr[1],
                  y=25,fsize=0.8)

nodelabels(pie=pd$ace,piecol=cols,cex=0.3)

tiplabels(pie=to.matrix(islands,sort(unique(islands))),piecol=cols,cex=0.1)

ggsave("C:/Users/carli/_projects/Island Bio/Cyanistes_simmap.pdf",width=8,height=8)

cyanistes_gmyc <- gmyc(cyanistes_ingroup)
summary(cyanistes_gmyc)
spec.list(cyanistes_gmyc)
plot(cyanistes_gmyc)
