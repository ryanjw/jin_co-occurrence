
library(vegan)
library(reshape)


# read in your dataset here
dataset<-read.csv(file.choose())


trts<-as.vector(unique(#add the variable determining the treatments here))



results<-data.frame()

options(warnings=-1)

for(a in 1:length(trts)){
	
	# replace #treatment# with your actual variable name here that determines treatments
	temp<-subset(dataset, #treatment#==trts[a])
	
	# change #X# to the column where your first variable starts
	for(b in #X#:(dim(temp)[2]-1)){
		#every species will be compared to every other species, so there has to be another loop that iterates down the rest of the columns
		for(c in (b+1):(dim(temp)[2])){
			
			#summing the abundances of species of the columns that will be compared
			species1.ab<-sum(temp[,b])
			species2.ab<-sum(temp[,c])
			#if the column is all 0's no co-occurrence will be performed
			if(species1.ab >0 & species2.ab >0){
				test<-cor.test(temp[,b],temp[,c],method="spearman",na.action=na.rm)
				rho<-test$estimate
				p.value<-test$p.value
			}
			
			if(species1.ab <=0 | species2.ab <= 0){
				rho<-0
				p.value<-1
			}	
			
			new.row<-c(trts[a],names(temp)[b],names(temp)[c],rho,p.value,species1.ab,species2.ab)
			results<-rbind(results,new.row)			
			print(paste(c/dim(temp)[2]*100,"% Done of Treatment ",trts[a],sep=""))
		}
		
	}	
}

names(results)<-c("trt","taxa1","taxa2","rho","p.value","ab1","ab2")

