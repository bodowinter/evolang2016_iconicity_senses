## Bodo Winter
## August 8, 2015
## Analysis of iconicity data for Evolang

## Analysis is split into two parts:
## (1) - Part of Speech differences
## (2) - Sensory modality differences

library(ggplot2)

## Load in data:

setwd("/Users/teeniematlock/Desktop/research/iconicity/evolang_structure_analysis/")
icon <- read.csv("iconicity_ratings_both.csv",stringsAsFactors=F)

## Make a POS column with grammatical class:

icon$POS_fivecat <- icon$POS
these_NA <- c("#N/A","Number","Interjection","Unclassified","Name")
icon[icon$POS %in% these_NA,]$POS_fivecat <- NA
these_grammatical <- c("Article","Conjunction","Determiner","Ex","Preposition","Pronoun","To","Not")
icon[icon$POS %in% these_grammatical,]$POS_fivecat <- "Grammatical"
icon$POS_fivecat <- factor(icon$POS_fivecat,levels=c("Verb","Adjective","Adverb","Noun","Grammatical"))

## Make a plot of this:

quartz("",9,6)
ggplot(icon[complete.cases(icon$POS_fivecat),],
	aes(x=POS_fivecat,y=Written,fill=POS_fivecat)) +
	geom_boxplot()

## Simple analysis:

summary(aov(Written ~ POS_fivecat,icon))

## Sensory modality strength and sense strength max:

icon$SenseStrength <- with(icon,VisualStrengthMean+HapticStrengthMean+AuditoryStrengthMean+
	OlfactoryStrengthMean+GustatoryStrengthMean)
icon$SenseStrengthMax <- with(icon,pmax(VisualStrengthMean,HapticStrengthMean,AuditoryStrengthMean,
	OlfactoryStrengthMean,GustatoryStrengthMean))

icon$DantzigSenseStrength <- with(icon,DantzigVisual+DantzigHaptic+DantzigAuditory+
	DantzigOlfactory+DantzigGustatory)
icon$DantzigSenseStrengthMax <- with(icon,pmax(DantzigVisual+DantzigHaptic+DantzigAuditory+
	DantzigOlfactory+DantzigGustatory))
	
## Aggregates:

aggregate(Written ~ DominantModality,icon,mean)
aggregate(Written ~ DantzigDominantModality,icon,mean)

## Analysis of this:

summary(lm(Written ~ SenseStrength,icon[complete.cases(icon$SenseStrength),]))
summary(lm(Written ~ SenseStrengthMax,icon[complete.cases(icon$SenseStrength),]))

summary(lm(Written ~ DantzigSenseStrength,icon[complete.cases(icon$DantzigSenseStrength),]))
summary(lm(Written ~ DantzigSenseStrengthMax,icon[complete.cases(icon$DantzigSenseStrength),]))

summary(lm(Written ~ DominantModality,icon))
summary(lm(Written ~ DantzigDominantModality,icon))

summary(lm(Written ~ VisualStrengthMean,icon[complete.cases(icon$VisualStrengthMean),]))
summary(lm(Written ~ HapticStrengthMean,icon[complete.cases(icon$HapticStrengthMean),]))
summary(lm(Written ~ AuditoryStrengthMean,icon[complete.cases(icon$AuditoryStrengthMean),]))
summary(lm(Written ~ OlfactoryStrengthMean,icon[complete.cases(icon$OlfactoryStrengthMean),]))
summary(lm(Written ~ GustatoryStrengthMean,icon[complete.cases(icon$GustatoryStrengthMean),]))

summary(lm(Written ~ DantzigVisual,icon[complete.cases(icon$DantzigVisual),]))
summary(lm(Written ~ DantzigHaptic,icon[complete.cases(icon$DantzigHaptic),]))
summary(lm(Written ~ DantzigAuditory,icon[complete.cases(icon$DantzigAuditory),]))
summary(lm(Written ~ DantzigOlfactory,icon[complete.cases(icon$DantzigOlfactory),]))
summary(lm(Written ~ DantzigGustatory,icon[complete.cases(icon$DantzigGustatory),]))

## Define bimodality functions ala Freeman & Ambady et al. (2010):

# third moment from Jack Vevea's slides
# (calculating the mean and the sd before the main equation saves processing time)
skew = function(x){
	N=length(x)
	themean=mean(x)
	thesd=sd(x)
	(N/((N-1)*(N-2)))*sum(((x-themean)^3)/(thesd^3))
	}
# fourth moment from Jack Vevea's slides
# (calculating the mean and the sd before the main equation saves processing time)
kurt = function(x){
	N=length(x)
	themean=mean(x)
	thesd=sd(x)
	((N*(N+1))/((N-1)*(N-2)*(N-3)))*
		sum(((x-themean)^4)/(thesd^4))-
		((3*((N-1)^2))/((N-2)*(N-3)))
	}

# bimodality coefficient function that uses skew and kurtosis functions
bimodality=function(myvector){
	N=length(myvector)
	numerator=skew(myvector)^2
	subnumerator=3*(N-1)^2
	subdenominator=(N-2)*(N-3)
	denominator=kurt(myvector)+subnumerator/subdenominator
	b=numerator/denominator
	return(b)
	}

## Bimodality stats:

library(diptest)
dip.test(icon[icon$POS_fivecat=="Verb",]$Written)
dip.test(icon[icon$POS_fivecat=="Noun",]$Written)
dip.test(icon[icon$POS_fivecat=="Adjective",]$Written)
dip.test(icon[icon$POS_fivecat=="Adverb",]$Written)
dip.test(icon[icon$POS_fivecat=="Grammatical",]$Written)
bimodality(icon$Written)			# no evidence for bimodality



