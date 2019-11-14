rm(list=ls())
setwd("C:/sample files ds")
test= read.csv("test.csv")
train= read.csv("train.csv")
head(test,10)
str(test$v2)
class(train$target)



table(train$V2)

#.............................missingvalue analysis..........................#

missingval= data.frame(apply(train,2,function(x){sum(is.na(x))}))
missingval$variables= row.names(missingval)
row.names(missingval)= NULL
names(missingval)[1]= "values"
missingval= missingval[,c(2,1)]

# there is no missing values in the train dataset

#...............................outliers......................................#

#.........................For outlier boxplot visualisation....................#
library(ggplot2)

train$target= as.factor(train$target)
numeric_index= sapply(train,is.numeric)
numeric_var= train[,numeric_index]
cnames= colnames(numeric_var)

# code for boxplot visualisation for all variables compred with the target variable

for( i in 1:length(cnames)){
  assign(paste0("gn",i),ggplot(aes_string(x= "target",y= (cnames[i])), data= subset(train))+
                                 stat_boxplot(geom="errorbar",width=0.5)+
                                 geom_boxplot(outlier.colour="red",fill="grey",outlier.shape= 18,outlier.size= 1,notch= FALSE)+
                                 theme(legend.position= "bottom")+
                                 labs(y= cnames[i],x="target")+
                                 ggtitle(paste("boxplt")))
}
                                 
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn11,gn10,ncol=2)


#code for boxplot visualisation of 10 variables with target variable to avoid complexity cause Rstudio runs on ram

df= colnames(numeric_var[,1:10])


for( i in 1:length(df)){
  assign(paste0("gn",i),ggplot(aes_string(x= "target",y= (df[i])), data= subset(train))+
           stat_boxplot(geom="errorbar",width=0.5)+
           geom_boxplot(outlier.colour="red",fill="grey",outlier.shape= 18,outlier.size= 1,notch= FALSE)+
           theme(legend.position= "bottom")+
           labs(y= df[i],x="target")+
           ggtitle(paste("boxplot")))
}


gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)

# for next 10 variables

df= colnames(numeric_var[,1:10])

for( i in 1:length(df)){
  assign(paste0("gn",i),ggplot(aes_string(x= "target",y= (df[i])), data= subset(train))+
           stat_boxplot(geom="errorbar",width=0.5)+
           geom_boxplot(outlier.colour="red",fill="grey",outlier.shape= 18,outlier.size= 1,notch= FALSE)+
           theme(legend.position= "bottom")+
           labs(y= df[i],x="target")+
           ggtitle(paste("boxplot")))
}

gridExtra::grid.arrange(gn1,gn5,gn9,ncol=3)

#removing outliers and replacing it with median values

for( i in cnames){
  val= train[,i][train[,i] %in% boxplot.stats(train[,i])$out ]
  train[,i][train[,i] %in% val] = NA
}

for( i in cnames){
  train[,i][is.na(train[,i])]= median(train[,i],na.rm = T)
}


#.....................................Feature selection....................................#
# for overall visualisation of correaltion between variables

library(corrgram)
numericindex_new= sapply(train, is.numeric)
corrgram(train[,numericindex_new],order= F, upper.panel= panel.pie, text.panel= panel.txt,main= 'corelationplot')
      
# Taking sample data by using stratified sampling method

library(sampling)


train_new= train[sample(nrow(train),6000,replace=F),]


#...................................machine learning....................................#
#...................................logistic regression..................................#

logit= glm(target~ .,data= train_new, family= "binomial")


# im regretful to say that i tried a lot of times in the Rstudio to execute the programme, but my system got hangged every time 
when i execute the code. i have reduced my data as low as 6000 samples,eventhough my system not responded.reducing more than 
that is not a good practice in building the model i thought and stoped working in Rstudio. 






