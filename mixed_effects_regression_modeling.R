library(xlsx)
library(lme4)
corr<-read.xlsx("partone.xlsx",sheetIndex = 1)
#plot correlation matrix
corrplot.mixed(cor(corr),order="hclust", tl.pos = "lt",diag = "u",lower.col = "black",upper = "ellipse",tl.cex=1.5,cl.cex=1.5,number.cex=1.5)

df<-read.xlsx("partone.xlsx",sheetIndex = 2)
df<-within(df,{
  sentencetype<-factor(sentencetype)
  animacy<-factor(animacy)
  givenness<-factor(givenness)
  definiteness<-factor(definiteness)
  parallelism<-factor(parallelism)
  pronominality<-factor(pronominality)
  genre<-factor(genre)
  verb<-factor(verb)
  verbsense<-factor(verbsense)
  noun<-factor(noun)
  z.lengthratio<-(lengthratio1-mean(lengthratio1))/sd(lengthratio1)
})

# the full model 
mod_fit1<-glmer(sentencetype~animacy+givenness+parallelism+pronominality+z.lengthratio+(1|noun)+(1+animacy+givenness+parallelism+pronominality+z.lengthratio|verb)+(1+animacy+givenness+parallelism+pronominality+z.lengthratio|verbsense)+(1|genre), data=df, family = "binomial",glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
# drop random effects according to principal component analyses
summary(rePCA(mod_fit1)) #result of PCA
VarCorr(mod_fit1) #random effect of model
# the second model
mod_fit2<-glmer(sentencetype~animacy+givenness+parallelism+pronominality+z.lengthratio+(1+animacy+givenness+parallelism|verb)+(1+animacy+givenness+parallelism|verbsense), data=df, family = "binomial",glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
# the third model
mod_fit3<-glmer(sentencetype~animacy+givenness+parallelism+pronominality+z.lengthratio+(1+animacy+givenness|verb)+(1+animacy+givenness|verbsense), data=df, family = "binomial",glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
...
mod_fit5<-glmer(sentencetype~animacy+givenness+parallelism+z.lengthratio+(1|verbsense), data=df, family = "binomial",glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
# drop fixed effects according to likelihood ratio test
library(car)
Anova(mod_fit5,type = "III",test.statistic = "Chisq")

# the final model
mod_fit6<-glmer(sentencetype~givenness+parallelism+z.lengthratio+(1|verbsense), data=df, family = "binomial",glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(mod_fit6)

# the intercept only model
mod_fit7<-glmer(sentencetype~1+(1|verbsense), data=df, family = "binomial",glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# compare the final model and the intercept only model
anova(mod_fit7,mod_fit6,method="LR")

#compute Concordance C of a mixed effect model
predictions.num <- fitted(mod_fit6)
write.csv(predictions.num,"predictions.csv") #paste sentencetype to this chart
predictions<-read.csv("predictions.csv")
library("InformationValue")
Concordance(predictions$actual,predictions$predict)

#compute R-square
library(MuMIn)
r.squaredGLMM(mod_fit6) 

# classification accuracy of the final model
predictions.num <- fitted(mod_fit6)   # how to compute classification accuracy
predictions.cat <- ifelse(predictions.num>=0.5, "svo", "ba")
table(df$sentencetype, predictions.cat)
(757+704)/length(df$sentencetype)

# model validation
library(caret)
for (i in 1:100){
  Train <- createDataPartition(df$sentencetype, p=0.75, list=FALSE)
  training <- df[Train, ]
  testing <- df[-Train, ]
  mod_fit <- glmer(sentencetype ~ givenness+parallelism+z.lengthratio+(1|verbsense), data=training, family="binomial")
  pred = predict(mod_fit, newdata=testing,allow.new.levels = TRUE)
  predictions.cat=ifelse(pred>0.5,"svo","ba")
  predictions.cat=as.factor(predictions.cat)
  result=confusionMatrix(data=predictions.cat, testing$sentencetype)
  print(result$overall[1])
}

# the importance of each variable in predicting syntactic choice
Anova(mod_fit6,type = "III")
library(ggplot2)
library(dplyr)
library(forcats)
df<-data.frame('name'=c('parallelism','givenness','LengthRatio'),'value'=c(130.1,93.46,90.17)) # increase in deviance if factor removed
df %>%
  mutate(name = fct_reorder(name, value)) %>%
  ggplot(aes(x=name, y=value,fill=name)) +
  geom_bar(stat="identity", alpha=.6, width=.6) +
  coord_flip()+
  ylab('Chisq')+
  xlab('')+
  theme_bw()+
  scale_fill_brewer(palette = 'Set2')+
  theme(axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))