data5 <- read.csv("https://raw.githubusercontent.com/Brandon-Bolte/PLSC309_F22/main/nov18survey.csv")
library(stargazer)
model1<-lm(hardtorelate~age,data=data5)
model2<-lm(hardtorelate~age + democrat + republican +
             gender + ideo5 + educ,data=data5)
stargazer(model1,model2,type="text")
predict(model2,
        newdata=data.frame(age=35,
                           gender=0,
                           educ=2,
                           ideo5=2,
                           republican=0,
                           democrat=1))
