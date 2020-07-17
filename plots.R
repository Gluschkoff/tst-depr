#####################################
## do data prep and analysis first ##
#####################################

# dichotomous TST as predictor, outcome depressive symptoms -------------------------------------------
mendik <- plot_coefs(di1, di2, di3, di4, di5, di6, di7, di8, di9, exp=T,
          colors=c("#59C7EB", "#FEA090", "#9AA0A7", "#077187", "#0A9086", "#3E5496", "#E0607E", "#8E2043", "#EFDC60"), 
          omit.coefs = c("(Intercept)","age", "bmi", "smo1", "alc1", "pa1", "pa2"),  
          legend.title = "Symptom",
          model.names= c("Little interest", "Feeling down", "Sleep problems", "Feeling tired", "Appetite problems",
                         "Feeling bad", "Trouble concentrating", "Psychomotor change", "Suicidality")) +
          theme(axis.text.y=element_blank()) +
          ggtitle("Men") +
          coord_cartesian(xlim=c(0.5,2)) +
          xlab("OR")
womendik <- plot_coefs(diw1, diw2, diw3, diw4, diw5, diw6, diw7, diw8, diw9,exp=T, 
          colors=c("#59C7EB", "#FEA090", "#9AA0A7", "#077187", "#0A9086", "#3E5496", "#E0607E", "#8E2043", "#EFDC60"), 
          omit.coefs = c("(Intercept)","age", "bmi", "smo1", "alc1", "pa1", "pa2", "preg1"),
          legend.title = "Symptom",
          model.names= c("Little interest", "Feeling down", "Sleep problems", "Feeling tired", "Appetite problems",
                         "Feeling bad", "Trouble concentrating", "Psychomotor change", "Suicidality")) +
          theme(axis.text.y=element_blank()) +
          ggtitle("Women") +
          coord_cartesian(xlim=c(0.5,2)) +
          xlab("OR")
# combine the plots (men and women)
ggarrange(mendik, womendik, common.legend = TRUE, legend = "right")

# plots for TST splines -----------------------------------------------------------------------------
# layout for the plots showing both the sum score and specific symptoms
lay <- rbind(c(1,1,2,3,4),
              c(1,1,5,6,7),
              c(1,1,8,9,10))
# men ------------------------------------------------------------------------------------------------------------------
# data frame for making adjusted predictions
ndatamen <- expand.grid(tst=seq(50, 1100, 10),  pa=0, alc=0, age=45.51,smo=0, bmi=29.04)  
cat <- c("alc", "pa", "smo")
ndatamen[,cat] <- lapply(ndatamen[,cat], factor) 

# sum score: predicted probabilities and 95% confidence intervals, men
prsummen <- as.data.frame(round(predict(summen, newdata=ndatamen, type="response"),2))
prsummen$upper <- prsummen$response+ prsummen $SE*1.96
prsummen$lower <- prsummen$response- prsummen $SE*1.96
# data for ggplot
avedsummen <- cbind(prsummen, ndatamen)

# specific symptoms: predicted probabilities and 95% confidence intervals, men
probs1 <- as.data.frame(predict(spl1, newdata=ndatamen, type="response")) 
probs1$upper <- probs1$response+probs1$SE*1.96
probs1$lower <- probs1$response-probs1$SE*1.96
probs2 <- as.data.frame(predict(spl2, newdata=ndatamen, type="response")) 
probs2$upper <- probs2$response+probs2$SE*1.96
probs2$lower <- probs2$response-probs2$SE*1.96
probs3 <- as.data.frame(predict(spl3, newdata=ndatamen, type="response")) 
probs3$upper <- probs3$response+probs3$SE*1.96
probs3$lower <- probs3$response-probs3$SE*1.96
probs4 <- as.data.frame(predict(spl4, newdata=ndatamen, type="response")) 
probs4$upper <- probs4$response+probs4$SE*1.96
probs4$lower <- probs4$response-probs4$SE*1.96
probs5 <- as.data.frame(predict(spl5, newdata=ndatamen, type="response")) 
probs5$upper <- probs5$response+probs5$SE*1.96
probs5$lower <- probs5$response-probs5$SE*1.96
probs6 <- as.data.frame(predict(spl6, newdata=ndatamen, type="response")) 
probs6$upper <- probs6$response+probs6$SE*1.96
probs6$lower <- probs6$response-probs6$SE*1.96
probs7 <- as.data.frame(predict(spl7, newdata=ndatamen, type="response")) 
probs7$upper <- probs7$response+probs7$SE*1.96
probs7$lower <- probs7$response-probs7$SE*1.96
probs8 <- as.data.frame(predict(spl8, newdata=ndatamen, type="response")) 
probs8$upper <- probs8$response+probs8$SE*1.96
probs8$lower <- probs8$response-probs8$SE*1.96
probs9 <- as.data.frame(predict(spl9, newdata=ndatamen, type="response"))
probs9$upper <- probs9$response+probs9$SE*1.96
probs9$lower <- probs9$response-probs9$SE*1.96

# data for ggplot
avedata1 <- cbind(probs1, ndatamen)
avedata2 <- cbind(probs2, ndatamen)
avedata3 <- cbind(probs3, ndatamen)
avedata4 <- cbind(probs4, ndatamen)
avedata5 <- cbind(probs5, ndatamen)
avedata6 <- cbind(probs6, ndatamen)
avedata7 <- cbind(probs7, ndatamen)
avedata8 <- cbind(probs8, ndatamen)
avedata9 <- cbind(probs9, ndatamen)

# plot for sum score, men
sumpmen<-ggplot(avedsummen, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="Testosterone, total (ng/dL)", breaks=seq(0,1100, 150), limits=c(0,1100)) +
  ylim(0, 7) +
  theme(legend.position = "none")+
  ylab("Depression sum score") +
  ggtitle("Depression sum score (range 0 to 27)") +
  geom_hline(yintercept = 4.11, linetype="dashed")

# plots for specific symptoms, men
pave1<-ggplot(avedata1, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="", breaks=seq(0,1100, 150), limits=c(0,1100)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("probability of symptom") +
  ggtitle("Little interest ") +
  geom_hline(yintercept=prop.table(svytable(~depd1, design=desmen))[2], linetype="dashed")
pave2<-ggplot(avedata2, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="", breaks=seq(0,1100, 150), limits=c(0,1100)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("") +
  ggtitle("Feeling down") +
  geom_hline(yintercept=prop.table(svytable(~depd2, design=desmen))[2], linetype="dashed")
pave3<-ggplot(avedata3, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  geom_line(data = subset(avedata3, tst> 769), colour = "red", size=1) +
  scale_x_continuous(name="", breaks=seq(0,1100, 150), limits=c(0,1100)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("") +
  ggtitle("Sleep problems*") +
  geom_hline(yintercept=prop.table(svytable(~depd3, design=desmen))[2], linetype="dashed")
pave4<-ggplot(avedata4, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  geom_line(data = subset(avedata4, tst> 999), colour = "red", size=1) +
  scale_x_continuous(name="", breaks=seq(0,1100, 150), limits=c(0,1100)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("probability of symptom") +
  ggtitle("Feeling tired†") +
  geom_hline(yintercept=prop.table(svytable(~depd4, design=desmen))[2], linetype="dashed")
pave5<-ggplot(avedata5, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  geom_line(data = subset(avedata5, tst> 30 & tst < 151), colour = "red", size=1) +
  scale_x_continuous(name="", breaks=seq(0,1100, 150), limits=c(0,1100)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("") +
  ggtitle("Appetite problems†") +
  geom_hline(yintercept=prop.table(svytable(~depd5, design=desmen))[2], linetype="dashed")
pave6<-ggplot(avedata6, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="", breaks=seq(0,1100, 150), limits=c(0,1100)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("") +
  ggtitle("Feeling bad") +
  geom_hline(yintercept=prop.table(svytable(~depd6, design=desmen))[2], linetype="dashed")
pave7<-ggplot(avedata7, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="Testosterone, total (ng/dL)", breaks=seq(0,1100, 150), limits=c(0,1100)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("probability of symptom") +
  ggtitle("Trouble concentrating") +
  geom_hline(yintercept=prop.table(svytable(~depd7, design=desmen))[2], linetype="dashed")
pave8<-ggplot(avedata8, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="Testosterone, total (ng/dL)", breaks=seq(0,1100, 150), limits=c(0,1100)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("") +
  ggtitle("Psychomotor change") +
  geom_hline(yintercept=prop.table(svytable(~depd8, design=desmen))[2], linetype="dashed")
pave9<-ggplot(avedata9, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="Testosterone, total (ng/dL)", breaks=seq(0,1100, 150), limits=c(0,1100)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("") +
  ggtitle("Suicidality") +
  geom_hline(yintercept=prop.table(svytable(~depd9, design=desmen))[2], linetype="dashed")

# combine sum score and symptoms into a single plot, men
grid.arrange(sumpmen, pave1, pave2, pave3, pave4, pave5, pave6, pave7, pave8, pave9, layout_matrix = lay)

# women ----------------------------------------------------------------------------------------------------------------
# data frame for making adjusted predictions
ndatawomen <- expand.grid(tst=seq(0, 70, 5),  pa=0, alc=0, age=47.88,smo=0, bmi=29.73, preg=0)  
catw <- c("alc", "pa", "smo", "preg")
ndatawomen[,catw] <- lapply(ndatawomen[,catw], factor)

# sum score: predicted probabilities and 95% confidence intervals, women
prsumw <- as.data.frame(round(predict(sumw, newdata=ndatawomen, type="response"),2))
prsumw$upper <- prsumw$response+ prsumw$SE*1.96
prsumw$lower <- prsumw$response- prsumw$SE*1.96
# data for ggplot
avedsumwomen <- cbind(prsumw, ndatawomen)

# specific symptoms: predicted probabilities and 95% confidence intervals, women
probs1w <- as.data.frame(predict(spl1w, newdata=ndatawomen, type="response")) 
probs1w$upper <- probs1w$response+probs1w$SE*1.96
probs1w$lower <- probs1w$response-probs1w$SE*1.96
probs2w <- as.data.frame(predict(spl2w, newdata=ndatawomen, type="response")) 
probs2w$upper <- probs2w$response+probs2w$SE*1.96
probs2w$lower <- probs2w$response-probs2w$SE*1.96
probs3w <- as.data.frame(predict(spl3w, newdata=ndatawomen, type="response")) 
probs3w$upper <- probs3w$response+probs3w$SE*1.96
probs3w$lower <- probs3w$response-probs3w$SE*1.96
probs4w <- as.data.frame(predict(spl4w, newdata=ndatawomen, type="response")) 
probs4w$upper <- probs4w$response+probs4w$SE*1.96
probs4w$lower <- probs4w$response-probs4w$SE*1.96
probs5w <- as.data.frame(predict(spl5w, newdata=ndatawomen, type="response")) 
probs5w$upper <- probs5w$response+probs5w$SE*1.96
probs5w$lower <- probs5w$response-probs5w$SE*1.96
probs6w <- as.data.frame(predict(spl6w, newdata=ndatawomen, type="response")) 
probs6w$upper <- probs6w$response+probs6w$SE*1.96
probs6w$lower <- probs6w$response-probs6w$SE*1.96
probs7w <- as.data.frame(predict(spl7w, newdata=ndatawomen, type="response")) 
probs7w$upper <- probs7w$response+probs7w$SE*1.96
probs7w$lower <- probs7w$response-probs7w$SE*1.96
probs8w <- as.data.frame(predict(spl8w, newdata=ndatawomen, type="response")) 
probs8w$upper <- probs8w$response+probs8w$SE*1.96
probs8w$lower <- probs8w$response-probs8w$SE*1.96
probs9w <- as.data.frame(predict(spl9w, newdata=ndatawomen, type="response"))
probs9w$upper <- probs9w$response+probs9w$SE*1.96
probs9w$lower <- probs9w$response-probs9w$SE*1.96

# data for ggplot
avedata1w <- cbind(probs1w, ndatawomen)
avedata2w <- cbind(probs2w, ndatawomen)
avedata3w <- cbind(probs3w, ndatawomen)
avedata4w <- cbind(probs4w, ndatawomen)
avedata5w <- cbind(probs5w, ndatawomen)
avedata6w <- cbind(probs6w, ndatawomen)
avedata7w <- cbind(probs7w, ndatawomen)
avedata8w <- cbind(probs8w, ndatawomen)
avedata9w <- cbind(probs9w, ndatawomen)

# plot for sum score, women
sumpwomen<-ggplot(avedsumwomen, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="Testosterone, total (ng/dL)", breaks=seq(0,70,10), limits=c(0,70)) +
  ylim(0, 7) +  theme(legend.position = "none")+
  ylab("Depression sum score") +
  ggtitle("Depression sum score (range 0 to 27)") +
  geom_hline(yintercept = 4.86, linetype="dashed")

# plots for specific symptoms, women
pave1w<-ggplot(avedata1w, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="", breaks=seq(0,70, 10), limits=c(0,70)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("probability of symptom") +
  ggtitle("Little interest ") +
  geom_hline(yintercept=(prop.table(svytable(~depd1, design=deswomen))[2]), linetype="dashed")
pave2w<-ggplot(avedata2w, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="", breaks=seq(0,70, 10), limits=c(0,70)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("") +
  ggtitle("Feeling down") +
  geom_hline(yintercept=(prop.table(svytable(~depd2, design=deswomen))[2]), linetype="dashed")
pave3w<-ggplot(avedata3w, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="", breaks=seq(0,70, 10), limits=c(0,70)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("") +
  ggtitle("Sleep problems") +
  geom_hline(yintercept=(prop.table(svytable(~depd3, design=deswomen))[2]), linetype="dashed")
pave4w<-ggplot(avedata4w, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="", breaks=seq(0,70, 10), limits=c(0,70)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("probability of symptom") +
  ggtitle("Feeling tired")  +
  geom_hline(yintercept=(prop.table(svytable(~depd4, design=deswomen))[2]), linetype="dashed")
pave5w<-ggplot(avedata5w, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="", breaks=seq(0,70, 10), limits=c(0,70)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("") +
  ggtitle("Appetite problems*") +
  geom_hline(yintercept=(prop.table(svytable(~depd5, design=deswomen))[2]), linetype="dashed")
pave6w<-ggplot(avedata6w, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="", breaks=seq(0,70, 10), limits=c(0,70)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("") +
  ggtitle("Feeling bad") +
  geom_hline(yintercept=(prop.table(svytable(~depd6, design=deswomen))[2]), linetype="dashed")
pave7w<-ggplot(avedata7w, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="Testosterone, total (ng/dL)", breaks=seq(0,70, 10), limits=c(0,70)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("probability of symptom") +
  ggtitle("Trouble concentrating") +
  geom_hline(yintercept=(prop.table(svytable(~depd7, design=deswomen))[2]), linetype="dashed")
pave8w<-ggplot(avedata8w, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="Testosterone, total (ng/dL)", breaks=seq(0,70, 10), limits=c(0,70)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("") +
  ggtitle("Psychomotor change") +
  geom_hline(yintercept=(prop.table(svytable(~depd8, design=deswomen))[2]), linetype="dashed")
pave9w<-ggplot(avedata9w, aes(y=response, x=tst)) +
  geom_ribbon(aes(ymin=pmax(0,lower), ymax=pmin(0.5, upper), fill = "band"), alpha = 0.3) +
  geom_line(aes(x=tst, y=response)) +
  scale_x_continuous(name="Testosterone, total (ng/dL)", breaks=seq(0,70, 10), limits=c(0,70)) +
  ylim(0, 0.5) +
  theme(legend.position = "none")+
  ylab("") +
  ggtitle("Suicidality") +
  geom_hline(yintercept=(prop.table(svytable(~depd9, design=deswomen))[2]), linetype="dashed")

# combine sum score and symptoms into a single plot, women 
grid.arrange(sumpwomen, pave1w, pave2w, pave3w, pave4w, pave5w, pave6w, pave7w, pave8w, pave9w, layout_matrix = lay)


