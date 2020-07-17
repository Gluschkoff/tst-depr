# ANALYSIS: predictor=dichotomous low/normal TST (TST=total testosterone)-------------------------------------------------------
## men, outcome=depression sum score, unadjusted and adjusted models
summend <- svyglm(depsum ~ tstdikm, data=dffull, design=desmen, family = "gaussian") 
summendadj <- svyglm(depsum ~ tstdikm+ age+ bmi +alc+ smo + pa, data=dffull, design=desmen, family = "gaussian") 
summ(summendadj, confint=T, scale=T, transform.response = TRUE)
## women, outcome=depression sum score, unadjusted and adjusted models
sumwd <- svyglm(depsum ~ tstdikw, data=dffull, design=deswomen, family = "gaussian") 
sumwdadj <- svyglm(depsum ~ tstdikw+ age+ bmi +alc+ smo + pa +preg, data=dffull, design=deswomen, family = "gaussian") 
summ(sumwdadj, confint=T, scale=T, transform.response = TRUE)
## men, outcome=depressive symptoms, unadjusted and adjusted models (for adjusted, remove #)
di1 <- svyglm(depd1 ~ tstdikm #+ age+ bmi +alc+ smo + pa
,data=dffull, design=desmen, family = "quasibinomial")
di2 <- svyglm(depd2 ~  tstdikm #+ age+ bmi +alc+ smo + pa 
,data=dffull, design=desmen, family = "quasibinomial")
di3 <- svyglm(depd3 ~  tstdikm #+ age+ bmi +alc+ smo + pa
,data=dffull, design=desmen, family = "quasibinomial")
di4 <- svyglm(depd4 ~  tstdikm  #+ age+ bmi +alc+ smo + pa 
,data=dffull, design=desmen, family = "quasibinomial")
di5 <- svyglm(depd5 ~  tstdikm #+ age+ bmi +alc+ smo + pa 
 ,data=dffull, design=desmen, family = "quasibinomial")
di6 <- svyglm(depd6 ~  tstdikm #+ age+ bmi +alc+ smo + pa 
 ,data=dffull, design=desmen, family = "quasibinomial")
di7 <- svyglm(depd7 ~  tstdikm #+ age+ bmi +alc+ smo + pa 
,data=dffull, design=desmen, family = "quasibinomial")
di8 <- svyglm(depd8 ~  tstdikm #+ age+ bmi +alc+ smo + pa 
,data=dffull, design=desmen, family = "quasibinomial")
di9 <- svyglm(depd9 ~  tstdikm #+ age+ bmi +alc+ smo + pa 
,data=dffull, design=desmen, family = "quasibinomial")
## women, outcome=depressive symptoms, unadjusted and adjusted models (for adjusted, remove #)
diw1 <- svyglm(depd1 ~ tstdikw #+ age+ bmi +alc+ smo + pa + preg 
              ,data=dffull, design=deswomen, family = "quasibinomial")
diw2 <- svyglm(depd2 ~  tstdikw #+ age+ bmi +alc+ smo + pa + preg
              ,data=dffull, design=deswomen, family = "quasibinomial")
diw3 <- svyglm(depd3 ~  tstdikw #+ age+ bmi +alc+ smo + pa + preg 
              ,data=dffull, design=deswomen, family = "quasibinomial")
diw4 <- svyglm(depd4 ~  tstdikw #+ age+ bmi +alc+ smo + pa + preg 
              ,data=dffull, design=deswomen, family = "quasibinomial")
diw5 <- svyglm(depd5 ~  tstdikw #+ age+ bmi +alc+ smo + pa + preg 
              ,data=dffull, design=deswomen, family = "quasibinomial")
diw6 <- svyglm(depd6 ~  tstdikw #+ age+ bmi +alc+ smo + pa+ preg 
              ,data=dffull, design=deswomen, family = "quasibinomial")
diw7 <- svyglm(depd7 ~  tstdikw #+ age+ bmi +alc+ smo + pa+ preg 
              ,data=dffull, design=deswomen, family = "quasibinomial")
diw8 <- svyglm(depd8 ~  tstdikw #+ age+ bmi +alc+ smo + pa+ preg 
              ,data=dffull, design=deswomen, family = "quasibinomial")
diw9 <- svyglm(depd9 ~  tstdikw #+ age+ bmi +alc+ smo + pa+ preg 
              ,data=dffull, design=deswomen, family = "quasibinomial")
              
# ANALYSIS: predictor=TST splines-------------------------------------------------------
## men, outcome=depression sum score, unadjusted and adjusted models, without transformation and log transformed TST as predictor
# (for adjusted models, remove #)
summen <- svyglm(depsum ~ ns(tst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "gaussian") 
regTermTest(summen, ~ns(tst, df=3))
summenlog <- svyglm(depsum ~ ns(logtst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "gaussian")
regTermTest(summenlog, ~ns(logtst, df=3))
## women, outcome=depression sum score, unadjusted model and adjusted models, without transformation and log transformed TST as predictor
# (for adjusted models, remove #)
sumw <- svyglm(depsum ~ ns(tst, df=3) #+ age+ +alc + bmi + smo + pa +preg
, data=dffull, design=deswomen, family = "gaussian")
regTermTest(sumw, ~ns(tst, df=3))
sumwlog <- svyglm(depsum ~ ns(logtst, df=3) #+ age+ +alc + bmi + smo + pa +preg
, data=dffull, design=deswomen, family = "gaussian") 
regTermTest(sumwlog, ~ns(logtst, df=3))
## men, outcome=depressive symptoms, unadjusted and adjusted models, without transformation and log transformed TST as predictor 
# (for adjusted models, remove #)
# TST without transformation, men
spl1 <- svyglm(depd1 ~ ns(tst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl2 <- svyglm(depd2 ~ ns(tst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl3 <- svyglm(depd3 ~ ns(tst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl4 <- svyglm(depd4 ~ ns(tst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl5 <- svyglm(depd5 ~ ns(tst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl6 <- svyglm(depd6 ~ ns(tst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl7 <- svyglm(depd7 ~ ns(tst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl8 <- svyglm(depd8 ~ ns(tst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl9 <- svyglm(depd9 ~ ns(tst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
# log transformed TST, men
spl1log <- svyglm(depd1 ~ ns(logtst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl2log <- svyglm(depd2 ~ ns(logtst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl3log <- svyglm(depd3 ~ ns(logtst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl4log <- svyglm(depd4 ~ ns(logtst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl5log <- svyglm(depd5 ~ ns(logtst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl6log <- svyglm(depd6 ~ ns(logtst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl7log <- svyglm(depd7 ~ ns(logtst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl8log <- svyglm(depd8 ~ ns(logtst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")
spl9log <- svyglm(depd9 ~ ns(logtst, df=3) #+ age+ +alc + bmi + smo + pa
, data=dffull, design=desmen, family = "quasibinomial")

## women, outcome=depressive symptoms, unadjusted models and adjusted models, without transformation and log transformed 
# (for adjusted models, remove #)
## TST without transformation, women
spl1w <- svyglm(depd1 ~ ns(tst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl2w <- svyglm(depd2 ~ ns(tst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl3w <- svyglm(depd3 ~ ns(tst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl4w <- svyglm(depd4 ~ ns(tst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl5w <- svyglm(depd5 ~ ns(tst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl6w <- svyglm(depd6 ~ ns(tst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl7w <- svyglm(depd7 ~ ns(tst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl8w <- svyglm(depd8 ~ ns(tst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl9w <- svyglm(depd9 ~ ns(tst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
## log transformed TST, women
spl1wlog <- svyglm(depd1 ~ ns(logtst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl2wlog <- svyglm(depd2 ~ ns(logtst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl3wlog <- svyglm(depd3 ~ ns(logtst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl4wlog <- svyglm(depd4 ~ ns(logtst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl5wlog <- svyglm(depd5 ~ ns(logtst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl6wlog <- svyglm(depd6 ~ ns(logtst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl7wlog <- svyglm(depd7 ~ ns(logtst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl8wlog <- svyglm(depd8 ~ ns(logtst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")
spl9wlog <- svyglm(depd9 ~ ns(logtst, df=3) #+age+ bmi +alc+ smo + pa +preg
, data=dffull, design=deswomen, family = "quasibinomial")

# test for the significance of TST, example
regTermTest(spl1, ~ns(tst, df=3)) # men
regTermTest(spl1log, ~ns(logtst, df=3)) # men, logtst
regTermTest(spl1w, ~ns(tst, df=3)) # women
regTermTest(spl1wlog, ~ns(logtst, df=3)) # women, logtst
