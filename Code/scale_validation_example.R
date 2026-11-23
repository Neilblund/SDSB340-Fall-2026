library(tidyverse)
## Data -------
# codebook at:https://dataverse.harvard.edu/file.xhtml?fileId=11081573&version=1.0
datafile<-'https://github.com/Neilblund/SDSB340-Spring-2026/raw/refs/heads/master/Data/polarization3.rds'
dat3<-readRDS(url(datafile))

# selecting the covariates from my model and then dropping any rows with 
# missing data:
dat_select<-dat3|>
  select(apstot9, age, ba, white, hisplat, male, inc, rpidex, ridex, wbias, ANESdnorm)|>
  drop_na()

# association of affective polarization measure with:
# 1. partisan identification extremity:
m1<-lm(rpidex ~ apstot9+age+ba+white+hisplat+male+inc, data=dat_select)
# 2. ideoloigcal identification extremity:
m2<-lm(ridex ~ apstot9+age+ba+white+hisplat+male+inc, data=dat_select)
# 3. warmth bias:
m3<-lm(wbias ~ apstot9+age+ba+white+hisplat+male+inc, data=dat_select)
# 4. Democratic norms:
m4<-lm(ANESdnorm ~ apstot9+age+ba+white+hisplat+male+inc, data=dat_select)


library(modelsummary)

mlist<-list("Partisan Extremity" = m1,
            "Ideological Extremity"= m2,
            "Warmth Bias"= m3 ,
            "Democratic Norms Scale" = m4
            )

# Formatted model summary:
modelsummary(
  mlist,
  coef_map = c(
    "(Intercept)" = "Intercept",
    "apstot9" = "Affective Partisanship Index",
    "age" = "Age",
    "ba" = "Bachelor's degree",
    "white" = "White",
    "hisplat" = "Hispanic/Latino",
    "male" = "Male",
    "inc" = "Income"
    
  ),
  estimate = "{estimate}{stars}",
  
  gof_map = c("nobs", "r.squared", "aic", "bic")
)


# In all three cases, the affective partisanship index is positively associated
# with other things we expect it to be associated with and the effects are
# significant at the .05 level


# we can also see that including it in a model gives us a better fit than using
# warmth bias when trying to predict commitment to democratic norms.
ma <- lm(ANESdnorm ~ apstot9 + age + ba + white + hisplat + male + inc, data =
           dat_select)
mb <- lm(ANESdnorm ~ wbias + age + ba + white + hisplat + male + inc, data =
           dat_select)

BIC(ma, mb)
AIC(ma, mb)




