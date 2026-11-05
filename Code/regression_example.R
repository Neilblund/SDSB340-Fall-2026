library(tidyverse)
library(gtsummary)

filloc<-url('https://github.com/Neilblund/SDSB340-Fall-2026/raw/refs/heads/master/Data/atus25_subset.rds')
atus<-readRDS(filloc)

# removing respondents without at least one child
atus_child<-atus|>
  filter(num_children>0)
# simple difference of means test with time spent on childcare as the dependent variable
# and sex as the IV
atus_child|>
  tbl_summary(include = childcare_combined, 
              by=sex,
              statistic = all_continuous() ~ "{mean} ({sd})"
  )|>
  add_difference()


# a regression model using only sex as a predictor (note this is basically
# equivalent to the difference of means test)
model0<-lm(childcare_combined ~  sex ,
           data =atus_child)
summary(model0)
# a regression model including number of children, youngest child and sex
model1<-lm(childcare_combined ~ num_children + youngest_child + sex ,
          data =atus_child)

summary(model1)

# a regression model with employment status added
model2<-lm(childcare_combined ~ num_children + youngest_child + employment_status +  sex ,
           data =atus_child)

summary(model2)


# formatted regression output using gtsummary ----------------------------------

tbl1<-tbl_regression(model1, intercept=TRUE)|>
  add_glance_source_note(include =c("nobs", "r.squared", "AIC"))|>
  modify_header(label="**DV: Minutes spent on childcare**")
tbl2



# formatted regression table with all three models using modelsummary-----------
library(modelsummary)
library(gt)

model_list<-list("Model" = model0, 
                 "Model 1" = model1,
                 "Model 2" = model2
                 
                 ) # putting the modelsin a named list

modelsummary(model_list, 
             fmt = 0,  # rounding coefficients to the nearest whole number
             statistic = "conf.int",  # showing confidence intervals
             coef_rename=TRUE,        # adding variable labels to the output
             gof_map = c("nobs", "r.squared", "adj.r.squared"), # showing only selected fit stats
             include_reference = TRUE,                           # showing reference levels
             output = 'gt',                                      # Using gt backend so we can add formatting with gt functions
)|>
  tab_header( title = md("**DV: Minutes spent on childcare**"))  # adding a title



# plotting model coefficients
modelplot(model_list, 
          coef_rename=TRUE,
          coef_omit = "Intercept"
) +
  geom_vline(xintercept = 0, lty=2, col='red') +
  labs(title = "Dependent Variable: Time spent on childcare") +
  scale_color_manual(values = c("tomato", "cornflowerblue", "gold"))



