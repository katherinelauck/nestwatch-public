library('gt') #you may need to first run install.packages('gt')

#EXAMPLE CODE, for full model, success, tmax^2 (success~stdmax2laydate2way.AK.RDS)

#read in the model result
<<<<<<< HEAD
modresult<-readRDS("~/Coding/Karp lab/Github_repository/nestwatch/results/Question 1-2/success~stdmax2laydate2way.AK.RDS") #CHANGE
=======
modresult<-readRDS("~/Documents/nestwatch/results/Question 1-2/success~stdmax2laydate2way.AK.RDS") #CHANGE
>>>>>>> 526a41e1edb778dfa9cee20b0ab1365555367eb3

#take a look at the summary
summ<-summary(modresult)$coefficients
Coefficient<-rownames(summ) #add a column for Coefficients, because in the summary the coefficient is not considered its own column 

summ_table<-data.frame(Coefficient,summ) #make a table of the summary

summ_table %>% 
  gt() %>% #use gt package to make a pretty table
  tab_header( #add title
    title = "Success ~ Tmax^2") %>% #CHANGE
  cols_label( #fix some of the column names
    Std..Error = "Std. Error",
    z.value = "z value",
    Pr...z.. = "Pr(>|z|)") %>% 
  fmt_number( #round the numbers to 3 decimal places
    columns = 2:5,
    decimals = 3,
    suffixing = TRUE)

#make tables for three likelihood ratio tests

#1. full model vs. model without any Tmax interaction with squared terms
LRTCompare1<-readRDS("~/Documents/nestwatch/results/Question 1-2/success~stdmax2laydate2way.LRT.AK.RDS") #read in model file #CHANGE

LRT_Table_1<-data.frame(Model = c("Alternative","Full"),anova(modresult, LRTCompare1)) #make data frame of the result

#make table using gt
LRT_Table_1 %>%
  gt() %>% #use gt package
  tab_header( #add title #CHANGE TITLE
    title = "LRT with model without any T-max interaction with squared terms") %>% 
  cols_label( #fix some of the column names 
    Pr..Chisq. = "Pr(>Chisq)") %>% 
  fmt_number( #round the numbers
    columns = c(3:7),
    decimals = 1,
    suffixing = FALSE) %>% 
  fmt_number( #round the numbers of the last column differently
    columns = c(9),
    decimals = 3,
    suffixing = FALSE)

#2. full model vs. model without any squared terms whatsoever. Repeat code for #1 but change the model that we read in and the title of the table
LRTCompare2<-readRDS("~/Documents/nestwatch/results/Question 1-2/success~stdmaxlaydate2way.AK.RDS") #CHANGE

LRT_Table_2<-data.frame(Model = c("Alternative","Full"),anova(modresult, LRTCompare2))

#make table using gt
LRT_Table_2 %>%
  gt() %>% #use gt package
  tab_header( #add title
    title = "LRT with model without any squared terms whatsoever") %>% #CHANGE
  cols_label( #fix some of the column names
    Pr..Chisq. = "Pr(>Chisq)") %>% 
  fmt_number( #round numbers
    columns = c(3:7),
    decimals = 1,
    suffixing = FALSE) %>% 
  fmt_number( #round numbers of the last column differently
    columns = c(9),
    decimals = 3,
    suffixing = FALSE)

#3. full model vs. model without any Tmax interaction terms (both squared and linear terms), but with singular terms. Repeat code for #1 but change the model that we read in and the title of the table

LRTCompare3<-readRDS("~/Documents/nestwatch/results/Question 1-2/success~stdmax2laydate_LRT_AK.RDS") #CHANGE

LRT_Table_3<-data.frame(Model = c("Alternative","Full"),anova(modresult, LRTCompare3))

#make table using gt
LRT_Table_3 %>%
  gt() %>% #use gt package
  tab_header( #add title
    title = "LRT with model without any Tmax interaction terms") %>% #CHANGE
  cols_label( #fix some of the column names
    Pr..Chisq. = "Pr(>Chisq)") %>% 
  fmt_number( #round numbers
    columns = c(3:7),
    decimals = 1,
    suffixing = FALSE) %>% 
  fmt_number( #round numbers of the last column differently
    columns = c(9),
    decimals = 3,
    suffixing = FALSE)


###Kees: repeat example code but for failure and tmin
##skip failure~stdminlaydate2way.AK.RDS because it did not converge (part 2)


#failure~stdmin2laydate2way.AK.RDS

modresult<-readRDS("~/Documents/nestwatch/results/Question 1-2/failure~stdmin2laydate2way.AK.RDS") 

#take a look at the summary
summ<-summary(modresult)$coefficients
Coefficient<-rownames(summ) #add a column for Coefficients, because in the summary the coefficient is not considered its own column 

summ_table<-data.frame(Coefficient,summ) #make a table of the summary

summ_table %>% 
  gt() %>% #use gt package to make a pretty table
  tab_header( #add title
    title = "Success ~ Tmin^2") %>% 
  cols_label( #fix some of the column names
    Std..Error = "Std. Error",
    z.value = "z value",
    Pr...z.. = "Pr(>|z|)") %>% 
  fmt_number( #round the numbers to 3 decimal places
    columns = 2:5,
    decimals = 3,
    suffixing = TRUE)

#make tables for three likelihood ratio tests

#1. full model vs. model without any Tmax interaction with squared terms
LRTCompare1<-readRDS("~/Documents/nestwatch/results/Question 1-2/failure~stdmin2laydate2way.LRT.AK.RDS") #read in model file #CHANGE

LRT_Table_1<-data.frame(Model = c("Alternative","Full"),anova(modresult, LRTCompare1)) #make data frame of the result

#make table using gt
LRT_Table_1 %>%
  gt() %>% #use gt package
  tab_header( #add title #CHANGE TITLE
    title = "LRT with model without any T-min interaction with squared terms") %>% 
  cols_label( #fix some of the column names 
    Pr..Chisq. = "Pr(>Chisq)") %>% 
  fmt_number( #round the numbers
    columns = c(3:7),
    decimals = 1,
    suffixing = FALSE) %>% 
  fmt_number( #round the numbers of the last column differently
    columns = c(9),
    decimals = 3,
    suffixing = FALSE)

#2. full model vs. model without any squared terms whatsoever. Repeat code for #1 but change the model that we read in and the title of the table DIDNT CONVERGE


#3. full model vs. model without any Tmax interaction terms (both squared and linear terms), but with singular terms. Repeat code for #1 but change the model that we read in and the title of the table

LRTCompare3<-readRDS("~/Documents/nestwatch/results/Question 1-2/failure~stdmin2laydate.LRT.AK.RDS") #CHANGE

LRT_Table_3<-data.frame(Model = c("Alternative","Full"),anova(modresult, LRTCompare3))

#make table using gt
LRT_Table_3 %>%
  gt() %>% #use gt package
  tab_header( #add title
    title = "LRT with model without any Tmin interaction terms") %>% #CHANGE
  cols_label( #fix some of the column names
    Pr..Chisq. = "Pr(>Chisq)") %>% 
  fmt_number( #round numbers
    columns = c(3:7),
    decimals = 1,
    suffixing = FALSE) %>% 
  fmt_number( #round numbers of the last column differently
    columns = c(9),
    decimals = 3,
    suffixing = FALSE)

###Tom: repeat example code but for success and tmin
##skip failure~stdmin2laydate_LRT_AK.RDS because it did not converge (part 3)

library('gt') #you may need to first run install.packages('gt')

#EXAMPLE CODE, for full model, success, tmax^2 (success~stdmax2laydate2way.AK.RDS)

#read in the model result
modresult<-readRDS("~/Coding/Karp lab/Github_repository/nestwatch/results/Question 1-2/success~stdmin2laydate2way.AK.RDS") #CHANGE

#take a look at the summary
summ<-summary(modresult)$coefficients
Coefficient<-rownames(summ) #add a column for Coefficients, because in the summary the coefficient is not considered its own column 

summ_table<-data.frame(Coefficient,summ) #make a table of the summary

summ_table %>% 
  gt() %>% #use gt package to make a pretty table
  tab_header( #add title
    title = "Success ~ Tmin^2") %>% #CHANGE
  cols_label( #fix some of the column names
    Std..Error = "Std. Error",
    z.value = "z value",
    Pr...z.. = "Pr(>|z|)") %>% 
  fmt_number( #round the numbers to 3 decimal places
    columns = 2:5,
    decimals = 3,
    suffixing = TRUE)

#make tables for three likelihood ratio tests

#1. full model vs. model without any Tmax interaction with squared terms
LRTCompare1<-readRDS("~/Coding/Karp lab/Github_repository/nestwatch/results/Question 1-2/success~stdmin2laydate2way.LRT.AK.RDS") #read in model file #CHANGE

LRT_Table_1<-data.frame(Model = c("Alternative","Full"),anova(modresult, LRTCompare1)) #make data frame of the result

#make table using gt
LRT_Table_1 %>%
  gt() %>% #use gt package
  tab_header( #add title #CHANGE TITLE
    title = "LRT with model without any T-min interaction with squared terms") %>% 
  cols_label( #fix some of the column names 
    Pr..Chisq. = "Pr(>Chisq)") %>% 
  fmt_number( #round the numbers
    columns = c(3:7),
    decimals = 1,
    suffixing = FALSE) %>% 
  fmt_number( #round the numbers of the last column differently
    columns = c(9),
    decimals = 3,
    suffixing = FALSE)

#2. full model vs. model without any squared terms whatsoever. Repeat code for #1 but change the model that we read in and the title of the table
LRTCompare2<-readRDS("~/Coding/Karp lab/Github_repository/nestwatch/results/Question 1-2/success~stdminlaydate2way.AK.RDS") #CHANGE

LRT_Table_2<-data.frame(Model = c("Alternative","Full"),anova(modresult, LRTCompare2))

#make table using gt
LRT_Table_2 %>%
  gt() %>% #use gt package
  tab_header( #add title
    title = "LRT with model without any squared terms whatsoever") %>% #CHANGE
  cols_label( #fix some of the column names
    Pr..Chisq. = "Pr(>Chisq)") %>% 
  fmt_number( #round numbers
    columns = c(3:7),
    decimals = 1,
    suffixing = FALSE) %>% 
  fmt_number( #round numbers of the last column differently
    columns = c(9),
    decimals = 3,
    suffixing = FALSE)


###Alison: repeat example code but for failure and tmax
##skip failure~stdmax2laydate_LRT_AK.RDS because it did not converge (part 3)
##skip failure~stdmax2laydate2way.LRT.AK.RDS because it did not converge (part 1)

modresult<-readRDS("~/Documents/nestwatch/results/Question 1-2/failure~stdmax2laydate2way.AK.RDS") #CHANGE

#take a look at the summary
summ<-summary(modresult)$coefficients
Coefficient<-rownames(summ) #add a column for Coefficients, because in the summary the coefficient is not considered its own column 

summ_table<-data.frame(Coefficient,summ) #make a table of the summary

summ_table %>% 
  gt() %>% #use gt package to make a pretty table
  tab_header( #add title
    title = "Failure ~ Tmax^2") %>% #CHANGE
  cols_label( #fix some of the column names
    Std..Error = "Std. Error",
    z.value = "z value",
    Pr...z.. = "Pr(>|z|)") %>% 
  fmt_number( #round the numbers to 3 decimal places
    columns = 2:5,
    decimals = 3,
    suffixing = TRUE)

#make tables for three likelihood ratio tests


#2. full model vs. model without any squared terms whatsoever. Repeat code for #1 but change the model that we read in and the title of the table
LRTCompare2<-readRDS("~/Documents/nestwatch/results/Question 1-2/failure~stdmaxlaydate2way.AK.RDS") #CHANGE

LRT_Table_2<-data.frame(Model = c("Alternative","Full"),anova(modresult, LRTCompare2))

#make table using gt
LRT_Table_2 %>%
  gt() %>% #use gt package
  tab_header( #add title
    title = "LRT with model without any squared terms whatsoever") %>% #CHANGE
  cols_label( #fix some of the column names
    Pr..Chisq. = "Pr(>Chisq)") %>% 
  fmt_number( #round numbers
    columns = c(3:7),
    decimals = 1,
    suffixing = FALSE) %>% 
  fmt_number( #round numbers of the last column differently
    columns = c(9),
    decimals = 3,
    suffixing = FALSE)

