---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
In this example, I am documenting how to formulate an equation for random intercepts and slopes in multilevel modeling for longitudinal models along with the r code using the nlme package with interpretation of the output.  In this current data set there are five variables id, school, interven (i.e. an intervention that students are participating in), score (i.e. the score the student received on the intervention), and sf (school funding levels).    Overall there are 1,380 participants.  In this demonstration we will want to predict a student's score on an intervention while accounting for the nesting of students within school and make the model more complicated further into the example.  Multilevel modeling for longitudinal data analysis makes sense, because the researcher can allow participants to have their own mean values (i.e. random intercepts) and or trajectories on a covariate over time (i.e. random slopes).  Allowing for random intercepts and slopes is important, because we expect that participants scores will be correlated with each other over time therefore violating one of the assumptions of linear regression.  
```{r}
id = rep(1:100, each = 10)
time = rep(1:10, 100)
set.seed(12345)
score = c(rnorm(5000, 20, 5), rnorm(5000, 0, 5))
score = as.data.frame(round(scale(score, center = TRUE, scale = FALSE),0)); head(score)
colnames(score) = c("score")
interven = c(rep(1, 5000), rep(0,5000))
eth = rep(c(rep(1,10), rep(0,10)),500)
gpa = round(abs(rnorm(10000,2,1)),2)
dat = as.data.frame(cbind(id, interven, time, eth, gpa, score)); head(dat)
```
Here I try to break down what each component of the equation means.

gamma00 is the average or mean like score across the time points.  The indexes indicate where the parameter falls in each level.  For example, the first 0 in the gamma00 means that it is the intercept for the level one.  The second zero means that it is the mean or intercept for level two.  However, level two intercepts can vary by u0j, which will be discussed next. 

u0j this is the random deviation from the intercept or initial value for each time point  This is a level two error term, which is why it only has the subscript j instead of i and j.  It has a j, because j represents all of the 200 schools (i.e. from 1 to j time points).  For example the unique deviation from the intercept for time point one would be u01.  

gamma10 is the average regression coefficient for the change associated with interven variable (intervenij).  It is the average change we would see given a one unit change in the independent variable (in this case identifying as a being in the intervention).  The first 1 index means that it is the the first slope coefficient in in level one and the second 0 means that it is the average slope coefficient (i.e. no effect of any level two variables), because in this model the slope is not varying by time points.   

eij is the individual level one error term for each individual in each school. 
$$ Level~1:~~~{y_{ij} = \beta_{0j} + \beta_{1j}(interven_{ij}) + e_{ij}}~~~ (1.1)$$
$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} + u_{0j}} ~~~ (1.2)$$

$$Mixed~model: ~~~{y_{ij} = \gamma_{00} + \gamma_{10}(interven_{ij}) + u_{0j} + e_{ij}} ~~~(1.3)$$
Here is the r-code for the multilevel model described in equation 1 using the nlme package.  There is the fixed part, which will have the average intercept and the parameter estimate for the time variable. To model the random intercepts, we use the 1 to signify that this is a random intercepts model where we want intercepts for each time point.   
```{r}
library(nlme)
model1 = lme(fixed = score ~ interven, random = ~1 | time, data = dat)
sumModel1 = summary(model1); sumModel1
```
We are using REML, because it accounts for the included parameters correctly in the standard errors (i.e. making them larger by accounting for the reduced degrees of freedom). 

AIC, BIC, and logLik are all model comparison statistics.

Fixed effects: These are interpreted just as a regular single level regression.

Correlation (inter): The correlation between the fixed intercept and fixed slope for sex.  If the value is above zero, then we could say that as the intercept (i.e. initial values) increase so does the slope.  So for a positive relationship, a person who starts with a higher intercept will increase as their score increases.  In our case the opposite is true.  Therefore, as the intercept or starting values for like move up, we see a decrease in the slope for the intervention.

Standardized residuals = These are the residuals scaled by the standard deviation.

Now we can estimate the random slope model.   The random slope model adds one unique component which is u1j.  This is the random level two error for the slope allowing the slope to differ for each time point for the covariate interven.  It is indexed with j, because it can vary for each time point  It is multiplied by the covariate value of interven, because it is a slope therefore we need to multiply it by the covariate for which it is measuring the slope.
$$ Level~1:~~~{y_{ij} = \beta_{0j} + \beta_{1j}(interven_{ij}) + e_{ij}}~~~ (2.1)$$

$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} + u_{0j}} ~~~ (2.2)$$
$$ Level~2~Slope:~~~{\beta_{1j} = \gamma_{10} + u_{1j}} ~~~ (2.2)$$
$$ Mixed~model:~~~{y_{ij} = \gamma_{00} + \gamma_{10}(interven_{ij}) +u_{0j} + u_{1j}(interven_{ij}) + e_{ij}}~~~ (2.3)$$
For this model, in nlme, instead of using one, we now use the term that we want to have a random slope which is interven.
```{r}
model2 = lme(fixed = score ~ interven, random = ~ interven | time, data = dat)
sumModel2 = summary(model2); sumModel2
```
We could include one of two types of variables.  Time varying and time invariant.  Time invariant variables are ones that do not vary over time.  In our example, this means that they do not vary for the student.  These types of variables would be ethnicity and gender.  Additionally, we could include time varying variables, such as student's GPA, which will likely change from year to year.

I will first explain how to add the time varying variable GPA and then add the time invariant variable ethnicity (nonwhite =1 or white = 1) to each of the equations and then model them in R using nlme.

To include the time varying covariate gpa, we first include it in the level one part, because it is a level one variable.  Then for each student to have a slope for gpa, we create another level two slope equation which has the fixed or average slope for gpa over all the students (gamma20) and the variation in gpa for each student (u2j).

$$ Level~1:~~~{y_{ij} = \beta_{0j} + \beta_{1j}(interven_{ij}) +\beta_{2j}(gpa_{ij}) + e_{ij}}~~~ (3.1)$$
$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} + u_{0j}} ~~~ (3.2)$$
$$ Level~2~Slope~\beta_{1j}:~~~{\beta_{1j} = \gamma_{10} + u_{1j}} ~~~ (3.2)$$
$$ Level~2~Slope~\beta_{2j}:~~~{\beta_{2j} = \gamma_{20} + u_{2j}} ~~~ (3.3)$$
$$ Mixed~model:~~~{y_{ij} = \gamma_{00} + \gamma_{10}(interven_{ij}) +\gamma_{20}(gpa_{ij}) +u_{0j} + u_{1j}(interven_{ij}) +u_{2j}(gpa_{ij}) + e_{ij}}~~~ (3.4)$$
In the nlme package, this is relatively simple as we just add the additional covariate, gpa, to the fixed and random section of the model.
```{r}
model3 = lme(fixed = score ~ interven + gpa, random = ~ interven + gpa | time, data = dat)
sumModel3 = summary(model3); sumModel3
```
We could also include a time invariant variable (i.e. a level two variable) that affects a student average score over time (i.e. random intercept) and their average trajectory on intervention and gpa over time (random slopes).  To do this we add the time invarient covariate eth to the level two intercept (gamma01), level two slope for b1j (gamma11), level two slope for b2j (gamma21).
$$ Level~1:~~~{y_{ij} = \beta_{0j} + \beta_{1j}(interven_{ij}) +\beta_{2j}(gpa_{ij}) + e_{ij}}~~~ (4.1)$$
$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} +\gamma_{01}(eth_{j})+ u_{0j}} ~~~ (4.2)$$
$$ Level~2~Slope~\beta_{1j}:~~~{\beta_{1j} = \gamma_{10} +\gamma_{11}(eth_{j}) + u_{1j}} ~~~ (4.3)$$
$$ Level~2~Slope~\beta_{2j}:~~~{\beta_{2j} = \gamma_{20}+ \gamma_{21}(eth_{j}) + u_{2j}} ~~~ (4.4)$$
$$ Mixed~model:~~~{y_{ij} = \gamma_{00} + \gamma_{01}(eth_{j})+ \gamma_{10}(interven_{ij}) + \gamma_{11}(eth_{j})(interven_{ij})  +\gamma_{20}(gpa_{ij}) + \gamma_{21}(eth_{j})(gpa_{ij}) +u_{0j} + u_{1j}(interven_{ij}) +u_{2j}(gpa_{ij}) + e_{ij}}~~~ (4.5)$$
To include the eth variable, we can take the code from the previous model and add two interaction terms (eth and interven) and (eth and gpa) to acquire the values for the effect that eth has on the interven and gpa slopes as well as adding eth to the fixed part to acquire the average effect of being nonwhite (i.e. a 1 for the eth).
```{r}
model4 = lme(fixed = score ~ interven + gpa + eth + interven*eth + gpa*eth, random = ~ interven + gpa | time, data = dat)
sumModel4 = summary(model4); sumModel4
```


References:
Finch, W. H., Bolin, J. E., & Kelley, K. (2014). Multilevel modeling using R. Crc Press.

