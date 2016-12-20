[Back to Main Page](https://milesdwilliams15.github.io/)

I recently completed my master's thesis in political science at Eastern Illinois University. My research focused on the hereto understudied intersection of foreign aid and immigration. I hypothesized that bilateral aid commitments from donor countries attract bilateral immigration from aid recipients. Using a dyadic panel dataset for 18 OECD donor countries and 130 aid recipient countries from 1994 to 2011 I estimated a gravity-type equation using both OLS and Tobit. Nearly 20,000 dyad-years were included in the analysis. The results confirmed my hypothesis. (Yay!)

R proved to be a valuable tool for analysis and visualization for my thesis. However, as with any type of highly customized data visualizations, I encountered a few issues that required me to put on my thinking cap as I developed my own workarounds and tricks to get R to do what I so desperately wanted it to do.

In this post, I will to discuss some of the code I came up with to overcome some of the roadblocks I encountered in creating visualizations for my thesis--in particular, coefficient plots.

### Coefficient Plots Using "dotwhisker"

The field of political science has been moving farther and farther in the direction of utilizing coefficient plots rather than regression tables to display model estimates and standard errors. And it is no wonder: coefficient plots offer a more intuitive and visually appealing way to report the results from a regression model. 

While there are a number of packages available in R for developing coefficient plots, the dwplot() command in the dotwhisker package is one of my favorites. However, one of the problems I quickly encountered while using dwplot() is that it lacked straightforward functionality for picking and choosing which model covariates I wanted to display. Because I utilized A LOT of least squares dummy variable (LSDV) estimators in model estimation, simply using dwplot() to display my model coefficients and standard errors would result in convoluted coefficient plots with thousands of covariates. I wanted to create a coefficient plot that only showed the model estimates for covariates of interest. After a lot of trial and error, I was able to create the following workaround by exploiting the fact that the dwplot() command relies on a data.frame() of the following four elements: "term" (covariate name), "estimate" (coefficient), "std.error" (standard error), and "model" (the name for a given model). dwplot() would typically extract these elements from a lm(), glm(), vglm(), etc. object automatically, but one can easily create a data.frame for these objects for a selection of model covariates by manually extracting the desired elements from a model object.

I first created a model object using the lm() command:

    # 'r' = aid recipient characteristic; 'd' = aid donor characteristic; 'dr' = shared donor-recipient characteristic.
    ols1<-lm(logrInflowLead ~ logdAid+dRestrict + rConflict + logdrDistance + drColonyd + drLanguaged + rDemo + logdGDPcap 
    + dUnemployment + logdPopulation + logrInflow + factor(DonorNum) + factor(RecipientNum),data=Thesis) 

Then, using the plm and lmtest packages, I created degrees of freedom adjusted robust clustered standard errors and created a new model object with the new robust-clustered variance covariance matrix to use for data visualization.

    library(lmtest)
    library(plm)

    # Caclulate Clustered SEs for ols1
    G <- length(unique(Thesis$Dyad))
    N <- length(Thesis$Dyad)
    dfa1 <- (G/(G - 1)) * ((N - 1)/ols1$df.residual)
    dyad_c_vcov1 <- dfa1 * vcovHC(ols1, type = "HC0", cluster = "group", adjust = T)
    ols1CSE<-coeftest(ols1, vcov = dyad_c_vcov1)

This new object ("ols1CSE") will now serve as one of the models from which I extract the terms required by dwplot(). A second model was estimated using Tobit, which can be done with the vglm() command in the VGAM package.

After I created these model objects, I utilized the following code to create the plot shown below:

    term<-c("Aid Commitments","Migration Policy Restrictiveness",
        "Severity of Civil Conflict","Distance","Colonial Past",
        "Common Language","Democracy","GDP/capita","Unemployment Rate",
        "Population","Previous Migrant Inflows")
    estimate<-c(ols1CSE[2:12,1]) # extract desired estimates from a coeftest() object with robust clustered SEs.
    std.error<-c(ols1CSE[2:12,2]) # extract standard errors.
    model<-c("OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS")
    o1<-data.frame(term,estimate,std.error,model)
    estimate<-c(coeftest(vglmTob1)[3:13,1])
    std.error<-c(coeftest(vglmTob1)[3:13,2])
    model<-c("Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit",
         "Tobit")
    t1<-data.frame(term,estimate,std.error,model)
    dwplot(rbind(o1,t1))+theme_bw()+
      geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+
      xlab("Effect on Bilateral Immigration")+
      theme(legend.justification=c(1, 1), legend.position=c(1, 1),
            legend.background = element_rect(colour="grey80"),
            legend.title = element_blank()) 

![OLS and Tobit Results](https://cloud.githubusercontent.com/assets/23504082/21127692/9babfb90-c0ba-11e6-8293-79c1caa8cb07.jpg)

While the above might lack some elegance, it clearly works perfectly. Of course, as I continue to develop my skills with R, a quicker workaround will surely be something I'll consider developing in the future.

[Back to Main Page](https://milesdwilliams15.github.io/)
