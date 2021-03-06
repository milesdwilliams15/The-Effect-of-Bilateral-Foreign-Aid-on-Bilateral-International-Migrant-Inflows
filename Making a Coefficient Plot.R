

## How to Make a Coefficient Plot
# Summon
library(dotwhisker)
library(lmtest)

# Make data.frame() of model outputs to plot
term<-c() # Vector of covariate names
estimate<-c() # Vector of model coefficients
std.error<-c() # Vector of standard errors
model<-c() # Vector of model ID

m1<-data.frame(term,estimate,std.error) # create as many data.frames as models

# Combine data.frames (if you so desire) to make an object to be used for plotting
# multiModel<-rbind(m1,...mN)

# Plot
dwplot(multiModel)  # add whatever ggplot changes you want to the final product

########
# Plot 1 (see Tables 1 & 2 in paper)

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

# Plot 2 (see Tables 3 & 4 in paper)
term<-c("Aid Commitments*Colonial Past")
estimate<-c(ols2CSE["logdAid:drColony",1])
std.error<-c(ols2CSE["logdAid:drColony",2])
model<-c("OLS 1")
o1<-data.frame(term,estimate,std.error,model)
term<-c("Aid Commitments*Common Language")
estimate<-c(ols3CSE["logdAid:drLanguage",1])
std.error<-c(ols3CSE["logdAid:drLanguage",2])
model<-c("OLS 2")
o2<-data.frame(term,estimate,std.error,model)
term<-c("Aid Commitments*Democracy")
estimate<-c(ols4CSE["logdAid:rDemo",1])
std.error<-c(ols4CSE["logdAid:rDemo",2])
model<-c("OLS 3")
o3<-data.frame(term,estimate,std.error,model)
term<-c("Aid Commitments*Colonial Past")
estimate<-c(coeftest(vglmTob2)["logdAid:drColony",1])
std.error<-c(coeftest(vglmTob2)["logdAid:drColony",2])
model<-c("Tobit 1")
t1<-data.frame(term,estimate,std.error,model)
term<-c("Aid Commitments*Common Language")
estimate<-c(coeftest(vglmTob3)["logdAid:drLanguage",1])
std.error<-c(coeftest(vglmTob3)["logdAid:drLanguage",2])
model<-c("Tobit 2")
t2<-data.frame(term,estimate,std.error,model)
term<-c("Aid Commitments*Democracy")
estimate<-c(coeftest(vglmTob4)["logdAid:rDemo",1])
std.error<-c(coeftest(vglmTob4)["logdAid:rDemo",2])
model<-c("Tobit 3")
t3<-data.frame(term,estimate,std.error,model)
dwplot(rbind(o1,o2,o3,t1,t2,t3))+theme_bw()+
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+
  xlab("Effect on Bilateral Immigration")+
  theme(legend.justification=c(1, 1), legend.position=c(1, 1),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

# Plot 3 (see Tables 3 & 4 in paper)
term<-c("Aid Commitments","Migration Policy Restrictiveness","Severity of Civil Conflict",
        "Distance","Colonial Past","Common Language","Democracy","GDP/capita",
        "Unemployment Rate","Population","Previous Migrant Inflows")
estimate<-c(ols3CSE[2:12,1])
std.error<-c(ols3CSE[2:12,2])
model<-c("OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS")
o1<-data.frame(term,estimate,std.error,model)
term<-c("Aid Commitments*Common Language")
estimate<-c(ols3CSE["logdAid:drLanguage",1])
std.error<-c(ols3CSE["logdAid:drLanguage",2])
model<-c("OLS")
o2<-data.frame(term,estimate,std.error,model)
o<-rbind(o1,o2)
term<-c("Aid Commitments","Migration Policy Restrictiveness","Severity of Civil Conflict",
        "Distance","Colonial Past","Common Language","Democracy","GDP/capita",
        "Unemployment Rate","Population","Previous Migrant Inflows")
estimate<-c(coeftest(vglmTob3)[3:13,1])
std.error<-c(coeftest(vglmTob3)[3:13,2])
model<-c("Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit",
         "Tobit")
t1<-data.frame(term,estimate,std.error,model)
term<-c("Aid Commitments*Common Language")
estimate<-c(coeftest(vglmTob3)["logdAid:drLanguage",1])
std.error<-c(coeftest(vglmTob3)["logdAid:drLanguage",2])
model<-c("Tobit")
t2<-data.frame(term,estimate,std.error,model)
t<-rbind(t1,t2)
dwplot(rbind(o,t))+theme_bw()+
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+
  xlab("Coefficient Estimate")+
  theme(legend.justification=c(1, 1), legend.position=c(1, 1),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

# Plot 4 (see Tables 3 & 4 in paper)
term<-c("Aid Commitments","Migration Policy Restrictiveness","Severity of Civil Conflict",
        "Distance","Colonial Past","Common Language","Democracy","GDP/capita",
        "Unemployment Rate","Population","Previous Migrant Inflows")
estimate<-c(ols4CSE[2:12,1])
std.error<-c(ols4CSE[2:12,2])
model<-c("OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS")
o1<-data.frame(term,estimate,std.error,model)
term<-c("Aid Commitments*Democracy")
estimate<-c(ols4CSE["logdAid:rDemo",1])
std.error<-c(ols4CSE["logdAid:rDemo",2])
model<-c("OLS")
o2<-data.frame(term,estimate,std.error,model)
o<-rbind(o1,o2)
term<-c("Aid Commitments","Migration Policy Restrictiveness","Severity of Civil Conflict",
        "Distance","Colonial Past","Common Language","Democracy","GDP/capita",
        "Unemployment Rate","Population","Previous Migrant Inflows")
estimate<-c(coeftest(vglmTob4)[3:13,1])
std.error<-c(coeftest(vglmTob4)[3:13,2])
model<-c("Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit",
         "Tobit")
t1<-data.frame(term,estimate,std.error,model)
term<-c("Aid Commitments*Democracy")
estimate<-c(coeftest(vglmTob4)["logdAid:rDemo",1])
std.error<-c(coeftest(vglmTob4)["logdAid:rDemo",2])
model<-c("Tobit")
t2<-data.frame(term,estimate,std.error,model)
t<-rbind(t1,t2)
dwplot(rbind(o,t))+theme_bw()+
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+
  xlab("Coefficient Estimate")+
  theme(legend.justification=c(1, 1), legend.position=c(1, 1),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

# Plot 5 (see Table 5 in paper)
term<-c("Aid Commitments*Distance")
estimate<-c(ols6CSE["logdAid:logdrDistance",1])
std.error<-c(ols6CSE["logdAid:logdrDistance",2])
model<-c("OLS 1")
o1<-data.frame(term,estimate,std.error,model)
term<-c("Aid Commitments*Migration Policy Restrictiveness")
estimate<-c(ols7CSE["logdAid:dRestrict",1])
std.error<-c(ols7CSE["logdAid:dRestrict",2])
model<-c("OLS 2")
o2<-data.frame(term,estimate,std.error,model)
term<-c("Aid Commitments*Distance")
estimate<-c(coeftest(vglmTob6)["logdAid:logdrDistance",1])
std.error<-c(coeftest(vglmTob6)["logdAid:logdrDistance",2])
model<-c("Tobit 1")
t1<-data.frame(term,estimate,std.error,model)
term<-c("Aid Commitments*Migration Policy Restrictiveness")
estimate<-c(coeftest(vglmTob7)["logdAid:dRestrict",1])
std.error<-c(coeftest(vglmTob7)["logdAid:dRestrict",2])
model<-c("Tobit 2")
t2<-data.frame(term,estimate,std.error,model)
dwplot(rbind(o1,o2,t1,t2))+theme_bw()+
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+
  xlab("Effect on Bilateral Immigration")+
  theme(legend.justification=c(1, 1), legend.position=c(1, 1),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

# Plot 6 (see Table 5 in paper)
term<-c("Aid Commitments","Migration Policy Restrictiveness","Severity of Civil Conflict",
        "Distance","Colonial Past","Common Language","Democracy","GDP/capita",
        "Unemployment Rate","Population","Previous Migrant Inflows")
estimate<-c(ols7CSE[2:12,1])
std.error<-c(ols7CSE[2:12,2])
model<-c("OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS","OLS")
o1<-data.frame(term,estimate,std.error,model)
term<-c("Aid Commitments*Migration Policy Restrictiveness")
estimate<-c(ols7CSE["logdAid:dRestrict",1])
std.error<-c(ols7CSE["logdAid:dRestrict",2])
model<-c("OLS")
o2<-data.frame(term,estimate,std.error,model)
o<-rbind(o1,o2)
term<-c("Aid Commitments","Migration Policy Restrictiveness","Severity of Civil Conflict",
        "Distance","Colonial Past","Common Language","Democracy","GDP/capita",
        "Unemployment Rate","Population","Previous Migrant Inflows")
estimate<-c(coeftest(vglmTob7)[3:13,1])
std.error<-c(coeftest(vglmTob7)[3:13,2])
model<-c("Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit","Tobit",
         "Tobit")
t1<-data.frame(term,estimate,std.error,model)
term<-c("Aid Commitments*Migration Policy Restrictiveness")
estimate<-c(coeftest(vglmTob7)["logdAid:dRestrict",1])
std.error<-c(coeftest(vglmTob7)["logdAid:dRestrict",2])
model<-c("Tobit")
t2<-data.frame(term,estimate,std.error,model)
t<-rbind(t1,t2)
dwplot(rbind(o,t))+theme_bw()+
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+
  xlab("Coefficient Estimate")+
  theme(legend.justification=c(1, 1), legend.position=c(1, 1),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

# Plot 7 (see Table 6 in paper)
term<-c("Aid Commitments","Aid Commitments^2")
estimate<-c(ols8CSE[2:3,1])
std.error<-c(ols8CSE[2:3,2])
model<-c("OLS","OLS")
o1<-data.frame(term,estimate,std.error,model)
estimate<-c(coeftest(vglmTob8)[3:4,1])
std.error<-c(coeftest(vglmTob8)[3:4,2])
model<-c("Tobit","Tobit")
t1<-data.frame(term,estimate,std.error,model)
dwplot(rbind(o1,t1))+theme_bw()+
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+
  xlab("Effect on Bilateral Immigration")+
  theme(legend.justification=c(1, 1), legend.position=c(1, 1),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

# Plot 8 (See Tables 7, 8, & 9)
term<-c("Aid Commitments","Common Language",
        "Migration Policy Restrictiveness","GDP/capita",
        "Unemployment","Severity of Civil Conflict","Democracy")
estimate<-c(coeftest(m1)[2:8,1])
std.error<-c(coeftest(m1)[2:8,2])
model<-c("Britain & France","Britain & France","Britain & France","Britain & France","Britain & France","Britain & France","Britain & France")
o1<-data.frame(term,estimate,std.error,model)
estimate<-c(coeftest(m2)[2:8,1])
std.error<-c(coeftest(m2)[2:8,2])
model<-c("Britain","Britain","Britain","Britain","Britain","Britain","Britain")
o2<-data.frame(term,estimate,std.error,model)
term<-c("Aid Commitments","Migration Policy Restrictiveness","GDP/capita",
        "Unemployment","Severity of Civil Conflict")
estimate<-c(coeftest(m3)[2:6,1])
std.error<-c(coeftest(m3)[2:6,2])
model<-c("France","France","France","France","France")
o3<-data.frame(term,estimate,std.error,model)
dwplot(rbind(o1,o2,o3))+theme_bw()+
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+
  xlab("Effect on Bilateral Immigraiton")+
  theme(legend.justification=c(1, 1), legend.position=c(1, 1),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 
