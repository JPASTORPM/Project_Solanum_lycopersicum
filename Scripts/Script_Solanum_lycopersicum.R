#------------------------------------------------
# Script: Solanum lycopersicum
# Autor: Junior Pastor Pérez-Molina
# Date: 04-13-2019
#------------------------------------------------


#------------------------------------------------
# Initial steps: Packages & Functions
#------------------------------------------------
rm(list = ls()) # Remove all objects
graphics.off()  # Remove all graphics
cat("\014")     # Remove script in console
if(!grepl("Project_Solanum_lycopersicum", getwd())){
    x= cat(prompt = "Please set the working directory to the project folder")}

# R Packages, Important: Run row by row
# The following commands will install these packages if they are not already installed, 
# then you have to load the package, simply running this code again 
if(!require(devtools)){install.packages("devtools")}
if(!require(githubinstall)){devtools::install_github("hoxo-m/githubinstall")}
if(!require(yarrr)){githubinstall("yarrr")}
if(!require(yarrr)){githubinstall("yarrr")}
if(!require(ggplot2)){githubinstall("ggplot2")}
if(!require(splitstackshape)){githubinstall("splitstackshape")}
if(!require(Rmisc)){githubinstall("Rmisc")}
if(!require(lsmeans)){githubinstall("lsmeans")}
if(!require(car)){githubinstall("car")}
if(!require(multcomp)){githubinstall("multcomp")}
if(!require(multcompView)){githubinstall("multcompView")}
if(!require(broom)){githubinstall("broom")}
if(!require(openxlsx)){githubinstall("openxlsx")}
if(!require(cowplot)){githubinstall("cowplot")}
if(!require(factoextra)){githubinstall("factoextra")}
if(!require(ggtern)){githubinstall("ggtern")}
if(!require(mixtools)) { githubinstall("mixtools");  require("mixtools") }

# Functions
error.bar.vertical<-function(x, y, se.y, col){arrows(x, y-se.y, x, y+se.y, code=3, angle=90, length=0.25, col=col)}
function.mean<-function(data, variable){
    sum = summarySE(data, measurevar= variable, groupvars=c("deficiency_light", "time"), na.rm=TRUE)
    sum<-sum[c(1,2,3,4,6,7)]
    sum<-data.frame(sum)
    names(sum)<-c("deficiency_light","time","N","Mean","S.E.","C.I.95")
    sum
}
function.test.residuals.normal<-function(treatment, variable){
    test <- aov(variable ~ treatment)
    SW<-shapiro.test(test$residuals) #Opción 1, ver p-value 
    paste(as.character(ifelse(SW[2]>0.05,"Normality assumption", "No assumption of normality")),"| p value = ",round(as.numeric(SW[2]),2))
}
rm.whitespace <- function (x) gsub("^\\s+|\\s+$", "", x)
fun.table.anova<-function(variable,P1,P2,P3,P4){
    data.anova<-data.frame()
    ###### 1
    kw = kruskal.test(P1 ~ as.factor(Deficiency_Light), data= datasub1)
    model = lm(P1 ~ Deficiency*Light, data= datasub1)
    KW<-round(as.numeric(kw[1]),2)
    df<-round(as.numeric(kw[2]),0)
    P<-round(as.numeric(kw[3]),3)
    lsm = lsmeans(model, pairwise ~ Deficiency*Light, adjust="LSD")
    t1<-data.frame(cld(lsm[[1]], alpha=.05, Letters=letters))
    t2<-data.frame(t1[c(1)],t1[c(2)],t1[c(3)],t1[c(4)],t1[c(8)])
    t2$Deficiency_Light<-paste(t2$Deficiency,t2$Light, sep="*")
    t2$KW<-KW
    t2$df<-df
    t2$P<-P
    Sample<-data.frame(variable,"Time")
    names(Sample)<-c("Variable","Time")
    Sample$Time<-1
    data.anova <- rbind(data.anova,data.frame(Sample,t2))
    ###### 2
    kw = kruskal.test(P2 ~ as.factor(Deficiency_Light), data= datasub2)
    model = lm(P2 ~ Deficiency*Light, data= datasub2)
    KW<-round(as.numeric(kw[1]),2)
    df<-round(as.numeric(kw[2]),0)
    P<-round(as.numeric(kw[3]),3)
    lsm = lsmeans(model, pairwise ~ Deficiency*Light, adjust="LSD")
    t1<-data.frame(cld(lsm[[1]], alpha=.05, Letters=letters))
    t2<-data.frame(t1[c(1)],t1[c(2)],t1[c(3)],t1[c(4)],t1[c(8)])
    t2$Deficiency_Light<-paste(t2$Deficiency,t2$Light, sep="*")
    t2$KW<-KW
    t2$df<-df
    t2$P<-P
    Sample<-data.frame(variable,"Time")
    names(Sample)<-c("Variable","Time")
    Sample$Time<-2
    data.anova <- rbind(data.anova,data.frame(Sample,t2))
    ###### 3
    kw = kruskal.test(P3 ~ as.factor(Deficiency_Light), data= datasub3)
    model = lm(P3 ~ Deficiency*Light, data= datasub3)
    KW<-round(as.numeric(kw[1]),2)
    df<-round(as.numeric(kw[2]),0)
    P<-round(as.numeric(kw[3]),3)
    lsm = lsmeans(model, pairwise ~ Deficiency*Light, adjust="LSD")
    t1<-data.frame(cld(lsm[[1]], alpha=.05, Letters=letters))
    t2<-data.frame(t1[c(1)],t1[c(2)],t1[c(3)],t1[c(4)],t1[c(8)])
    t2$Deficiency_Light<-paste(t2$Deficiency,t2$Light, sep="*")
    t2$KW<-KW
    t2$df<-df
    t2$P<-P
    Sample<-data.frame(variable,"Time")
    names(Sample)<-c("Variable","Time")
    Sample$Time<-3
    data.anova <- rbind(data.anova,data.frame(Sample,t2))
    ###### 4
    kw = kruskal.test(P4 ~ as.factor(Deficiency_Light), data= datasub4)
    model = lm(P4 ~ Deficiency*Light, data= datasub4)
    KW<-round(as.numeric(kw[1]),2)
    df<-round(as.numeric(kw[2]),0)
    P<-round(as.numeric(kw[3]),3)
    require(lsmeans)
    lsm = lsmeans(model, pairwise ~ Deficiency*Light, adjust="LSD")
    t1<-data.frame(cld(lsm[[1]], alpha=.05, Letters=letters))
    t2<-data.frame(t1[c(1)],t1[c(2)],t1[c(3)],t1[c(4)],t1[c(8)])
    t2$Deficiency_Light<-paste(t2$Deficiency,t2$Light, sep="*")
    t2$KW<-KW
    t2$df<-df
    t2$P<-P
    Sample<-data.frame(variable,"Time")
    names(Sample)<-c("Variable","Time")
    Sample$Time<-4
    data.anova <- rbind(data.anova,data.frame(Sample,t2))
    ###
    return(data.anova)
}
fun.table.nested.anova<-function(variable,P4,d1,d2){
    data.anova<-data.frame()
    ###### 1
    model = aov(P4 ~ deficiency + deficiency/light, data= data2)
    summary_model<-summary(model)
    inf<-round(glance(model),3)
    P.value<-round(as.numeric(inf[5]),3)
    R2.adj<-round(as.numeric(inf[2]),2)
    F<-round(as.numeric(inf[4]),1)
    coef<-Anova(model, type="II")
    P<-data.frame(round(coef$`Pr(>F)`,3))
    P$Factor<-c("Light", "Deficiency(Light)", "NA")
    P<-data.frame(P)
    P<-na.omit(P)
    names(P)<-c("p","Factor")
    
    lsm = lsmeans(model, pairwise ~ deficiency*light, adjust="LSD")
    t1<-data.frame(cld(lsm[[1]], alpha=.05, Letters=letters))
    t2<-data.frame(t1[c(1)],t1[c(2)],t1[c(3)],t1[c(4)])
    t2$Deficiency_Light<-paste(t2$deficiency, t2$light, sep="*")
    
    rm.whitespace <- function (x) gsub("^\\s+|\\s+$", "", x)
    
    model_high = aov(d1 ~ deficiency, data= data2[data2$light=="High",])
    lsm = lsmeans(model_high, pairwise ~ deficiency, adjust="LSD")
    t1_high<-data.frame(cld(lsm[[1]], alpha=.05, Letters=letters))
    t2_high<-data.frame(t1_high[c(1)],t1_high[c(2)],t1_high[c(3)],t1_high[c(7)])
    t2_high$Deficiency_Light<-paste(t2_high$deficiency,"High", sep="*")
    t2_high$.group<-rm.whitespace(t2_high$.group)
    
    model_low = aov(d2 ~ deficiency, data= data2[data2$light=="Low",])
    lsm = lsmeans(model_high, pairwise ~ deficiency, adjust="LSD")
    t1_low<-data.frame(cld(lsm[[1]], alpha=.05, Letters=letters))
    t2_low<-data.frame(t1_low[c(1)],t1_low[c(2)],t1_low[c(3)],t1_low[c(7)])
    t2_low$Deficiency_Light<-paste(t2_low$deficiency,"Low", sep="*")
    t2_low$.group<-rm.whitespace(t2_low$.group)
    
    rm.na <- function (x) gsub("NA", "", x)
    
    dat<-merge(t2, t2_high[,c(4,5)], all=TRUE, by=c("Deficiency_Light"))
    dat<-merge(dat, t2_low[,c(4,5)], all=TRUE, by=c("Deficiency_Light"))
    dat$".group"<-paste(dat$.group.x,dat$.group.y,sep="")
    dat$".group"<-rm.na(dat$".group")
    
    data.anova <- rbind(data.anova,data.frame(variable,dat[,c(-6,-7)],"Light"=P[1,1], "Deficiency/Light"=P[2,1],  F, R2.adj, P.value))
    return(data.anova)
}
#------------------------------------------------


#------------------------------------------------
# Loading database
#------------------------------------------------
data<-read.delim("Data/Data_Solanum_lycopersicum.txt",header=T,sep="\t",dec=".")
data2<-data[data$time==4,]
data2$deficiency_light<-paste(data2$deficiency,data2$light, sep="*")
data2$deficiency_light<-paste(data2$deficiency,data2$light, sep="*")
data2$cod[data2$deficiency_light=="C*High"]<-1
data2$cod[data2$deficiency_light=="C*Low"]<-2
data2$cod[data2$deficiency_light=="(-) N*High"]<-3
data2$cod[data2$deficiency_light=="(-) N*Low"]<-4
#------------------------------------------------


#------------------------------------------------
# Fig. Time-course Fv.Fm, H, and NL.
#------------------------------------------------
data$deficiency_light<-paste(data$deficiency, data$light,sep="*")
df_t<-data.frame(data$deficiency_light, data$time,
                 data$Fv.Fm,data$NL,
                 data$H)
names(df_t)<-c("deficiency_light","time","Fv.Fm","NL","H")
df_t$time<-as.character(df_t$time)
df_t<-na.omit(df_t)
df_t<-cSplit(df_t, "deficiency_light", "*")
names(df_t)<-c("time", "Fv.Fm", "NL", "H","Deficiency","Light")
df_t$Deficiency_Light<-paste(df_t$Deficiency, df_t$Light, sep="*")
datasub1<-df_t[df_t$time== "1",]
datasub2<-df_t[df_t$time== "2",]
datasub3<-df_t[df_t$time== "3",]
datasub4<-df_t[df_t$time== "4",]

function.test.residuals.normal(treatment=datasub1$Deficiency_Light, variable=datasub1$H)
function.test.residuals.normal(treatment=datasub2$Deficiency_Light, variable=datasub2$H)
function.test.residuals.normal(treatment=datasub3$Deficiency_Light, variable=datasub3$H)
function.test.residuals.normal(treatment=datasub4$Deficiency_Light, variable=datasub4$H)

function.test.residuals.normal(treatment=datasub1$Deficiency_Light, variable=datasub1$NL)
function.test.residuals.normal(treatment=datasub2$Deficiency_Light, variable=datasub2$NL)
function.test.residuals.normal(treatment=datasub3$Deficiency_Light, variable=datasub3$NL)
function.test.residuals.normal(treatment=datasub4$Deficiency_Light, variable=datasub4$NL)

function.test.residuals.normal(treatment=datasub1$Deficiency_Light, variable=datasub1$Fv.Fm)
function.test.residuals.normal(treatment=datasub2$Deficiency_Light, variable=datasub2$Fv.Fm)
function.test.residuals.normal(treatment=datasub3$Deficiency_Light, variable=datasub3$Fv.Fm)
function.test.residuals.normal(treatment=datasub4$Deficiency_Light, variable=datasub4$Fv.Fm)

function.test.residuals.normal(treatment=data2$deficiency_light, variable=data2$RMR)
function.test.residuals.normal(treatment=data2$deficiency_light, variable=data2$LMR)
function.test.residuals.normal(treatment=data2$deficiency_light, variable=data2$SMR)
function.test.residuals.normal(treatment=data2$deficiency_light, variable=data2$RLR)
function.test.residuals.normal(treatment=data2$deficiency_light, variable=data2$RAB)
function.test.residuals.normal(treatment=data2$deficiency_light, variable=data2$TDM)

function.test.residuals.normal(treatment=data2$deficiency_light, variable=data2$Chl_a)
function.test.residuals.normal(treatment=data2$deficiency_light, variable=data2$Chl_b)
function.test.residuals.normal(treatment=data2$deficiency_light, variable=data2$Chl_a.b)
function.test.residuals.normal(treatment=data2$deficiency_light, variable=data2$Chl_total)

Fv.Fm<-fun.table.anova(variable="Fv.Fm",P1=datasub1$"Fv.Fm",P2=datasub2$"Fv.Fm",P3=datasub3$"Fv.Fm",P4=datasub4$"Fv.Fm")
NL<-fun.table.anova(variable="NL",P1=datasub1$"NL",P2=datasub2$"NL",P3=datasub3$"NL",P4=datasub4$"NL")
H<-fun.table.anova(variable="H",P1=datasub1$"H",P2=datasub2$"H",P3=datasub3$"H",P4=datasub4$"H")

KW_time_course <- data.frame()
KW_time_course <- merge(H, NL, all=TRUE)
KW_time_course <- merge(KW_time_course, Fv.Fm, all=TRUE)

H$.group<-rm.whitespace(H$.group)
NL$.group<-rm.whitespace(NL$.group)
Fv.Fm$.group<-rm.whitespace(Fv.Fm$.group)

time_course <- data.frame()
time_course <- merge(H, NL, all=TRUE)
time_course <- merge(time_course, Fv.Fm, all=TRUE)
write.xlsx(time_course, "Results/Table/Table 1. Kruskal-Wallis for time.xlsx",
           sheetName="Time-course",col.names=TRUE,
           row.names=FALSE, append=FALSE,
           showNA=TRUE, password=NULL)
#------------------------------------------------
pdf(file = "Results/Figure/Fig. Time-course H, NL, Fv.Fm.pdf",
    width = 8, height = 4)
par(xpd = FALSE,mfrow=c(1,1),mgp = c(2.5,0.5,0), mar = c(6.5,4,1,1))
pirateplot(formula = Fv.Fm ~ Deficiency_Light + time, data = df_t, 
           main = "", xlab = "", ylab = "Fv´/Fm´ (±SE)",
           ylim=c(0.0,0.88),point.pch=NA, xaxt = NA,
           bar.f.col=c("white", "gray20"),avg.line.fun=mean,plot=TRUE,
           theme = 4, gl.col = NA,
           avg.line.col = "gray10",
           inf.f.col = "gray10",
           jitter.val = 0.09,
           avg.line.o = 1,bar.b.o=1,
           avg.line.lwd=3,
           inf.lwd=1,
           inf.method="se")
par(xpd = TRUE)
text(c(1:4,6:9,11:14,16:19)-0.5,-0.2,c("(-) N*High", "(-) N*Low", "C*High", "C*Low"),
     srt=45, cex = 1) 
mtext(c(1,2,3,4),side=1, line=4.5, cex=1.175,
      at=c(2.5,7.5,12.5,17.5)) #segments(0, 0.27, 1.5:4.5)
mtext("Time (week)", side=1, line=5.5, cex=1.25, at=10)
text(c(1:4,6:9,11:14,16:19),Fv.Fm$lsmean+0.05,Fv.Fm$.group, cex=1.35)
par(xpd = FALSE)

par(xpd = FALSE,mfrow=c(1,1),mgp = c(2.5,0.5,0), mar = c(6.5,4,1,1))
pirateplot(formula = NL ~ Deficiency_Light + time, data = df_t, 
           main = "", xlab = "", ylab = "Number of leaf (±SE)",
           ylim=c(0.0,12.5),point.pch=NA, xaxt = NA,
           bar.f.col=c("white", "gray20"),avg.line.fun=mean,plot=TRUE,
           theme = 4, gl.col = NA,
           avg.line.col = "gray10",
           inf.f.col = "gray10",
           jitter.val = 0.09,
           avg.line.o = 1,bar.b.o=1,
           avg.line.lwd=3,
           inf.lwd=1,
           inf.method="se")
par(xpd = TRUE)
text(c(1:4,6:9,11:14,16:19)-0.5,-3,c("(-) N*High", "(-) N*Low", "C*High", "C*Low"),
     srt=45, cex = 1) 
mtext(c(1,2,3,4),side=1, line=4.5, cex=1.175,
      at=c(2.5,7.5,12.5,17.5)) #segments(0, 0.27, 1.5:4.5)
mtext("Time (week)", side=1, line=5.5, cex=1.25, at=10)
text(c(1:4,6:9,11:14,16:19),NL$lsmean+1.25,NL$.group, cex=1.35)
par(xpd = FALSE)

par(xpd = FALSE,mfrow=c(1,1),mgp = c(2.5,0.5,0), mar = c(6.5,4,1,1))
pirateplot(formula = H ~ Deficiency_Light + time, data = df_t, 
           main = "", xlab = "", ylab = "Height (±SE)",
           ylim=c(0.0,23),point.pch=NA, xaxt = NA,
           bar.f.col=c("white", "gray20"),avg.line.fun=mean,plot=TRUE,
           theme = 4, gl.col = NA,
           avg.line.col = "gray10",
           inf.f.col = "gray10",
           jitter.val = 0.09,
           avg.line.o = 1,bar.b.o=1,
           avg.line.lwd=3,
           inf.lwd=1,
           inf.method="se")
par(xpd = TRUE)
text(c(1:4,6:9,11:14,16:19)-0.5,-6.5,c("(-) N*High", "(-) N*Low", "C*High", "C*Low"),
     srt=45, cex = 1) 
mtext(c(1,2,3,4),side=1, line=4.5, cex=1.175,
      at=c(2.5,7.5,12.5,17.5)) #segments(0, 0.27, 1.5:4.5)
mtext("Time (week)", side=1, line=5.5, cex=1.25, at=10)
text(c(1:4,6:9,11:14,16:19),H$lsmean+1.3,H$.group, cex=1.35)
par(xpd = FALSE)
dev.off()
#------------------------------------------------


#------------------------------------------------
# Fig. Dry mass partitioning RMR, LMR, and SMR.
#      RLR, TDM, Chl_a, Chl_b, Chl_a.b, and Chl_total in table
#------------------------------------------------
RMR<-fun.table.nested.anova(variable="RMR",P4=data2$RMR,
                          d1= data2$RMR[data2$light=="High"],
                          d2= data2$RMR[data2$light=="Low"])
LMR<-fun.table.nested.anova(variable="LMR",P4=data2$LMR,
                            d1= data2$LMR[data2$light=="High"],
                            d2= data2$LMR[data2$light=="Low"])
SMR<-fun.table.nested.anova(variable="SMR",P4=data2$SMR,
                            d1= data2$SMR[data2$light=="High"],
                            d2= data2$SMR[data2$light=="Low"])
RLR<-fun.table.nested.anova(variable="RLR",P4=data2$RLR,
                            d1= data2$RLR[data2$light=="High"],
                            d2= data2$RLR[data2$light=="Low"])
TDM<-fun.table.nested.anova(variable="TDM",P4=data2$TDM,
                            d1= data2$TDM[data2$light=="High"],
                            d2= data2$TDM[data2$light=="Low"])
Chl_a<-fun.table.nested.anova(variable="Chl_a",P4=data2$Chl_a,
                            d1= data2$Chl_a[data2$light=="High"],
                            d2= data2$Chl_a[data2$light=="Low"])
Chl_b<-fun.table.nested.anova(variable="Chl_b",P4=data2$Chl_b,
                            d1= data2$Chl_b[data2$light=="High"],
                            d2= data2$Chl_b[data2$light=="Low"])
Chl_a.b<-fun.table.nested.anova(variable="Chl_a.b",P4=data2$Chl_a.b,
                            d1= data2$Chl_a.b[data2$light=="High"],
                            d2= data2$Chl_a.b[data2$light=="Low"])
Chl_total<-fun.table.nested.anova(variable="Chl_total",P4=data2$Chl_total,
                            d1= data2$Chl_total[data2$light=="High"],
                            d2= data2$Chl_total[data2$light=="Low"])
nested_anova <- data.frame()
nested_anova <- merge(RMR, LMR, all=TRUE)
nested_anova <- merge(nested_anova, SMR, all=TRUE)
nested_anova <- merge(nested_anova, RLR, all=TRUE)
nested_anova <- merge(nested_anova, TDM, all=TRUE)
nested_anova <- merge(nested_anova, Chl_a, all=TRUE)
nested_anova <- merge(nested_anova, Chl_b, all=TRUE)
nested_anova <- merge(nested_anova, Chl_a.b, all=TRUE)
nested_anova <- merge(nested_anova, Chl_total, all=TRUE)
write.xlsx(nested_anova, "Results/Table/Table 2. Nested ANOVA.xlsx",
           sheetName="Nested ANOVA",col.names=TRUE,
           row.names=FALSE, append=FALSE,
           showNA=TRUE, password=NULL)
#------------------------------------------------
pdf(file = "Results/Figure/Fig. Dry mass partitioning.pdf", width = 5, height = 5)
Tratamientos=factor(c(1,1,1,2,2,2,3,3,3,4,4,4), 
                    labels =c("High*(-)N","High*C","Low*(-)N","Low*C"))  
Biomasa=factor(c(1,2,3,1,2,3,1,2,3,1,2,3),labels = c("1_SMR" , "3_LMR" , "3_RMR"))                 
Porcentaje.de.particion=c(0.09,0.18,0.21,                  
                          0.34,0.91,0.43,
                          0.06,0.15,0.12,
                          0.10,0.32,0.13)
data=data.frame(Tratamientos,Biomasa,Porcentaje.de.particion) 
ggplot(data, aes(fill=Biomasa, y=Porcentaje.de.particion, x=Tratamientos)) +
    geom_bar( stat="identity", position="fill",color="black")+                  
    scale_fill_manual("Dry mass", values=c("gray95", "gray50","gray10")) +
    labs(y = "Dry mass partitioning", x= "Treatment") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#------------------------------------------------


#------------------------------------------------
# Fig. Ternary plot: RMR, SMR, LMR, Fv.Fm, and TDM.
#------------------------------------------------
df<-data.frame(data2$deficiency_light, 
               data2$Fv.Fm,data2$RMR,
               data2$SMR,data2$LMR,
               data2$TDM)
names(df) <- c("deficiency_light", "Fv.Fm", "RMR", "SMR", "LMR", "TDM")
df = df[with(df, order(-TDM)), ]
df<-na.omit(df)

pdf(file = "Results/Figure/Fig. Ternary plot.pdf", width = 6*0.95, height = 5.5*0.95)
par(mfrow=c(1,1),mgp = c(1.75,0.5,0), mar = c(1.5,3,1,1))
a<-ggtern(data = df[-57,], aes(x = RMR, y = SMR, z = LMR)) +
    #the layers
    geom_mask() + #MASK UNDER POINTS
    geom_point(aes(fill = Fv.Fm,
                   size = TDM,
                   shape = deficiency_light)) +
    #scales
    scale_shape_manual(values = c(21,22,23,24)) +
    scale_size_continuous(range = c(1,  7)) +
    scale_fill_gradient(low = 'white', high = 'black') +
    #theme tweaks
    theme_bw()  +
    theme_showarrows() + 
    theme(legend.position      = c(0, 1),
          legend.justification = c(0, 1),
          legend.box.just      = 'left') +
    #tweak guides
    guides(shape= guide_legend(order   =1,
                               override.aes=list(size=5)),
           size = guide_legend(order   =3),
           fill = guide_colourbar(order=2)) +
    #labels and title
    labs(size = 'TDM',
         fill = 'Fv´/Fm´') +
    ggtitle('') 
a<-a + scale_T_continuous(limits=c(.10,.55))  +
    scale_L_continuous(limits=c(.15,.60))  + 
    scale_R_continuous(limits=c(.30,.75))  
print(a)
dev.off()
#------------------------------------------------


#------------------------------------------------
# Fig. Spearman - Correlations.
#------------------------------------------------
RMR <- lm(Fv.Fm ~ RMR, data = df)
summary(RMR)
confint(RMR)

SMR <- lm(Fv.Fm ~ SMR, data = df)
summary(SMR)
confint(SMR)

LMR <- lm(Fv.Fm ~ log(LMR), data = df)
summary(SMR)
confint(SMR)

shapiro.test(df$Fv.Fm)
shapiro.test(df$RMR)
shapiro.test(df$LMR)
shapiro.test(df$SMR)

cor.test(df$Fv.Fm, df$RMR, method = "spearman") # ***
cor.test(df$Fv.Fm, df$LMR, method = "spearman") # ***
cor.test(df$Fv.Fm, df$SMR, method = "spearman") # **
cor.test(df$RMR, df$LMR, method = "spearman")   # ***
cor.test(df$RMR, df$SMR, method = "spearman")   # ***
cor.test(df$LMR, df$SMR, method = "spearman")   # n.s.

d <- df[, c(2,3,5,4)]
hc <- hclust(as.dist(1-cor(d, method='spearman', use='pairwise.complete.obs')))
#hc.order <- order.dendrogram(as.dendrogram(hc))
#d <- d[ ,hc]#d[ ,hc.order]
gr <- as.factor(df$deficiency_light)

cols.key <- scales::muted(c('black', 'black', 'black', "black"))
cols.key <- adjustcolor(cols.key, alpha.f=1)
pchs.key <- c(19,15,18,17)

panel.hist <- function(x, ...) {
    usr <- par('usr'); on.exit(par(usr))
    par(usr=c(usr[1:2], 0, 1.5))
    h <- hist(x, plot=FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col='gray', ...)
}
panel.cor <- function(x, y, ...){
    usr <- par('usr'); on.exit(par(usr))
    par(usr=c(0,1,0,1))
    r <- cor(x, y, method='spearman', use='pairwise.complete.obs')
    zcol <- lattice::level.colors(r, at=seq(-1, 1, length=81), col.regions=colorRampPalette(c(scales::muted('red'),'white',scales::muted('blue')), space='rgb')(81))
    ell <- ellipse::ellipse(r, level=0.95, type='l', npoints=50, scale=c(.2, .2), centre=c(.5, .5))
    polygon(ell, col=zcol, border=zcol, ...)
    text(x=.5, y=.5, lab=100*round(r, 2), cex=2, col='black')
    # pval <- cor.test(x, y, method='spearman', use='pairwise.complete.obs')$p.value
    # sig <- symnum(pval, corr=FALSE, na=FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c('***', '**', '*', '.', ' '))
    # text(.6, .8, sig, cex=2, col='gray20')
}
panel.scatter <- function(x, y){
    points(x, y, col=cols.key[gr], pch=pchs.key[gr], cex=1.15)
    lines(lowess(x, y))
}

pdf(file = "Results/Figure/Fig. Spearman correlations.pdf", width = 4.5*0.95, height = 4.5*0.95)
pairs(d,
      diag.panel=panel.hist,
      lower.panel=panel.scatter,
      upper.panel=panel.cor,
      gap=0.5,
      labels=gsub('\\.', '\n', colnames(d)),
      label.pos=0.7,
      cex.labels=1.4
)
dev.off()
#------------------------------------------------


#------------------------------------------------
# Fig. Relationship RMR and LMR.
#------------------------------------------------
df.PCA<-data.frame(data2$deficiency_light,data2$Fv.Fm,
                   data2$RMR, data2$LMR, data2$SMR,
                   data2$NL,data2$H,data2$RAB,data2$TDM)
names(df.PCA) <- c("Treatment", "Fv.Fm", "RMR", "LMR", "SMR","NL", "H","RAB","TDM")
df.PCA = df.PCA[with(df.PCA, order(-TDM)), ]
df.PCA<-na.omit(df.PCA)

df.PCA$c2 <- (df.PCA$Fv.Fm/max(df.PCA$Fv.Fm))
df.PCA$Z <- cut(df.PCA$Fv.Fm,length(df.PCA$Fv.Fm), label = FALSE)
colr <- rev(heat.colors(length(df.PCA$Fv.Fm)))

c2 <- (df.PCA$Fv.Fm/max(df.PCA$Fv.Fm))
df.PCA$col<- rgb(0, 0, 0, c2)

pdf(file = "Results/Figure/Fig. Relationship RMR and LMR.pdf", width = 6, height = 5)
par(xpd = FALSE,mfrow=c(1,1),mgp = c(1.75,0.5,0), mar = c(3,3,0.5,2.25))
plot(df.PCA$RMR, df.PCA$LMR, pch=19, cex=0, xlim = c(0.15,0.517), ylim = c(0.3,0.66),
     xlab="RMR", ylab="LMR", cex.lab=1.25)
points(df.PCA$RMR[df.PCA$Treatment=="(-) N*High"], df.PCA$LMR[df.PCA$Treatment=="(-) N*High"], pch=19,
       col= colr[df.PCA$Z[df.PCA$Treatment=="(-) N*High"]], cex=2)#df.PCA$col[df.PCA$Treatment=="(-) N*High"], cex=2)
points(df.PCA$RMR[df.PCA$Treatment=="(-) N*Low"], df.PCA$LMR[df.PCA$Treatment=="(-) N*Low"], pch=15,
       col= colr[df.PCA$Z[df.PCA$Treatment=="(-) N*Low"]], cex=2)#df.PCA$col[df.PCA$Treatment=="(-) N*Low"], cex=2)
points(df.PCA$RMR[df.PCA$Treatment=="C*High"], df.PCA$LMR[df.PCA$Treatment=="C*High"], pch=18,
       col= colr[df.PCA$Z[df.PCA$Treatment=="C*High"]], cex=2)#df.PCA$col[df.PCA$Treatment=="C*High"], cex=2)
points(df.PCA$RMR[df.PCA$Treatment=="C*Low"], df.PCA$LMR[df.PCA$Treatment=="C*Low"], pch=17,
       col= colr[df.PCA$Z[df.PCA$Treatment=="C*Low"]], cex=2)#df.PCA$col[df.PCA$Treatment=="C*Low"], cex=2)
reg<-lm(df.PCA$LMR~df.PCA$RMR)
s_reg<-summary(reg)
s_reg
r2<-round(s_reg$r.squared, 2)
i<-round(s_reg$coefficients[1,1],3)
m<-round(s_reg$coefficients[2,1],3)
p<-"p<0.001"
text(0.4,0.65, paste("LMR = ", i, "-",m*-1, "*RMR"), cex=1.25)
text(0.435,0.624, paste("r2= ", r2, "; ",p), cex=1.25)
abline(reg, lwd=2, col="black", lty=2)
legend("bottomleft", c("(-) N*High","(-) N*Low", "C*High", "C*Low"),pch=c(19,15,18,17),merge = F, bg = NULL,bty='n', h=FALSE, cex=1.25)

data_f.k2 = mvnormalmixEM(as.matrix(df.PCA[df.PCA$Treatment=="(-) N*High",c(3,4)]), k=2, maxit=100, epsilon=0.05) 
data_f.k2$mu # estimated mean coordinates for the 2 multivariate Gaussians
data_f.k2$sigma # estimated covariance matrix 
for (i in 1: length(data_f.k2$mu))  ellipse(data_f.k2$mu[[i]],data_f.k2$sigma[[i]])

data_f.k2 = mvnormalmixEM(as.matrix(df.PCA[df.PCA$Treatment=="(-) N*Low",c(3,4)]), k=2, maxit=100, epsilon=0.05) 
data_f.k2$mu # estimated mean coordinates for the 2 multivariate Gaussians
data_f.k2$sigma # estimated covariance matrix 
for (i in 1: length(data_f.k2$mu))  ellipse(data_f.k2$mu[[i]],data_f.k2$sigma[[i]])

data_f.k2 = mvnormalmixEM(as.matrix(df.PCA[df.PCA$Treatment=="C*High",c(3,4)]), k=2, maxit=100, epsilon=0.05) 
data_f.k2$mu # estimated mean coordinates for the 2 multivariate Gaussians
data_f.k2$sigma # estimated covariance matrix 
for (i in 1: length(data_f.k2$mu))  ellipse(data_f.k2$mu[[i]],data_f.k2$sigma[[i]])

data_f.k2 = mvnormalmixEM(as.matrix(df.PCA[df.PCA$Treatment=="C*Low",c(3,4)]), k=2, maxit=100, epsilon=0.05) 
data_f.k2$mu # estimated mean coordinates for the 2 multivariate Gaussians
data_f.k2$sigma # estimated covariance matrix 
for (i in 1: length(data_f.k2$mu))  ellipse(data_f.k2$mu[[i]],data_f.k2$sigma[[i]])

legend.col <- function(col, lev){
    opar <- par
    n <- length(col)
    bx <- par("usr")
    box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
                bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
    box.cy <- c(bx[3], bx[3])
    box.sy <-  (bx[4]- bx[3]) / n
    xx <- rep(box.cx, each = 2)
    par(xpd = TRUE)
    for(i in 1:n){
        yy <- c(box.cy[1] + (box.sy * (i - 1)),
                box.cy[1] + (box.sy * (i)),
                box.cy[1] + (box.sy * (i)),
                box.cy[1] + (box.sy * (i - 1)))
        polygon(xx, yy, col = col[i], border = col[i])
    }
    par(new = TRUE)
    plot(0, 0, type = "n",
         ylim = c(min(lev), max(lev)),
         yaxt = "n", ylab = "",
         xaxt = "n", xlab = "",
         frame.plot = FALSE)
    axis(side = 4, las = 2, tick = FALSE, line = .25)
    par <- opar
}
legend.col(col = colr, lev = df.PCA$Fv.Fm)
par(xpd = FALSE)
dev.off()
#------------------------------------------------


#------------------------------------------------
# Fig. PCA plot.
#------------------------------------------------
df.PCA<-data.frame(data2$deficiency_light,data2$Fv.Fm,
                   data2$RMR, data2$LMR, data2$SMR,
                   data2$NL,data2$H,data2$RAB,data2$TDM)
names(df.PCA) <- c("Treatment", "Fv.Fm", "RMR", "LMR", "SMR","NL", "H","RAB","TDM")
df.PCA = df.PCA[with(df.PCA, order(-TDM)), ]
df.PCA<-na.omit(df.PCA)

res.pca1 <- prcomp(df.PCA[, c(-1,-9)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[,1]) # Only treatment

a1<-fviz_pca_ind(res.pca1, geom = c("point"), title="",
                 habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.60) 
a1<- a1 + theme_minimal()
a1<- a1 +  scale_shape_manual(values = c(19,15,18,17))

b1<-fviz_pca_var(res.pca1, col.var="coord", title="") 
b1<- b1 +  scale_color_gradient2(low="white", mid="gray", high="black", midpoint=0.50)
b1<- b1 + theme_minimal()

pdf(file = "Results/Figure/Fig. PCA.pdf", width = 9, height = 3.75)
plot_grid(b1,a1,
          labels=c(""),
          ncol = 2, nrow = 1)
dev.off()
#------------------------------------------------


# - The end - 
