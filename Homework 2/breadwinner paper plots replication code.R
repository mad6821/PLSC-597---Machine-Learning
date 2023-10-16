####################################################################################
####################################################################################
####  REPLICATION CODE FOR TO EMERGE? ####
####################################################################################


####################################################################################
####  PACKAGES ####
####################################################################################

library(foreign)
library(ggplot2)
library(RColorBrewer)
library(stargazer)
library(dplyr)
library(tidyr)
library(ggthemes)
library(broom)
library(data.table)
library(grid)
library(gridExtra)
library(arsenal)


####################################################################################
####  DATA ####
####################################################################################


#### Load data ####
# Set working directory to folder containing file and data folder
raw.data <- read.dta("Data/final_survey_data.dta")
df <- raw.data

offices <- read.dta("Data/emerge_officesrun.dta")
names(offices)[22] <- "state.o"
df <- merge(df, offices, by="responseid", all.x=T, all.y=F)
df$lvls <- as.numeric(df$lvls)

#### Clean variables ####
# Create variables
# Dependent variable
df$run <-ifelse(df$ran=="Ran", 1,
                ifelse(df$ran=="Didn't Run", 0,NA))
# Subsetting variable
df$novice <- ifelse(df$reasons=="previous", 0, 
                    ifelse(is.na(df$reasons), NA, 1))
# Key independent variables
df$singlehh <- ifelse(df$marstatus=="Single"|df$marstatus=="Widowed"|df$marstatus=="Divorced"|df$marstatus=="Separated", 1,
                      ifelse(df$marstatus=="Living with a partner"|df$marstatus=="Married", 0, NA))
df$job <- ifelse(df$employed=="Yes, full-time" | df$employed=="Yes, full-time", 1, 
                 ifelse(is.na(df$employed), NA, 0))
df$hhexpenses <- as.numeric(df$hhexpenses)
df$hhexpenses <- ifelse(grepl("No", df$employed)==T, 0, df$hhexpenses)
df$inc <- (as.numeric(df$income)-1)/8
df$binincome <- ifelse(as.numeric(df$income)<=2, 1,
                       ifelse(as.numeric(df$income)<=4, 2,
                              ifelse(as.numeric(df$income)<=6, 3,
                                     ifelse(as.numeric(df$income)<=8, 4, 5))))
df$newbininc <- ifelse(df$income=="<$30k",1,
                       ifelse(df$income=="$30-40k",1,
                              ifelse(df$income=="$40-50k",1,
                                     ifelse(df$income=="$50-60k",2,
                                            ifelse(df$income=="$60-70k",2,
                                                   ifelse(df$income=="$70-80k",2,
                                                          ifelse(df$income=="$80-90k",2,
                                                                 ifelse(df$income=="$90-100k",2,
                                                                        ifelse(df$income=="$100k+",3,NA)))))))))
# Multivariate control variables
df$eth <- ifelse(df$race=="African American", "Black",
                 ifelse(df$race=="Asian" | df$race=="Pacific Islander", "AAPI",
                        ifelse(df$race=="Hispanic/Latino", "Latinx",
                               ifelse(df$race=="White/Caucasian", "White",
                                      ifelse(is.na(df$race), NA, "Other/Prefer Not to Say")))))
df$nascent <- ifelse(df$reasons=="running", 1,
                     ifelse(df$reasons=="considering", .5, 
                            ifelse(df$reasons=="curious" | df$reasons=="pushed", 0, NA)))
df$activedem <- ifelse(grepl("not", df$activeparty)==T, 0,
                             ifelse(grepl("little", df$activeparty)==T, 1,
                                          ifelse(grepl("somewhat", df$activeparty)==T, 2,
                                                       ifelse(grepl("very", df$activeparty)==T, 3,NA))))
df$educ <- (df$edu-1)/8
df$vet <- ifelse(df$armedforces=="Yes", 1,
                 ifelse(df$armedforces=="No", 0, NA))
df$renter <- ifelse(df$rentown=="Rent", 1,
                    ifelse(df$rentown=="Own", 0, NA))
df$lgbt <- ifelse(is.na(df$lbgtq), NA, 
                  ifelse(df$lbgtq=="No", 0, 1))
df$kids <- ifelse(is.na(df$children), NA,
                  ifelse(df$children=="No", 0, 1))
df$psychindex <- rowMeans(df[43:58], na.rm=T)






####################################################################################
####  MAIN FINDINGS ####
####################################################################################


#### Figure 2: Bar Chart of Qualitative Responses to Why Not Run ####
# Based on primary consideration recodings: 

qual <- data.frame(c("Structural/Resources", "Political Environment", "Fears", "Motivations"),
                   c(.52, .46, .27, .25))
names(qual) <- c("Factor", "Percentage")
qual$Factor <- factor(qual$Factor, levels=c( "Motivations", "Fears", "Political Environment", "Structural/Resources"))

plot1 <- ggplot(qual, aes(x=Factor, y=Percentage)) +
  geom_bar(stat="identity") + 
  theme_minimal(base_size = 18) + 
  coord_flip() +
  theme(axis.title = element_text(), 
        axis.text.y = element_text(face="bold"),
        plot.title = element_text(size = rel(1.1), hjust = 0.5),
        plot.subtitle = element_text(size=16, color = "darkslategrey", face = "bold.italic", hjust=.5),
        plot.caption = element_text(size = rel(.8), margin = margin(t = 10), color = "grey30", hjust = 0.5)) +
  labs(subtitle = "Why haven't you run for office yet?",
       # title = "Deterrent Factors Cited by Respondents Not Seeking Office",
       # caption = "Primary codings only listed here.", 
       x = "", 
       y = "Percentage of Total Responses (n=216)") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) 

ggsave(file="Plots/fig2_qualcoding.eps", plot1, width=10, height=4)



#### Figure 3: Breadwinner Constraint ####

# Just novices - see appendices for other versions 

## INCOME CONSTRAINT
df2i <- df[!is.na(df$ran)&df$reasons!="previous", c("binincome", "run")]
df2i <- df2i[!is.na(df2i$binincome),]

# Statistical test
summary(lm(data=df2i, run~as.numeric(binincome)))

# Plotting
labels2i <- c("$0-40k", "$41-60k", "$61-80k", "$81-100k", "$101k+")

df2ip <- df2i[c("binincome", "run")] %>%
  group_by(binincome) %>%
  summarise_all(funs(mean=mean(., na.rm=T), sd=sd(., na.rm=T), n=sum(!is.na(.))))

df2ip$lowci <- df2ip$mean - (1.96*df2ip$sd)/sqrt(df2ip$n)
df2ip$highci <- df2ip$mean + (1.96*df2ip$sd)/sqrt(df2ip$n)

p2i <- ggplot(df2ip, aes(x=binincome, y=mean,  ymin=lowci, ymax=highci, label=n)) + 
  geom_pointrange(size=.8, fatten = 4) +
  geom_line(position=position_dodge(width=.1), size=1) +
  geom_label(vjust=0, nudge_y=-.2) + 
  theme_minimal(base_size = 18) + 
  theme(legend.position = "none",
        plot.margin = margin(10,10,10,0),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(size = rel(.9)), 
        axis.text.x = element_text(face="bold", margin=margin(10,0,10,0)),
        axis.text.y = element_text(face="bold", margin=margin(0,10,0,10))) +
  labs(x = "Total Household Income\n(n=562)", 
       y = "") + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits = c(0,.7), expand = c(0,0)) +
  scale_x_continuous(labels=labels2i)


### Main plot for breadwinner constraint

#  Novices only
df2b <- df[!is.na(df$employed)&!is.na(df$run)&df$reasons!="previous", c("hhexpenses", "run", "employed", "income")]

# Statistical analysis
summary(lm(data=df2b, run~hhexpenses))

labels2b <- c("0%", "1-25%", "26-50%", "51-75%", "76-100%")

df2bp <- df2b[c("hhexpenses", "run")] %>%
  group_by(hhexpenses) %>%
  summarise_all(funs(mean=mean(., na.rm=T), sd=sd(., na.rm=T), n=sum(!is.na(.))))

df2bp$lowci <- df2bp$mean - (1.96*df2bp$sd)/sqrt(df2bp$n)
df2bp$highci <- df2bp$mean + (1.96*df2bp$sd)/sqrt(df2bp$n)

p2b <- ggplot(df2bp, aes(x=hhexpenses, y=mean,  ymin=lowci, ymax=highci, label=n)) + 
  geom_pointrange(size=.8, fatten = 4) +
  geom_line(position=position_dodge(width=.1), size=1) +
  geom_label(vjust=0, nudge_y=-.2) + 
  theme_minimal(base_size = 18) + 
  theme(legend.position = "none",
        plot.margin = margin(10,20,10,10),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(size = rel(.9)), 
        axis.text.x = element_text(face="bold", margin=margin(10,0,10,0)),
        axis.text.y = element_text(face="bold", margin=margin(0,0,0,0))) +
  labs(x = "Share of Household Income Contributed\n(n = 551)", 
       y = "Run Rate") + 
  scale_x_continuous(labels=labels2b) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits = c(.0,.7), expand = c(0,0)) 

fig2all<-grid.arrange(p2b, p2i, ncol = 2)

ggsave(file="Plots/fig3_breadwinners_novices.eps", fig2all, width=10, height=5)




#### Figure 4: Relationship between hhexpenses, income, and running ####

df3 <- df[!is.na(df$employed)&!is.na(df$run)&df$reasons!="previous",]

### HH Contributions

# Bivariate
hh.bi <- lm(data=df3[df3$hhexpenses!="",], run~hhexpenses)
hhcis.bi <- data.frame(confint(hh.bi))[2,]

# FEs for Emerge state and year
hh.fes <- lm(data=df3[df3$hhexpenses!="",], run~hhexpenses+
               state+yeargrad)
hhcis.fes <- data.frame(confint(hh.fes))[2,]

# Demographic variables
hh.demo <- lm(data=df3[df3$hhexpenses!="",], run~hhexpenses+
               state+yeargrad+
               eth+edu+area+lgbt)
hhcis.demo <- data.frame(confint(hh.demo))[2,]

# Political environment
hh.pol <- lm(data=df3[df3$hhexpenses!="",], run~hhexpenses+
               state+yeargrad+
               eth+edu+area+lgbt+
                nascent+activedem)
hhcis.pol <- data.frame(confint(hh.pol))[2,]

# Psychological fears 
hh.psy <- lm(data=df3[df3$hhexpenses!="",], run~hhexpenses+
               state+yeargrad+
               eth+edu+area+lgbt+
               nascent+activedem+
               psychindex)
hhcis.psy <- data.frame(confint(hh.psy))[2,]

# Family variables 
hh.fam <- lm(data=df3[df3$hhexpenses!="",], run~hhexpenses+
               state+yeargrad+
               eth+edu+area+lgbt+
               nascent+activedem+
               psychindex + 
               singlehh+kids)
hhcis.fam <- data.frame(confint(hh.fam))[2,]

# Combine into dataframe 
hhdf <- data.frame(rbind(hhcis.bi, hhcis.fes, hhcis.demo, hhcis.pol, hhcis.psy, hhcis.fam))
names(hhdf) <- c("low", "high")
hhdf$mean <- as.numeric(hhdf$high-abs(((hhdf$high-hhdf$low)/2)))
hhdf$vars <- row.names(hhdf)

row.names(hhdf) <- NULL

hhdf$sig <- ifelse(hhdf$low<0&hhdf$high<0, 1,
                    ifelse(hhdf$low>0&hhdf$high>0, 1,0))

hhdf$model <- c(5, 4, 3, 2, 1, 0)
hhdf$var <- "Household\nContribution"

  
### Income

# Bivariate
inc.bi <- lm(data=df3[df3$inc!="",], run~binincome)
inccis.bi <- data.frame(confint(inc.bi))[2,]

# FEs for Emerge state and year
inc.fes <- lm(data=df3[df3$inc!="",], run~binincome+
               state+yeargrad)
inccis.fes <- data.frame(confint(inc.fes))[2,]

# Demographic variables
inc.demo <- lm(data=df3[df3$inc!="",], run~binincome+
                state+yeargrad+
                eth+edu+area+lgbt)
inccis.demo <- data.frame(confint(inc.demo))[2,]

# Political environment
inc.pol <- lm(data=df3[df3$inc!="",], run~binincome+
               state+yeargrad+
               eth+edu+area+lgbt+
               nascent+activedem)
inccis.pol <- data.frame(confint(inc.pol))[2,]

# Psychological fears 
inc.psy <- lm(data=df3[df3$inc!="",], run~binincome+
               state+yeargrad+
               eth+edu+area+lgbt+
               nascent+activedem+
               psychindex)
inccis.psy <- data.frame(confint(inc.psy))[2,]

# Family variables 
inc.fam <- lm(data=df3[df3$inc!="",], run~binincome+
                state+yeargrad+
                eth+edu+area+lgbt+
                nascent+activedem+
                psychindex + 
                singlehh+kids)
inccis.fam <- data.frame(confint(inc.fam))[2,]

# Combine into dataframe 
incdf <- data.frame(rbind(inccis.bi, inccis.fes, inccis.demo, inccis.pol, inccis.psy, inccis.fam))
names(incdf) <- c("low", "high")
incdf$mean <- as.numeric(incdf$high-abs(((incdf$high-incdf$low)/2)))
incdf$vars <- row.names(incdf)

row.names(incdf) <- NULL

incdf$sig <- ifelse(incdf$low<0&incdf$high<0, 1,
                   ifelse(incdf$low>0&incdf$high>0, 1,0))

incdf$model <- c(5, 4, 3, 2, 1, 0)
incdf$var <- "Income"

## Combine into single dataframe
coefs <- rbind(hhdf,incdf)

### Plot 

labels3 <- c("Bivariate", 
             "+ Emerge Fixed Effects",
             "+ Demographics",
             "+ Political Environment",
             "+ Psychological Fears",
             "+ Household Composition")

# Legend Key Coordinate Flip
GeomPointrange$draw_key <-  function (data, params, size)     {
  
  draw_key_vpath <- function (data, params, size) {
    # only need to change the x&y coords so that the line is horizontal
    # originally, the vertical line was `0.5, 0.1, 0.5, 0.9`
    segmentsGrob(0.1, 0.5, 0.9, 0.5, 
                 gp = gpar(col = alpha(data$colour, data$alpha), 
                           lwd = data$size * .pt, lty = data$linetype, 
                           lineend = "butt"), arrow = params$arrow)
  }
  
  grobTree(draw_key_vpath(data, params, size), 
           draw_key_point(transform(data, size = data$size * 4), params))
}

## Full Plot 
plot3 <- ggplot(coefs, aes(x=model, y=mean, ymin=low, ymax=high, group=var)) +
  geom_pointrange(position=position_dodge(width=1), size=1) + 
  theme_minimal(base_size = 24) + 
  facet_grid(~var) +
  coord_flip() +
  geom_hline(yintercept=0, lty=2) +
  theme_minimal(base_size = 20) + 
  theme(legend.position = "bottom",
        panel.spacing=unit(2, "lines"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face="bold"),
        axis.title = element_text(), 
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold")) +
  labs(x = "", 
       y = "Change in Run Rate") + 
  scale_x_continuous(labels=labels3,
                     breaks=c(5,4,3,2,1,0)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) 

ggsave(file="Plots/fig4_multivariate.eps", plot3, width=10, height=6)

#### Replicate Figure 4 with lost income measure ####
# Have to update psych index to remove fear of lost income from other variables
df3$psych2 <- ifelse(df3$loseincome==1,df3$psychindex-(1/15),df3$psychindex)


# Bivariate
li.bi <- lm(data=df3[!is.na(df3$loseincome),], run~loseincome)
licis.bi <- data.frame(confint(li.bi))[2,]

# FEs for Emerge state and year
li.fes <- lm(data=df3[!is.na(df3$loseincome),], run~loseincome+
               state+yeargrad)
licis.fes <- data.frame(confint(li.fes))[2,]

# Demographic variables
li.demo <- lm(data=df3[!is.na(df3$loseincome),], run~loseincome+
                state+yeargrad+
                eth+edu+area+lgbt)
licis.demo <- data.frame(confint(li.demo))[2,]

# Political environment
li.pol <- lm(data=df3[!is.na(df3$loseincome),], run~loseincome+
               state+yeargrad+
               eth+edu+area+lgbt+
               nascent+activedem)
licis.pol <- data.frame(confint(li.pol))[2,]

# Psychological fears 
li.psy <- lm(data=df3[!is.na(df3$loseincome),], run~loseincome+
               state+yeargrad+
               eth+edu+area+lgbt+
               nascent+activedem+
               psych2)
licis.psy <- data.frame(confint(li.psy))[2,]

# Family variables 
li.fam <- lm(data=df3[!is.na(df3$loseincome),], run~loseincome+
               state+yeargrad+
               eth+edu+area+lgbt+
               nascent+activedem+
               psych2 + 
               singlehh+kids)
licis.fam <- data.frame(confint(li.fam))[2,]

# Combine into dataframe 
lidf <- data.frame(rbind(licis.bi, licis.fes, licis.demo, licis.pol, licis.psy, licis.fam))
names(lidf) <- c("low", "high")
lidf$mean <- as.numeric(lidf$high-abs(((lidf$high-lidf$low)/2)))
lidf$vars <- row.names(lidf)

row.names(lidf) <- NULL

lidf$sig <- ifelse(lidf$low<0&lidf$high<0, 1,
                   ifelse(lidf$low>0&lidf$high>0, 1,0))

lidf$model <- c(5, 4, 3, 2, 1, 0)
lidf$var <- "Lost Income"


plot6 <- ggplot(lidf, aes(x=model, y=mean, ymin=low, ymax=high)) +
  geom_pointrange(position=position_dodge(width=1), size=1) + 
  theme_minimal(base_size = 24) + 
  coord_flip() +
  geom_hline(yintercept=0, lty=2) +
  theme_minimal(base_size = 20) + 
  theme(legend.position = "bottom",
        panel.spacing=unit(2, "lines"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face="bold"),
        axis.title = element_text(), 
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold")) +
  labs(x = "", 
       y = "Change in Run Rate") + 
  scale_x_continuous(labels=labels3,
                     breaks=c(5,4,3,2,1,0)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) 

ggsave(file="Plots/fig5_subjectiveloss.eps", plot6, width=10, height=6)



#### Figure 6: Breadwinning by parenting and partner status ####

df4 <- df[!is.na(df$employed)&!is.na(df$run)&df$reasons!="previous", c("hhexpenses", "run", "employed", "income", "singlehh", "children", "childage", "agegroup")]
df4 <- df4[!is.na(df4$children)&!is.na(df4$hhexpenses),]
df4 <- df4[!is.na(df4$singlehh),]

# Statistical analysis
summary(lm(data=df4, run~hhexpenses*singlehh))
  # hhexpenses stays neg and sig, no interaction
summary(lm(data=df4, run~hhexpenses*children))
  # hhexpenses zeroes out, but interaction effect is sig and negative
summary(lm(data=df4, run~hhexpenses*children*singlehh))

labels4 <- c("0%", "1-25%", "26-50%", "51-75%", "76-100%")

df4$grp <- ifelse(df4$children=="Yes" & df4$singlehh==1, "Single Mothers",
                   ifelse(df4$children=="No" & df4$singlehh==1, "Single, No Children",
                          ifelse(df4$children=="No" & df4$singlehh==0, "Partnered, No Children", "Partnered Mothers")))
df4$grp <- factor(df4$grp, levels=c("Partnered Mothers", "Partnered, No Children", "Single Mothers", "Single, No Children"))


# fit model and get lower/upper 95% confidence limits at the observed x's
mods <- glm(run ~ hhexpenses*singlehh*children, data = df4, family = binomial)
mpreds <- predict.glm(mods, type="response", se.fit=T)
mpred <- data.frame(cbind(mpreds[[1]], mpreds[[2]]))
names(mpred) <- c("pred.fit", "se.fit")

# bind predictions to original data and plot
df4 <- cbind(df4, mpred)

p4 <- ggplot(df4, aes(x=hhexpenses, y=pred.fit)) + 
  facet_wrap(.~grp, ncol=2, scales = "fixed") + 
  geom_smooth(method = 'lm', se = FALSE, color="black") +
  geom_line(aes(y = pred.fit+(1.96*se.fit)), color = 'black', linetype = 2) +
  geom_line(aes(y = pred.fit-(1.96*se.fit)), color = 'black', linetype = 2) + 
  theme_minimal(base_size = 18) + 
  theme(legend.title=element_blank(),
        panel.spacing=unit(2, "lines"),
        plot.margin = margin(10,20,10,20),
        panel.grid.minor = element_blank(),
        axis.title = element_text(), 
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold")) +
  labs(x = "\nShare of Household Income Contributed (n=539)", 
       y = "Predicted Run Rate") + 
  scale_x_continuous(labels=labels4) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1))

ggsave(file="Plots/fig6_moms_single_log.eps", p4, width=10, height=6)


#### MISC. IN-TEXT FIGURES
### Breadwinning and income effects by level of office first sought 
df3$hhbinary <- ifelse(df3$hhexpenses>2,1,0)
t.test(data=df3, run~hhbinary)

summary(lm(data=df3[df3$hhexpenses!="",], lvls~inc)) # No relationship
summary(lm(data=df3[df3$hhexpenses!="",], lvls~hhexpenses)) # Small sig (negative) relationship
summary(lm(data=df3[df3$hhexpenses!="",], lvls~hhexpenses+
             state+yeargrad))
# Relationship between contribution level and level of races entered

summary(lm(data=df3[df3$hhexpenses!=""&df3$lvls>0,], run~hhexpenses+lvls+
             state+yeargrad))
# Not significant once entered
length(unique(df3$responseid[df3$party==0&df3$county==1]))

mean(df3$hhbinary[df3$run==0], na.rm=T) # 346 people
mean(df3$hhbinary[df3$city==1], na.rm=T) # 68 people
mean(df3$hhbinary[df3$county==1], na.rm=T) # 110 people
mean(df3$hhbinary[df3$state.o==1], na.rm=T) # 72 people
# mean(df3$hhexpenses[df3$natl==1], na.rm=T) # 3 people - not enough to estimate

mean(df3$inc[df3$run==0], na.rm=T) # 346 people
mean(df3$inc[df3$city==1], na.rm=T) # 68 people
mean(df3$inc[df3$county==1], na.rm=T) # 110 people
mean(df3$inc[df3$state.o==1], na.rm=T) # 72 people
# mean(df3$inc[df3$natl==1], na.rm=T) # 3 people - not enough to estimate


# Checking % of novices who ran for intro to results section:
prop.table(table(df4$run))




####################################################################################
####  APPENDICES ####
####################################################################################

#### APPENDIX B-2 - SUMMARY STATISTICS ####

mycontrols <- tableby.control(
  test = T,
  total = T,
  numeric.test = "kwt", cat.test = "chisq",
  numeric.stats = c("meansd", "medianq1q3", "range"),
  cat.stats = c("countpct"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    range = "Min - Max"
  )
)


table_covs <- tableby(run~hhexpenses+binincome+
                        state+yeargrad+
                        eth+edu+area+lgbt+
                        nascent+activedem+
                        psychindex + 
                        singlehh+kids, 
                      data = df3,
                      control = mycontrols,
                      digits=2, digits.p=3, digits.pct=1)

attr(df3$run,'label')  <- 'Ran for Office'

mylabels <- list(run = "Ran for Office",
                 hhexpenses = "Contribution to Household Income",
                 binincome = "Total Household Income",
                 state = "State",
                 yeargrad = "Emerge Graduation Year",
                 eth = "Ethnicity",
                 edu = "Education",
                 area = "Area",
                 lgbt = "LGBT",
                 nascent = "Nascent Ambition",
                 activedem = "Active in Local Democratic Party",
                 psychindex = "Psychological Fears Index",
                 singlehh = "Single (Unpartnered) Household",
                 kids = "Children"
)

write2pdf(
  table_covs, "Tables/SI_covs_table.pdf", quiet = TRUE,
  title = "Summary Statistics by Run for Office",      
  labelTranslations = mylabels, 
  total = FALSE                 
)

## B fig 1 - how hear
df$newhowhear <- df$howhear
df$newhowhear <- factor(df$newhowhear, levels=c("Brochure/mailing", "Colleague", "Convention/Event booth",
                                                "Email announcement", "Emerge alum", "Emerge event", "Emerge organizer",
                                                "Friend/Family", "Social media", "Other (please specify)"))


how<- ggplot(df, aes(x=newhowhear)) + 
  geom_bar(fill="#3d0060") + 
  geom_label(stat='count', aes(label=..count.., y=..count..+20)) + 
  theme_minimal(base_size = 18) + 
  theme(axis.title = element_text(), 
        axis.text.x = element_text(face="bold", angle=45, hjust=1),
        axis.text.y = element_text(face="bold"),
        plot.title = element_text(size = rel(1.1), hjust = 0.5),
        plot.subtitle = element_text(size=16, color = "darkslategrey", face = "bold.italic", hjust=.5),
        plot.caption = element_text(size = rel(.8), margin = margin(t = 10), color = "grey30", hjust = 0.5)) +
  labs(subtitle = "How did you hear about Emerge?",
       x = "", 
       y = "Respondent Count") 

ggsave(file="Plots/SI_howhear.pdf", how, width=8, height=6)


## B fig 2 - summary stats for why participate in Emerge
df$newreasons <- df$reasons
df$newreasons <- factor(df$newreasons, 
                        levels=c( "running", "previous", "considering", "curious", "pushed"),
                        labels=c("Running", "Previous", "Considering", "Curious",  "Pushed"))

why<- ggplot(df, aes(x=newreasons)) + 
  geom_bar(fill="#003707") + 
  geom_label(stat='count', aes(label=..count.., y=..count..+20)) + 
  coord_flip() + 
  theme_minimal(base_size = 18) + 
  theme(axis.title = element_text(), 
        axis.text.y = element_text(face="bold"),
        plot.title = element_text(size = rel(1.1), hjust = 0.5),
        plot.subtitle = element_text(size=16, color = "darkslategrey", face = "bold.italic", hjust=.5),
        plot.caption = element_text(size = rel(.8), margin = margin(t = 10), color = "grey30", hjust = 0.5)) +
  labs(subtitle = "Which of the following best describes\nyour reason for attending Emerge?",
       x = "", 
       y = "Respondent Count") 

ggsave(file="Plots/SI_whyattend.pdf", why, width=6, height=4)



####  APPENDIX B-3 - 1-CATEGORY QUAL CODING ####

## Version with primary and secondary for the appendix:
quala <- data.frame(c("Structural/Resources", "Political Environment", "Fears", "Motivations"),
                    c(.44, .31, .15, .10))
names(quala) <- c("Factor", "Percentage")
quala$Factor <- factor(quala$Factor, levels=c( "Motivations", "Fears", "Political Environment", "Structural/Resources"))

plot1a <- ggplot(quala, aes(x=Factor, y=Percentage)) +
  geom_bar(stat="identity") + 
  theme_minimal(base_size = 18) + 
  coord_flip() +
  theme(axis.title = element_text(), 
        axis.text.y = element_text(face="bold"),
        plot.title = element_text(size = rel(1.1), hjust = 0.5),
        plot.subtitle = element_text(size=16, color = "darkslategrey", face = "bold.italic", hjust=.5),
        plot.caption = element_text(size = rel(.8), margin = margin(t = 10), color = "grey30", hjust = 0.5)) +
  labs(subtitle = "There are a lot of important reasons why people decide\nnot to run for political office or find they are no longer\nable to. Why haven't you run for office yet?",
       # title = "Deterrent Factors Cited by Respondents Not Seeking Office",
       # caption = "Responses could be coded in more than one category, so responses do not sum to 100%.", 
       x = "", 
       y = "Percentage of Total Responses") +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) 

ggsave(file="Plots/SI_plot1_qualcoding.pdf", plot1a, width=10, height=4)



#### APPENDIX C-2 - LOGISTIC REGRESSIONS ####

### HH Contributions

# Bivariate
hh.bi <- glm(data=df3[df3$hhexpenses!="",], run~hhexpenses, family = binomial)
hhodds.bi <- data.frame(cbind(exp(hh.bi$coefficients[2]), exp(confint(hh.bi)[2,1]), exp(confint(hh.bi)[2,2]), coef(summary(hh.bi))[2,4]))

# FEs for Emerge state and year
hh.fes <- glm(data=df3[df3$hhexpenses!="",], run~hhexpenses+
                state+yeargrad, family = binomial)
hhodds.fes <- data.frame(cbind(exp(hh.fes$coefficients[2]), exp(confint(hh.fes)[2,1]), exp(confint(hh.fes)[2,2]), coef(summary(hh.fes))[2,4]))

# Demographic variables
hh.demo <- glm(data=df3[df3$hhexpenses!="",], run~hhexpenses+
                 state+yeargrad+
                 eth+edu+area+lgbt, family = binomial)
hhodds.demo <- data.frame(cbind(exp(hh.demo$coefficients[2]), exp(confint(hh.demo)[2,1]), exp(confint(hh.demo)[2,2]), coef(summary(hh.demo))[2,4]))

# Political environment
hh.pol <- glm(data=df3[df3$hhexpenses!="",], run~hhexpenses+
                state+yeargrad+
                eth+edu+area+lgbt+
                nascent+activedem, family = binomial)
hhodds.pol <- data.frame(cbind(exp(hh.pol$coefficients[2]), exp(confint(hh.pol)[2,1]), exp(confint(hh.pol)[2,2]), coef(summary(hh.pol))[2,4]))

# Psychological fears 
hh.psy <- glm(data=df3[df3$hhexpenses!="",], run~hhexpenses+
                state+yeargrad+
                eth+edu+area+lgbt+
                nascent+activedem+
                psychindex, family = binomial)
hhodds.psy <- data.frame(cbind(exp(hh.psy$coefficients[2]), exp(confint(hh.psy)[2,1]), exp(confint(hh.psy)[2,2]), coef(summary(hh.psy))[2,4]))

# Family variables 
hh.fam <- glm(data=df3[df3$hhexpenses!="",], run~hhexpenses+
                state+yeargrad+
                eth+edu+area+lgbt+
                nascent+activedem+
                psychindex + 
                singlehh+kids, family = binomial)
hhodds.fam <- data.frame(cbind(exp(hh.fam$coefficients[2]), exp(confint(hh.fam)[2,1]), exp(confint(hh.fam)[2,2]), coef(summary(hh.fam))[2,4]))

# Combine into dataframe 
hhdf <- data.frame(rbind(hhodds.bi, hhodds.fes, hhodds.demo, hhodds.pol, hhodds.psy, hhodds.fam))
names(hhdf) <- c("mean", "low", "high", "p")
hhdf$vars <- row.names(hhdf)

row.names(hhdf) <- NULL

hhdf$sig <- ifelse(hhdf$p<.050, 1, 0)

hhdf$model <- c(5, 4, 3, 2, 1, 0)
hhdf$var <- "Household\nContribution"


### Income

# Bivariate
inc.bi <- glm(data=df3[df3$inc!="",], run~binincome, family = binomial)
incodds.bi <- data.frame(cbind(exp(inc.bi$coefficients[2]), exp(confint(inc.bi)[2,1]), exp(confint(inc.bi)[2,2]), coef(summary(inc.bi))[2,4]))

# FEs for Emerge state and year
inc.fes <- glm(data=df3[df3$inc!="",], run~binincome+
                 state+yeargrad, family = binomial)
incodds.fes <- data.frame(cbind(exp(inc.fes$coefficients[2]), exp(confint(inc.fes)[2,1]), exp(confint(inc.fes)[2,2]), coef(summary(inc.fes))[2,4]))

# Demographic variables
inc.demo <- glm(data=df3[df3$inc!="",], run~binincome+
                  state+yeargrad+
                  eth+edu+area+lgbt, family = binomial)
incodds.demo <- data.frame(cbind(exp(inc.demo$coefficients[2]), exp(confint(inc.demo)[2,1]), exp(confint(inc.demo)[2,2]), coef(summary(inc.demo))[2,4]))

# Political environment
inc.pol <- glm(data=df3[df3$inc!="",], run~binincome+
                 state+yeargrad+
                 eth+edu+area+lgbt+
                 nascent+activedem, family = binomial)
incodds.pol <- data.frame(cbind(exp(inc.pol$coefficients[2]), exp(confint(inc.pol)[2,1]), exp(confint(inc.pol)[2,2]), coef(summary(inc.pol))[2,4]))

# Psychological fears 
inc.psy <- glm(data=df3[df3$inc!="",], run~binincome+
                 state+yeargrad+
                 eth+edu+area+lgbt+
                 nascent+activedem+
                 psychindex, family = binomial)
incodds.psy <- data.frame(cbind(exp(inc.psy$coefficients[2]), exp(confint(inc.psy)[2,1]), exp(confint(inc.psy)[2,2]), coef(summary(inc.psy))[2,4]))

# Family variables 
inc.fam <- glm(data=df3[df3$inc!="",], run~binincome+
                 state+yeargrad+
                 eth+edu+area+lgbt+
                 nascent+activedem+
                 psychindex + 
                 singlehh+kids, family = binomial)
incodds.fam <- data.frame(cbind(exp(inc.fam$coefficients[2]), exp(confint(inc.fam)[2,1]), exp(confint(inc.fam)[2,2]), coef(summary(inc.fam))[2,4]))

# Combine into dataframe 
incdf <- data.frame(rbind(incodds.bi, incodds.fes, incodds.demo, incodds.pol, incodds.psy, incodds.fam))
names(incdf) <- c("mean", "low", "high", "p")
incdf$vars <- row.names(incdf)

row.names(incdf) <- NULL

incdf$sig <- ifelse(incdf$p<.050, 1, 0)

incdf$model <- c(5, 4, 3, 2, 1, 0)
incdf$var <- "Income"

## Combine into single dataframe

coefsl <- rbind(hhdf,incdf)


## Full Plot 
plot4l <- ggplot(coefsl, aes(x=model, y=mean, ymin=low, ymax=high, group=var)) +
  geom_pointrange(position=position_dodge(width=1), size=1) + 
  theme_minimal(base_size = 24) + 
  facet_grid(~var) +
  coord_flip() +
  geom_hline(yintercept=1, lty=2) +
  theme_minimal(base_size = 20) + 
  theme(legend.position = "bottom",
        panel.spacing=unit(2, "lines"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face="bold"),
        axis.title = element_text(), 
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold")) +
  labs(x = "", 
       y = "Odds Ratios for Change in Run Rate") + 
  scale_x_continuous(labels=labels3,
                     breaks=c(5,4,3,2,1,0)) 

ggsave(file="Plots/SI_plot4_log_multivariate.pdf", plot4l, width=10, height=6)


####  APPENDIX C-3 - NON-CUMULATIVE COEFFICIENT PLOTS ####

#  Sequential version of coefficient plot for appendix

### HH Contributions

# Same bivariate and FE models

# Demographic variables
hha.demo <- lm(data=df3[df3$hhexpenses!="",], run~hhexpenses+
                 eth+edu+area+lgbt)
hhacis.demo <- data.frame(confint(hha.demo))[2,]

# Political environment
hha.pol <- lm(data=df3[df3$hhexpenses!="",], run~hhexpenses+
                nascent+activedem)
hhacis.pol <- data.frame(confint(hha.pol))[2,]

# Psychological fears 
hha.psy <- lm(data=df3[df3$hhexpenses!="",], run~hhexpenses+
                psychindex)
hhacis.psy <- data.frame(confint(hha.psy))[2,]

# Family variables 
hha.fam <- lm(data=df3[df3$hhexpenses!="",], run~hhexpenses+
                singlehh+kids)
hhacis.fam <- data.frame(confint(hha.fam))[2,]

# Combine into dataframe 
hhadf <- data.frame(rbind(hhcis.bi, hhcis.fes, hhacis.demo, hhacis.pol, hhacis.psy, hhacis.fam))
names(hhadf) <- c("low", "high")
hhadf$mean <- as.numeric(hhadf$high-abs(((hhadf$high-hhadf$low)/2)))
hhadf$vars <- row.names(hhadf)

row.names(hhadf) <- NULL

hhadf$sig <- ifelse(hhadf$low<0&hhadf$high<0, 1,
                    ifelse(hhadf$low>0&hhadf$high>0, 1,0))

hhadf$model <- c(5, 4, 3, 2, 1, 0)
hhadf$var <- "Household\nContribution"


### Income

# Same bivariate and FE models

# Demographic variables
inca.demo <- lm(data=df3[df3$inc!="",], run~binincome+
                  state+yeargrad+
                  eth+edu+area+lgbt)
incacis.demo <- data.frame(confint(inca.demo))[2,]

# Political environment
inca.pol <- lm(data=df3[df3$inc!="",], run~binincome+
                 state+yeargrad+
                 eth+edu+area+lgbt+
                 nascent+activedem)
incacis.pol <- data.frame(confint(inca.pol))[2,]

# Psychological fears 
inca.psy <- lm(data=df3[df3$inc!="",], run~binincome+
                 state+yeargrad+
                 eth+edu+area+lgbt+
                 nascent+activedem+
                 psychindex)
incacis.psy <- data.frame(confint(inca.psy))[2,]

# Family variables 
inca.fam <- lm(data=df3[df3$inc!="",], run~binincome+
                 state+yeargrad+
                 eth+edu+area+lgbt+
                 nascent+activedem+
                 psychindex + 
                 singlehh+kids)
incacis.fam <- data.frame(confint(inca.fam))[2,]

# Combine into dataframe 
incadf <- data.frame(rbind(inccis.bi, inccis.fes, incacis.demo, incacis.pol, incacis.psy, incacis.fam))
names(incadf) <- c("low", "high")
incadf$mean <- as.numeric(incadf$high-abs(((incadf$high-incadf$low)/2)))
incadf$vars <- row.names(incadf)

row.names(incadf) <- NULL

incadf$sig <- ifelse(incadf$low<0&incadf$high<0, 1,
                     ifelse(incadf$low>0&incadf$high>0, 1,0))

incadf$model <- c(5, 4, 3, 2, 1, 0)
incadf$var <- "Income"

## Combine into single dataframe

coefsa <- rbind(hhadf,incadf)

## Plot 

labels3a <- c("Bivariate", 
              "Emerge Fixed Effects Only",
              "Demographics Only",
              "Political Environment Only",
              "Psychological Fears Only",
              "Household Composition Only")

plot4a <- ggplot(coefsa, aes(x=model, y=mean, ymin=low, ymax=high, group=var)) +
  geom_pointrange(position=position_dodge(width=1), size=1) + 
  theme_minimal(base_size = 24) + 
  facet_grid(~var) +
  coord_flip() +
  geom_hline(yintercept=0, lty=2) +
  theme_minimal(base_size = 20) + 
  theme(legend.position = "bottom",
        panel.spacing=unit(2, "lines"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face="bold"),
        axis.title = element_text(), 
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold")) +
  labs(x = "", 
       y = "Change in Run Rate") + 
  scale_x_continuous(labels=labels3a,
                     breaks=c(5,4,3,2,1,0)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) 

ggsave(file="Plots/SI_plot4_multivariate_sequential.pdf", plot4a, width=10, height=6)


### Breadwinner moms
labels2 <- c("0%", "1-25%", "26-50%", "51-75%", "76-100%")

df2n <- df[!is.na(df$employed)&!is.na(df$ran)&df$reasons!="previous", c("hhexpenses", "ran", "employed", "children")]
df2n$ran <- ifelse(as.numeric(df2n$ran)==2, 1, 0)

df2n <- df2n[!is.na(df2n$hhexpenses),]

df2np <- df2n[c("hhexpenses", "ran", "children")] %>%
  group_by(hhexpenses, children) %>%
  summarise_all(funs(mean=mean(., na.rm=T), sd=sd(., na.rm=T), n=sum(!is.na(.))))

df2np$lowci <- df2np$mean - (1.96*df2np$sd)/sqrt(df2np$n)
df2np$highci <- df2np$mean + (1.96*df2np$sd)/sqrt(df2np$n)

p2nm <- ggplot(df2np, aes(x=hhexpenses, y=mean,  ymin=lowci, ymax=highci, label=n, group=children)) + 
  geom_pointrange() +
  geom_line(position=position_dodge(width=.1)) +
  facet_grid(.~children) + 
  geom_label(vjust=0, nudge_y=-.2) + 
  theme_minimal(base_size = 18) + 
  theme(legend.position = "none",
        legend.title = element_text(face = "bold"),
        axis.title = element_text(), 
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        plot.title = element_text(size = rel(1.1), hjust = 0.5),
        plot.subtitle = element_text(color = "darkslategrey", 
                                     face = "italic", hjust=0.5),
        plot.caption = element_text(size = rel(.8), 
                                    margin = margin(t = 10), 
                                    color = "grey30", 
                                    hjust = 0.5)) +
  labs(x = "Share of Household Income Contributed, by Motherhood", 
       y = "Run Rate") + 
  # title = "Novice Breadwinners Less Likely To Run",
  # caption = "Bivariate regression coefficients are reported with 95% confidence intervals.
  # Number of respondents in each category is reported in the bubble below each category.") +
  scale_x_continuous(labels=labels2,
                     breaks=c(0,1,2,3,4)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) 

ggsave(file="Plots/plot2_breadwinners_moms.pdf", p2nm, width=15, height=7)



#### APPENDIX C-4 - BREADWINNER EFFECT, ALL ALUMNAE ####
df2 <- df[!is.na(df$employed)&!is.na(df$ran), c("hhexpenses", "ran", "employed")]
df2$ran <- ifelse(as.numeric(df2$ran)==2, 1, 0)

df2 <- df2[!is.na(df2$hhexpenses),]

df2p <- df2[c("hhexpenses", "ran")] %>%
  group_by(hhexpenses) %>%
  summarise_all(funs(mean=mean(., na.rm=T), sd=sd(., na.rm=T), n=sum(!is.na(.))))

df2p$lowci <- df2p$mean - (1.96*df2p$sd)/sqrt(df2p$n)
df2p$highci <- df2p$mean + (1.96*df2p$sd)/sqrt(df2p$n)

p2 <- ggplot(df2p, aes(x=hhexpenses, y=mean,  ymin=lowci, ymax=highci, label=n)) + 
  geom_pointrange(size=.8, fatten = 4) +
  geom_line(position=position_dodge(width=.1), size=1) +
  geom_label(vjust=0, nudge_y=-.2) + 
  theme_minimal(base_size = 18) + 
  theme(legend.position = "none",
        legend.title = element_text(face = "bold"),
        axis.title = element_text(size = rel(.9))) +  
  labs(x = "Share of Household Income Contributed (n=652)", 
       y = "Run Rate") + 
  scale_x_continuous(labels=labels2,
                     breaks=c(0,1,2,3,4)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits = c(.20,.75), expand = c(0,0)) 

ggsave(file="Plots/SI_plot2_breadwinners.pdf", p2, width=10, height=7)

summary(lm(data=df2, ran~hhexpenses))



#### APPENDIX D-1 - SUMMARY STATS BY HHEXPENSES ####

df3$employ <- ifelse(grepl("Yes",df3$employed), "Yes (Full-/Part-time)", "No (Retired/Unemployed)")
df3$breadwinner <- ifelse(df3$hhexpenses>2,1,
                          ifelse(df3$hhexpenses<=2,0,NA))

labels(df3$breadwinner)  <- 'Breadwinner'
labels(df3$employ)  <- 'Employment Status'
labels(df3$income)  <- 'Total Household Income'

## BY BREADWINNER BINARY (NOVICES ONLY)

table_covs <- tableby(breadwinner~binincome+employ+
                        state+yeargrad+
                        eth+edu+area+lgbt+
                        nascent+activedem+
                        psychindex + 
                        singlehh+kids, 
                      data = df3,
                      control = mycontrols,
                      digits=2, digits.p=3, digits.pct=1)

mylabels <- list(breadwinner = "Breadwinner",
                 binincome = "Total Household Income",
                 employ = "Currently Employed",
                 state = "State",
                 yeargrad = "Emerge Graduation Year",
                 eth = "Ethnicity",
                 edu = "Education",
                 area = "Area",
                 lgbt = "LGBT",
                 nascent = "Nascent Ambition",
                 activedem = "Active in Local Democratic Party",
                 psychindex = "Psychological Fears Index",
                 singlehh = "Single (Unpartnered) Household",
                 kids = "Children"
)

write2pdf(
  table_covs, "Tables/SI_bread_covs_table.pdf", quiet = TRUE,
  title = "Summary Statistics by Breadwinner Status",      
  labelTranslations = mylabels, 
  total = FALSE                 
)



## BY HOUSEHOLD COMPOSITION
mycontrols <- tableby.control(
  test = T,
  total = T,
  numeric.test = "kwt", cat.test = "chisq",
  numeric.stats = c("meansd", "medianq1q3", "range"),
  cat.stats = c("countpct"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    range = "Min - Max"
  )
)

table_covs <- tableby(hhexpenses~income+employ+
                        eth+agegroup, 
                      data = df3[df3$singlehh==1&df3$kids==1,],
                      control = mycontrols,
                      digits=2, digits.p=3, digits.pct=1)

mylabels <- list(hhexpenses = "Contribution to Household Income",
                 income = "Total Household Income",
                 employ = "Currently Employed",
                 eth = "Ethnicity",
                 agegroup = "Age Category"
)

write2word(
  table_covs, "Tables/SI_hh_sumstats_singlemoms.docx", quiet = TRUE,
  title = "Summary Statistics by Household Expenses for Single Mothers",      
  labelTranslations = mylabels, 
  total = FALSE                 
)

table_covs <- tableby(hhexpenses~income+employ+
                        eth+agegroup, 
                      data = df3[df3$singlehh==0&df3$kids==1,],
                      control = mycontrols,
                      digits=2, digits.p=3, digits.pct=1)

mylabels <- list(hhexpenses = "Contribution to Household Income",
                 income = "Total Household Income",
                 employ = "Currently Employed",
                 eth = "Ethnicity",
                 agegroup = "Age Category"
)

write2word(
  table_covs, "Tables/SI_hh_sumstats_partneredmoms.docx", quiet = TRUE,
  title = "Summary Statistics by Household Expenses for Partnered Mothers",      
  labelTranslations = mylabels, 
  total = FALSE                 
)

table_covs <- tableby(hhexpenses~income+employ+
                        eth+agegroup, 
                      data = df3[df3$singlehh==1&df3$kids==0,],
                      control = mycontrols,
                      digits=2, digits.p=3, digits.pct=1)

mylabels <- list(hhexpenses = "Contribution to Household Income",
                 income = "Total Household Income",
                 employ = "Currently Employed",
                 eth = "Ethnicity",
                 agegroup = "Age Category"
)

write2word(
  table_covs, "Tables/SI_hh_sumstats_singlenons.docx", quiet = TRUE,
  title = "Summary Statistics by Household Expenses for Single Non-Mothers",      
  labelTranslations = mylabels, 
  total = FALSE                 
)

table_covs <- tableby(hhexpenses~income+employ+
                        eth+agegroup, 
                      data = df3[df3$singlehh==0&df3$kids==0,],
                      control = mycontrols,
                      digits=2, digits.p=3, digits.pct=1)

mylabels <- list(hhexpenses = "Contribution to Household Income",
                 income = "Total Household Income",
                 employ = "Currently Employed",
                 eth = "Ethnicity",
                 agegroup = "Age Category"
)

write2word(
  table_covs, "Tables/SI_hh_sumstats_partnerednons.docx", quiet = TRUE,
  title = "Summary Statistics by Household Expenses for Partnered Non-Mothers",      
  labelTranslations = mylabels, 
  total = FALSE                 
)


#### APPENDIX D-2 - INCOME * BREADWINNING ####

# Part 1 - plot with distribution
xdf <- df3[!is.na(df3$hhexpenses)&!is.na(df3$income),c("hhexpenses", "income")] %>%
  group_by(hhexpenses) %>%
  count(income)
xdf$hhexpenses <- factor(xdf$hhexpenses, labels=labels2b, levels=c(4,3,2,1,0))

my_palette <- colorRampPalette(brewer.pal(9, "Greens"))(11)[3:11]

xplot <- ggplot(xdf, aes(fill=income, y=n, x=hhexpenses)) + 
  geom_bar(position="fill", stat="identity") + 
  theme_minimal(base_size = 20) + 
  theme(legend.position = "right",
        panel.spacing=unit(2, "lines"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face="bold"),
        axis.title = element_text(), 
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold")) +
  labs(x = "Share of Household Income Contributed\n(n = 598)", 
       y = "Percentage of Responses") + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  scale_fill_manual("Household\nIncome",
                    values=my_palette)

ggsave(xplot, file="Plots/SI_income_x_hh.jpg", width=9, height=5)

# Part 2 - plot broken down by household composition
xdf2 <- df3[!is.na(df3$hhexpenses)&!is.na(df3$income)&!is.na(df3$singlehh),c("hhexpenses", "income", "singlehh")] %>%
  group_by(singlehh, hhexpenses) %>%
  count(income)

xdf2$hhexpenses <- factor(xdf2$hhexpenses, labels=labels2b, levels=c(4,3,2,1,0))

my_palette1 <- colorRampPalette(brewer.pal(9, "RdPu"))(11)[3:11]
my_palette2 <- colorRampPalette(brewer.pal(9, "Blues"))(11)[3:11]

xplot1 <- ggplot(xdf2[xdf2$singlehh==1,], aes(fill=income, y=n, x=hhexpenses)) + 
  geom_bar(position="fill", stat="identity") + 
  theme_minimal(base_size = 20) + 
  theme(legend.position = "right",
        panel.spacing=unit(2, "lines"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face="bold"),
        axis.title = element_text(), 
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold")) +
  labs(x = "Share of Household Income Contributed,\n Single Households (n = 190)", 
       y = " ") + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  scale_fill_manual("Household\nIncome",
                    values=my_palette1)

xplot2 <- ggplot(xdf2[xdf2$singlehh==0,], aes(fill=income, y=n, x=hhexpenses)) + 
  geom_bar(position="fill", stat="identity") + 
  theme_minimal(base_size = 20) + 
  theme(legend.position = "right",
        panel.spacing=unit(2, "lines"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face="bold"),
        axis.title = element_text(), 
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold")) +
  labs(x = "Partnered Households (n = 357)", 
       y = " ") + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  scale_fill_manual("Household\nIncome",
                    values=my_palette2)

xplothh <- grid.arrange(xplot1, xplot2, nrow=2)

ggsave(xplothh, file="Plots/SI_income_x_hh2.pdf", width=9, height=9)


## Correlation is negative and sig
cor.test(df3$binincome, df3$hhexpenses, alternative="two.sided", method="pearson")

summary(lm(data=df3[df3$newbininc!=1,], run~hhexpenses*as.factor(newbininc)))

df6a <- df3[df3$newbininc==3,] # Wealthy breadwinners much less likely to run
summary(lm(data=df6a, run~hhexpenses))
df6b <- df3[df3$newbininc==2,] # Middle class breadwinners much less likely to run
summary(lm(data=df6b, run~hhexpenses))
df6c <- df3[df3$newbininc==1,] # Working class breadwinners much less likely to run
summary(lm(data=df6c, run~hhexpenses))


df6 <- df3[df3$newbininc>1,]
mean(df3$run[df3$newbininc==1], na.rm=T)

## recreate with just "wealthyenough"
### HH Contributions

# Bivariate
hh.bi <- lm(data=df6[df6$hhexpenses!="",], run~hhexpenses)
hhcis.bi <- data.frame(confint(hh.bi))[2,]

# FEs for Emerge state and year
hh.fes <- lm(data=df6[df6$hhexpenses!="",], run~hhexpenses+
               state+yeargrad)
hhcis.fes <- data.frame(confint(hh.fes))[2,]

# Demographic variables
hh.demo <- lm(data=df6[df6$hhexpenses!="",], run~hhexpenses+
                state+yeargrad+
                eth+edu+area+lgbt)
hhcis.demo <- data.frame(confint(hh.demo))[2,]

# Political environment
hh.pol <- lm(data=df6[df6$hhexpenses!="",], run~hhexpenses+
               state+yeargrad+
               eth+edu+area+lgbt+
               nascent+activedem)
hhcis.pol <- data.frame(confint(hh.pol))[2,]

# Psychological fears 
hh.psy <- lm(data=df6[df6$hhexpenses!="",], run~hhexpenses+
               state+yeargrad+
               eth+edu+area+lgbt+
               nascent+activedem+
               psychindex)
hhcis.psy <- data.frame(confint(hh.psy))[2,]

# Family variables 
hh.fam <- lm(data=df6[df6$hhexpenses!="",], run~hhexpenses+
               state+yeargrad+
               eth+edu+area+lgbt+
               nascent+activedem+
               psychindex + 
               singlehh+kids)
hhcis.fam <- data.frame(confint(hh.fam))[2,]

# Combine into dataframe 
hhdf <- data.frame(rbind(hhcis.bi, hhcis.fes, hhcis.demo, hhcis.pol, hhcis.psy, hhcis.fam))
names(hhdf) <- c("low", "high")
hhdf$mean <- as.numeric(hhdf$high-abs(((hhdf$high-hhdf$low)/2)))
hhdf$vars <- row.names(hhdf)

row.names(hhdf) <- NULL

hhdf$sig <- ifelse(hhdf$low<0&hhdf$high<0, 1,
                   ifelse(hhdf$low>0&hhdf$high>0, 1,0))

hhdf$model <- c(5, 4, 3, 2, 1, 0)
hhdf$var <- "Household\nContribution"


### Plot 
plot.x <- ggplot(hhdf, aes(x=model, y=mean, ymin=low, ymax=high, group=var)) +
  geom_pointrange(position=position_dodge(width=1), size=1) + 
  theme_minimal(base_size = 24) + 
  coord_flip() +
  geom_hline(yintercept=0, lty=2) +
  theme_minimal(base_size = 20) + 
  theme(legend.position = "bottom",
        panel.spacing=unit(2, "lines"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face="bold"),
        axis.title = element_text(), 
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold")) +
  labs(x = "", 
       y = "Change in Run Rate") + 
  scale_x_continuous(labels=labels3,
                     breaks=c(5,4,3,2,1,0)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) 

ggsave(file="Plots/SI_multivariate_x.pdf", plot.x, width=6, height=4)



### Coefficient plot showing both breadwinning and income interaction

# Just interaction
x.bi <- lm(data=df3[df3$inc!="",], run~binincome*hhexpenses)
xcis.bi <- data.frame(confint(x.bi))[c(2:4),]

# FEs for Emerge state and year
x.fes <- lm(data=df3[df3$inc!="",], run~binincome*hhexpenses+
                state+yeargrad)
i <- as.numeric(length(confint(x.fes))/2)
xcis.fes <- data.frame(confint(x.fes))[c(2:3, i),]

# Demographic variables
x.demo <- lm(data=df3[df3$inc!="",], run~binincome*hhexpenses+
                 state+yeargrad+
                 eth+edu+area+lgbt)
i <- as.numeric(length(confint(x.demo))/2)
xcis.demo <- data.frame(confint(x.demo))[c(2:3, i),]

# Political environment
x.pol <- lm(data=df3[df3$inc!="",], run~binincome*hhexpenses+
                state+yeargrad+
                eth+edu+area+lgbt+
                nascent+activedem)
i <- as.numeric(length(confint(x.pol))/2)
xcis.pol <- data.frame(confint(x.pol))[c(2:3, i),]

# Psychological fears 
x.psy <- lm(data=df3[df3$inc!="",], run~binincome*hhexpenses+
                state+yeargrad+
                eth+edu+area+lgbt+
                nascent+activedem+
                psychindex)
i <- as.numeric(length(confint(x.psy))/2)
xcis.psy <- data.frame(confint(x.psy))[c(2:3, i),]

# Family variables 
x.fam <- lm(data=df3[df3$inc!="",], run~binincome*hhexpenses+
                state+yeargrad+
                eth+edu+area+lgbt+
                nascent+activedem+
                psychindex + 
                singlehh+kids)
i <- as.numeric(length(confint(x.fam))/2)
xcis.fam <- data.frame(confint(x.fam))[c(2:3, i),]

# Combine into dataframe 
x.df <- data.frame(rbind(xcis.bi, xcis.fes, xcis.demo, xcis.pol, xcis.psy, xcis.fam))
names(x.df) <- c("low", "high")
x.df$mean <- as.numeric(x.df$high-abs(((x.df$high-x.df$low)/2)))
x.df$vars <- row.names(x.df)

row.names(x.df) <- NULL

x.df$sig <- ifelse(x.df$low<0&x.df$high<0, 1,
                    ifelse(x.df$low>0&x.df$high>0, 1,0))

x.df$model <- c(5, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1, 0, 0, 0)


x.df$vars <- ifelse(grepl(":", x.df$vars), "Interaction",
                    ifelse(grepl("bininc", x.df$vars), "Total Income", "Household Contribution"))



##  Plot 
plot.o <- ggplot(x.df, aes(x=model, y=mean, ymin=low, ymax=high, group=vars)) +
  geom_pointrange(position=position_dodge(width=1), size=1) + 
  theme_minimal(base_size = 24) + 
  facet_grid(~vars) +
  coord_flip() +
  geom_hline(yintercept=0, lty=2) +
  theme_minimal(base_size = 20) + 
  theme(legend.position = "bottom",
        panel.spacing=unit(2, "lines"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face="bold"),
        axis.title = element_text(), 
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold")) +
  labs(x = "", 
       y = "Change in Run Rate") + 
  scale_x_continuous(labels=labels3,
                     breaks=c(5,4,3,2,1,0)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) 

ggsave(file="Plots/SI_interaction_multivariate.pdf", plot.o, width=13, height=10)

#### APPENDIX E-1 - MOTHERS + YOUNGEST CHILD ANALYSIS ####
df5 <- df3[df3$kids==1,]

## Simple descriptive plot for run rate by child age

kiddos <- ggplot(df5, aes(x=childage, y=run)) + 
  geom_smooth(method="lm", color="black", lty="dashed") + 
  theme_minimal(base_size = 20) + 
  theme(legend.position = "bottom",
        panel.spacing=unit(2, "lines"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face="bold"),
        axis.title = element_text(), 
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold")) +
  labs(x = "Youngest Child's Age (Years)", 
       y = "Run Rate") + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) 

ggsave(kiddos, file="Plots/SI_childage.jpg", width=6, height=4)



#### APPENDIX E-2 - HOME OWNING VS RENTING ####
t.test(df3$run~df3$renter)

df3o <- df3[df3$renter==0,]
df3r <- df3[df3$renter==1,]

## Owners
# Bivariate
own.bi <- lm(data=df3o[!is.na(df3o$hhexpenses),], run~hhexpenses)
owncis.bi <- data.frame(confint(own.bi))[2,]

# FEs for Emerge state and year
own.fes <- lm(data=df3o[!is.na(df3o$hhexpenses),], run~hhexpenses+
                state+yeargrad)
owncis.fes <- data.frame(confint(own.fes))[2,]

# Demographic variables
own.demo <- lm(data=df3o[!is.na(df3o$hhexpenses),], run~hhexpenses+
                 state+yeargrad+
                 eth+edu+area+lgbt)
owncis.demo <- data.frame(confint(own.demo))[2,]

# Political environment
own.pol <- lm(data=df3o[!is.na(df3o$hhexpenses),], run~hhexpenses+
                state+yeargrad+
                eth+edu+area+lgbt+
                nascent+activedem)
owncis.pol <- data.frame(confint(own.pol))[2,]

# Psychological fears 
own.psy <- lm(data=df3o[!is.na(df3o$hhexpenses),], run~hhexpenses+
                state+yeargrad+
                eth+edu+area+lgbt+
                nascent+activedem+
                psych2)
owncis.psy <- data.frame(confint(own.psy))[2,]

# Family variables 
own.fam <- lm(data=df3o[!is.na(df3o$hhexpenses),], run~hhexpenses+
                state+yeargrad+
                eth+edu+area+lgbt+
                nascent+activedem+
                psych2 + 
                singlehh+kids)
owncis.fam <- data.frame(confint(own.fam))[2,]

# Combine into dataframe 
owndf <- data.frame(rbind(owncis.bi, owncis.fes, owncis.demo, owncis.pol, owncis.psy, owncis.fam))
names(owndf) <- c("low", "high")
owndf$mean <- as.numeric(owndf$high-abs(((owndf$high-owndf$low)/2)))
owndf$vars <- row.names(owndf)

row.names(owndf) <- NULL

owndf$sig <- ifelse(owndf$low<0&owndf$high<0, 1,
                    ifelse(owndf$low>0&owndf$high>0, 1,0))

owndf$model <- c(5, 4, 3, 2, 1, 0)
owndf$var <- "Owners"

## Renters
# Bivariate
rent.bi <- lm(data=df3r[!is.na(df3r$hhexpenses),], run~hhexpenses)
kidcis.bi <- data.frame(confint(rent.bi))[2,]

# FEs for Emerge state and year
rent.fes <- lm(data=df3r[!is.na(df3r$hhexpenses),], run~hhexpenses+
                 state+yeargrad)
kidcis.fes <- data.frame(confint(rent.fes))[2,]

# Demographic variables
rent.demo <- lm(data=df3r[!is.na(df3r$hhexpenses),], run~hhexpenses+
                  state+yeargrad+
                  eth+edu+area+lgbt)
kidcis.demo <- data.frame(confint(rent.demo))[2,]

# Political environment
rent.pol <- lm(data=df3r[!is.na(df3r$hhexpenses),], run~hhexpenses+
                 state+yeargrad+
                 eth+edu+area+lgbt+
                 nascent+activedem)
kidcis.pol <- data.frame(confint(rent.pol))[2,]

# Psychological fears 
rent.psy <- lm(data=df3r[!is.na(df3r$hhexpenses),], run~hhexpenses+
                 state+yeargrad+
                 eth+edu+area+lgbt+
                 nascent+activedem+
                 psych2)
kidcis.psy <- data.frame(confint(rent.psy))[2,]

# Family variables 
rent.fam <- lm(data=df3r[!is.na(df3r$hhexpenses),], run~hhexpenses+
                 state+yeargrad+
                 eth+edu+area+lgbt+
                 nascent+activedem+
                 psych2 + 
                 singlehh+kids)
kidcis.fam <- data.frame(confint(rent.fam))[2,]

# Combine into dataframe 
rentdf <- data.frame(rbind(kidcis.bi, kidcis.fes, kidcis.demo, kidcis.pol, kidcis.psy, kidcis.fam))
names(rentdf) <- c("low", "high")
rentdf$mean <- as.numeric(rentdf$high-abs(((rentdf$high-rentdf$low)/2)))
rentdf$vars <- row.names(rentdf)

row.names(rentdf) <- NULL

rentdf$sig <- ifelse(rentdf$low<0&rentdf$high<0, 1,
                     ifelse(rentdf$low>0&rentdf$high>0, 1,0))

rentdf$model <- c(5, 4, 3, 2, 1, 0)
rentdf$var <- "Renters"

## Combine into single dataframe

or.coefs <- rbind(owndf,rentdf)

#### Plot 
hplot <- ggplot(or.coefs, aes(x=model, y=mean, ymin=low, ymax=high, group=var)) +
  geom_pointrange(position=position_dodge(width=1), size=1) + 
  theme_minimal(base_size = 24) + 
  facet_grid(~var) +
  coord_flip() +
  geom_hline(yintercept=0, lty=2) +
  theme_minimal(base_size = 20) + 
  theme(legend.position = "bottom",
        panel.spacing=unit(2, "lines"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face="bold"),
        axis.title = element_text(), 
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold")) +
  labs(x = "", 
       y = "Change in Run Rate") + 
  scale_x_continuous(labels=labels3,
                     breaks=c(5,4,3,2,1,0)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) 

# No sig differences anywhere

ggsave(file="Plots/SI_homeown.pdf", hplot, width=10, height=10)



#### MISC RESPONSE TO REVIEWERS

### Use measures of time spent caring for family
df3$famcare <- as.numeric(df3$famcaring)

cor.test(x=df3$kids, y=df3$famcare) # Cor around .59
cor.test(x=df3$singlehh, y=df3$famcare) # Cor around -.36
t.test(df3$famcare~df3$run) # No difference
t.test(df3$kids~df3$run) # Big difference
t.test(df3$singlehh~df3$run) # Medium difference
t.test(df3$timefam~df3$run) # Almost sig


mycontrols <- tableby.control(
  test = T,
  total = T,
  numeric.test = "kwt", cat.test = "chisq",
  numeric.stats = c("meansd", "medianq1q3", "range"),
  cat.stats = c("countpct"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    range = "Min - Max"
  )
)

