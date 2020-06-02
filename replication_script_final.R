#Replication Script
#Probabilistic Social Learning Improve's Public's Detection of Misinformation
#Guilbeault, Woolley, Becker, 2020
rm(list=ls());gc();options(warn=-1); windows()
library(dplyr); library(ggplot2)
library(ggridges);theme_set(theme_ridges())
library(tidyverse); library(lme4);library(multiwayvcov)
library(aod); library(plm); library(lmtest)
library(RCurl)

#load data
data_url <- getURL("https://raw.githubusercontent.com/drguilbe/misinfoCI/master/final_data.csv")
d <- read.csv(text = data_url, stringsAsFactors = F)

#Figure 2A
d_prob_fig2A_plot<-subset(d, condition=="Probabilistic") %>% 
  group_by(playerID, trialID, condition, question, poli.party) %>% 
  dplyr::mutate(error_1 = abs(as.numeric(correctanswer) - as.numeric(response_1)), 
                error_3 = abs(as.numeric(correctanswer) - as.numeric(response_3)), 
                improve = error_3 < error_1) %>% 
  group_by(trialID, condition, question, poli.party) %>%
  dplyr::summarise(response_1 = mean(as.numeric(response_1), na.rm=T), 
                   response_3 = mean(as.numeric(response_3), na.rm=T), 
                   error_1 = mean(error_1, na.rm=T), error_3 = mean(error_3, na.rm=T),
                   prop_improve = length(unique(playerID[improve]))/length(improve) ) %>% 
  subset(poli.party %in% c("Democratic", "Republican", "Independent")) %>% 
  group_by(trialID, condition, poli.party) %>% 
  dplyr::summarise_all(function(x) mean(x)) %>% select(-question) %>% 
  group_by(condition, poli.party) %>% dplyr::summarise(cimin=t.test(prop_improve)$conf.int[1], 
    cimax=t.test(prop_improve)$conf.int[2], prop_improve = mean(prop_improve) ) 

d_binary_fig2A_plot<-subset(d, condition=="Binary") %>% 
  group_by(playerID, trialID, condition, question, poli.party) %>% 
  dplyr::mutate(improve = ifelse(correct_1 == F & correct_3 == T, TRUE, FALSE)) %>% 
  group_by(trialID, condition, question, poli.party) %>%
  dplyr::summarise(prop_improve = length(unique(playerID[improve]))/length(improve)
  ) %>% subset(poli.party %in% c("Democratic", "Republican", "Independent")) %>% 
  group_by(trialID, condition, poli.party) %>% 
  dplyr::summarise_all(function(x) mean(x)) %>% select(-question) %>% 
  group_by(condition, poli.party) %>% dplyr::summarise(cimin=t.test(prop_improve)$conf.int[1], 
                   cimax=t.test(prop_improve)$conf.int[2], prop_improve = mean(prop_improve) ) 

fig2A_plot<-rbind(data.frame(d_prob_fig2A_plot), data.frame(d_binary_fig2A_plot))
fig2A_plot$poli.party<-factor(fig2A_plot$poli.party, levels=c("Independent", "Democratic", "Republican"))

ggplot(fig2A_plot, aes(x = condition, fill=poli.party, y=prop_improve)) +
  geom_bar(stat="identity", position = position_dodge(0.9), color = "black", aes(fill=poli.party), size=2) +  
  geom_errorbar(aes(ymin=cimin, ymax=cimax), position = position_dodge(0.9), linetype="solid", width=0.1, size=2)+ 
  scale_fill_manual(values=c("grey", "Dodgerblue", "firebrick2", "grey", "Dodgerblue","firebrick2")) +
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30),
        axis.title.x = element_blank(), axis.title.y = element_text(size=30, hjust=0.5, vjust=1),
        strip.text.x = element_text(size =25), legend.position=c(0.02,0.9),
        legend.title=element_blank(), legend.text=element_text(size=30), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")
  ) + labs(y="Fraction of Subjects Improving\n(First to Final Round)", linetype=NULL) + 
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) + coord_cartesian(ylim=c(0,0.66))

#Figure 2B
d_prob_fig2B<-subset(d, condition=="Probabilistic") %>% 
  group_by(trialID, condition, question, correctanswer) %>% 
  dplyr::summarise(response_1 = mean(as.numeric(as.character(response_1)), na.rm=T), 
                   response_3 = mean(as.numeric(as.character(response_3)), na.rm=T)) %>%
  dplyr::mutate(group_correct_1 = abs(response_1 - as.numeric(correctanswer))<50, 
                group_correct_3 = abs(response_3 - as.numeric(correctanswer))<50, 
                group_increase_acc=group_correct_3 > group_correct_1) %>% 
  subset(group_correct_1 == FALSE) %>% group_by(trialID, condition) %>% 
  dplyr::summarise(group_increase_acc = sum(group_increase_acc)/length(group_increase_acc)) 

d_prob_fig2B_plot<- d_prob_fig2B %>% group_by(condition) %>% 
  dplyr::summarise(cimin = t.test(group_increase_acc, conf.level = 0.95)$conf.int[1],
                   cimax = t.test(group_increase_acc, conf.level = 0.95)$conf.int[2],
                   group_increase_acc=mean(group_increase_acc))

d_binary_fig2B<-subset(d, condition=="Binary") %>% 
  group_by(trialID, condition, question, correctanswer) %>% 
  dplyr::summarise(prop_corr1 = sum(correct_1)/length(correct_1), 
                   prop_corr3 = sum(correct_3)/length(correct_3)) %>%
  dplyr::mutate(group_correct_1 = prop_corr1 > 0.5, group_correct_3 = prop_corr3 > 0.5,
                group_increase_acc= group_correct_3 > group_correct_1) %>% 
  subset(group_correct_1 == FALSE) %>% group_by(trialID, condition) %>% 
  dplyr::summarise(group_increase_acc = sum(group_increase_acc)/length(group_increase_acc))

d_binary_fig2B_plot<- d_binary_fig2B %>% group_by(condition) %>% 
  dplyr::summarise(cimin = t.test(group_increase_acc, conf.level = 0.95)$conf.int[1],
                   cimax = t.test(group_increase_acc, conf.level = 0.95)$conf.int[2],
                   group_increase_acc=mean(group_increase_acc))

fig2B_plot<-rbind(data.frame(d_prob_fig2B_plot), data.frame(d_binary_fig2B_plot))

ggplot(fig2B_plot, aes(x = condition, color=condition, y=group_increase_acc)) + 
  geom_point(aes(color=condition), size=12) +  
  geom_errorbar(aes(ymin=cimin, ymax=cimax), position = position_dodge(0.9), 
                linetype="solid", width=0.1, size=2)+ 
  scale_color_manual(values=c("grey20", "grey20")) +
  theme(axis.text.x = element_text(size=30), axis.text.y = element_text(size=30),
        axis.title.x = element_blank(), axis.title.y = element_text(size=30, hjust=0.5, vjust=1),
        strip.text.x = element_text(size =25),  legend.position="none",  
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Probability of Peer Networks Increasing\n in Accuracy (First to Final Round)", linetype=NULL) + 
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) + 
  geom_hline(yintercept = 0, size= 2, linetype=2,color="red") 

#Figure 2CD
fig2C_Binary_plot<- subset(d, condition=="Binary") %>% 
  mutate(trust_1=response_1=="Yes", trust_3=response_3=="Yes") %>%
  group_by(trialID, condition, questionset, question, poli.party) %>% 
  dplyr::summarise(trust_1 = sum(trust_1)/length(trust_1), trust_3 = sum(trust_3)/length(trust_3)) %>%  
  group_by(trialID, condition, poli.party) %>% 
  dplyr::summarise(trust_1 = mean(trust_1), trust_3 = mean(trust_3)) %>%  
  gather(round, estimate, trust_1:trust_3, factor_key=T) %>% 
  subset(poli.party %in% c("Democratic", "Republican")) %>% 
  mutate(poli.party=factor(poli.party, levels=c("Republican", "Democratic")), 
         round = factor(round, levels = c("trust_3", "trust_1"))) 

ggplot(fig2C_Binary_plot, aes(x = estimate, y = round, fill=poli.party)) +
  geom_density_ridges(aes(scale=0.9, y = round),alpha=0.6, size=1.2) +
  scale_fill_manual(values=c("firebrick", "dodgerblue")) + 
  ylab("Trials") + xlab("Trust") + ggtitle("Binary") + 
  guides(fill=guide_legend(title="Categories")) + 
  theme(legend.text=element_text(size=30), legend.title=element_text(size=30), 
        legend.position="none",legend.background = element_rect(colour = "white"),
        legend.box.background = element_rect(colour = "black", fill="white"),
        plot.title=element_blank(), axis.title.y=element_blank(),
        axis.title.x=element_blank(),axis.text.y=element_blank(),
        axis.text.x=element_text(size = 30,hjust = 0.6),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
  scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), labels =c(0, 0.25, 0.5, 0.75, 1)) + 
  coord_cartesian(xlim=c(0,1))

fig2D_Prob_plot<- subset(d, condition=="Probabilistic") %>% 
  group_by(trialID, condition, questionset, question, poli.party) %>% 
  dplyr::summarise(response_1 = mean(as.numeric(as.character(response_1)), na.rm=T), 
                   response_3 = mean(as.numeric(as.character(response_3)), na.rm=T), 
  ) %>%  group_by(trialID, condition, poli.party) %>% 
  dplyr::summarise(response_1 = mean(response_1), response_3 = mean(response_3)) %>%  
  gather(round, estimate, response_1:response_3, factor_key=T) %>% 
  subset(poli.party %in% c("Democratic", "Republican")) %>% 
  mutate(estimate_perc=estimate/100, poli.party=factor(poli.party, levels=c("Republican", "Democratic")), 
         round = factor(round, levels = c("response_3", "response_1"))) 

ggplot(fig2D_Prob_plot, aes(x = estimate_perc, y = round, fill=poli.party)) +
  geom_density_ridges(aes(scale=0.9, y = round),alpha=0.6, size=1.2) +
  scale_fill_manual(values=c("firebrick", "dodgerblue")) + ylab("Trials") + xlab("Trust") + 
  ggtitle("Probabilistic") + guides(fill=guide_legend(title="Categories")) + 
  theme(legend.text=element_text(size=30),legend.title=element_text(size=30), 
        legend.position="none",legend.background = element_rect(colour = "white"),
        legend.box.background = element_rect(colour = "black", fill="white"),
        plot.title=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.text.x=element_text(size = 30,hjust = 0.6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), labels =c(0, 0.25, 0.5, 0.75, 1)) + 
  coord_cartesian(xlim=c(0,1))

############
#Statistics#
############

#Fig 2A
d_prob_initial<-d %>% group_by(trialID, condition, question) %>%
  dplyr::summarise(prop_correct_1 = sum(correct_1) / length(correct_1)) %>% 
  group_by(trialID, condition) %>% dplyr::summarise_all(function(x) mean(x)) %>% select(-question)

wilcox.test(subset(d_prob_initial, condition == "Probabilistic")$prop_correct_1, 
            subset(d_prob_initial, condition == "Binary")$prop_correct_1)

d_prob_improve<-subset(d, condition=="Probabilistic") %>% 
  group_by(playerID, trialID, condition, question, poli.party) %>% 
  dplyr::mutate(error_1 = abs(as.numeric(correctanswer) - as.numeric(response_1)), 
                error_3 = abs(as.numeric(correctanswer) - as.numeric(response_3)), 
                improve = error_3 < error_1) %>% select(-error_1, -error_3)

d_binary_improve<-subset(d, condition=="Binary") %>% group_by(playerID, trialID, condition, question, poli.party) %>% 
  dplyr::mutate(improve = ifelse(correct_1 == F & correct_3 == T, TRUE, FALSE))

##Fig 2A: Improvement effect, controlling for demographics
d_improve_compare<-rbind(data.frame(d_prob_improve), data.frame(d_binary_improve))
d_improve_compare$improve<-as.numeric(d_improve_compare$improve)
d_improve_compare$condition<-as.factor(d_improve_compare$condition)

comp_improve<-glm(improve ~ question + poli.party + gender + race + 
                    marital + ideology_strength + income.ordinal + relig.ordinal + 
                    years.in.us + edu.ordinal + religion + eng.first.lang +
                    condition, family="binomial",
                  data = d_improve_compare)

summary(comp_improve)
exp(coef(comp_improve))
comp_improve_vcov <- cluster.vcov(comp_improve, d_improve_compare$trialID)
coeftest(comp_improve, comp_improve_vcov)

##Fig 2B
wilcox.test(d_binary_fig2B$group_increase_acc); 
mean(d_binary_fig2B$group_increase_acc)

wilcox.test(d_prob_fig2B$group_increase_acc); 
mean(d_prob_fig2B$group_increase_acc)

##Fig 2C
fig2C_Binary<- subset(d, condition=="Binary") %>% 
  mutate(trust_1=response_1=="Yes", trust_3=response_3=="Yes") %>%
  group_by(trialID, condition, questionset, question, poli.party) %>% 
  dplyr::summarise(trust_1 = sum(trust_1)/length(trust_1), 
                   trust_3 = sum(trust_3)/length(trust_3)) %>% 
  group_by(trialID, condition, poli.party) %>% 
  summarise_all(function(x)mean(x))

wilcox.test(subset(fig2C_Binary, poli.party=="Democratic")$trust_1, 
            subset(fig2C_Binary, poli.party=="Republican")$trust_1, 
            paired=T)

wilcox.test(subset(fig2C_Binary, poli.party=="Democratic")$trust_3, 
            subset(fig2C_Binary, poli.party=="Republican")$trust_3, 
            paired=T)

#Change in difference
r1_partisan_diff_binary<-subset(fig2C_Binary, poli.party=="Democratic")$trust_1 - 
  subset(fig2C_Binary, poli.party=="Republican")$trust_1

r3_partisan_diff_binary<-subset(fig2C_Binary, poli.party=="Democratic")$trust_3 - 
  subset(fig2C_Binary, poli.party=="Republican")$trust_3

wilcox.test(r1_partisan_diff_binary, r3_partisan_diff_binary, paired=T)

##Fig 2D
fig2D_Prob<- subset(d, condition=="Probabilistic") %>% 
  group_by(trialID, condition, questionset, question, poli.party) %>% 
  dplyr::summarise(trust_1 = mean(as.numeric(as.character(response_1)), na.rm=T), 
                   trust_3 = mean(as.numeric(as.character(response_3)), na.rm=T)) %>% 
  group_by(trialID, condition, poli.party) %>% 
  summarise_all(function(x)mean(x))

wilcox.test(subset(fig2D_Prob, poli.party=="Democratic")$trust_1, 
            subset(fig2D_Prob, poli.party=="Republican")$trust_1, 
            paired=T)

wilcox.test(subset(fig2D_Prob, poli.party=="Democratic")$trust_3, 
            subset(fig2D_Prob, poli.party=="Republican")$trust_3, 
            paired=T)

