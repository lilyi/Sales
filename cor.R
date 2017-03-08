#install.packages("quantmod")
#install.packages("tidyr")

library(quantmod) 
library(tidyr)
library(plyr)
library(ggplot2)
require(scales)

############
Sdata <- read.csv('C:/Users/Lily/Documents/GA/R/sales/sales_Feb_2017.csv', header = T)
from <- levels(Sdata$Currency)
to <- c("USD")
A <- getQuote(paste0(from, to, "=X"))
table <- data.frame(Currency=rownames(A), Last=A$Last)
#Sdata[, "exrate"] <- c(0)
#table[table$Currency=="CHFUSD=X",]$Last
#Sdata[Sdata$Currency=="CHF",]$exrate <- table[table$Currency=="CHFUSD=X",]$Last
final_df <- separate(table , Currency , c("Currency" , "dis") , sep = "USD=")
new <- merge(Sdata,final_df)
new$exrate <- NULL
new$dis <- NULL
new[,"USD"] <- new$Sales*new$Last


SUM[,"Country"] <- unlist(clist)
final <- merge(SUM,new)
cor_data <- ddply(final, .(Country, Sessions), summarize, USD = sum(USD)) #lack of 3 countries: Canada. Iran. Mexico
cor.test(cor_data$Sessions, cor_data$USD, alternative = "g")
cor.test(cor_data$Sessions, cor_data$USD, method = "kendall", alternative = "greater")
cor.test(cor_data$Sessions, cor_data$USD, method = "kendall", alternative = "greater",
         exact = FALSE) # using large sample approximation
cor.test(cor_data$Sessions, cor_data$USD, method = "spearm", alternative = "g")

rownames(cor_data) <- cor_data$Country

abbn <- read.csv('C:/Users/Lily/Documents/GA/R/sales/abbn.csv', header = F)
cor_data[,"abbr"] <- abbn
pairs(cor_data[,c("Sessions","USD")])



#
p <- ggplot(cor_data, aes(x=Sessions, y=USD, label = abbr)) +
  geom_point(aes(colour = factor(Country))) + # shape=1
  geom_smooth(method=lm, se=FALSE) 
p + scale_y_continuous(labels = comma) +
  geom_label()+
  #geom_text(aes(label=ifelse(USD>1000000,as.character(Country),'')),hjust=0,nudge_y = 0.05) +
  #geom_text(angle = 45) +
  scale_x_continuous(labels = comma)+
  labs(title = "Feb. 2017", x = "Sessions", y = "Sales (USD)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), title = element_text(size = 10))
##############################################################################
p <- ggplot(cor_data, aes(x=Sessions, y=USD, label = abbr))
A <- p + geom_point() + # shape=1 aes(colour = factor(Country))
  scale_y_continuous(labels = comma) +
  geom_smooth(method=lm, se=FALSE) +
  scale_x_continuous(labels = comma)+
  labs(title = "Feb. 2017", x = "Sessions", y = "Sales (USD)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), title = element_text(size = 10))
ggsave('C:/Users/Lily/Documents/GA/R/sales/cor_1.png', A)

B <- p + geom_label() +
  scale_y_continuous(labels = comma) +
  geom_smooth(method=lm, se=FALSE) +
  scale_x_continuous(labels = comma)+
  labs(title = "Feb. 2017", x = "Sessions", y = "Sales (USD)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), title = element_text(size = 10))
ggsave('C:/Users/Lily/Documents/GA/R/sales/cor_2.png', B)
C <- p + geom_point(aes(colour = factor(Country))) +
  geom_text(aes(label=ifelse(USD>1000000,as.character(Country),'')),hjust=1,vjust=1) +
  scale_y_continuous(labels = comma) +
  #geom_smooth(method=lm, se=FALSE) +
  scale_x_continuous(labels = comma)+
  labs(title = "Feb. 2017", x = "Sessions", y = "Sales (USD)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), title = element_text(size = 10))
ggsave('C:/Users/Lily/Documents/GA/R/sales/cor_3.png', C)
