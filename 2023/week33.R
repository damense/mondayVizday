# 14Aug23
#Author: David Mendez


library(tidyverse)
library(tidytuesdayR)
library(tidymodels)
library(randomForest)
library(ggsankey)
library("PerformanceAnalytics")
library(rpart.plot)
tidymodels_prefer()
set.seed(31)

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 33)
spam <- tuesdata$spam %>%
        mutate(yesno = factor(yesno))
spam0 <- spam  %>% filter(dollar==0, bang==0, money==0, n000==0, make==0)
spam_rest <- spam  %>% filter(dollar!=0| bang!=0| money!=0| n000!=0| make!=0)

# Data division
spam_split <- initial_split(spam_rest, prop=0.8)
spam_train <- training(spam_split)
spam_test  <-  testing(spam_split)

#build the model

fit <- rpart(yesno~., data = spam_rest, method = 'class')
rpart.plot(fit, extra = 106)


# wrangle the data for the Sankey plot



node <- c(rep("spam",1821),rep("$,!,money,000,make ==0",1821),rep("no",1821),
          rep("spam",0.32*2780),rep("$|!|money|000|make !=0",0.32*2780), rep("dollar>0.056",0.32*2780),rep("bang>0.05",0.32*2780),rep("yes",0.32*2780),
          rep("spam",0.03*2780),rep("$|!|money|000|make !=0",0.03*2780), rep("dollar>0.056",0.03*2780),rep("bang<0.05",0.03*2780),rep("money>0.025",0.03*2780),rep("yes",0.03*2780),
          rep("spam",0.01*2780),rep("$|!|money|000|make !=0",0.01*2780), rep("dollar>0.056",0.01*2780),rep("bang<0.05",0.01*2780),rep("money<0.025",0.01*2780),rep("n000>0.47",0.01*2780),rep("yes",0.01*2780),
          rep("spam",0.05*2780),rep("$|!|money|000|make !=0",0.05*2780), rep("dollar>0.056",0.05*2780),rep("bang<0.05",0.05*2780),rep("money<0.025",0.05*2780),rep("n000<0.47",0.05*2780),rep("no",0.05*2780),
          rep("spam",0.27*2780),rep("$|!|money|000|make !=0",0.27*2780), rep("dollar<0.056",0.27*2780),rep("bang<0.19",0.27*2780),rep("money<0.01",0.27*2780),rep("no",0.27*2780),
          rep("spam",0.03*2780),rep("$|!|money|000|make !=0",0.03*2780), rep("dollar<0.056",0.03*2780),rep("bang<0.19",0.03*2780),rep("money>0.01",0.03*2780),rep("yes",0.03*2780),
          rep("spam",0.16*2780),rep("$|!|money|000|make !=0",0.16*2780), rep("dollar<0.056",0.16*2780),rep("bang>0.19",0.16*2780),rep("crl.tot>69",0.16*2780),rep("yes",0.16*2780),
          rep("spam",0.09*2780),rep("$|!|money|000|make !=0",0.09*2780), rep("dollar<0.056",0.09*2780),rep("bang>0.19",0.09*2780),rep("crl.tot<69",0.09*2780),rep("bang<0.8",0.09*2780),rep("no",0.09*2780),
          rep("spam",0.02*2780),rep("$|!|money|000|make !=0",0.02*2780), rep("dollar<0.056",0.02*2780),rep("bang>0.19",0.02*2780),rep("crl.tot<69",0.02*2780),rep("bang>0.8",0.02*2780),rep("crl.tot<23",0.02*2780),rep("no",0.02*2780),
          rep("spam",0.02*2780),rep("$|!|money|000|make !=0",0.02*2780), rep("dollar<0.056",0.02*2780),rep("bang>0.19",0.02*2780),rep("crl.tot<69",0.02*2780),rep("bang>0.8",0.02*2780),rep("crl.tot>23",0.02*2780),rep("yes",0.02*2780))

next_node <- c(rep("$,!,money,000,make ==0",1821),rep("no",1821),rep(NA,1821),
               rep("$|!|money|000|make !=0",0.32*2780), rep("dollar>0.056",0.32*2780),rep("bang>0.05",0.32*2780),rep("yes",0.32*2780),rep(NA,0.32*2780),
               rep("$|!|money|000|make !=0",0.03*2780), rep("dollar>0.056",0.03*2780),rep("bang<0.05",0.03*2780),rep("money>0.025",0.03*2780),rep("yes",0.03*2780),rep(NA,0.03*2780),
               rep("$|!|money|000|make !=0",0.01*2780), rep("dollar>0.056",0.01*2780),rep("bang<0.05",0.01*2780),rep("money<0.025",0.01*2780),rep("n000>0.47",0.01*2780),rep("yes",0.01*2780),rep(NA,0.01*2780),
               rep("$|!|money|000|make !=0",0.05*2780), rep("dollar>0.056",0.05*2780),rep("bang<0.05",0.05*2780),rep("money<0.025",0.05*2780),rep("n000<0.47",0.05*2780),rep("no",0.05*2780),rep(NA,0.05*2780),
               rep("$|!|money|000|make !=0",0.27*2780), rep("dollar<0.056",0.27*2780),rep("bang<0.19",0.27*2780),rep("money<0.01",0.27*2780),rep("no",0.27*2780), rep(NA, 0.27*2780),
               rep("$|!|money|000|make !=0",0.03*2780), rep("dollar<0.056",0.03*2780),rep("bang<0.19",0.03*2780),rep("money>0.01",0.03*2780),rep("yes",0.03*2780), rep(NA, 0.03*2780),
               rep("$|!|money|000|make !=0",0.16*2780), rep("dollar<0.056",0.16*2780),rep("bang>0.19",0.16*2780),rep("crl.tot>69",0.16*2780),rep("yes",0.16*2780), rep(NA, 0.16*2780),
               rep("$|!|money|000|make !=0",0.09*2780), rep("dollar<0.056",0.09*2780),rep("bang>0.19",0.09*2780),rep("crl.tot<69",0.09*2780),rep("bang<0.8",0.09*2780), rep("no",0.09*2780), rep(NA,0.09*2780),
               rep("$|!|money|000|make !=0",0.02*2780), rep("dollar<0.056",0.02*2780),rep("bang>0.19",0.02*2780),rep("crl.tot<69",0.02*2780),rep("bang>0.8",0.02*2780),rep("crl.tot<23",0.02*2780),rep("no",0.02*2780),rep(NA,0.02*2780),
               rep("$|!|money|000|make !=0",0.02*2780), rep("dollar<0.056",0.02*2780),rep("bang>0.19",0.02*2780),rep("crl.tot<69",0.02*2780),rep("bang>0.8",0.02*2780),rep("crl.tot>23",0.02*2780),rep("yes",0.02*2780),rep(NA,0.02*2780))

x <- c(rep(1,1821),rep(2,1821),rep(8,1821),
       rep(1, 0.32*2780),rep(2, 0.32*2780),rep(3, 0.32*2780),rep(4, 0.32*2780),rep(8, 0.32*2780),
       rep(1, 0.03*2780),rep(2, 0.03*2780),rep(3, 0.03*2780),rep(4, 0.03*2780),rep(5, 0.03*2780),rep(8, 0.03*2780),
       rep(1, 0.01*2780),rep(2, 0.01*2780),rep(3, 0.01*2780),rep(4, 0.01*2780),rep(5, 0.01*2780),rep(6, 0.01*2780),rep(8, 0.01*2780),
       rep(1, 0.05*2780),rep(2, 0.05*2780),rep(3, 0.05*2780),rep(4, 0.05*2780),rep(5, 0.05*2780),rep(6, 0.05*2780),rep(8, 0.05*2780),
       rep(1, 0.27*2780),rep(2, 0.27*2780),rep(3, 0.27*2780),rep(4, 0.27*2780),rep(5, 0.27*2780),rep(8, 0.27*2780),
       rep(1, 0.03*2780),rep(2, 0.03*2780),rep(3, 0.03*2780),rep(4, 0.03*2780),rep(5, 0.03*2780),rep(8, 0.03*2780),
       rep(1, 0.16*2780),rep(2, 0.16*2780),rep(3, 0.16*2780),rep(4, 0.16*2780),rep(5, 0.16*2780),rep(8, 0.16*2780),
       rep(1, 0.09*2780),rep(2, 0.09*2780),rep(3, 0.09*2780),rep(4, 0.09*2780),rep(5, 0.09*2780),rep(6, 0.09*2780),rep(8, 0.09*2780),
       rep(1, 0.02*2780),rep(2, 0.02*2780),rep(3, 0.02*2780),rep(4, 0.02*2780),rep(5, 0.02*2780),rep(6, 0.02*2780),rep(7, 0.02*2780),rep(8, 0.02*2780),
       rep(1, 0.02*2780),rep(2, 0.02*2780),rep(3, 0.02*2780),rep(4, 0.02*2780),rep(5, 0.02*2780),rep(6, 0.02*2780),rep(7, 0.02*2780),rep(8, 0.02*2780))

next_x <- c(rep(2,1821),rep(8,1821),rep(NA,1821),
            rep(2, 0.32*2780),rep(3, 0.32*2780),rep(4, 0.32*2780),rep(8, 0.32*2780),rep(NA,0.32*2780),
            rep(2, 0.03*2780),rep(3, 0.03*2780),rep(4, 0.03*2780),rep(5, 0.03*2780),rep(8, 0.03*2780),rep(NA, 0.03*2780),
            rep(2, 0.01*2780),rep(3, 0.01*2780),rep(4, 0.01*2780),rep(5, 0.01*2780),rep(6, 0.01*2780),rep(8, 0.01*2780),rep(NA,0.01*2780),
            rep(2, 0.05*2780),rep(3, 0.05*2780),rep(4, 0.05*2780),rep(5, 0.05*2780),rep(6, 0.05*2780),rep(8, 0.05*2780),rep(NA,0.05*2780),
            rep(2, 0.27*2780),rep(3, 0.27*2780),rep(4, 0.27*2780),rep(5, 0.27*2780),rep(8, 0.27*2780),rep(NA, 0.27*2780),
            rep(2, 0.03*2780),rep(3, 0.03*2780),rep(4, 0.03*2780),rep(5, 0.03*2780),rep(8, 0.03*2780),rep(NA, 0.03*2780),
            rep(2, 0.16*2780),rep(3, 0.16*2780),rep(4, 0.16*2780),rep(5, 0.16*2780),rep(8, 0.16*2780),rep(NA, 0.16*2780),
            rep(2, 0.09*2780),rep(3, 0.09*2780),rep(4, 0.09*2780),rep(5, 0.09*2780),rep(6, 0.09*2780),rep(8, 0.09*2780),rep(NA, 0.09*2780),
            rep(2, 0.02*2780),rep(3, 0.02*2780),rep(4, 0.02*2780),rep(5, 0.02*2780),rep(6, 0.02*2780),rep(7, 0.02*2780),rep(8, 0.02*2780), rep(NA,0.02*2780),
            rep(2, 0.02*2780),rep(3, 0.02*2780),rep(4, 0.02*2780),rep(5, 0.02*2780),rep(6, 0.02*2780),rep(7, 0.02*2780),rep(8, 0.02*2780), rep(NA,0.02*2780))

# Data allocation

df <- data.frame(node=node,
                 next_node=next_node,
                 x=x,
                 next_x=next_x)

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill=factor(x))) +
        geom_sankey(flow.alpha = 0.75, node.color = 1)+
        geom_sankey_label(aes(label=node)) +
        theme_sankey()+
        xlab('')
# Idea 2

x0 <- c(0,1,1,2,3,
        4,4)
x1 <- c(1,8,2,3,4,
        8,8)
y0 <- c(0,0.75,-0.75,-1,-2,
        -3,-3)
y1 <- c(0,0.75,-0.75,-2,-3,
        -10,-9)
size <- c(4601,1821,fit$frame$n[1:5])
spam_perc <- c(0.394,0.085,fit$frame$yval2[1:5,5])
df2 <- data.frame(x0=x0,
                  x1=x1,
                  y0=y0,
                  y1=y1,
                  size=size,
                  spam_perc=spam_perc)

ggplot(data=df2,
       aes(size=size/100,
           color=spam_perc)) +
        geom_segment(aes(x=x0,
                         xend=x1,
                         y=y0,
                         yend=y1)) +
        scale_size_identity()+
        theme_void()

#predict
pred <- predict(fit, spam_test, type = 'class')
table_mat <- table(spam_test$yesno, pred)
table_mat

# Checking correlations
ggplot(data=spam_train %>%
               pivot_longer(cols = 1:6)) +
        geom_point(aes(x=value, y=yesno)) +
        facet_wrap(scales="free_x", ~name)


# Set-up the model
rf <- randomForest(yesno~., data=spam_train, proximity=TRUE) 
plot(rf)
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
#Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)
MDSplot(rf, spam_train$yesno)







# sauces <- tuesdata$sauces
# seasons <- tuesdata$seasons
# 
# broken <- data.frame(season=c(1,1,2,2,2,
#                               3,4,5,7,7,
#                               8,10,12,15,17),
#                      episode_season=c(1,8,12,13,17,
#                                       4,19,1,5,6,
#                                       8,10,3,3,10),
#                      broke=c(NA,3,7,8,8,
#                              8,8,6,8,8,
#                              9,8,8,8,8)) 
# d <- data.frame(x = 2.5,
#                 y = 5e5,
#                 image = "hot_ones.png")
# 
# sauces_plus <- sauces %>%
#         left_join(episodes %>%
#                           select(season, episode_season, guest), multiple = "all") %>%
#         left_join(broken,
#                   by=c("season","episode_season"))
# 
# my_breaks <- c(1000,10000,100000, 1000000)
# 
# ggplot(data=sauces_plus %>%
#                mutate(guest=ifelse(is.na(broke),
#                                    NA,
#                                    guest),
#                       straw=sauce_number-broke) %>%
#                filter(straw==0),
#        aes(x=factor(sauce_number),
#            y=scoville)) +
#         geom_point(data=sauces_plus %>%
#                            select(season,sauce_number, scoville) %>%
#                            distinct(),
#                    aes(x=factor(sauce_number),
#                        y=scoville,
#                        color=scoville),
#                    size=2,
#                    position = position_jitter(0.2,
#                                               seed = 31))+
#         geom_point( aes(color=scoville),
#                 size=2, 
#                    position=position_jitter(0.2,
#                                             seed = 31)
#                    ) +
#         geom_label_repel(aes(label=guest,
#                  #           nudge_y=scoville*0.0001
#                  ),
#                  alpha=0.8,
#                          force_pull=0.1,
#                          force=1,
#                  max.overlaps = 100,
#                    position = position_jitter(0.2,
#                                               seed = 31),
#                    min.segment.length = 0)+
#         geom_image(data = d,
#                    aes(x=x,y=y,image=image),
#                    size=0.3)+
#         scale_color_continuous(name="Scoville",
#                                type="gradient",
#                                low="yellow",
#                                high="darkred",
#                                trans="log",
#                                breaks = my_breaks, labels = my_breaks)+
#         scale_y_log10(labels = scales::label_log()) +
#     #    scale_color_brewer(palette="Dark2", trans="log")+
#         theme_bw() +
#         theme(legend.position = "none")+
#         xlab("Sauce Number") +
#         ylab("Scoville factor")
# ggsave("week32.jpg")
