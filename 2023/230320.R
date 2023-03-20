# 25-Feb-22
#Author: David Mendez

library(tidyverse)
library(tidytuesdayR)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read the dataset ----

tuesdata <- tidytuesdayR::tt_load(2023, week = 11)
drugs <- tuesdata$drugs
hiv_drugs <- drugs %>% filter(grepl("HIV",therapeutic_area))
company <- c("ViiV Healthcare BV"                           ,"Janssen-Cilag International NV",              
             "Teva B.V."                                    ,"Merck Sharp & Dohme B.V."       ,             
             "Gilead Sciences Ireland UC"                   ,"KRKA, d.d., Novo mesto"          ,            
             "Mylan Pharmaceuticals Limited"                ,"ViiV Healthcare B.V."             ,           
             "Theratechnologies Europe Limited"             ,"Janssen-Cilag International N.V."  ,          
             "Gilead Sciences International Ltd "           ,"AbbVie Deutschland GmbH  Co. KG"    ,         
             "Roche Registration GmbH"                      ,"Bristol-Myers Squibb Pharma EEIG"    ,        
             "Janssen-Cilag International N.V.   "          ,"Mylan S.A.S"                          ,       
             "Zentiva k.s."                                 ,"Gilead Sciences Ireland Unlimited Company",   
             "Boehringer Ingelheim International GmbH"      ,"Teva Pharma B.V. ",                           
             "Teva B.V. "                                   ,"Krka, d.d., Novo mesto",                      
             "Gilead Sciences International Ltd"            ,"Merck Sharp  Dohme Limited",                  
             "Roche Registration Ltd."                      ,"Glaxo Group Ltd.",                            
             "Novartis Ophthalmics Europe Ltd."        ,"Dr. Gerhard Mann, Chem.-Pharm. Fabrik GmbH..")
company_better <- c("ViiV Healthcare BV","Janssen",              
                    "Teva B.V.", "Merck Sharp & Dohme",             
                    "Gilead Sciences","KRKA",            
                    "Mylan Pharmaceuticals","ViiV Healthcare BV",           
                    "Theratechnologies Europe Limited","Janssen",          
                    "Gilead Sciences","AbbVie Deutschland GmbH  Co. KG"    ,         
                    "Roche","Bristol-Myers Squibb",        
                    "Janssen","Mylan Pharmaceuticals"                          ,       
                    "Zentiva k.s.","Gilead Sciences",   
                    "Boehringer Ingelheim","Teva B.V.",                           
                    "Teva B.V. ","KRKA",                      
                    "Gilead Sciences","Merck Sharp & Dohme",                  
                    "Roche","GSK",                            
                    "Novartis","Dr. Gerhard Mann")
for (i in 1:length(company)){
        hiv_drugs$marketing_authorisation_holder_company_name[grepl(
                company[i],hiv_drugs$marketing_authorisation_holder_company_name)] <- company_better[i]
}



# 1st plot
f1 <- ggplot(data=hiv_drugs %>% filter(authorisation_status=="authorised"), 
             aes(marketing_authorisation_date))+
        geom_histogram(aes(y = cumsum(..count..)), fill="red") +
        theme_void() + 
        labs(title = "Cummulative amount of drugs authorised \n for HIV treatment through the years")+
        theme(
                axis.title=element_blank(),
                axis.text = element_text(size=14)
        )
# 2nd plot

f2 <- ggplot(data = hiv_drugs %>%
               group_by(marketing_authorisation_holder_company_name, authorisation_status) %>%
               summarise(n=n()) %>%
               mutate(authorisation_status = factor(authorisation_status),
                       n=ifelse(authorisation_status=="withdrawn",-n,n))
               ,
       aes(x=marketing_authorisation_holder_company_name)) +
        geom_bar(aes(y=n,
                fill=authorisation_status),
                stat='identity') + 
        scale_fill_manual(name="Status", values=c("authorised"="red","withdrawn"="lightblue"))+
        geom_text(data = hiv_drugs %>%
                          group_by(marketing_authorisation_holder_company_name, authorisation_status) %>%
                          summarise(n=n()) %>%
                          mutate(n=ifelse(authorisation_status=="withdrawn",-n,n)) %>%
                          mutate(marketing_authorisation_holder_company_name=ifelse(authorisation_status=="withdrawn" &
                                                                                            marketing_authorisation_holder_company_name=="Bristol-Myers Squibb",
                                                                                    "",marketing_authorisation_holder_company_name)) %>%
                          mutate(marketing_authorisation_holder_company_name=ifelse(authorisation_status=="withdrawn" &
                                                                                            marketing_authorisation_holder_company_name=="Gilead Sciences",
                                                                                    "",marketing_authorisation_holder_company_name))%>%
                          mutate(marketing_authorisation_holder_company_name=ifelse(authorisation_status=="withdrawn" &
                                                                                            marketing_authorisation_holder_company_name=="KRKA",
                                                                                    "",marketing_authorisation_holder_company_name))%>%
                          mutate(marketing_authorisation_holder_company_name=ifelse(authorisation_status=="withdrawn" &
                                                                                            marketing_authorisation_holder_company_name=="Roche",
                                                                                    "",marketing_authorisation_holder_company_name))%>%
                          mutate(marketing_authorisation_holder_company_name=ifelse(authorisation_status=="withdrawn" &
                                                                                            marketing_authorisation_holder_company_name=="Merck Sharp & Dohme",
                                                                                    "",marketing_authorisation_holder_company_name)),
                  aes(y=n,label=marketing_authorisation_holder_company_name), angle=90,hjust=0, size=3)+
        ylim(c(-2,20)) +
        theme_void() + 
        labs(title = "Amount of drugs authorised for HIV \ntreatment per company", size=10)+
        theme(
                axis.title=element_blank(),
                axis.text.y = element_text(size=12),
                axis.text.x=element_blank()
        )
# 3rd plot
active <- unlist(str_split(paste(gsub("/",
                               ",",
                               hiv_drugs$common_name
                               ),
                          collapse = ", "
                          ),
                    ", "), use.names = F)      
active_df <- data.frame(table(word(active,
                                1)
                           )
                     ) %>%
        filter(Freq>3) %>%
        rbind(data.frame(Var1 = c("23 other"),
                         Freq = 32)) %>%
        mutate(Var1=paste0(Var1," \n(",round(Freq/119*100,0),"%)"))
colnames(active_df) <- c("name", "number")

active_df$name <- as.character(active_df$name)

# 
# active_df$label[active_df$number<3] <- ""
# active_df
#%>%
#        mutate(Freq=Freq/sum(Freq)*100)
# active_df[order(-active_df$Freq),] %>%
#         filter(Freq>=4)

library(treemapify)
library (RColorBrewer)
colFunc <- colorRampPalette(c("#ffcccb", "darkred"))
f3 <- ggplot(data=active_df %>% mutate(name=factor(name)), 
       aes(area=number, fill=name, label=name)) + 
        geom_treemap()+
        geom_treemap_text()+
        theme_void()+
        scale_fill_manual(values = colFunc(12))+ 
        labs(title="Most common active ingredients")+
        theme(legend.position = "none",
              title = element_text(vjust=-2)) 
# merge
library(ggpubr)
library(png)
img <- readPNG("data/red_ribbon.png.png")
f4 <- ggplot() + 
        background_image(img) +
        theme_void()+
        # This ensures that the image leaves some space at the edges
        theme(plot.margin = margin(t=0, l=2, r=2, b=0, unit = "cm"))
ggarrange("Act up!",f1, "", f2,f4, f3, ncol=3, nrow = 2) + bgcolor("#ffcccb")    
ggsave("230320.png")
