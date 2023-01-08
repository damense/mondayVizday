# load packages
library(tidyverse)
library(rvest)
library(readtext)
library(flextable)
library(webdriver)
library(igraph)
library(networkD3)
library(lubridate)

# define url
url <- "https://cran.r-project.org/web/packages/available_packages_by_name.html"
url_basic <- "https://cran.r-project.org/web/packages/"
# download content
webc <- rvest::read_html(url)

webc %>%
        # extract paragraphs
        rvest::html_nodes("a") %>%
        # extract text
        rvest::html_attr('href') -> webtxt
# inspect
webs <- paste0(url_basic,unlist(str_split(webtxt,"../../")))
packs <- unlist(str_split(unlist(str_split(webtxt,"../../web/packages/")),"/index.html"))
packs <- packs[28:length(to)]
packs <- packs[packs!=""]
db_packs <- list()

i <- 1
for (i in i:length(packs)){
        url_temp <- paste(url_basic,packs[i],'/index.html', sep = "")
        all_info <- read_html(url_temp)
        
        name <- packs[i]
        title<- as.character(all_info %>% html_nodes("h2"))
        desc<- as.character({all_info %>% html_nodes("p")}[1])
        
        html_doc <- as.character(all_info %>% html_nodes("td"))
        html_doc <- gsub("<td>","",gsub("</td>","",html_doc))
        vers <- which(html_doc == "Version:")+1
        dep <- which(html_doc == "Depends:")+1
        imp <- which(html_doc == "Imports:")+1
        pub <- which(html_doc == "Published:")+1
        if (length(vers)>0){
                version <- html_doc[vers]
        }
        if (length(dep)>0){
                depend <- gsub("^ ",
                               "",
                               gsub("\n",
                                    "",
                                    gsub("<(.*?)>",
                                         "",
                                         str_split(html_doc[dep], 
                                                   ","
                                         )[[1]]
                                    )
                               )
                )
                r_vers <- gsub("R \\(",
                               "",
                               gsub("\\)",
                                    "",
                                    depend[1]
                               )
                )
                depend[1] <- "R"
                
        }
        if (length(imp)>0){
                imports <- gsub(" \\((.*?)\\)",
                                "",
                                gsub("^ ",
                                     "",
                                     gsub("\n",
                                          "",
                                          gsub("<(.*?)>",
                                               "",
                                               str_split(html_doc[imp], 
                                                         ","
                                               )[[1]]
                                          )
                                     )
                                )
                                )
        } else {
                imports <- ''
        }
        if(length(pub)){
                date_pub <- as.Date(html_doc[pub])
        }
        
        db_packs[[i]] <- data.frame(name=name,
                             title=title,
                             desc=desc,
                             vers=vers,
                             depend=paste(depend,collapse=","),
                             r_vers=r_vers,
                             imports=paste(imports,collapse=","),
                             date_pub=date_pub)
        
        
        
        if((i/1000)%%1 ==0){
                print(i*100/length(packs))
        }
        Sys.sleep(0.001)
        
}
i_old <- i

df_packs <- do.call("rbind", db_packs)
connections <- data.frame(source=character(0),
                          target=character(0))
for (j in 1:dim(df_packs)[1]){
        new_df <- data.frame(target=unlist(c(str_split(df_packs$depend[j],","),
                                str_split(df_packs$import[j],","))))
        new_df$source <- df_packs$name[j]
        connections <- rbind(connections,
                             new_df)
}
connections <- distinct(connections[connections$target!="",])
connections$target <- gsub(" \\((.*?)\\)", "", connections$target)
connections$source <- gsub(" \\((.*?)\\)", "", connections$source)
#connections$target <- gsub(" \\((.*?)\\)", "", connections$target)
# create the network object

network <- graph_from_data_frame(d=connections, directed=F) 
network_to <- graph_from_data_frame(d=connections[1:1000,], directed=F)
network_dplyr <- graph_from_data_frame(connections[connections$source=="dplyr"|connections$target=="dplyr",])

# plot it
plot(network_to, vertex.label=NA)

#function

network_it <- function(name){
        packages <- name
        counter <- 1
        new_added <- 1
        while(new_added !=0){
                new_source <- connections[connections$source %in% packages,]$target
                new_vector_packages <- unique(c(packages,new_source))
                new_added <- length(new_vector_packages) - length(packages)
                packages <- new_vector_packages
        }
        plot_data <- connections[connections$source %in% packages,]
        network <- graph_from_data_frame(d=plot_data, directed=F) 
        plot(network)
}
