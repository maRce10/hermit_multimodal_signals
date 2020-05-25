setwd("C:/Users/KWJ/Dropbox/Phethornis synchro/WWH_individual files")
# setwd("C:/Users/KWJ/Dropbox/Phethornis synchro/LBH_individual files")

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

dataframes <- list.files(path = getwd())
list_dataframes <- llply(dataframes, read.csv, header = T, sep = ",", dec = ".") 

# Loop for the graphs

for (i in 1:length(dataframes)){
  
  # Subsetting data sets
  
  trial_sb <- list_dataframes[[i]] %>% 
    dplyr::filter(code == "voice_start" | code == "tail_up") %>%
    dplyr::select(time, code, class)

  trial_rest <- list_dataframes[[i]] %>% 
    dplyr::filter(code != "voice_start") %>%
    dplyr::filter(code != "tail_up") %>%
    dplyr::filter(code != "END") %>%
    dplyr::select(time, code, class)
  
  trial_rest$row <- 1:nrow(trial_rest)
  trial_rest$short <- substr(trial_rest$code,1,4)
  trial_rest$short <- sub(pattern = " ", replacement =  "", trial_rest$short)
  trial_rest$short <- substr(trial_rest$short, 1,3)
  
  
  # Rectangles data frames....

  if(nrow(trial_rest[trial_rest$short == "con",]) > 0) {
    df_cb <- trial_rest[trial_rest$short == "con",]
    cb_xmin <- df_cb$time[seq(1, length(df_cb$time), 2)]
    cb_xmax <- df_cb$time[seq(2, length(df_cb$time), 2)]
    df_cb <- data.frame(xmin=cb_xmin, xmax=cb_xmax, ymin=rep(-Inf, length(cb_xmin)), ymax=rep(Inf, length(cb_xmin)))
  }
  
  if(nrow(trial_rest[trial_rest$short == "int",]) > 0) {
    df_int <- trial_rest[trial_rest$short == "int",]
    cb_xmin <- df_int$time[seq(1, length(df_int$time), 2)]
    cb_xmax <- df_int$time[seq(2, length(df_int$time), 2)]
    df_int <- data.frame(xmin=cb_xmin, xmax=cb_xmax, ymin=rep(-Inf, length(cb_xmin)), ymax=rep(Inf, length(cb_xmin)))
  }
  
  if(nrow(trial_rest[trial_rest$short == "vid",]) > 0) {
    df_vid <- trial_rest[trial_rest$short == "vid",]
    cb_xmin <- df_vid$time[seq(1, length(df_vid$time), 2)]
    cb_xmax <- df_vid$time[seq(2, length(df_vid$time), 2)]
    df_vid <- data.frame(xmin=cb_xmin, xmax=cb_xmax, ymin=rep(-Inf, length(cb_xmin)), ymax=rep(Inf, length(cb_xmin)))
  }
  
  

  # Plot, and savig the plot - first version
  
  
  FileName <- paste(dataframes[[i]], ".jpg", sep="")
  jpeg(file = FileName)
  
  if(exists("df_cb") & exists("df_int") & exists("df_vid")){
    ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
      geom_line(data = trial_sb, aes(x = time, y = class)) + 
      theme_classic() +
      scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
      geom_rect(data=df_cb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.2, inherit.aes = FALSE) +
      geom_rect(data=df_int, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="tomato", alpha=0.2, inherit.aes = FALSE) +
      geom_rect(data=df_vid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
    
  } else {
    if (exists("df_cb") & exists("df_vid")) {
      ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
        geom_line(data = trial_sb, aes(x = time, y = class)) + 
        theme_classic() +
        scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
        geom_rect(data=df_cb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.2, inherit.aes = FALSE) +
        geom_rect(data=df_vid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
      
    } else {
      if (exists("df_int") & exists("df_vid")) {
        ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
          geom_line(data = trial_sb, aes(x = time, y = class)) + 
          theme_classic() +
          scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
          geom_rect(data=df_int, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.2, inherit.aes = FALSE) +
          geom_rect(data=df_vid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
        
      }else{
        if (exists("df_int") & exists("df_vid")) {
          ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
            geom_line(data = trial_sb, aes(x = time, y = class)) + 
            theme_classic() +
            scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
            geom_rect(data=df_int, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.2, inherit.aes = FALSE) +
            geom_rect(data=df_cb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
          
        } else{
          if (exists("df_cb")) {
            ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
              geom_line(data = trial_sb, aes(x = time, y = class)) + 
              theme_classic() +
              scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
              geom_rect(data=df_cb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
            
            
          } else {
            if (exists("df_int")){
              ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
                geom_line(data = trial_sb, aes(x = time, y = class)) + 
                theme_classic() +
                scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
                geom_rect(data=df_int, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
              
            } else {
              if (exists("df_vid") >= 1){
                ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
                  geom_line(data = trial_sb, aes(x = time, y = class)) + 
                  theme_classic() +
                  scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
                  geom_rect(data=df_vid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
                
              } else {
                ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
                  geom_line(data = trial_sb, aes(x = time, y = class)) + 
                  theme_classic() +
                  scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) 
              }
            }
          }
        }
      }
    }
  }
  
  
  dev.off()

  }
 



 
  # # Plot, and savig the plot - first version
  # 
  # 
  # FileName <- paste(dataframes[[i]], ".jpg", sep="")
  # setwd("C:/Users/KWJ/Dropbox/Phethornis synchro/Graphs")
  # jpeg(file = FileName)
  # 
  # if(nrow(df_vid) >= 1 & nrow(df_cb) >= 1 &  nrow(df_int) >= 1){
  #   ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
  #     geom_line(data = trial_sb, aes(x = time, y = class)) + 
  #     theme_classic() +
  #     scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
  #     geom_rect(data=df_cb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.2, inherit.aes = FALSE) +
  #     geom_rect(data=df_int, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="tomato", alpha=0.2, inherit.aes = FALSE) +
  #     geom_rect(data=df_vid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
  #   
  # } else {
  #   if (nrow(df_vid) >= 1 & nrow(df_cb)) {
  #     ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
  #       geom_line(data = trial_sb, aes(x = time, y = class)) + 
  #       theme_classic() +
  #       scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
  #       geom_rect(data=df_cb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.2, inherit.aes = FALSE) +
  #       geom_rect(data=df_vid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
  #     
  #   } else {
  #     if (nrow(df_vid) >= 1 & nrow(df_int)) {
  #       ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
  #         geom_line(data = trial_sb, aes(x = time, y = class)) + 
  #         theme_classic() +
  #         scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
  #         geom_rect(data=df_int, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.2, inherit.aes = FALSE) +
  #         geom_rect(data=df_vid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
  #       
  #     }else{
  #       if (nrow(df_cb) >= 1 & nrow(df_int)) {
  #         ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
  #           geom_line(data = trial_sb, aes(x = time, y = class)) + 
  #           theme_classic() +
  #           scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
  #           geom_rect(data=df_int, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.2, inherit.aes = FALSE) +
  #           geom_rect(data=df_cb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
  #         
  #       } else{
  #         if (nrow(df_cb) >= 1) {
  #           ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
  #             geom_line(data = trial_sb, aes(x = time, y = class)) + 
  #             theme_classic() +
  #             scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
  #             geom_rect(data=df_cb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
  #           
  #           
  #         } else {
  #           if (nrow(df_int) >= 1){
  #             ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
  #               geom_line(data = trial_sb, aes(x = time, y = class)) + 
  #               theme_classic() +
  #               scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
  #               geom_rect(data=df_int, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
  #             
  #           } else {
  #             if (nrow(df_vid) >= 1){
  #               ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
  #                 geom_line(data = trial_sb, aes(x = time, y = class)) + 
  #                 theme_classic() +
  #                 scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
  #                 geom_rect(data=df_vid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
  #               
  #             } else {
  #               ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
  #                 geom_line(data = trial_sb, aes(x = time, y = class)) + 
  #                 theme_classic() +
  #                 scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) 
  #             }
  #           }
  #         }
  #       }
  #     }
  #   }
  # }
  # 
  # 
  # dev.off()
  # 
  # 
  
    






#######################################################################################






# Trial


# trial <- read.csv (file = "entry0017_2000-01-01T000000_000.csv",header = T, sep = ",")
# 
# 
# # Data frame with significant behaviours only
# trial_sb <- trial %>% 
#   dplyr::filter(code == "voice_start" | code == "tail_up") %>%
#   dplyr::select(time, code, class)
# 
# 
# 
# # Data frame with the other stuff (various potentially confounding behavs, interactions, video distrubance)
# trial_rest <- trial %>% 
#   dplyr::filter(code != "voice_start") %>%
#   dplyr::filter(code != "tail_up") %>%
#   dplyr::filter(code != "END") %>%
#   dplyr::select(time, code, class)
# 
# trial_rest$row <- 1:nrow(trial_rest)
# trial_rest$short <- substr(trial_rest$code,1,4)
# trial_rest$short <- sub(pattern = " ", replacement =  "", trial_rest$short)
# trial_rest$short <- substr(trial_rest$short, 1,3)
# 
# 
# 
# # Trial rectangle
# 
#   df_cb <- trial_rest[trial_rest$short == "con",]
#   cb_xmin <- df_cb$time[seq(1, length(df_cb$time), 2)]
#   cb_xmax <- df_cb$time[seq(2, length(df_cb$time), 2)]
#   df_cb <- data.frame(xmin=cb_xmin, xmax=cb_xmax, ymin=rep(-Inf, length(cb_xmin)), ymax=rep(Inf, length(cb_xmin)))
#   
#   df_int <- trial_rest[trial_rest$short == "int",]
#   cb_xmin <- df_int$time[seq(1, length(df_int$time), 2)]
#   cb_xmax <- df_int$time[seq(2, length(df_int$time), 2)]
#   df_int <- data.frame(xmin=cb_xmin, xmax=cb_xmax, ymin=rep(-Inf, length(cb_xmin)), ymax=rep(Inf, length(cb_xmin)))
#   
#   df_vid <- trial_rest[trial_rest$short == "vid",]
#   cb_xmin <- df_vid$time[seq(1, length(df_vid$time), 2)]
#   cb_xmax <- df_vid$time[seq(2, length(df_vid$time), 2)]
#   df_vid <- data.frame(xmin=cb_xmin, xmax=cb_xmax, ymin=rep(-Inf, length(cb_xmin)), ymax=rep(Inf, length(cb_xmin)))
#   
# 
#   
# # Plot
# library(ggplot2)
# 
#   
# # Conditional plot
# 
# if(nrow(df_vid) >= 1 & nrow(df_cb) >= 1 &  nrow(df_int) >= 1){
#   ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
#     geom_line(data = trial_sb, aes(x = time, y = class)) + 
#     theme_classic() +
#     scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
#     geom_rect(data=df_cb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.2, inherit.aes = FALSE) +
#     geom_rect(data=df_int, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="tomato", alpha=0.2, inherit.aes = FALSE) +
#     geom_rect(data=df_vid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
#   
# } else {
#   if (nrow(df_vid) >= 1 & nrow(df_cb)) {
#     ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
#       geom_line(data = trial_sb, aes(x = time, y = class)) + 
#       theme_classic() +
#       scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
#       geom_rect(data=df_cb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.2, inherit.aes = FALSE) +
#       geom_rect(data=df_vid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
#     
#   } else {
#     if (nrow(df_vid) >= 1 & nrow(df_int)) {
#       ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
#         geom_line(data = trial_sb, aes(x = time, y = class)) + 
#         theme_classic() +
#         scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
#         geom_rect(data=df_int, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.2, inherit.aes = FALSE) +
#         geom_rect(data=df_vid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
#       
#     }else{
#       if (nrow(df_cb) >= 1 & nrow(df_int)) {
#         ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
#           geom_line(data = trial_sb, aes(x = time, y = class)) + 
#           theme_classic() +
#           scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
#           geom_rect(data=df_int, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.2, inherit.aes = FALSE) +
#           geom_rect(data=df_cb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
#         
#       } else{
#         if (nrow(df_cb) >= 1) {
#           ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
#             geom_line(data = trial_sb, aes(x = time, y = class)) + 
#             theme_classic() +
#             scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
#             geom_rect(data=df_cb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
#           
#           
#         } else {
#           if (nrow(df_int) >= 1){
#             ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
#               geom_line(data = trial_sb, aes(x = time, y = class)) + 
#               theme_classic() +
#               scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
#               geom_rect(data=df_int, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
#             
#           } else {
#             if (nrow(df_vid) >= 1){
#             ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
#               geom_line(data = trial_sb, aes(x = time, y = class)) + 
#               theme_classic() +
#               scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
#               geom_rect(data=df_vid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
#             
#             } else {
#               ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
#                 geom_line(data = trial_sb, aes(x = time, y = class)) + 
#                 theme_classic() +
#                 scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) 
#             }
#         }
#     }
# }
#     }
#   }
# }
#   
#   
##########################################################################  
### Draft ### Draft ### Draft ### Draft ### Draft ### Draft ### Draft ### 
##########################################################################  
  
  # ggplot() + geom_point(data = trial_sb, aes(x = time, y = class)) + 
  #   geom_line(data = trial_sb, aes(x = time, y = class)) + 
  #   theme_classic() +
  #   scale_y_continuous(name = NULL, breaks = c(1,2), labels = c("call", "wag")) +
  #   geom_rect(data=df_cb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.2, inherit.aes = FALSE) +
  #   geom_rect(data=df_int, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="tomato", alpha=0.2, inherit.aes = FALSE)
  #   geom_rect(data=df_vid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.7, inherit.aes = FALSE)
  
  