install.packages("ggplot2")
library("ggplot2")
library("ggimage")

# create a function that generates a data frame based on trial info
trial_info_to_df <-function(quantityLeft, quantityRight, colorLeft, colorRight, shapeLeft, shapeRight, trialType, count) {
  
  if (trialType == "nhBoth"){
    homoLeft <- "nh"
    homoRight <- "nh"
  } else if (trialType == "nhLeft") {
    homoLeft <- "nh"
    homoRight <- "h"
  } else if (trialType == "nhRight") {
    homoLeft <- "h"
    homoRight <- "nh"
  } else {
    homoLeft <- "h"
    homoRight <- "h"
  }
  
  temp_image_name <- paste(count, quantityLeft, colorLeft, shapeLeft, homoLeft, quantityRight,
                           colorRight, shapeRight, homoRight, sep="_")
  Image_Name <- paste(temp_image_name, ".png", sep="")
  
  numberLeft <- as.integer(quantityLeft)
  numberRight <- as.integer(quantityRight)
  
  colorLeft <-  rep(colorLeft, numberLeft)
  colorRight <- rep(colorRight, numberRight)
  Color <- append(colorLeft, colorRight)
  
  
  if (trialType == "nhBoth") {
    shape_left <- append(rep(shapeLeft, numberLeft - 1), shapeRight) 
    shape_right <- append(rep(shapeRight, numberRight - 1), shapeLeft)
    Shape <- append(shape_left, shape_right)
  } else if (trialType == "nhLeft") {
    shape_left <- append(rep(shapeLeft, numberLeft - 1), shapeRight) 
    shape_right <- rep(shapeRight, numberRight)
    Shape <- append(shape_left, shape_right)
  } else if (trialType == "nhRight") {
    shape_right <- append(rep(shapeRight, numberRight - 1), shapeLeft) 
    shape_left <- rep(shapeLeft, numberLeft)
    Shape <- append(shape_left, shape_right)
  } else {
    shape_left <- rep(shapeLeft, numberLeft)
    shape_right <- rep(shapeRight, numberRight)
    Shape <- append(shape_left, shape_right)
  }
  
  sum <- numberLeft + numberRight
  
  xZone <-append(rep("xZone1",numberLeft),rep("xZone2",numberRight))
  xZone1 <- 288:1264
  left_x_cords <- sample(xZone1,numberLeft,replace=FALSE)
  
  xZone2 <- 776:1632
  right_x_cords <- sample(xZone2,numberRight,replace=FALSE)
  
  X <- append(left_x_cords, right_x_cords)
  
  yZone <- 162:918
  left_y_cords <- sample(yZone,numberLeft,replace=FALSE)
  right_y_cords <- sample(yZone,numberRight,replace=FALSE)
  Y <- append(left_y_cords, right_y_cords)
  
  #check if shapes are colliding
  for (i in 1:sum) {
    goodPosition <- FALSE
    while (goodPosition == FALSE){
      #generate new positions
      if (xZone[i] == "xZone1") {
        x_new <-sample(xZone1,1,replace=FALSE)
      } else {
        x_new <-sample(xZone2,1,replace=FALSE)
      }
      y_new <- sample(yZone,1,replace=FALSE)
      
      nonOverlapCounter <- rep(NA,i-1)
      for (oldObjectNumber in 1:(i-1)){ 
        ifelse(sqrt((x_new - X[oldObjectNumber])^2 + (y_new - Y[oldObjectNumber])^2) > 
                 (100),nonOverlapCounter[oldObjectNumber] <- TRUE,
               nonOverlapCounter[oldObjectNumber] <- FALSE)
      } 
      
      ifelse(sum(nonOverlapCounter) == (i-1),goodPosition <- TRUE,goodPosition <- FALSE)
    } #end while (goodPosition == FALSE)
    X[i] <- x_new; Y[i] <- y_new;
    
    nonOverlapCounter <- rep(NA,i-1)
    
  }
  
  #find the distance from center for each element
  x_left_coords <- X[1:15]
  left_centroid_X <- mean(x_left_coords)
  y_left_coords <- Y[1:15]
  left_centroid_Y <- mean(y_left_coords)
  
  x_right_coords <- X[16:30]
  right_centroid_X <- mean(x_right_coords)
  y_right_coords <- Y[16:30]
  right_centroid_Y <- mean(y_right_coords)
  
  left_distances <- c()
  
  for (i in 1:15) {
    temp_distance <- sqrt((x_left_coords[i] - left_centroid_X)^2 + 
                            (y_left_coords[i] - left_centroid_Y)^2)
    left_distances <- append(left_distances, temp_distance)
  }
  
  right_distances <- c()
  for (i in 1:15) {
    temp_distance <- sqrt((x_right_coords[i] - right_centroid_X)^2 + 
                            (y_right_coords[i] - right_centroid_Y)^2)
    right_distances <- append(right_distances, temp_distance)
  }
  
  Distance_From_Center <- append(left_distances, right_distances)
  
  df <- data.frame(Image_Name, Color, Shape, X, Y, Distance_From_Center)
  
  return(df)
}





trials <- data.frame(quantityLeft = rep(15, 64),
                     quantityRight = rep(15,64),
                     colorLeft = rep(c("blue", "red", "purple", "white"), times = 16),
                     colorRight = rep(c("red", "blue", "white", "purple"), times = 16),
                     shapeLeft = rep(c("T", "0", "Horizontal", "Vertical"), each = 16),
                     shapeRight = rep(c("0", "T", "Vertical", "Horizontal"), each = 16),
                     trialType = rep(rep(c("nhLeft", "nhRight", "nhBoth", "hBoth"), each = 4), times = 2)
)

write.csv(trials, "trialsDF.csv", row.names=TRUE) 


#loop through trials DF
for (i in 1:nrow(trials)){
  current_trial <- trials[i,]
  trial_info <- as.character(current_trial)
  count <- paste("image", as.character(i), sep="")
  temp_df <- trial_info_to_df(trial_info[1],trial_info[2],trial_info[3],trial_info[4], 
                              trial_info[5], trial_info[6], trial_info[7], count)
  
  #save image info csv
  #write.csv(temp_df, paste(file= temp_df[1,1],".csv", sep=""), row.names=TRUE)
  write.csv(temp_df, file=file.path('display-metadata', paste(file= temp_df[1,1],".csv", sep="")), 
            row.names=TRUE)
  
  #save plot png
  #if T 0, do it one way (& get colors), otherwise other way 
  colorLeft <- trial_info[3]
  colorRight <- trial_info[4]
  
  
  if (trial_info[5] ==  "T" | trial_info[5] =="0"){
    if (trial_info[5] == "T"){
      if (trial_info[7] == "nhLeft"){
        data_subset1 <- temp_df[c(1:14),c(1:5)]
        data_subset1Different <-temp_df[c(15),c(1:5)]
        data_subset2 <- temp_df[c(16:30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "T"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset1Different, aes(x = X, y = Y,
                                                           label = "0"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                  label = "0"), color=colorRight, stat = "unique", size=8) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
        
      } else if (trial_info[7] == "nhRight") {
        data_subset1 <- temp_df[c(1:15),c(1:5)]
        data_subset2 <-temp_df[c(16:29),c(1:5)]
        data_subset2Different <- temp_df[c(30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "T"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                  label = "0"), color=colorRight, stat = "unique", size=8) +
                 geom_text(data=data_subset2Different, aes(x = X, y = Y,
                                                           label = "T"), color=colorRight, stat = "unique", size=8) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
        
      } else if (trial_info[7] == "nhBoth") {
        data_subset1 <- temp_df[c(1:14),c(1:5)]
        data_subset1Different <- temp_df[c(15),c(1:5)]
        data_subset2 <-temp_df[c(16:29),c(1:5)]
        data_subset2Different <- temp_df[c(30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "T"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset1Different, aes(x = X, y = Y,
                                                           label = "0"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                  label = "0"), color=colorRight, stat = "unique", size=8) +
                 geom_text(data=data_subset2Different, aes(x = X, y = Y,
                                                           label = "T"), color=colorRight, stat = "unique", size=8) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
        
      } else {
        data_subset1 <- temp_df[c(1:15),c(1:5)]
        data_subset2 <-temp_df[c(16:30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "T"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                  label = "0"), color=colorRight, stat = "unique", size=8) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
      }
    } else {
      # T on right 
      if (trial_info[7] == "nhLeft"){
        data_subset1 <- temp_df[c(1:14),c(1:5)]
        data_subset1Different <-temp_df[c(15),c(1:5)]
        data_subset2 <- temp_df[c(16:30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "0"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset1Different, aes(x = X, y = Y,
                                                           label = "T"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                  label = "T"), color=colorRight, stat = "unique", size=8) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
      } else if (trial_info[7] == "nhRight") {
        data_subset1 <- temp_df[c(1:15),c(1:5)]
        data_subset2 <-temp_df[c(16:29),c(1:5)]
        data_subset2Different <- temp_df[c(30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "0"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                  label = "T"), color=colorRight, stat = "unique", size=8) +
                 geom_text(data=data_subset2Different, aes(x = X, y = Y,
                                                           label = "0"), color=colorRight, stat = "unique", size=8) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
      } else if (trial_info[7] == "nhBoth") {
        data_subset1 <- temp_df[c(1:14),c(1:5)]
        data_subset1Different <- temp_df[c(15),c(1:5)]
        data_subset2 <-temp_df[c(16:29),c(1:5)]
        data_subset2Different <- temp_df[c(30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "0"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset1Different, aes(x = X, y = Y,
                                                           label = "T"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                  label = "T"), color=colorRight, stat = "unique", size=8) +
                 geom_text(data=data_subset2Different, aes(x = X, y = Y,
                                                           label = "0"), color=colorRight, stat = "unique", size=8) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
        
      } else {
        data_subset1 <- temp_df[c(1:15),c(1:5)]
        data_subset2 <-temp_df[c(16:30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "0"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                  label = "T"), color=colorRight, stat = "unique", size=8) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
      }
    }
  } else {
    # horizontal and vertical bars 
    # vertical on left 
    if (trial_info[5] == "Vertical"){
      if (trial_info[7] == "nhLeft"){
        data_subset1 <- temp_df[c(1:14),c(1:5)]
        data_subset1Different <-temp_df[c(15),c(1:5)]
        data_subset2 <- temp_df[c(16:30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "l"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset1Different, aes(x = X, y = Y,
                                                           label = "l"), color=colorLeft, stat = "unique", size=8, angle=90) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                  label = "l"), color=colorRight, stat = "unique", size=8, angle=90) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
        
      } else if (trial_info[7] == "nhRight") {
        data_subset1 <- temp_df[c(1:15),c(1:5)]
        data_subset2 <-temp_df[c(16:29),c(1:5)]
        data_subset2Different <- temp_df[c(30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "l"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                  label = "l"), color=colorRight, stat = "unique", size=8, angle=90) +
                 geom_text(data=data_subset2Different, aes(x = X, y = Y,
                                                           label = "l"), color=colorRight, stat = "unique", size=8) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
        
      } else if (trial_info[7] == "nhBoth") {
        data_subset1 <- temp_df[c(1:14),c(1:5)]
        data_subset1Different <- temp_df[c(15),c(1:5)]
        data_subset2 <-temp_df[c(16:29),c(1:5)]
        data_subset2Different <- temp_df[c(30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "l"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset1Different, aes(x = X, y = Y,
                                                           label = "l"), color=colorLeft, stat = "unique", size=8, angle=90) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                  label = "l"), color=colorRight, stat = "unique", size=8, angle=90) +
                 geom_text(data=data_subset2Different, aes(x = X, y = Y,
                                                           label = "l"), color=colorRight, stat = "unique", size=8) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
        
      } else {
        data_subset1 <- temp_df[c(1:15),c(1:5)]
        data_subset2 <-temp_df[c(16:30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "l"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                  label = "l"), color=colorRight, stat = "unique", size=8, angle=90) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
      }
    } else {
      #horizontal on left
      if (trial_info[7] == "nhLeft"){
        data_subset1 <- temp_df[c(1:14),c(1:5)]
        data_subset1Different <-temp_df[c(15),c(1:5)]
        data_subset2 <- temp_df[c(16:30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "l"), color=colorLeft, stat = "unique", size=8, angle=90) +
                 geom_text(data=data_subset1Different, aes(x = X, y = Y,
                                                           label = "l"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                  label = "l"), color=colorRight, stat = "unique", size=8) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
      } else if (trial_info[7] == "nhRight") {
        data_subset1 <- temp_df[c(1:15),c(1:5)]
        data_subset2 <-temp_df[c(16:29),c(1:5)]
        data_subset2Different <- temp_df[c(30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "l"), color=colorLeft, stat = "unique", size=8, angle=90) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                  label = "l"), color=colorRight, stat = "unique", size=8) +
                 geom_text(data=data_subset2Different, aes(x = X, y = Y,
                                                           label = "l"), color=colorRight, stat = "unique", size=8, angle=90) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
      } else if (trial_info[7] == "nhBoth") {
        data_subset1 <- temp_df[c(1:14),c(1:5)]
        data_subset1Different <- temp_df[c(15),c(1:5)]
        data_subset2 <-temp_df[c(16:29),c(1:5)]
        data_subset2Different <- temp_df[c(30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "l"), color=colorLeft, stat = "unique", size=8, angle=90) +
                 geom_text(data=data_subset1Different, aes(x = X, y = Y,
                                                           label = "l"), color=colorLeft, stat = "unique", size=8) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                  label = "l"), color=colorRight, stat = "unique", size=8) +
                 geom_text(data=data_subset2Different, aes(x = X, y = Y,
                                                           label = "l"), color=colorRight, stat = "unique", size=8, angle=90) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
        
      } else {
        data_subset1 <- temp_df[c(1:15),c(1:5)]
        data_subset2 <-temp_df[c(16:30),c(1:5)]
        
        ggsave(filename = file.path('display-images',temp_df[1,1]), ggplot() + geom_text(data=data_subset1, aes(x = X, y = Y,
                                                                         label = "l"), color=colorLeft, stat = "unique", size=8, angle=90) +
                 geom_text(data=data_subset2, aes(x = X, y = Y,
                                                   label = "l"), color=colorRight, stat = "unique", size=8) +
                 xlim(0,1920) +ylim(0,1080) +
                 
                 theme(axis.line=element_blank(),axis.text.x=element_blank(),
                       axis.text.y=element_blank(),axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),legend.position="none",
                       panel.background=element_rect(fill = 'black', color ='black'),panel.border=element_blank(),panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),plot.background=element_blank()), width=1920, height=1080, units="px")
      }
    }
    
  }
  
}
