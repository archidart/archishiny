
#' Import a single RSML file into a table object
#' @param rsml.path    The path to the .rsml file. Mandatory.
#' @param threed  Does the RSML file contains a 3D root system?
#' @keywords rsml
#' @keywords threed is the plant in 3d
#' @import XML 
#' @export
#' @examples
#' # 2D example
#' path <- "http://rootsystemml.github.io/images/examples/arabidopsis-simple.rsml"
#' pl <- rsmlToTable(path) # import the file as a 2D plant
#' 
#' # 3D example
#' path <- "http://rootsystemml.github.io/images/examples/anagallis.rsml"
#' pl <- rsmlToTable(path, threed=TRUE) # import the file as a 2D plant

rsmlToTable <- function(rsml.path, threed = FALSE){
  
  # Create the plant object
  # pl <- plant()
  rsml <- xmlToList(xmlParse(rsml.path))
  
  # rsml <- read_xml(rsml.path)
  
  plant.name <- strsplit(rsml.path, "/")[[1]]
  plant.name <- gsub(".rsml", "", plant.name[[length(plant.name)]])
  
  # Get the scale in pixels / cm
  meta <- rsml$meta
  scale <- 1
  if(!is.null(meta$unit) & !is.null(meta$resolution)){
    if(grepl("inch", meta$unit)) scale <- as.numeric(meta$resolution) / 2.54 
    if(grepl("cm", meta$unit)) scale <- as.numeric(meta$resolution)
    if(grepl("mm", meta$unit)) scale <- as.numeric(meta$resolution) * 10
  }
  
  plants <- rsml$scene
  inc  = 1
  
  pl <- NULL
  template <- data.frame(plant=character(1), root=character(1), order=numeric(1), parent=character(1),
                     x1=numeric(1), y1 = numeric(1), z1 = numeric(1), x2 = numeric(1), y2 = numeric(1), z2 = numeric(1),
                     diameter = numeric(1), orientation = numeric(1), bLength = numeric(1), length = numeric(1), age = numeric(1), 
                     stringsAsFactors = F)
  
  for(r0 in plants){ # Each plant
    
    
    for(r1 in r0){ # Each first order root
      if(class(r1) == "list"){
        
        root <- NULL
        
        # Create an empty root
        id1 <- tryCatch({r1$.attrs[["ID"]]},
                        error=function(cond) {r1$.attrs[["id"]]},
                        finally={}
        )   
        # r <- root(id = id1)

        
        # Add the nodes
        ns <- r1$geometry$polyline
        cumulDist <- 0
        for(i in 2:length(ns)){
          # Get the distance of the node from the base of the root
          if(i == 1){ dist <- 0
          }else{
            dx <- as.numeric(ns[[i]][[1]]) - as.numeric(ns[[i-1]][[1]]) # x
            dy <- as.numeric(ns[[i]][[2]]) - as.numeric(ns[[i-1]][[2]]) # y
            if(threed){
              dz <- as.numeric(ns[[i]][[3]]) - as.numeric(ns[[i-1]][[3]]) # z
              dist = sqrt(dx^2 + dy^2 + dz^2) / scale
            }else{
              dist = sqrt(dx^2 + dy^2) / scale
            }
          }
          cumulDist = cumulDist + dist
          
          temp <- template
          temp$plant <- plant.name
          
          temp$root <- id1
          temp$order <- 0
          temp$parent <- NA
          
          temp$length <- dist
          temp$x1 <- as.numeric(ns[[i]][[1]]) / scale
          temp$y1 <- as.numeric(ns[[i]][[2]]) / scale
          temp$x2 <- as.numeric(ns[[i-1]][[1]]) / scale
          temp$y2 <- as.numeric(ns[[i-1]][[2]]) / scale
          if(threed) {
            temp$z1 <- as.numeric(ns[[i]][[3]]) / scale
            temp$z2 <- as.numeric(ns[[i-1]][[3]]) / scale
          } 
          temp$bLength <- cumulDist
          
          root <- rbind(root, temp)
        }  
        # Add the functions
        if("functions" %in% names(r1)){
          for(f in r1$functions){
            # Diameter
            if(grepl("diam", f$.attr[1])){
              for(i in 1:(length(f)-2)){
                root$diameter[i] = as.numeric(f[[i]]) / scale
              }
            }
            # Orientation
            if(grepl("orien", f$.attr[1])){
              for(i in 1:(length(f)-2)){
                root$orientation[i] = f[[i]]
              }
            }
            # Age
            if(grepl("age", f$.attr[1])){
              for(i in 1:(length(f)-2)){
                root$age[i] = f[[i]]
              }
            }            
          }
        }
        
        pl <- rbind(pl, root)
        
        # If there is lateral roots
        if("root" %in% names(r1)){
          for(r2 in r1){  
            if("geometry" %in% names(r2)){
              # Create an empty root
              id2 <- tryCatch({r2$.attrs[["ID"]]},
                              error=function(cond) {r2$.attrs[["id"]]},
                              finally={}
              )   
              
              root <- NULL
              
              # Add the nodes
              ns <- r2$geometry$polyline
              cumulDist <- 0
              for(i in 2:length(ns)){
                # Get the distance of the node from the base of the root
                if(i == 1){ dist <- 0
                }else{
                  dx <- as.numeric(ns[[i]][[1]]) - as.numeric(ns[[i-1]][[1]]) # x
                  dy <- as.numeric(ns[[i]][[2]]) - as.numeric(ns[[i-1]][[2]]) # y
                  if(threed){
                    dz <- as.numeric(ns[[i]][[3]]) - as.numeric(ns[[i-1]][[3]]) # z
                    dist = sqrt(dx^2 + dy^2 + dz^2) / scale
                  }else{
                    dist = sqrt(dx^2 + dy^2) / scale
                  }
                }
                cumulDist = cumulDist + dist   
                
                temp <- template
                temp$plant <- plant.name
                temp$root <- id2
                temp$order <- 1
                temp$parent <- id1
                temp$length <- dist
                temp$x1 <- as.numeric(ns[[i]][[1]]) / scale
                temp$y1 <- as.numeric(ns[[i]][[2]]) / scale
                temp$x2 <- as.numeric(ns[[i-1]][[1]]) / scale
                temp$y2 <- as.numeric(ns[[i-1]][[2]]) / scale
                if(threed) {
                  temp$z1 <- as.numeric(ns[[i]][[3]]) / scale
                  temp$z2 <- as.numeric(ns[[i-1]][[3]]) / scale
                } 
                temp$bLength <- cumulDist
                
                root <- rbind(root, temp)
              }
              if("functions" %in% names(r2)){
                for(f in r2$functions){
                  if(grepl("diam", f$.attr[1])){
                    for(i in 1:(length(f)-2)){
                      root$diameter[i] = as.numeric(f[[i]]) / scale
                    }
                  }
                  # Get the orientation and the insertion angel
                  if(grepl("orien", f$.attr[1])){
                    for(i in 1:(length(f)-2)){
                      root$orientation[i] = f[[i]]
                    }
                    # rr$insertion_angle <- getInsertionAngle(r, rr)                              
                  }
                  # Age
                  if(grepl("age", f$.attr[1])){
                    for(i in 1:(length(f)-2)){
                      root$age[i] = f[[i]]
                    }
                  }                   
                }
              }
              
              pl <- rbind(pl, root)
              
              # If their is third order roots
              if("root" %in% names(r2)){
                for(r3 in r2){  
                  
                  root <- NULL
                  
                  if("geometry" %in% names(r3)){
                    # Create an empty root
                    id3 <- tryCatch({r3$.attrs[["ID"]]},
                                    error=function(cond) {r3$.attrs[["id"]]},
                                    finally={}
                    )   
                    
                    # Add the nodes
                    ns <- r3$geometry$polyline
                    cumulDist <- 0
                    for(i in 1:length(ns)){
                      # Get the distance of the node from the base of the root
                      if(i == 1){ dist <- 0
                      }else{
                        dx <- as.numeric(ns[[i]][[1]]) - as.numeric(ns[[i-1]][[1]]) # x
                        dy <- as.numeric(ns[[i]][[2]]) - as.numeric(ns[[i-1]][[2]]) # y
                        if(threed){
                          dz <- as.numeric(ns[[i]][[3]]) - as.numeric(ns[[i-1]][[3]]) # z
                          dist = sqrt(dx^2 + dy^2 + dz^2) / scale
                        }else{
                          dist = sqrt(dx^2 + dy^2) / scale
                        }
                      }
                      cumulDist = cumulDist + dist  
                      
                      temp <- template
                      temp$plant <- plant.name
                      temp$root <- id3
                      temp$order <- 2
                      temp$parent <- id2 
                      temp$length <- dist
                      temp$x1 <- as.numeric(ns[[i]][[1]]) / scale
                      temp$y1 <- as.numeric(ns[[i]][[3]]) / scale
                      temp$x2 <- as.numeric(ns[[i-1]][[1]]) / scale
                      temp$y2 <- as.numeric(ns[[i-1]][[2]]) / scale
                      if(threed) {
                        temp$z1 <- as.numeric(ns[[i]][[3]]) / scale
                        temp$z2 <- as.numeric(ns[[i-1]][[3]]) / scale
                      } 
                      temp$bLength <- cumulDist
                      
                      root <- rbind(root, temp)
                    }
                    
                    # Get the diameter an orientation, if
                    if("functions" %in% names(r3)){
                      for(f in r3$functions){
                        if(grepl("diam", f$.attr[1])){
                          for(i in 1:(length(f)-2)){
                            root$diameter[i] = as.numeric(f[[i]]) / scale
                          }
                        }
                        # Get the orientation and the orientation ange
                        if(grepl("orien", f$.attr[1])){
                          for(i in 1:(length(f)-2)){
                            root$orientation[i] = f[[i]]
                          }
                          # rrr$insertion_angle <- getInsertionAngle(rr, rrr)                                    
                        } 
                        # Age
                        if(grepl("age", f$.attr[1])){
                          for(i in 1:(length(f)-2)){
                            root$age[i] = f[[i]]
                          }
                        }                         
                      }
                    }
                    
                    pl <- rbind(pl, root)
                    # Get the insertion position
                    # rrr$insertion <- getInsertionPosition(rr, rrr)
                    # rr <- addChildToRoot(rr, rrr)
                  }
                }
              }
              # Get the insertion position
              # rr$insertion <- getInsertionPosition(r, rr)
              # r <- addChildToRoot(r, rr)
            }
          }
        }
        # pl <- addRootToPlant(pl, r)
      }
    }
  }
  pl
}




