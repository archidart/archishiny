
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(
  function(input, output, clientData, session) {  
    
    rs <- reactiveValues(architect = NULL,
                         genotypes = NULL,
                         archi = NULL)
    
    
  # Load the dataset
  observeEvent(input$load_data, {
    
    
    if(input$use_example){
      load("www/architect.RData")
      load("www/archi.RData")
      load("www/genotypes.RData")
      rs$architect <- architect
      rs$genotypes <- genotypes
      rs$archi <- archi
    }else{
      path <- input$path
      # path <- "../rsmls/"
      withProgress(message = 'Architect working on blueprints', {
        
        architect <- architect(inputrsml=path, rsml.connect=F, rsml.date="age")
        
        genotypes <- unlist(lapply(strsplit(as.character(architect$FileName), "-"), `[[`, 1))[]
        architect$genotype <- genotypes
      
        # save(architect, file="www/architect.RData")
        # save(genotypes, file="www/genotypes.RData")
      })
      
      withProgress(message = 'Architect building the plots', {
        filenames.rsml<-list.files(path=path, pattern="\\.rsml$")
        archi <- NULL
        for(f in filenames.rsml){
          archi <- rbind(archi, rsmlToTable(paste0(path, f)))
          message(paste0(f," done"))
        }
        genotypes <- unlist(lapply(strsplit(as.character(archi$plant), "-"), `[[`, 1))[]
        rep <- unlist(lapply(strsplit(as.character(archi$plant), "-"), `[[`, 3))[]
        archi$genotype <- genotypes
        archi$rep <- rep
        archi$age <- as.numeric(archi$age)    
        # save(archi, file="www/archi.RData")
      })
      rs$architect <- architect
      rs$genotypes <- genotypes
      rs$archi <- archi
    }

  })
  
  
  
  ############################################################
  ### UI commands
  ############################################################
  
  observe({
    if(is.null(rs$architect)){return()}
    vars <- colnames(rs$architect)[-c(1,2)]
    ct_options <- list()
    sel <- input$to_plot_1
    print(sel)
    for(ct in vars) ct_options[[ct]] <- ct
    if(length(sel) == 0 | sel == "") sel = ct_options[1]
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "to_plot_1", choices = ct_options, selected=sel) 
  }) 
  
  observe({
    if(is.null(rs$architect)){return()}
    vars <- colnames(rs$architect)[-c(1,2)]
    ct_options <- list()
    sel <- input$to_plot
    for(ct in vars) ct_options[[ct]] <- ct
    if(length(sel) == 0 | sel == "") sel = ct_options[1]
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "to_plot", choices = ct_options, selected=sel) 
  }) 
  
  
  observe({
    if(is.null(rs$genotypes)){return()}
    vars <- unique(rs$genotypes)
    ct_options <- list()
    sel <- input$genotypes_to_plot
    if(length(sel) == 0) sel = vars
    for(ct in vars) ct_options[[ct]] <- ct
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "genotypes_to_plot", choices = ct_options, selected=sel) 
  }) 
  
  observe({
    if(is.null(rs$genotypes)){return()}
    vars <- unique(rs$genotypes)
    ct_options <- list()
    sel <- input$genotypes_to_plot_1
    if(length(sel) == 0) sel = vars
    for(ct in vars) ct_options[[ct]] <- ct
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "genotypes_to_plot_1", choices = ct_options, selected=sel) 
  }) 
  
  
  ############################################################
  ### PLOTS
  ############################################################
  
  # Variable distribution
  output$distribution_plot <- renderPlot({
    if(is.null(rs$architect)){return()}
    
    temp <- rs$architect
    temp$value <- temp[[input$to_plot_1]]
    
    pl <- ggplot(temp, aes(value)) + 
      geom_density(col="gray", fill="gray", alpha=0.5) + 
      ylab("") + 
      xlab(input$to_plot_1) + 
      ggtitle(input$to_plot_1) + 
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), text=element_text(size=15))
    pl
    
  }) 

  
  # Time plot
  
  output$time_plot <- renderPlotly({
    if(is.null(rs$architect)){return()}
    
    temp <- rs$architect[rs$architect$genotype %in% input$genotypes_to_plot,]
    temp$value <- temp[[input$to_plot]]
    
    pl <- ggplot(temp) +  
      xlab("Time [days]") + 
      ylab(input$to_plot) + 
      ggtitle(input$to_plot) + 
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), text=element_text(size=15))
    
    if(input$plot_mean){
      pl <- pl+ stat_smooth(aes(Time, value, colour=genotype)) 
    } else{
      pl <- pl + geom_line(aes(Time, value, colour=genotype, group=FileName))
    }
    
    ggplotly(pl)
    
  }) 
  
  
  
  # archi plot
  output$archi_plot <- renderPlot({
    if(is.null(rs$archi)){return()}
    
    temp <- rs$archi[rs$archi$genotype %in% input$genotypes_to_plot_1,]
    temp <- temp[as.numeric(temp$rep) <= input$reps_to_plot,]
    temp$value <- temp[[input$to_plot_2]]
    
    if(!input$plot_mean_archi){
      pl <- ggplot(temp) + 
          geom_segment(aes(x = x1, y = -y1, xend = x2, yend = -y2, colour=value)) + 
          coord_fixed() + 
          theme_bw() + 
          facet_wrap(~plant, ncol=input$ncol) + 
          scale_colour_gradientn(colours=cscale3, 
                               name = "Water potential",
                               limits = input$psirange)
    }else{
      pl <- ggplot(temp) + 
        geom_segment(aes(x = x1, y = -y1, xend = x2, yend = -y2, colour=value), size=0.5, alpha=0.5) + 
        coord_fixed() + 
        theme_bw() + 
        facet_wrap(~genotype, ncol=input$ncol)
    }
    
    pl
  })
  

  
  
  ############################################################
  ### TABLE
  ############################################################  

  output$distribution_data <- DT::renderDataTable({
    if(is.null(rs$architect)){return()}
    DT::datatable(rs$architect, options = list(scrollX = TRUE, pageLength = 5))
  })
  
  
})
