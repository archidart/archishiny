# Copyright © 2017, Université catholique de Louvain
# All rights reserved.
# 
# Copyright © 2017 Forschungszentrum Jülich GmbH
# All rights reserved.
# 
# Developers: Guillaume Lobet
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v3 and provided that the following conditions are met:
#   
# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
# 
# Disclaimer
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# You should have received the GNU GENERAL PUBLIC LICENSE v3 with this file in license.txt but can also be found at http://www.gnu.org/licenses/gpl-3.0.en.html
# 
# NOTE: The GPL.v3 license requires that all derivative work is distributed under the same license. That means that if you use this source code in any other program, you can only distribute that program with the full source code included and licensed under a GPL license.


library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.css",
                  
  # Application title
  navbarPage("archiDART",
    tabPanel("Load data", id="tab1", icon = icon("upload"),
      fluidRow(
        column(4,
               h4("archiDART"),
               helpText("This app show the capabilities of the archiDART package. To do so, we create 70 synthetic root system using the root model ArchiSimple [Pagès et al. 2014]."),
               helpText("The root architectures are classified into different genotypes (mock, dense and sparse, steep and shallow, slow and fast), to show how archiDART can differentiate them."),
               #  textInput('path', 'Choose folder with all data files'),
               #  # fileInput('test_file', 'Choose file with testing data', accept=c('text/comma-separated-values', '.csv')),
               # checkboxInput('use_example', "Use example data", value = T, width = NULL),
               # bsButton(inputId = "load_data", type = "action", style="primary", label="Load data",icon("upload")),
               tags$hr(),
               img(src='logo.jpg', align = "left", width="100%")
        ),
        column(7, 
               
                 h4("Overview of the dataset"),
                 # column(6, checkboxInput('show_load_code', "Show me the code to load the data", value = F, width = NULL))
                 # column(6,  actionButton("load_code", "Show modal dialog"))
                 actionButton("load_code", label="Show me the code", icon=icon("eye"),style='padding:4px; font-size:80%'),
                
               # conditionalPanel(
               #   condition = "input.show_load_code == true",
               #   verbatimTextOutput("load_code")
               # ),
               # tags$hr(),
               # selectInput("to_plot_1", label = "Variable to plot", choices = c("Load datafile")),
               # plotOutput("distribution_plot"),
               # tags$hr(),
               # helpText("Distribution of the data computed by architect."),
               tags$hr(),
               DT::dataTableOutput('distribution_data')
        )
      )
    ),
    tabPanel("archiTect", id="tab2", icon = icon('sliders'),
        fluidRow(
          column(3, 
              h4("archiTect"),
              helpText("The architect function load the data from the different RSML files, store them into a unique data table, then computes aggregated metrics for each root system."),
              selectInput("genotypes_to_plot", label="Genotypes to plot", choices = c("Load datafile"), 
                          selected = NULL, multiple = TRUE, width="100%"),
              checkboxInput('plot_mean', "Plot average by genotype", value = T, width = NULL),
              tags$hr(),
              img(src='logo.jpg', align = "left", width="80%")
          ),
          column(8,
                 fluidRow(
                   column(6, h4("Evolution of root system metrics")),
                   column(6, checkboxInput('show_time_code', "Show me the code for this awesome plot", value = F, width = NULL))
                  ),
                 conditionalPanel(
                   condition = "input.show_time_code == true",
                   verbatimTextOutput("time_code")
                  ),
                 selectInput("to_plot", label = "Variable to plot", choices = c("Load datafile")),
                 plotOutput("time_plot")
          )
        )
    ),
    tabPanel("archiDraw", id="tab3",icon = icon("pencil"),
       fluidRow(
         column(3, 
                helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
                selectInput("genotypes_to_plot_1", label="Genotypes to plot", choices = c("Load datafile"), 
                            selected = NULL, multiple = TRUE, width="100%"),
                sliderInput("reps_to_plot", "Number of repetitions to plot", min = 1, max=10, step = 1, value = 3),
                sliderInput("ncol", "Number of columns", min = 1, max=10, step = 1, value = 3),
                sliderInput("linesize", "Line width", min = 0.5, max=1.5, step = 0.25, value = 0.5),
                checkboxInput('plot_mean_archi', "Plot average architecture by genotype", value = T, width = NULL),
                tags$hr(),
                img(src='logo.jpg', align = "left", width="100%")
         ),
         column(8,
                fluidRow(
                  column(6, h4("Plot the architecture")),
                  column(6, checkboxInput('show_archi_code', "Show me the code for this awesome plot", value = F, width = NULL))
                ),         
                uiOutput("clip_archi_code"),
                conditionalPanel(
                  condition = "input.show_archi_code == true",
                  verbatimTextOutput("archi_code")
                ),
                selectInput("to_plot_2", label = "Variable to plot", choices = c("diameter" = "diameter1", "order"="order", "depth" = "y1", "geodesic distance"="geodesic")),
                plotOutput("archi_plot", height = 1000)
          )
       )
    ),
    tabPanel("archiHomology", id="tab4",icon = icon("barcode"),
             fluidRow(
               column(3, 
                      helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
                      selectInput("genotypes_to_plot_2", label="Genotypes to plot", choices = c("Load datafile"), 
                                  selected = NULL, multiple = TRUE, width="100%"),
                      sliderInput("reps_to_plot_2", "Number of repetitions to plot", min = 1, max=10, step = 1, value = 3),
                      tags$hr(),
                      img(src='logo.jpg', align = "left", width="100%")
               ),
               column(5,
                      fluidRow(
                        column(6, h4("Plot the barcodes")),
                        column(6, checkboxInput('show_barcode_code', "Show me the code for this awesome plot", value = F, width = NULL))
                      ),                
                      conditionalPanel(
                        condition = "input.show_barcode_code == true",
                        verbatimTextOutput("barcode_code")
                      ),
                      selectInput("to_plot_3", label = "Variable to plot", choices = c("depth" = "depth", "geodesic distance"="geodesic")),
                      plotOutput("barcode_plot", height = 700)
               ),
              column(3, 
                      h4("Boxplot the barcodes"),
                      selectInput("to_plot_4", label = "Variable to plot", choices = c("Load datafile")),
                      plotOutput("barcode_boxplot", height = 300),
                     actionButton("boxcode_code", label="Show me the code", icon=icon("eye"),style='padding:4px; font-size:80%'),
                     actionButton("boxcode_code_downlaod", label="Download plot", icon=icon("download"),style='padding:4px; font-size:80%'),
                     tags$hr(),
                     
                     fluidRow(
                       column(6, h4("PCA the barcodes")),
                       column(6, checkboxInput('show_barcode_PCA_code', "Show me the code for this awesome plot", value = F, width = NULL))
                     ),                
                     conditionalPanel(
                       condition = "input.show_barcode_PCA_code == true",
                       verbatimTextOutput("boxcode_PCA_code")
                     ),                         
                     plotOutput("barcode_PCA", height = 300) 
               )
             )
    ),    
    tabPanel("archiPCA", id="tab3",icon = icon("bullseye"),
      fluidRow(
      column(3, 
             helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
             selectInput("variable_to_pca", label="Variables to include in PCA", choices = c("Load datafile"), 
                         selected = NULL, multiple = TRUE, width="100%"),
             selectInput("genotypes_to_plot_3", label="Genotypes to plot", choices = c("Load datafile"), 
                         selected = NULL, multiple = TRUE, width="100%"),
             sliderInput("time_to_plot", "Time point to analyse", min = 1, max=10, step = 1, value = 10),
             tags$hr(),
             img(src='logo.jpg', align = "left", width="100%")
      ),
      column(7,
             fluidRow(
               column(6, h4("Plot the principal component analysis")),
               column(6, checkboxInput('show_pca_code', "Show me the code for this awesome plot", value = F, width = NULL))
             ),                
             conditionalPanel(
               condition = "input.show_pca_code == true",
               verbatimTextOutput("pca_code")
             ),
             plotOutput("pca_plot", height = 800)
      )
    )      
    ), 
    tabPanel("archiGrow", id="tab3",icon = icon("hourglass-half")
    ), 
    tabPanel("About", id="tab4", icon=icon("plus-circle"),
      fluidRow(
        column(3),
        column(6,
            h4("What is archiDART"),
            helpText("TODO: \n- better name for variables \n-load data only once and store eveyrthing in dataframe \n- plot woith ggplot"),
            tags$hr(),
            h4("How to use archiDART"),
            helpText(""),
            actionButton(inputId='ab1', label="archiDART webpage", icon = icon("cogs"), onclick ="window.open('https://cran.r-project.org/web/packages/archiDART/index.html', '_blank')"),
            tags$hr(),
            h4("How to cite archiDART"),
            tags$strong("archiDART: an R package for the automated computation of plant root architectural traits"),
            helpText("Delory BM, Baudson C, Brostaux Y, Lobet G, du Jardin P, Pagès L, Delaplace P"),
            actionButton(inputId='ab1', label="View paper", icon = icon("flask"), onclick ="window.open('http://link.springer.com/10.1007/s11104-015-2673-4', '_blank')"),                                              
            tags$hr(),
            h4("Licence"),
            helpText("")                    
          )
      )
    )
  )
))
