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
               helpText("This app shows the capabilities of the archiDART package. To do so, we created a library of 70 synthetic root systems using the root architecture model ArchiSimple [Pagès et al. 2014]. The root architectures were classified into seven genotypes (mock, dense, sparse, steep, shallow, slow, and fast). This app will show you how the functions in the archiDART package can be used to differentiate them."),
               tags$hr(),
               img(src='logo.jpg', align = "left", width="100%")
        ),
        column(7, 
               h4("Overview of the dataset"),
               actionButton("load_code", label="Show me the code", icon=icon("eye"),style='padding:4px; font-size:80%'),
               tags$hr(),
               DT::dataTableOutput('distribution_data')
        )
      )
    ),
    tabPanel("archiTect", id="tab2", icon = icon('sliders'),
        fluidRow(
          column(3, 
              h4("archiTect"),
              helpText("The architect function loads the data from the different RSML files and computes aggregated metrics describing the global architecture of each root system at each observation date."),
              selectInput("genotypes_to_plot", label="Genotypes to plot", choices = c("Load datafile"), 
                          selected = NULL, multiple = TRUE, width="100%"),
              checkboxInput('plot_mean', "Plot average by genotype", value = T, width = NULL),
              tags$hr(),
              img(src='logo.jpg', align = "left", width="80%")
          ),
          column(8,
                h4("Evolution of root system metrics"),
                actionButton("time_code", label="Show me the code", icon=icon("eye"),style='padding:4px; font-size:80%'),
                tags$hr(),
                selectInput("to_plot", label = "Variable to plot", choices = c("Load datafile")),
                plotOutput("time_plot")
          )
        )
    ),
    tabPanel("archiDraw", id="tab3",icon = icon("pencil"),
       fluidRow(
         column(3, 
                helpText("The archidraw function loads the data from the different RSML files and plots each root system using the basic graphic functions of R. With archiDART version 3.0, it is now possible to use other data visualization tools, such as the functions of the ggplot2 package."),
                selectInput("genotypes_to_plot_1", label="Genotypes to plot", choices = c("Load datafile"), 
                            selected = NULL, multiple = TRUE, width="100%"),
                sliderInput("reps_to_plot", "Number of repetitions to plot", min = 1, max=10, step = 1, value = 3),
                sliderInput("ncol", "Number of columns", min = 1, max=10, step = 1, value = 3),
                sliderInput("linesize", "Line width", min = 0.5, max=1.5, step = 0.25, value = 0.5),
                checkboxInput('plot_mean_archi', "Plot average architecture by genotype", value = T, width = NULL),
                tags$hr(),
                img(src='logo.jpg', align = "left", width="100%")
         ),
         column(5,
                fluidRow(
                  column(6, h4("Plot the architecture")),
                  column(6, checkboxInput('show_distri', "Hey, show me the histograms instead!", value = F, width = NULL))
                ),
                conditionalPanel(
                  condition = "input.show_distri == false",
                  actionButton("archi_code", label="Show me the code", icon=icon("eye"),style='padding:4px; font-size:80%'),
                  tags$hr(),
                  selectInput("to_plot_2", label = "Variable to plot", choices = c("diameter" = "diameter1", "growth"="growth", "orientation"="orientation","order"="order", "depth" = "y1", "geodesic distance"="geodesic", "magnitude"="magnitude", "path length"="pathlength")),
                  plotOutput("archi_plot", height = 1000)
                ),
                conditionalPanel(
                  condition = "input.show_distri == true",
                  actionButton("distri_code", label="Show me the code", icon=icon("eye"), style='padding:4px; font-size:80%; color-background="#62bfad'),
                  tags$hr(),
                  selectInput("to_plot_distri", label = "Variable to plot", choices = c("diameter" = "diameter", "growth"="growth", "orientation"="angle", "length"="length", "depth"="depth", "geodesic distance"="geodesic", "magnitude"="magnitude", "path length"="pathlength")),
                  plotOutput("distri_plot", height = 600)
                )
          ),
         column(3, 
                h4("Boxplot the architectures"),
                actionButton("boxplot_archi_code", label="Show me the code", icon=icon("eye"), style='padding:4px; font-size:80%; color-background="#62bfad'),
                tags$hr(), 
                selectInput("to_plot_2_bis", label = "Variable to plot", choices = c("Load datafile")),
                plotOutput("archi_boxplot", height = 300)
         )
       )
    ),
    tabPanel("archiHomology", id="tab4",icon = icon("barcode"),
             fluidRow(
               column(3, 
                      helpText("In archiDART version 3.0, a set of new functions were developed to analyse and compare the topology of plant root systems using persistent homology. Here, we want to show you how these functions can be used to compute persistence barcodes and compare the topology of root systems using non-metric multidimensional scaling (NMDS) on a pairwise bottleneck distance matrix."),
                      selectInput("genotypes_to_plot_2", label="Genotypes to plot", choices = c("Load datafile"), 
                                  selected = NULL, multiple = TRUE, width="100%"),
                      sliderInput("reps_to_plot_2", "Number of repetitions to plot", min = 1, max=10, step = 1, value = 3),
                      tags$hr(),
                      img(src='logo.jpg', align = "left", width="100%")
               ),
               column(5,
                      h4("Plot the barcodes"),
                      actionButton("barcode_code", label="Show me the code", icon=icon("eye"), style='padding:4px; font-size:80%; color-background="#62bfad'),
                      tags$hr(),
                      selectInput("to_plot_3", label = "Variable to plot", choices = c("depth" = "depth", "geodesic distance"="geodesic")),
                      plotOutput("barcode_plot", height = 700)
               ),
              column(3, 
                     h4("Boxplot the barcodes"),
                     actionButton("boxcode_code", label="Show me the code", icon=icon("eye"), style='padding:4px; font-size:80%; color-background="#62bfad'),
                     tags$hr(), 
                     selectInput("to_plot_4", label = "Variable to plot", choices = c("Load datafile")),
                     plotOutput("barcode_boxplot", height = 300),
                     tags$hr(),
                     
                     h4("PCA the barcodes"),
                     actionButton("barcode_PCA_code", label="Show me the code", icon=icon("eye"), style='padding:4px; font-size:80%; color-background="#62bfad'),
                     tags$hr(), 
                     plotOutput("barcode_PCA", height = 300) 
               )
             )
    ),    
    tabPanel("archiPCA", id="tab3",icon = icon("bullseye"),
      fluidRow(
      column(3, 
             helpText("The archiPCA panel of this app was developed to show you how multivariate statistical analysis techniques, such as principal component analysis (PCA), can be used to differentiate root systems based on the results of the architect function."),
             selectInput("variable_to_pca", label="Variables to include in PCA", choices = c("Load datafile"), 
                         selected = NULL, multiple = TRUE, width="100%"),
             selectInput("genotypes_to_plot_3", label="Genotypes to plot", choices = c("Load datafile"), 
                         selected = NULL, multiple = TRUE, width="100%"),
             sliderInput("time_to_plot", "Time point to analyse", min = 1, max=10, step = 1, value = 10),
             tags$hr(),
             img(src='logo.jpg', align = "left", width="100%")
      ),
      column(7,
             h4("Plot the principal component analysis"),
             actionButton("pca_code", label="Show me the code", icon=icon("eye"), style='padding:4px; font-size:80%; color-background="#62bfad'),
             tags$hr(),
             plotOutput("pca_plot", height = 800)
      )
    )      
    ), 
    tabPanel("About", id="tab4", icon=icon("plus-circle"),
      fluidRow(
        column(3),
        column(6,
            h4("What is archiDART"),
            helpText("archiDART is an R package that was developed for the automated analysis of plant root system architectures using Data Analysis of Root Tracings (DART) and Root System Markup Language (RSML) files. This R package is the result of an international collaboration between the Plant Biology Unit of Gembloux Agro-Bio Tech (University of Liège, Belgium), the Earth and Life Institute of the Catholic University of Louvain-la-Neuve (Belgium), the Ecosystem Functioning and Services lab of the Leuphana University Lüneburg (Germany), the Forschungszentrum Jülich GmbH (IBG-3, Germany), the French National Institute for Agricultural Research (Centre PACA UR 1115 PSH, France), and the Donald Danforth Plant Science Center (USA). For more information about archiDART, please contact its principal maintainer (Benjamin.Delory@leuphana.de)."),
            tags$hr(),
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
