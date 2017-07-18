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
                textInput('path', 'Choose folder with all data files'),
                # fileInput('test_file', 'Choose file with testing data', accept=c('text/comma-separated-values', '.csv')),
               checkboxInput('use_example', "Use example data", value = T, width = NULL),
               bsButton(inputId = "load_data", type = "action", style="primary", label="Load data",icon("upload")),
               tags$hr(),
               textOutput("text0"),
               tags$head(tags$style("#text0{color: #28aa46;
                                   font-weight: bold;
                                   }"
               )
               ),
               tags$hr(),
               img(src='logo.jpg', align = "left", width="80%")
        ),
        column(7, 
               h4("Overview of the dataset"),
               tags$hr(),
               selectInput("to_plot_1", label = "Variable to plot", choices = c("Load datafile")),
               plotOutput("distribution_plot"),
               tags$hr(),
               helpText("Distribution of the data computed by architect."),
               tags$hr(),
               DT::dataTableOutput('distribution_data')
               

               
               # DT::dataTableOutput('train_data')
        )#,
        # column(4, 
        #        h4("Testing datatable"),
        #        tags$hr(),
        #        helpText("This table contains the data that will be used for the testing the Random Forest model"),
        #        tags$hr(),
        #        textOutput("test_text"),
        #        tags$head(tags$style("#test_text{color: #28aa46;
        #                            font-weight: bold;
        #                             }"
        #        )),
        #        tags$hr(),
        #        DT::dataTableOutput('test_data')
        # )
      )
    ),
    tabPanel("ArchiTect", id="tab2", icon = icon('sliders'),
        fluidRow(
          column(3, 
              helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
              selectInput("genotypes_to_plot", label="Genotypes to plot", choices = c("Load datafile"), 
                          selected = NULL, multiple = TRUE, width="100%"),
              checkboxInput('plot_mean', "Plot average by genotype", value = T, width = NULL),
              
              tags$hr(),
              img(src='logo.jpg', align = "left", width="80%")
          ),
          column(9,
                 selectInput("to_plot", label = "Variable to plot", choices = c("Load datafile")),
                 plotlyOutput("time_plot")
          )
        )
    ),
    tabPanel("ArchiDraw", id="tab3",icon = icon("pencil"),
       fluidRow(
         column(3, 
                helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
                selectInput("genotypes_to_plot_1", label="Genotypes to plot", choices = c("Load datafile"), 
                            selected = NULL, multiple = TRUE, width="100%"),
                sliderInput("reps_to_plot", "Number of repetitions to plot", min = 1, max=10, step = 1, value = 10),
                sliderInput("ncol", "Number of columns", min = 1, max=10, step = 1, value = 10),
                checkboxInput('plot_mean_archi', "Plot average architecture by genotype", value = T, width = NULL),
                tags$hr(),
                img(src='logo.jpg', align = "left", width="80%")
         ),
         column(9,
                selectInput("to_plot_2", label = "Variable to plot", choices = c("age", "diameter", "order")),
                plotOutput("archi_plot", height = 1000),
                tags$head(tags$style("#archi_plot{valign: top;}"))
         )
       )
    ),
    tabPanel("ArchiGrow", id="tab3",icon = icon("hourglass-half")
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
            actionButton(inputId='ab1', label="PRIMAL webpage", icon = icon("cogs"), onclick ="window.open('https://plantmodelling.github.io/primal/', '_blank')"),
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
