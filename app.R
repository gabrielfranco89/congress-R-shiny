library(shiny)
library(tidyverse)

df = read_delim('data/Ano-2018.csv',
                delim=';',
                col_types = cols())

source('src/functions.R')


## UI ##################################################
ui = fluidPage(

    titlePanel('Gasto médio dos parlamentares por partido em 2018'),

    sidebarLayout(

        sidebarPanel(
            helpText('Selecione os partidos que deseja visualizar, depois se gostaria de ver a média geral de gastos e clique em Submeter ao final.'),
            checkboxGroupInput('partidos',
                               h3('Partidos'),
                               choices = sort(unique(df$sgPartido))
                               ),
            h3('Mostral média geral?'),
            checkboxInput("geral",
                          label = "Sim",
                          value = TRUE),
            submitButton("Submeter")
    ), # end sidebarpanel

    mainPanel(

            tabsetPanel(
                id = 'dataset',
                type = 'tabs',
                tabPanel("Gráfico", plotOutput("plotGastos")),
                tabPanel("Tabela Gastos por Partido",
                         DT::dataTableOutput("tabMedia")),
                tabPanel("Tabela Gastos Médios",
                         DT::dataTableOutput("tabGeral"))
            )


        )
    ) # end sidebarLayout

) # end UI


## SERVER ============================================
server = function(input, output){

    d = reactive({
        obj = plotGastos(data = df,
                         partidos = input$partidos,
                         geral =input$geral)
    })

    output$plotGastos = renderPlot(d()$plot)
    output$tabMedia = DT::renderDataTable({
        DT::datatable(d()$tabela,
                      options = list(pageLength = 12))
        })
    output$tabGeral = DT::renderDataTable({
        DT::datatable(d()$tabelaGeral,
                      options = list(pageLength = 12))
        })

}

## APP LAUNCH =========================================
shinyApp(ui = ui, server = server)
