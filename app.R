library(shiny)
library(tidyverse)

df = read_delim('data/Ano-2018.csv',
                delim=';',
                col_types = cols())

plotGastos = function(data,
                      partidos,
                      geral){

    mes_ord = c('janeiro', 'fevereiro', 'março', 'abril',
                'maio', 'junho', 'julho', 'agosto',
                'setembro', 'outubro', 'novembro', 'dezembro')

    df_plot = data %>%
        filter(sgPartido %in% partidos) %>%
        mutate(mes = months(datEmissao)) %>%
        group_by(sgPartido, mes) %>%
        summarise(Mediana = median(vlrLiquido, na.rm = TRUE),
                  Média = mean(vlrLiquido, na.rm = TRUE),
                  DP = sd(vlrLiquido, na.rm = TRUE),
                  Min = min(vlrLiquido, na.rm = TRUE),
                  Max = max(vlrLiquido, na.rm = TRUE),
                  p25 = quantile(vlrLiquido, na.rm = TRUE, probs = .25),
                  p75 = quantile(vlrLiquido, na.rm = TRUE, probs = .75)
                  ) %>%
        filter(!is.na(mes)) %>%
        mutate(mes = fct_relevel(mes, mes_ord))
    
    df_geral = data %>%
      #  filter(sgPartido %in% partidos) %>%
        mutate(mes = months(datEmissao)) %>%
        group_by(mes) %>%
        summarise(Mediana = median(vlrLiquido, na.rm = TRUE),
                  Média = mean(vlrLiquido, na.rm = TRUE),
                  DP = sd(vlrLiquido, na.rm = TRUE),
                  Min = min(vlrLiquido, na.rm = TRUE),
                  Max = max(vlrLiquido, na.rm = TRUE),
                  p25 = quantile(vlrLiquido, na.rm = TRUE, probs = .25),
                  p75 = quantile(vlrLiquido, na.rm = TRUE, probs = .75)
                  ) %>%
        filter(!is.na(mes)) %>%
        mutate(mes = fct_relevel(mes, mes_ord))

    p_out = df_plot %>%
        ggplot(aes(x = mes, y = Média, group = sgPartido)) +
        geom_line(aes(col = sgPartido)) +
        geom_point(aes(col = sgPartido))

    if(geral){
        p_out = p_out +
            geom_point(data = df_geral,
                       mapping =aes(x = mes,
                                    y = Média,
                                    group = 1,
                                    col = 'Geral')) +
            geom_line(data = df_geral,
                       mapping =aes(x = mes,
                                    y = Média,
                                    group = 1,
                                    col = 'Geral'),
                      linetype = 2) 
    } # end if geral

##     if(dispersao){

##         p_out = p_out +
##             geom_ribbon(aes(ymin = Média - DP,
##                             ymax = Média + DP),
## #                        fill = sgPartido,
##                         alpha = .3,
##                         data = df_plot)

##     } # end if dispersao


    
    return(p_out)
    

}






##################################################


ui = fluidPage(

    titlePanel('Gasto médio dos parlamentares por partido'),

    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput('partidos',
                               h3('Partidos'),
                               choices = sort(unique(df$sgPartido))
                               ),
            h3('Mostral média geral?'),
            checkboxInput("geral",
                          label = "Sim",
                          value = TRUE)
        ), # end sidebarpanel

        mainPanel(

            plotOutput(outputId = 'gastos')

        )
    ) # end sidebarLayout

) # end UI

server = function(input, output){

    output$gastos = renderPlot({plotGastos(data = df,
               partidos = input$partidos,
               geral =input$geral)
               })
    
}

shinyApp(ui = ui, server = server)
