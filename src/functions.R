
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
        summarise(
            Média = round(mean(vlrLiquido, na.rm = TRUE),2),
            DP = round(sd(vlrLiquido, na.rm = TRUE),2),
            Min = min(vlrLiquido, na.rm = TRUE),
            p25 = quantile(vlrLiquido, na.rm = TRUE, probs = .25),
            Mediana = median(vlrLiquido, na.rm = TRUE),
            p75 = quantile(vlrLiquido, na.rm = TRUE, probs = .75),
            Max = max(vlrLiquido, na.rm = TRUE)                  
        ) %>%
        filter(!is.na(mes)) %>%
        mutate(mes = fct_relevel(mes, mes_ord)) %>%
        arrange(sgPartido, mes)
    
    df_geral = data %>%
      #  filter(sgPartido %in% partidos) %>%
        mutate(mes = months(datEmissao)) %>%
        group_by(mes) %>%
        summarise(
            Média = round(mean(vlrLiquido, na.rm = TRUE),2),
            DP = round(sd(vlrLiquido, na.rm = TRUE),2),
            Min = min(vlrLiquido, na.rm = TRUE),
            p25 = quantile(vlrLiquido, na.rm = TRUE, probs = .25),
            Mediana = median(vlrLiquido, na.rm = TRUE),
            p75 = quantile(vlrLiquido, na.rm = TRUE, probs = .75),
            Max = max(vlrLiquido, na.rm = TRUE)
        ) %>%
        filter(!is.na(mes)) %>%
        mutate(mes = fct_relevel(mes, mes_ord)) %>%
        arrange(mes)

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

    return(list('plot' = p_out,
                'tabela' = df_plot,
                'tabelaGeral' = df_geral))

}
