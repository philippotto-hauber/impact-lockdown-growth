calcrR2pval <- function(df)
{
      add_row_to_df <- function(x, y, name, xval, yval)
      {
        corr <- cor.test(x, y)
        pval_str <- ""
        if (corr$p.value <= 0.01)
        {
          pval_str <- "***"
        } else if (corr$p.value <= 0.05)
        {
          pval_str <- "**"
        } else if (corr$p.value <= 0.1)
        {
          pval_str <- "*"
        }
        
        df_out <- data.frame(var = name,
                         r = corr$estimate,
                         R2 = corr$estimate ^ 2,
                         pval = corr$p.value,
                         text = paste0("r = ", format(round(corr$estimate, 2), nsmall = 2), 
                                       ifelse(pval_str == "", "", paste0(" (", pval_str, ")"))
                                      ),
                         x = xval,
                         y = yval)
        
      }
      
      ind_var <- df$var == "stringency"
      df_R2 <- add_row_to_df(df$gdpH1[ind_var], df$value[ind_var], "Lockdown-Stringenz", 35, -22)

      ind_var <- df$var == "openness"
      df_R2 <- rbind(df_R2, add_row_to_df(df$gdpH1[ind_var], df$value[ind_var], "Offenheit", 170, -20))
      
      
      ind_var <- df$var == "traveltourism"
      df_R2 <- rbind(df_R2, add_row_to_df(df$gdpH1[ind_var], df$value[ind_var], "Tourismus (WTTC)", 15, -7.5))
      

      ind_var <- df$var == "tourism"
      df_R2 <- rbind(df_R2, add_row_to_df(df$gdpH1[ind_var], df$value[ind_var], "Tourismus (Weltbank)", 7.5, -7.5))
      
      ind_var <- df$var == "covid19cases"
      df_R2 <- rbind(df_R2, add_row_to_df(df$gdpH1[ind_var], df$value[ind_var], "Covid-19-Infektionen", 10000, -20))
      

      ind_var <- df$var == "covid19deaths"
      ind_var <- df$var == "covid19deaths"
      df_R2 <- rbind(df_R2, add_row_to_df(df$gdpH1[ind_var], df$value[ind_var], "Covid-19-Todesfälle", 500, -5))
      
      ind_var <- df$var == "mobility"
      df_R2 <- rbind(df_R2, add_row_to_df(df$gdpH1[ind_var], df$value[ind_var], "Mobilitätsdaten", 70, -21))
      
      ind_var <- df$var == "structure"
      df_R2 <- rbind(df_R2, add_row_to_df(df$gdpH1[ind_var], df$value[ind_var], "Wirtschaftsstruktur", 25, -5))
      

      return(df_R2)
}