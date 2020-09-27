calcR2 <- function(df)
{
      ind_var <- df$var == "stringency"
      modl <- lm(df$gdpH1[ind_var] ~ df$value[ind_var])
      df_R2 <- data.frame(var = "Lockdown-Stringenz",
                          R2 = paste0("R^2 = ", format(round(summary(modl)$r.squared, 2), nsmall = 2)),
                          x = 30,
                          y = -20)
      
      ind_var <- df$var == "openness"
      modl <- lm(df$gdpH1[ind_var] ~ df$value[ind_var])
      df_R2 <- rbind(df_R2, data.frame(var = "Offenheit",
                                       R2 = paste0("R^2 = ", format(round(summary(modl)$r.squared, 2), nsmall = 2)),
                                       x = 200,
                                       y = -20)
      )
      
      ind_var <- df$var == "traveltourism"
      modl <- lm(df$gdpH1[ind_var] ~ df$value[ind_var])
      df_R2 <- rbind(df_R2, data.frame(var = "Tourismus (WTTC)",
                                       R2 = paste0("R^2 = ", format(round(summary(modl)$r.squared, 2), nsmall = 2)),
                                       x = 17.5,
                                       y = -7.5)
      )
      
      ind_var <- df$var == "tourism"
      modl <- lm(df$gdpH1[ind_var] ~ df$value[ind_var])
      df_R2 <- rbind(df_R2, data.frame(var = "Tourismus (Weltbank)",
                                       R2 = paste0("R^2 = ", format(round(summary(modl)$r.squared, 2), nsmall = 2)),
                                       x = 7.5,
                                       y = -7.5)
      )
      
      ind_var <- df$var == "covid19cases"
      modl <- lm(df$gdpH1[ind_var] ~ df$value[ind_var])
      df_R2 <- rbind(df_R2, data.frame(var = "Covid-19-Infektionen",
                                       R2 = paste0("R^2 = ", format(round(summary(modl)$r.squared, 2), nsmall = 2)),
                                       x = 10000,
                                       y = -20)
      )
      
      ind_var <- df$var == "covid19deaths"
      modl <- lm(df$gdpH1[ind_var] ~ df$value[ind_var])
      df_R2 <- rbind(df_R2, data.frame(var = "Covid-19-Todesfälle",
                                       R2 = paste0("R^2 = ", format(round(summary(modl)$r.squared, 2), nsmall = 2)),
                                       x = 700,
                                       y = -7.5)
      )
  
      ind_var <- df$var == "retail_rec"
      modl <- lm(df$gdpH1[ind_var] ~ df$value[ind_var])
      df_R2 <- rbind(df_R2, data.frame(var = "retail and recreation",
                                       R2 = paste0("R^2 = ", format(round(summary(modl)$r.squared, 2), nsmall = 2)),
                                       x = 70,
                                       y = -21)
      )
      
      
      ind_var <- df$var == "transit"
      modl <- lm(df$gdpH1[ind_var] ~ df$value[ind_var])
      df_R2 <- rbind(df_R2, data.frame(var = "transit stations",
                                       R2 = paste0("R^2 = ", format(round(summary(modl)$r.squared, 2), nsmall = 2)),
                                       x = 77,
                                       y = -21)
      )
      
      ind_var <- df$var == "workplace"
      modl <- lm(df$gdpH1[ind_var] ~ df$value[ind_var])
      df_R2 <- rbind(df_R2, data.frame(var = "workplace",
                                       R2 = paste0("R^2 = ", format(round(summary(modl)$r.squared, 2), nsmall = 2)),
                                       x = 84,
                                       y = -21)
      )
      
      ind_var <- df$var == "G.I"
      modl <- lm(df$gdpH1[ind_var] ~ df$value[ind_var])
      df_R2 <- rbind(df_R2,data.frame(var = "trade, transport, accommodation",
                                      R2 = paste0("R^2 = ", format(round(summary(modl)$r.squared, 2), nsmall = 2)),
                                      x = 24,
                                      y = -2.5)
      )
      
      ind_var <- df$var == "R.U"
      modl <- lm(df$gdpH1[ind_var] ~ df$value[ind_var])
      df_R2 <- rbind(df_R2,
                     data.frame(var = "arts, entertainment, recreation",
                                R2 = paste0("R^2 = ", format(round(summary(modl)$r.squared, 2), nsmall = 2)),
                                x = 4.5,
                                y = -2.5
                     )
      )
  
    return(df_R2)
}