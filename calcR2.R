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
  
      ind_var <- df$var == "mobility"
      modl <- lm(df$gdpH1[ind_var] ~ df$value[ind_var])
      df_R2 <- rbind(df_R2, data.frame(var = "Mobilitätsdaten",
                                       R2 = paste0("R^2 = ", format(round(summary(modl)$r.squared, 2), nsmall = 2)),
                                       x = 70,
                                       y = -21)
      )
      
      ind_var <- df$var == "structure"
      modl <- lm(df$gdpH1[ind_var] ~ df$value[ind_var])
      df_R2 <- rbind(df_R2,data.frame(var = "Dienstleistungsbereiche",
                                      R2 = paste0("R^2 = ", format(round(summary(modl)$r.squared, 2), nsmall = 2)),
                                      x = 24,
                                      y = -2.5)
      )

    return(df_R2)
}