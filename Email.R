setwd(getwd())
# (Libraries) -----------------------------------------------------------------
 library(lubridate)
 library(dplyr)
 library(knitr)
 library(markdown)
 library(rmarkdown)
 library(gtable)
 library(gt)
# -------------------------------------------------------------------------------

# (1) Load and clean data -----------------------------------------------------

#Load data
source("HTML.R")
base <- read.csv("email_temp.csv") %>% select(-c(correo_empresa,user,pass))
# -------------------------------------------------------------------------------

# (2) Summary Companies -------------------------------------------------------

enviados <- c()

for (emp in unique(base$empresa)){
  
    datos <- base %>% filter(empresa == emp)
    tables   <- c()
    
    for (fecha in unique(datos$fecha_cita)) {
      
      tabla <- 
        datos%>%filter( fecha_cita == fecha) %>%
        select(-c(empresa,fecha_cita))
      
      colnames(tabla) <- c("HORA CITA","SALA","EMPRESA","ENCARGADO",
                           "PAÍS")
      
      tabla <- 
        tabla %>%
        gt() %>%
        tab_header(  title = md(fecha)  )%>%  
        tab_style(
          locations = cells_title(groups = "title"),
          style     = 
            list(
              cell_text(weight = "bold", size = 24)
            )
        )%>%
        tab_options(
          column_labels.background.color = "gray",
          heading.align = "center"
        )%>%
        as_raw_html()
      
      tables   <- c(tables,tabla)
    }
 
    email <- construir_html(emp,tables) %>%
      smtp_send(
        to = "ljurado@corferias.com",
        from = "giancarlo.guarnizo@coaspharma.co",
        subject = paste0("Horarios de rueda de negocio para: ", emp),
        credentials = creds_envvar(
          user = "giancarlo.guarnizo@coaspharma.co",
          pass_envvar =  "SMTP_PASSWORD",
          provider = "gmail", 
          host = "smtp.gmail.com", 
          port = 465, 
          use_ssl = TRUE)
      )
    
    enviados <- c(enviados,emp)
}


# -------------------------------------------------------------------------------
