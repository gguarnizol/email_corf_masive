## Envíó y creación de correo electrónico

setwd(getwd())

library(blastula)
library(glue)
library(kableExtra)

Sys.setenv("SMTP_PASSWORD"="clapalico2007")      # :Pass correo

footer_a = md(
  c(
    "Diseñado por la nueva victima de Sebas"
  )
)

# Tratamiento de imagen
imagen_string <- function(img){
  img_file_path <- paste0("images/",img,".JPEG")
  img_string <- add_image(file = img_file_path)
  return (img_string)
}

# Construcción del HTML
construir_html <- function(nombre,tabla){
 img_string <- imagen_string("img")
  
  #Header
  header_a <- 
    md(
    paste0(
      "<h2 style=\"text-align:center;\">Empresa: ",
      nombre,
      "</h2><h3 style=\"text-align:center;\">Detalle citas aprobadas</h3>"
    )
     
    )
  
  
  email <- compose_email(title = "Email", 
                         header = md( c(img_string, header_a) ),
                         body = md(
                           tabla
                         ),
                         footer = footer_a
  )
  
  return (email)
}
