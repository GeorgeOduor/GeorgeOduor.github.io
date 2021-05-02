library(xlsx)
calculo<-function(archivo){
  incrementos_descuentos<-read.xlsx(archivo,sheetName="incrementos - descuentos",  check.names = FALSE)  
  incrementos_descuentos$`Desde / mm`<-as.numeric(as.character(incrementos_descuentos$`Desde / mm`))
  incrementos_descuentos$`A  / mm`<-as.numeric(as.character(incrementos_descuentos$`A  / mm`))
  incrementos_descuentos$`Volumen / L`<-as.numeric(as.character(incrementos_descuentos$`Volumen / L`))
  incrementos_descuentos$`Resultado L/m`<-as.numeric(as.character(incrementos_descuentos$`Resultado L/m`))
  incrementos_descuentos$`Resultado L/m`<-incrementos_descuentos$`Volumen / L`/(incrementos_descuentos$`A  / mm`-incrementos_descuentos$`Desde / mm`)
  Resutado<-incrementos_descuentos
  Resutado
}