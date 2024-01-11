library(tidyverse)
library(ggthemes)
library(PNADcIBGE)
library(survey)
library(geobr)
library(convey)
library(ggplot2)
library(xtable)


pnadc_anual_visita <- PNADcIBGE::get_pnadc(year=2022, interview=1, labels=TRUE, deflator=TRUE, design=FALSE)

# Realizando coleta de lixo acumulada durante a obtenção dos microdados
gc(verbose=FALSE, reset=FALSE, full=TRUE)

pnadc_anual_visita <- transform(pnadc_anual_visita, ID_DOMICILIO=paste0(UPA,V1008,V1014))
pnadc_anual_visita <- transform(pnadc_anual_visita, Pais=as.factor("Brasil"))
pnadc_anual_visita$Pais <- factor(x=pnadc_anual_visita$Pais, levels=c("Brasil"))
pnadc_anual_visita <- transform(pnadc_anual_visita, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadc_anual_visita$GR <- factor(x=pnadc_anual_visita$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

# Realizando processo de obtenção da estimativa do rendimento domiciliar real
pnadc_anual_visita <- transform(pnadc_anual_visita, V2001_rendimento=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,1))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4019real_proprioano=ifelse(is.na(VD4019) | is.na(V2001_rendimento),NA,VD4019*CO1))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4048real_proprioano=ifelse(is.na(VD4048) | is.na(V2001_rendimento),NA,VD4048*CO1e))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4019real_ultimoano=ifelse(is.na(VD4019) | is.na(V2001_rendimento),NA,VD4019*CO2))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4048real_ultimoano=ifelse(is.na(VD4048) | is.na(V2001_rendimento),NA,VD4048*CO2e))
pnadc_anual_visita_rendimento <- pnadc_anual_visita %>% dplyr::group_by(ID_DOMICILIO) %>% dplyr::summarise(moradores_rendimento=sum(V2001_rendimento, na.rm=TRUE),
                                                                                                           rendimento_todos_trabalhos_proprioano=sum(VD4019real_proprioano, na.rm=TRUE),
                                                                                                           rendimento_outras_fontes_proprioano=sum(VD4048real_proprioano, na.rm=TRUE),
                                                                                                           rendimento_todos_trabalhos_ultimoano=sum(VD4019real_ultimoano, na.rm=TRUE),
                                                                                                           rendimento_outras_fontes_ultimoano=sum(VD4048real_ultimoano, na.rm=TRUE))
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5007real_proprioano=rendimento_todos_trabalhos_proprioano+rendimento_outras_fontes_proprioano)
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5008real_proprioano=VD5007real_proprioano/moradores_rendimento)
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5007real_ultimoano=rendimento_todos_trabalhos_ultimoano+rendimento_outras_fontes_ultimoano)
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5008real_ultimoano=VD5007real_ultimoano/moradores_rendimento)
pnadc_anual_visita <- pnadc_anual_visita[,!(names(pnadc_anual_visita) %in% c("V2001_rendimento","VD4019real_proprioano","VD4048real_proprioano","VD4019real_ultimoano","VD4048real_ultimoano"))]
pnadc_anual_visita_rendimento <- pnadc_anual_visita_rendimento[,!(names(pnadc_anual_visita_rendimento) %in% c("moradores_rendimento","rendimento_todos_trabalhos_proprioano","rendimento_outras_fontes_proprioano","rendimento_todos_trabalhos_ultimoano","rendimento_outras_fontes_ultimoano"))]
pnadc_anual_visita <- merge(x=pnadc_anual_visita, y=pnadc_anual_visita_rendimento, by.x="ID_DOMICILIO", by.y="ID_DOMICILIO", all.x=TRUE, all.y=FALSE)
rm(pnadc_anual_visita_rendimento)
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5007real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_proprioano))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5008real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_proprioano))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5007real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_ultimoano))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5008real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_ultimoano))

# Realizando processo de incorporação do desenho amostral nos microdados
pnadc_anual_visita <- tibble::as_tibble(x=pnadc_anual_visita)
pnadc_anual_visita <- PNADcIBGE::pnadc_design(data_pnadc=pnadc_anual_visita)
str(object=pnadc_anual_visita)

print(x=rendimento_domiciliar_per_capita_media_proprioano <- survey::svybys(formula=~VD5008real_proprioano, bys=~Pais+GR+UF, design=pnadc_anual_visita, FUN=svymean, na.rm=TRUE))
print(x=list(cv(object=rendimento_domiciliar_per_capita_media_proprioano[[1]]), cv(object=rendimento_domiciliar_per_capita_media_proprioano[[2]]), cv(object=rendimento_domiciliar_per_capita_media_proprioano[[3]])))


pnadc_anual_visita <- convey::convey_prep(design=pnadc_anual_visita)
print(x=indice_gini <- survey::svybys(formula=~VD5008real_proprioano, bys=~Pais+GR+UF, design=pnadc_anual_visita, FUN=svygini, na.rm=TRUE))
print(x=list(cv(object=indice_gini[[1]]), cv(object=indice_gini[[2]]), cv(object=indice_gini[[3]])))


class(pnadc_anual_visita)

rdpc.quantis = svyquantile(~VD5008real_proprioano,design=pnadc_anual_visita,
                           quantiles =seq(0,1,.01) , na.rm = TRUE )
rdpc.quantis.ci <- confint( rdpc.quantis )
rdpc.quantis






# Assuming rdpc.quantis is your model or data
library(ggplot2)

# Generate data
fractions <- seq(0, 1, 0.01)
quantis_coef <- coef(rdpc.quantis)
data <- data.frame(fractions, quantis_coef)

# Generate data
fractions <- seq(0, 1, 0.01)
quantis_coef <- coef(rdpc.quantis)
data <- data.frame(fractions, quantis_coef)


# Generate data
fractions <- seq(0, 1, 0.01)
quantis_coef <- coef(rdpc.quantis)
data <- data.frame(fractions, quantis_coef)

# Obtain the values corresponding to x = 0.5, 0.9, and 0.99
y_value_at_x_0.5 <- data$quantis_coef[data$fractions == 0.5]
y_value_at_x_0.9 <- data$quantis_coef[data$fractions == 0.9]
y_value_at_x_0.99 <- data$quantis_coef[data$fractions == 0.99]

# Format the values with two decimal places
formatted_y_value <- sprintf("%.2f", y_value_at_x_0.5)
formatted_y_value_0.9 <- sprintf("%.2f", y_value_at_x_0.9)
formatted_y_value_0.99 <- sprintf("%.2f", y_value_at_x_0.99)

# Create ggplot
ggplot(data, aes(x = fractions, y = quantis_coef)) +
  geom_line() +
  geom_point(pch = 19, size = 2) +
  geom_segment(aes(x = 0, y = y_value_at_x_0.5, xend = 0.5, yend = y_value_at_x_0.5), linetype = "dashed", color = "red") +
  geom_segment(aes(x = 0.5, y = y_value_at_x_0.5, xend = 0.5, yend = 0), linetype = "dashed", color = "red") +
  annotate("text", x = 0, y = y_value_at_x_0.5, label = formatted_y_value, color = "red", size = 3, vjust = 1.5) +
  geom_segment(aes(x = 0, y = y_value_at_x_0.9, xend = 0.9, yend = y_value_at_x_0.9), linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 0.9, y = y_value_at_x_0.9, xend = 0.9, yend = 0), linetype = "dashed", color = "blue") +
  annotate("text", x = 0, y = y_value_at_x_0.9, label = formatted_y_value_0.9, color = "blue", size = 3, vjust = 1.5) +
  geom_segment(aes(x = 0, y = y_value_at_x_0.99, xend = 0.99, yend = y_value_at_x_0.99), linetype = "dashed", color = "green") +
  geom_segment(aes(x = 0.99, y = y_value_at_x_0.99, xend = 0.99, yend = 0), linetype = "dashed", color = "green") +
  annotate("text", x = 0, y = y_value_at_x_0.99, label = formatted_y_value_0.99, color = "green", size = 3, vjust = 1.5) +
  labs(
    title = "Distribuição da Renda Domiciliar per capita",
    y = "Renda Domiciliar Per Capita",
    x = "Quantis"
  ) +
  theme_tufte() +
  scale_x_continuous(
    breaks = seq(0.1, 1, 0.1),
    limits = c(0, 1)
  ) +
  scale_y_continuous(limits = c(0, 12000))


rdpc.quantis_2 = svyquantile(~VD5008real_proprioano,design=pnadc_anual_visita,
                             quantiles = c(.1,.25,.5,.75,.90,.95,.99,.999),
                             na.rm=TRUE, ci=TRUE)
class(rdpc.quantis_2)
  
rdpc.quantis_2

quantis <- c(.1,.25,.5,.75,.90,.95,.99,.999)
renda <- coef(rdpc.quantis_2)

df2<-data.frame(quantis, quantis_coef)
print(df2)


# Desired y-value
desired_y_value <- 1212.00

# Find the corresponding x-value
x_value_for_desired_y <- approx(data$quantis_coef, data$fractions, xout = desired_y_value)$y

# Print the result
cat("For y =", desired_y_value, "the corresponding x-value is approximately", x_value_for_desired_y, "\n")