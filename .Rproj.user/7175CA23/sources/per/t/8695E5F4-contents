install.packages("pacman")

pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

injuv2022 <- read_dta("input/data_orig/BBDD Respuesta - Encuesta Jóvenes.dta")
View(injuv2022)

find_var(data = injuv2022,"educacion")

find_var(data = injuv2022,"mujer")

#FILTRO >=26 años
injuv2022<- injuv2022 %>% filter( EDAD >= "26")

#Hombres como datos perdidos
injuv2022$SEXO <- recode(injuv2022$SEXO, "c(1)=NA")


proc_data <- injuv2022 %>% select(SEXO, # sexo
                                  EDAD, # edad
                                  P14, # Nivel educacional alcanzado
                                  P12_1, # Maternalismo cuidados 
                                  P12_4) # Maternalismo hijos

# Comprobar
names(proc_data)

sjlabelled::get_label(proc_data)

frq(proc_data$SEXO)

frq(proc_data$EDAD)

frq(proc_data$P14)

frq(proc_data$P12_1)

frq(proc_data$P12_4)

#Datos perdidos
proc_data$P14 <- recode(proc_data$P14, "c(98, 99)=NA")
proc_data$P12_1 <- recode(proc_data$P12_1, "c(98, 99)=NA")
proc_data$P12_4 <- recode(proc_data$P12_4, "c(98, 99)=NA")
sum(is.na(proc_data))

proc_data <-na.omit(proc_data)
dim(proc_data)

proc_data_original <-proc_data
dim(proc_data)

sum(is.na(proc_data))

proc_data <-na.omit(proc_data)
dim(proc_data)


#Renombrar variables

proc_data <- proc_data %>% rename("nivel_educ"=P14, # Nivel educacional alcanzado
                                  "mater_cuidados"=P12_1, # Maternalismo cuidados
                                  "mater_hijos"=P12_4) # Maternalismo hijos

proc_data$nivel_educ <- set_label(x = proc_data$nivel_educ,label = "Nivel educacional alcanzado")
get_label(proc_data$nivel_educ)                                  

proc_data$mater_cuidados <- set_label(x = proc_data$mater_cuidados,label = "Maternalismo cuidados")
get_label(proc_data$mater_cuidados)

proc_data$mater_hijos <- set_label(x = proc_data$mater_hijos,label = "Maternalismo hijos")
get_label(proc_data$mater_hijos)

proc_data$ideas_mater <- (proc_data$mater_cuidados+proc_data$mater_hijos)
summary(proc_data$ideas_mater)

proc_data$ideas_mater  <- set_label(x = proc_data$ideas_mater, label = "Ideas Maternalistas")
get_label(proc_data$ideas_mater)

frq(proc_data$nivel_educ)

frq(proc_data$mater_cuidados)

frq(proc_data$mater_hijos)

#Recodificacion sexo
frq(proc_data$SEXO)

proc_data$sexo <- car::recode(proc_data$SEXO, "2=1")


proc_data$sexo <- factor(proc_data$sexo,
                         labels=c( "Mujer"),
                         levels=c(1))


get_label(proc_data$sexo)


frq(proc_data$sexo)


# Recodificacion nivel educacional
proc_data$Re_nivel_educ <- car::recode(proc_data$nivel_educ, "c(1)=1; c(2,3)= 2; c(4,5,6)=3; c(7,8,9,10)=4; c(11,12,13, 14)=5; c(15,16)=6")
frq(proc_data$Re_nivel_educ)

proc_data$Re_nivel_educ <- factor(proc_data$Re_nivel_educ,
                                  labels = c("No educacion", "Educacion pre-escolar", "Educacion basica", "Educacion media", "Educacion superior", "Postgrado"),
                                  levels = c(1, 2, 3, 4, 5, 6))

proc_data <- rename(proc_data,"educ_alcanzada"=Re_nivel_educ)

get_label(proc_data$educ_alcanzada)

proc_data$educ_alcanzada <- set_label(x = proc_data$educ_alcanzada,label = "Educacion Alcanzada")

frq(proc_data$educ_alcanzada)

#Recodificacion edad
frq(proc_data$EDAD)

proc_data$edad_rec <- car::recode(proc_data$EDAD, "c(26,27,28,29)=1")

proc_data$edad_rec <- factor(proc_data$edad_rec,
                             labels=c("Mujeres de 26 a 29 años"),
                             levels=c(1))


get_label(proc_data$edad_rec)

proc_data$edad_rec <- set_label(x = proc_data$edad_rec,label = "Edad")

frq(proc_data$edad_rec)

#GUARDAR BBDD Filtrada

proc_data <-as.data.frame(proc_data)
stargazer(proc_data, type="text")

save(proc_data,file = "C:\\Users\\pauli\\Documents\\GitHub\\Trabajo_2\\input\\data_proc/INJUV_Maternalismo2022.RData")



#CREACIÓN DE TABLAS Y GRÁFICOS

pacman::p_load(sjlabelled,
               heaven,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2) # Para la mayoría de los gráficos

pacman::p_load(heaven)

names(proc_data) # Muestra los nombres de las variables de la base de datos
dim(proc_data) # Dimensiones

stargazer(proc_data,type = "text")

#me arroja error
sjmisc::descr(proc_data)

#Tabla descriptiva
summarytools::dfSummary(proc_data, plain.ascii = FALSE)

view(dfSummary(proc_data, headings=FALSE))

#Visualización de variables

# Crear el gráfico usando ggplot2

#Ideas maternalistas
graph1 <- proc_data %>% ggplot(aes(x = ideas_mater)) + 
  geom_bar(fill = "pink")+
  labs(title = "Ideas Maternalista",
       x = "Ideas maternalistas",
       y = "Frecuencia") +
  theme_bw()

graph1

#guardar imagen:

ggsave(graph1, file="files/img/graph1.png")


#Maternalismo en torno al cuidado
graph1.1 <- proc_data %>% ggplot(aes(x = mater_cuidados)) + 
  geom_bar(fill = "pink")+
  labs(title = "Ideas Maternalista: cuidados",
       x = "Ideas maternalistas en torno a los cuidados",
       y = "Frecuencia") +
  theme_bw()

graph1.1

#guardar imagen:

ggsave(graph1.1, file="files/img/graph1.png")


#Maternalismo en torno a los hijos
graph1.2 <- proc_data %>% ggplot(aes(x = mater_hijos)) + 
  geom_bar(fill = "pink")+
  labs(title = "Ideas Maternalista: hijos",
       x = "Ideas maternalistas en torno a los hijos",
       y = "Frecuencia") +
  theme_bw()

graph1.2

#guardar imagen:

ggsave(graph1.2, file="files/img/graph1.png")

#Tablas de contingencia para variables categóricas
sjt.xtab(proc_data$educ_alcanzada, proc_data$ideas_mater)

sjt.xtab(proc_data$educ_alcanzada, proc_data$mater_cuidados)

sjt.xtab(proc_data$educ_alcanzada, proc_data$mater_hijos)

sjt.xtab(proc_data$educ_alcanzada, proc_data$ideas_mater,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

sjt.xtab(proc_data$educ_alcanzada, proc_data$mater_cuidados,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

sjt.xtab(proc_data$educ_alcanzada, proc_data$mater_hijos,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

#Tablas de promedio

#Ideas maternalistas
graph <- ggplot(proc_data, aes(x =educ_alcanzada, y = ideas_mater)) +
  geom_boxplot() +
  labs(x = "Educación", y = "Ideas Maternalistas") +
  theme_minimal()

graph

#maternalismo hijos
graph <- ggplot(proc_data, aes(x =educ_alcanzada, y = mater_hijos)) +
  geom_boxplot() +
  labs(x = "Educación", y = "Ideas Maternalistas: hijos") +
  theme_minimal()

graph

#maternalismo cuidados
graph <- ggplot(proc_data, aes(x =educ_alcanzada, y = mater_cuidados)) +
  geom_boxplot() +
  labs(x = "Educación", y = "Ideas Maternalistas: cuidados") +
  theme_minimal()

graph

#Otro gráfico

graph2 <- sjPlot::plot_stackfrq(dplyr::select(proc_data,
                                              mater_hijos,
                                              mater_cuidados),
                                title = "Ideas maternalistas") +
  theme(legend.position="bottom")

graph2

datos <- proc_data %>% group_by(educ_alcanzada) %>% 
  summarise(promedio = mean(mater_hijos))

ggplot(datos, aes(x =educ_alcanzada, y = promedio)) +
  geom_point() +
  labs(x = "Educación", y = "Ideas Maternalistas: hijos") +
  theme_minimal()+
  ylim(0, 12)

datos <- proc_data %>% group_by(educ_alcanzada) %>% 
  summarise(promedio = mean(mater_cuidados))

ggplot(datos, aes(x =educ_alcanzada, y = promedio)) +
  geom_point() +
  labs(x = "Educación", y = "Ideas Maternalistas: cuidados") +
  theme_minimal()+
  ylim(0, 12)

datos <- proc_data %>% group_by(educ_alcanzada) %>% 
  summarise(promedio = mean(ideas_mater))

ggplot(datos, aes(x =educ_alcanzada, y = promedio)) +
  geom_point() +
  labs(x = "Educación", y = "Ideas Maternalistas") +
  theme_minimal()+
  ylim(0, 12)


