source("teoriadecision_funciones_incertidumbre.R")

tabla1 = crea.tablaX(c(1, 6, 3, 5,
                      2, 1, 1, 3,
                      5, 4, 2, 1,
                      1, 5, 2, 4),
                    numalternativas = 4, numestados = 4)

# Vamos a comparar las conclusiones según sea el caso favorable (beneficios)
# o desfavorable (costos)

## WALD
wald.ben<-criterio.Wald(tabla1, favorable = T)
wald.cos<-criterio.Wald(tabla1, favorable = F)

wald.ben
# Las cuatro alternativas son óptimas, con valor óptimo de 1
wald.cos
# La alternativa 2 es la óptima, con valor óptimo de 3


## OPTIMISTA
opt.ben<-criterio.Optimista(tabla1, favorable=T)
opt.cos<-criterio.Optimista(tabla1, favorable=F)

opt.ben
# La alternativa óptima es la 1 con valor óptimo de 6
opt.cos
# Las cuatro alternativas son óptimas con valor óptimo de 1


## HURWICZ (con alpha = 0.5)
hurw.ben<-criterio.Hurwicz(tabla1, alfa=0.5, favorable=T)
hurw.cos<-criterio.Hurwicz(tabla1, alfa=0.5, favorable=F)

hurw.ben
# La alternativa óptima es la 1 con valor óptimo de 3.5
hurw.cos
# La alternativa óptima es la 2 on valor óptimo de 2

# Vamos a dibujar este criterio, representando las alternativas óptimas en
# función del valor de alfa.
dibuja.criterio.Hurwicz(tabla1, favorable=T)
dibuja.criterio.Hurwicz_Intervalos(tabla1, favorable = T)
# No sé por qué da error
dibuja.criterio.Hurwicz(tabla1, favorable=F)
dibuja.criterio.Hurwicz_Intervalos(tabla1, favorable=F)
# Siempre es la alternativa 2 la favorable


# SAVAGE
sav.ben<-criterio.Savage(tabla1, favorable = T)
sav.cos<-criterio.Savage(tabla1, favorable = F)

sav.ben
# Las alternativas óptimas son la 1, 3 y 4 con valor óptimo de 4
sav.cos
# La alternativa óptima es la 2 con valor óptimo de 2


## LAPLACE
lap.ben<-criterio.Laplace(tabla1, favorable=T)
lap.cos<-criterio.Laplace(tabla1, favorable=F)

lap.ben
# La alternativa óptima es la 1 con valor óptimo de 3.75
lap.cos
# La alternativa óptima es la 2 con valor óptimo de 1.75


## PUNTO IDEAL
pid.ben<-criterio.PuntoIdeal(tabla1, favorable = T)
pid.cos<-criterio.PuntoIdeal(tabla1, favorable = F)

pid.ben
# La alternativa óptima es la 1 con valor óptimo de 4
pid.cos
# La alternativa óptima es la 2 con valor óptimo de 2.23

# Vemos que para el caso favorable, 1 siempre aparece como alternativa
# óptima. Con la alternativa 2 ocurre lo mismo en el caso desfavorable


### EJERCICIO 2

# Ana ha obtenido una oferta de trabajo en una empresa extranjera,
# y le dan la opción de teletrabajo o mudarse al país y trabajar presencialmente.
# Si trabaja online, la empresa le ofrece 1500 euros más horas extra, que suponen
# un 5% del sueldo mínimo, siendo los gastos diarios viviendo en su país de 500 euros.
# Si se muda y trabaja presencialmente, la empresa le permite realizar proyectos
# que supondrían un 15% extra del sueldo mínimo y las horas extra son un 7% del sueldo mínimo,
# pero el alojamiento y gastos
# diarios le supondrían 700 euros.
# La empresa está ahora mismo realizando un proyecto que debe
# estar acabado dentro de un año. Sin embargo, este proyecto está teniendo varios
# problemas y la empresa no sabe si para la fecha límite van a estar resueltos.
# En el caso de que salga todo bien, la empresa no tendría ningún problema futuro,
# pero si no logran terminar el proyecto, se verían obligados a despedir a
# las personas trabajando presencialmente y reducir el extra de las horas
# extra a un 4%.
# En caso de que Ana sea despedida de esa empresa, sabe que puede
# volver al trabajo que tiene actualmente con un ingreso mensual de 1300 euros.
# ¿Debería Ana trabajar online o mudarse?


# Alternativas: 
# d1: trabajar online
# d2: mudarse

# Estados de la naturaleza
# e1: La empresa consigue terminar el proyecto
# e2: la empresa no termina el proyecto 

# Problema de incertidumbre. Queremos maximizar los beneficios

m11<-1500+(0.05*1500)-500
m12<-1500+(0.04*1500)-500
m21<-1500+(0.07*1500)+(0.15*1500)-700
m22<-1300-500


tabla2<-crea.tablaX(c(m11, m12, m21, m22), 2, 2)
tabla2

criterio.Todos(tabla2, alfa=0.5, favorable = T)

# Todos los criterios coinciden en que Ana debería trabajar online, excepto
# el criterio Optimista, que concluye que Ana debería mudarse.
