#install.packages("sets", dependencies = T)

#Establecimiento de libreria
library(sets)

#Establecer universo 1
universo1 <- seq(0, 70, 1)

#Establecer universo 2
universo2 <- seq(350, 700, 1)

#Establecer universo 3
universo3 <- seq(0, 50, 1,)

#Establecer universo 4
universo4 <- seq(7000, 35000, 1)

#Establecer universo 5
universo5 <- seq(0, 100, 1)

#Creacion de las variables
variables <- set(
  presentacion = fuzzy_variable(mala = fuzzy_trapezoid_gset(corners = c(-2, 0, 10, 25),universe = universo1),
                                regular = fuzzy_trapezoid_gset(corners = c(15, 28, 45, 50),universe= universo1),
                                buena = fuzzy_trapezoid_gset(corners = c(38, 55, 70, 72),universe= universo1)),
                                
  cantidad = fuzzy_partition(varnames = c(poca = 350, media = 525, mucha = 700), sd = 50, universe = universo2),
                            
  sabor = fuzzy_partition(varnames = c(desagradable = 0, agradable = 25, delicioso = 50), sd = 6,universe = universo3),
                  
  precio = fuzzy_partition(varnames = c(barato = 7000, justo = 20000, excesivo = 35000), radius = 10000, FUN = fuzzy_cone,universe = universo4),
                          
  calificacion = fuzzy_variable(no_recomendable = fuzzy_trapezoid_gset(corners = c(-2, 0, 22, 30),universe= universo5),
                                recomendable = fuzzy_trapezoid_gset(corners = c(25, 35, 60, 70),universe= universo5),
                                muy_recomendable = fuzzy_trapezoid_gset(corners = c(60, 80, 100, 102),universe= universo5))
  )

#Definición de las reglas
reglas <-
  set(
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	poca	&&	sabor	%is%	desagradable	&&	precio	%is%	barato	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	poca	&&	sabor	%is%	desagradable	&&	precio	%is%	justo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	poca	&&	sabor	%is%	desagradable	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	poca	&&	sabor	%is%	agradable	&&	precio	%is%	barato	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	poca	&&	sabor	%is%	agradable	&&	precio	%is%	justo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	poca	&&	sabor	%is%	agradable	&&	precio	%is%	excesivo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	poca	&&	sabor	%is%	delicioso	&&	precio	%is%	barato	,	calificacion	%is%	muy_recomendable	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	poca	&&	sabor	%is%	delicioso	&&	precio	%is%	justo	,	calificacion	%is%	muy_recomendable	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	poca	&&	sabor	%is%	delicioso	&&	precio	%is%	excesivo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	media	&&	sabor	%is%	desagradable	&&	precio	%is%	barato	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	media	&&	sabor	%is%	desagradable	&&	precio	%is%	justo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	media	&&	sabor	%is%	desagradable	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	media	&&	sabor	%is%	agradable	&&	precio	%is%	barato	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	media	&&	sabor	%is%	agradable	&&	precio	%is%	justo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	media	&&	sabor	%is%	agradable	&&	precio	%is%	excesivo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	media	&&	sabor	%is%	delicioso	&&	precio	%is%	barato	,	calificacion	%is%	muy_recomendable	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	media	&&	sabor	%is%	delicioso	&&	precio	%is%	justo	,	calificacion	%is%	muy_recomendable	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	media	&&	sabor	%is%	delicioso	&&	precio	%is%	excesivo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	mucha	&&	sabor	%is%	desagradable	&&	precio	%is%	barato	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	mucha	&&	sabor	%is%	desagradable	&&	precio	%is%	justo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	mucha	&&	sabor	%is%	desagradable	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	mucha	&&	sabor	%is%	agradable	&&	precio	%is%	barato	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	mucha	&&	sabor	%is%	agradable	&&	precio	%is%	justo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	mucha	&&	sabor	%is%	agradable	&&	precio	%is%	excesivo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	mucha	&&	sabor	%is%	delicioso	&&	precio	%is%	barato	,	calificacion	%is%	muy_recomendable	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	mucha	&&	sabor	%is%	delicioso	&&	precio	%is%	justo	,	calificacion	%is%	muy_recomendable	),
    fuzzy_rule(	presentacion	%is%	buena	&&	cantidad	%is%	mucha	&&	sabor	%is%	delicioso	&&	precio	%is%	excesivo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	poca	&&	sabor	%is%	desagradable	&&	precio	%is%	barato	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	poca	&&	sabor	%is%	desagradable	&&	precio	%is%	justo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	poca	&&	sabor	%is%	desagradable	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	poca	&&	sabor	%is%	agradable	&&	precio	%is%	barato	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	poca	&&	sabor	%is%	agradable	&&	precio	%is%	justo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	poca	&&	sabor	%is%	agradable	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	poca	&&	sabor	%is%	delicioso	&&	precio	%is%	barato	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	poca	&&	sabor	%is%	delicioso	&&	precio	%is%	justo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	poca	&&	sabor	%is%	delicioso	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	media	&&	sabor	%is%	desagradable	&&	precio	%is%	barato	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	media	&&	sabor	%is%	desagradable	&&	precio	%is%	justo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	media	&&	sabor	%is%	desagradable	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	media	&&	sabor	%is%	agradable	&&	precio	%is%	barato	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	media	&&	sabor	%is%	agradable	&&	precio	%is%	justo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	media	&&	sabor	%is%	agradable	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	media	&&	sabor	%is%	delicioso	&&	precio	%is%	barato	,	calificacion	%is%	muy_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	media	&&	sabor	%is%	delicioso	&&	precio	%is%	justo	,	calificacion	%is%	muy_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	media	&&	sabor	%is%	delicioso	&&	precio	%is%	excesivo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	mucha	&&	sabor	%is%	desagradable	&&	precio	%is%	barato	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	mucha	&&	sabor	%is%	desagradable	&&	precio	%is%	justo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	mucha	&&	sabor	%is%	desagradable	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	mucha	&&	sabor	%is%	agradable	&&	precio	%is%	barato	,	calificacion	%is%	muy_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	mucha	&&	sabor	%is%	agradable	&&	precio	%is%	justo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	mucha	&&	sabor	%is%	agradable	&&	precio	%is%	excesivo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	mucha	&&	sabor	%is%	delicioso	&&	precio	%is%	barato	,	calificacion	%is%	muy_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	mucha	&&	sabor	%is%	delicioso	&&	precio	%is%	justo	,	calificacion	%is%	muy_recomendable	),
    fuzzy_rule(	presentacion	%is%	regular	&&	cantidad	%is%	mucha	&&	sabor	%is%	delicioso	&&	precio	%is%	excesivo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	poca	&&	sabor	%is%	desagradable	&&	precio	%is%	barato	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	poca	&&	sabor	%is%	desagradable	&&	precio	%is%	justo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	poca	&&	sabor	%is%	desagradable	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	poca	&&	sabor	%is%	agradable	&&	precio	%is%	barato	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	poca	&&	sabor	%is%	agradable	&&	precio	%is%	justo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	poca	&&	sabor	%is%	agradable	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	poca	&&	sabor	%is%	delicioso	&&	precio	%is%	barato	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	poca	&&	sabor	%is%	delicioso	&&	precio	%is%	justo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	poca	&&	sabor	%is%	delicioso	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	media	&&	sabor	%is%	desagradable	&&	precio	%is%	barato	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	media	&&	sabor	%is%	desagradable	&&	precio	%is%	justo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	media	&&	sabor	%is%	desagradable	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	media	&&	sabor	%is%	agradable	&&	precio	%is%	barato	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	media	&&	sabor	%is%	agradable	&&	precio	%is%	justo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	media	&&	sabor	%is%	agradable	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	media	&&	sabor	%is%	delicioso	&&	precio	%is%	barato	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	media	&&	sabor	%is%	delicioso	&&	precio	%is%	justo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	media	&&	sabor	%is%	delicioso	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	mucha	&&	sabor	%is%	desagradable	&&	precio	%is%	barato	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	mucha	&&	sabor	%is%	desagradable	&&	precio	%is%	justo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	mucha	&&	sabor	%is%	desagradable	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	mucha	&&	sabor	%is%	agradable	&&	precio	%is%	barato	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	mucha	&&	sabor	%is%	agradable	&&	precio	%is%	justo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	mucha	&&	sabor	%is%	agradable	&&	precio	%is%	excesivo	,	calificacion	%is%	no_recomendable	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	mucha	&&	sabor	%is%	delicioso	&&	precio	%is%	barato	,	calificacion	%is%	muy_recomendable	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	mucha	&&	sabor	%is%	delicioso	&&	precio	%is%	justo	,	calificacion	%is%	recomendable 	),
    fuzzy_rule(	presentacion	%is%	mala	&&	cantidad	%is%	mucha	&&	sabor	%is%	delicioso	&&	precio	%is%	excesivo	,	calificacion	%is%	recomendable 	)
  )

#Construccion del sistema
sistema <- fuzzy_system(variables, reglas)
sistema
plot(sistema)

#inferencia Inferencia #1
inferencia1 <- fuzzy_inference(sistema, list(presentacion = 50, cantidad = 450, sabor = 50, precio = 26000))
inferencia1
plot(inferencia1)

#Defuzificacion #1
def = gset_defuzzify(inferencia1, "centroid")
def

plot(sistema$variables$calificacion)
lines(inferencia1, col = "purple", lwd = 4)

#------------------------------------------------------

#inferencia Inferencia #2
inferencia2 <- fuzzy_inference(sistema, list(presentacion = 35, cantidad = 680, sabor = 2, precio = 8500))
inferencia2
plot(inferencia2)

#Defuzificacion #2
def = gset_defuzzify(inferencia2, "centroid")
def

#------------------------------------------------------

#inferencia Inferencia #3
inferencia3 <- fuzzy_inference(sistema, list(presentacion = 60, cantidad = 500, sabor = 85, precio = 20000))
inferencia3
plot(inferencia3)

#Defuzificacion #3
def = gset_defuzzify(inferencia3, "centroid")
def

#Analisis resultado
plot(sistema$variables$calificacion)
lines(inferencia1, col = "purple", lwd = 4)
lines(inferencia2, col = "red", lwd = 4)
lines(inferencia3, col = "blue", lwd = 4)
