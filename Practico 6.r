
#Ejercicio practico 6

# Integrantes:
# - Ekaterina Cornejo 20.187.903-5
# - Catalina Y��ez 19.516.593-9
# - Aldo Castillo 19.839.621-4



# Pregunta A --------------------------------------------------------------


"
Contexto:

Una de las primeras preguntas a responder por el �ltimo estudio nacional de obesidad infantil fue si exist�an
diferencias en la prevalencia de la obesidad entre ni�os y ni�as o si, por el contrario, el porcentaje de obesos no
var�a entre sexos. Se les solicita responder esta pregunta, contando con las primeras observaciones obtenidas en
el estudio sobre una muestra de 14 menores:
"

#Hip�tesis
#H0: El porcentaje de obesidad no var�a entre sexos (es independiente del sexo)
#H1: El porcentaje de obesidad s� var�a entre sexos (s� depende del sexo)

# Construir la tabla de contingencia
obesidad <- c(rep("S�", 8), rep("No", 6))
sexo <- c(rep("Ni�o", 7), rep("Ni�a", 5), rep("Ni�o", 2))
datos <- data.frame(sexo, obesidad)
tabla <- xtabs(~. , datos)

# Aplicar prueba exacta de Fisher
alpha <- 0.05
prueba <- fisher.test(tabla, 1 - alpha)
print(prueba)

"
Se obtiene un valor p = 0.09091, y con un valor de significaci�n alpha = 0.05, se
falla al rechazar la hip�tesis nula H0. Por lo tanto, se concluye con un 95% de 
confianza que el porcentaje de obesidad no var�a entre sexos.

"


# Pregunta B --------------------------------------------------------------

"
Contexto:

En un art�culo de Garc�a y colaboradores (2010) se describe un estudio en que 
se compararon diferentes versiones de algoritmos evolutivos para resolver variadas
instancias de problemas de clasificaci�n tomadas desde el repositorio UCI Machine 
Learning. Suponga que la siguiente tabla muestra los resultados de la clasificaci�n 
hecha por dos versiones de un algoritmo gen�tico evaluado en el estudio para el 
problema Breast Cancer. �Consigue uno de los algoritmos mejor desempe�o?"

#Hip�tesis
#H0: El AGv1 tiene igual desempe�o que el AGv2
#H1: El AGv1 no tiene igual desempe�o que el AGv2


#Se ingresan los datos de la tabla
algoritmo <- seq(1:12)
AGv1 <- c(rep("Correcto", 5), rep("Incorrecto", 7))
AGv2 <- c(rep("Correcto", 3), rep("Incorrecto", 3), rep("Correcto", 6))

#Se genera la tabla de datos
datos <- data.frame(algoritmo, AGv1, AGv2)
tabla <- table (AGv1, AGv2)
addmargins(tabla)

# Aplicar prueba de McNemar
prueba <- mcnemar.test(tabla)
print(prueba)

"
Se obtiene un valor p de 0.2888, y con un nivel de significaci�n alpha de 0.05, 
no hay evidencia suficiente para rechazar la hip�tesis nula H0 (p > alpha). Por 
lo tanto, se puede concluir con un 95% de confianza que ambos algoritmos tienen 
el mismo desempe�o.

"


# Pregunta F --------------------------------------------------------------

"
Contexto:

F. Un estudio sobre las creencias de los estadounidenses acerca del origen y desarrollo de los seres humanos lleva
haci�ndose regularmente desde hace d�cadas, con las siguientes opciones:

  1. Human beings have developed over millions of years from less advanced forms of life, but God guided this
     process.
  2. Human beings have developed over millions of years from less advanced forms of life, but God had no part in
     this process.
  3. God created human beings pretty much in their present form at one time within the last 10,000 years or so.
  
1.019 personas fueron consultadas en 2010 sobre cu�l de las opciones anteriores representaba mejor su punto
de vista: 387 se inclinaron por la opci�n 1, 163 por la opci�n 2, 408 por la opci�n 3 y 61 personas no supieron o 
no quisieron responder. En el a�o 2007, esta misma encuesta registr� las siguientes proporciones: 38% opci�n 1,
14% opci�n 2, 43% opci�n 3 (m�s detalles en https://news.gallup.com/poll/145286/Four-Americans-BelieveStrict-Creationism.aspx). 
�Cambiaron las creencias de los estadounidenses acerca del origen y desarrollo de los seres humanos entre 2007 y 2010?
"

#H0: Las creencias de los estadounidenses son las mismas entre los a�os 2010 y 2017
#H1: Las creencias de los estadounidenses no son las mismas entre los a�os 2010 y 2017

# Se contruye la matriz de datos
dosmildiez<- c(38,16,40,6)#387,163,408,61, de un total de 1019 en porcentajes
dosmildiezysiete <- c(38, 14, 43,5) # no supieron:5%

tabla <- as.table ( rbind (dosmildiez , dosmildiezysiete ))
dimnames(tabla)<-list ( a�o = c("dosmildiez", "dosmildiezysiete"),
                        opciones=c("uno", " dos", " tres", " ninguna"))

# Prueba chi - cuadrado de Bondad de ajuste
prueba <-chisq.test (tabla , correct = FALSE )
print( prueba )

"
El valor p resultante es p = 0.9538; 823, por lo que se falla al rechazar 
la hip�tesis nula con niveles de significaci�n por defecto (0.05) 
En consecuencia, se concluye con 95% de confianza que las creencias de los 
estadounidenses  acerca del origen y desarrollo de los seres humanos 
entre 2007 y 2010 no han cambiado.
"
