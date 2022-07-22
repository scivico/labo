#este script necesita para correr en Google Cloud
# RAM     16 GB
# vCPU     4
# disco  256 GB


#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")


setwd( "~/buckets/b1/" )  #cambiar por la carpeta local

#leo el dataset
dataset  <- fread( "./exp/8221FEb/paquete_premium_ext.csv.gz", stringsAsFactors= TRUE)

#me quedo SOLO con los BAJA+2
dataset  <- dataset[  clase_ternaria =="BAJA+2"  & foto_mes>=202001  & foto_mes<=202011, ]
gc()

#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )


#valvula de seguridad para evitar valores infinitos
#paso los infinitos a NULOS
infinitos <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
infinitos_qty <- sum( unlist( infinitos) )
if( infinitos_qty > 0 )
{
  cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
  dataset[mapply(is.infinite, dataset)] <<- NA
}

#valvula de seguridad para evitar valores NaN que es 0/0
#paso los NaN a 0 , decision polemica si las hay
#se invita a asignar un valor razonable segun la semantica del campo creado
nans <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
nans_qty <- sum( unlist( nans) )
if( nans_qty > 0 )
{
  cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
  cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
  dataset[mapply(is.nan, dataset)] <<- 0
}

#los campos que arbitrariamente decido considerar para el clustering
#por supuesto, se pueden cambiar
campos_buenos  <- c( "ctrx_quarter","mcaja_ahorro","mtarjeta_visa_consumo","mdescubierto_preacordado_tend6","ctarjeta_visa_trx",
                     "mcuentas_saldo","mpasivos_margen_max3","mprestamos_personales_ratioavg6","Visa_status_max6","cpayroll_trx",
                     "mdescubierto_preacordado","mprestamos_personales_ratiomax6","mpayroll","mcaja_ahorro_lag1","mpasivos_margen",
                     "mtarjeta_visa_consumo_ratioavg6","foto_mes","mpasivos_margen_avg3","mcaja_ahorro_ratioavg6",
                     "cpayroll_trx_ratiomax6","ctarjeta_visa_trx_ratioavg6","ccomisiones_mantenimiento_tend6","cproductos_ratioavg3",
                     "mdescubierto_preacordado_delta1","mpayroll_ratioavg6","cproductos_ratioavg6","ctarjeta_visa_ratioavg6","mes",
                     "Visa_msaldopesos","mcuenta_corriente","mcuenta_corriente_lag1","ctarjeta_visa_tend6",
                     "ccomisiones_mantenimiento_ratioavg6","mcuentas_saldo_tend6","cpayroll_trx_ratioavg6","mprestamos_personales",
                     "Visa_Finiciomora","mcaja_ahorro_dolares_delta1","mcuentas_saldo_ratiomax6","mcuentas_saldo_ratioavg6",
                     "mcuenta_corriente_ratioavg6","ccaja_ahorro_ratioavg6","mprestamos_personales_tend6","mactivos_margen_avg3",
                     "ctrx_quarter_ratioavg6","ctrx_quarter_ratiomax6","thomebanking_ratioavg6","ccaja_ahorro_max6",
                     "mactivos_margen_min3","Visa_fechaalta_tend6","mcomisiones_mantenimiento_ratioavg6",
                     "mcomisiones_mantenimiento_tend6","mpayroll_min6","mcaja_ahorro_ratiomax6","cproductos_ratiomax6",
                     "ctarjeta_master_tend6","mtransferencias_recibidas","mactivos_margen","ccaja_seguridad","mcuenta_corriente_tend6",
                     "mcuenta_corriente_avg6","mtarjeta_visa_consumo_ratiomax6","Visa_mpagominimo","ccajas_trx_ratioavg6","mactivos_margen_avg6",
                     "cliente_edad_min6","chomebanking_trx_delta1","internet_ratioavg6","ccaja_ahorro_avg6","mtarjeta_master_consumo",
                     "Visa_fechaalta_lag1","ctarjeta_visa_trx_tend6","mactivos_margen_ratioavg6","cprestamos_personales_ratioavg6",
                     "Master_fechaalta_tend6","Visa_mpagominimo_max6","mcuenta_corriente_ratiomax6","mpagomiscuentas","Master_mlimitecompra_ratioavg6",
                     "chomebanking_trx_ratioavg6","mrentabilidad_annual_ratiomax6","Visa_mpagominimo_avg6","mtarjeta_visa_consumo_min6",
                     "Visa_fechaalta","cliente_edad_min3","Visa_fechaalta_ratioavg6","ctarjeta_visa_debitos_automaticos_ratioavg6","cliente_edad",
                     "mcaja_ahorro_dolares_ratiomax6","cproductos","mtransferencias_emitidas_max6","mactivos_margen_ratiomax6","Visa_Finiciomora_max6",
                     "mpayroll_avg6","mprestamos_personales_min6","Visa_mfinanciacion_limite_ratioavg6","Visa_fultimo_cierre_avg6","Master_mpagospesos_tend6",
                     "mpasivos_margen_ratioavg6","mrentabilidad_annual_ratioavg6","mpayroll_ratiomax6","mcomisiones_mantenimiento_max6","mpayroll_delta1",
                     "mcaja_ahorro_delta1","cpayroll_trx_avg6","Master_mfinanciacion_limite_lag1","mcaja_ahorro_dolares_ratioavg6","mcuentas_saldo_delta1",
                     "chomebanking_trx","ctransferencias_emitidas_max6","mcomisiones_otras_ratiomax6","mpasivos_margen_ratioavg3","mpasivos_margen_avg6",
                     "Visa_Finiciomora_lag1","thomebanking","ctarjeta_master_trx_ratioavg6","Master_status_max6","mtarjeta_visa_debitos_automaticos_ratiomax6",
                     "mtarjeta_visa_debitos_automaticos_ratioavg6","mtransferencias_recibidas_max6","Master_fechaalta_min6","ctarjeta_visa_trx_min6",
                     "Master_fultimo_cierre_avg6","cliente_edad_avg6","Master_fechaalta_lag1","mrentabilidad_annual_tend3","cliente_edad_max6",
                     "mactivos_margen_delta1","mrentabilidad_annual_delta1","ctrx_quarter_tend6","cliente_edad_lag1","cprestamos_personales_avg6",
                     "ctarjeta_visa_debitos_automaticos_tend6","mcomisiones_otras_max6","cproductos_tend6","mcuenta_debitos_automaticos_ratiomax6",
                     "ccallcenter_trx","mrentabilidad_max6","numero_de_cliente","mcaja_ahorro_min6","Master_fultimo_cierre_tend6","mcomisiones_min3",
                     "ccomisiones_otras_ratioavg6","ccomisiones_mantenimiento")

#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[  , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox=  TRUE )

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )


#primero, creo la carpeta donde van los resultados
dir.create( "./exp/", showWarnings= FALSE )
dir.create( "./exp/ST7620", showWarnings= FALSE )
setwd( "~/buckets/b1/exp/ST7620" )


#imprimo un pdf con la forma del cluster jerarquico
pdf( "cluster_jerarquico.pdf" )
plot( hclust.rf )
dev.off()


#genero 7 clusters
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=6 & distintos <=8 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)
  
  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]
  
  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( dataset,
        file= "cluster_de_bajas.txt",
        sep= "\t" )


#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

# dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
# dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
# dataset[  , mean(mcuentas_saldo),  cluster2 ]
# dataset[  , mean(chomebanking_trx),  cluster2 ]
# dataset[  , mean(cliente_edad),  cluster2 ]
# dataset[  , mean(cliente_antiguedad),  cluster2 ]
