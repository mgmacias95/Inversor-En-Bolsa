;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; ASESOR PARA INVERTIR EN BOLSA ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sistema experto para invertir en bolsa, basado en reglas.
; Hecho por Marta Gómez Macías
; Mayo 2016
; Universidad de Granada
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;; DATOS EMPRESA IBEX35 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftemplate ValorEmpresaIbex
    (field Nombre)                          ; nombre de la empresa
    (field Precio (type FLOAT))             ; precio en euros
    (field PerVarDia (type FLOAT))          ; % de variación del precio respecto al día anterior
    (field Capitalizacion (type INTEGER))   ; valor total de la empresa: precioAccion * n_acciones
    (field PER (type FLOAT))                ; capitalizacion / beneficio anual
    (field RPD (type FLOAT))                ; repartido a los accionistas por dividendos
    (field Tamanio)                         ; puede ser pequenio, mediano o grande
    (field PerIbex (type FLOAT))            ; % de capitalización respecto a la capitalización total del ibex
    (field EtiqPER)                         ; puede ser alto, medio o bajo
    (field EtiqRPD)                         ; puede ser alto, medio o bajo
    (field Sector)                          ; sector de la empresa
    (field PerVar5Dias (type FLOAT))        ; % de variación del precio respecto al de hace 5 días
    (field Perd3consec)                     ; Verdadero o Falso -> "bajando en los 3 últimos días"
    (field Perd5consec)                     ; Verdadero o Falso -> "bajando los 5 últimos días"
    (field PerVarRespSector5 (type FLOAT))  ; % de variación respecto del sector en los últimos 5 días
    (field VRS5)                            ; Verdadero o falso -> "% varación con respecto a sector últimos 5 días < -5"
    (field PerVarMen (type FLOAT))          ; % de variación del precio respecto al de hace un mes
    (field PerVarTri (type FLOAT))          ; % de variación del precio respecto al de hace 3 meses
    (field PerVarSem (type FLOAT))          ; % de variación del precio respecto al de hace 6 meses
    (field PerVarAn (type FLOAT))           ; % de variación del precio respecto al de hace 1 año
)

;;;;;;;;;;;;;;;;;;;;;;;;;; DATOS SECTOR IBEX35 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftemplate ValorSector
    (field Nombre)                          ; Nombre del sector
    (field PerVarDia (type FLOAT))          ; Media % variación dia en las empresas del sector
    (field Capitalizacion (type INTEGER))   ; Suma capitalización empresas del sector
    (field PER (type FLOAT))                ; Media del PER en las empresas del sector
    (field RPD (type FLOAT))                ; % repartido por dividendos en el último año
    (field PerIbex (type FLOAT))            ; % capitalización con respecto a la capitalización total del ibex
    (field PerVar5Dias (type FLOAT))        ; Media % variación en los últimos 5 días de las empresas del sector
    (field Perd3consec)                     ; Verdadero o falso -> "bajando 3 últimos días"
    (field Perd5consec)                     ; Verdadero o falso -> "bajando 5 últimos días"
    (field PerVarMen (type FLOAT))          ; Media % variación último mes en las empresas del sector
    (field PerVarTri (type FLOAT))          ; Media % variación último trimestre en las empresas del sector
    (field PerVarSem (type FLOAT))          ; Media % variación último semestre en las empresas del sector
    (field PerVarAn (type FLOAT))           ; Media % variación último año
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CARTERA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftemplate ValorCartera
    (field Empresa)
    (field Acciones (type FLOAT))
    (field ValorActual (type FLOAT))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Noticia ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftemplate Noticia
    (field Sobre)                       ; Empresa o sector afectado
    (field Tipo)                        ; Buena o mala
    (field Antiguedad (type INTEGER))   ; Días de antigüedad
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Módulo de lectura de datos ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;; BLOQUE PARA LEER DATOS DE EMPRESAS DEL IBEX ;;;;;;;;;;;;;;;;;;
(defrule leer_analisis
    =>
    (open "Analisis.txt" analisis)
    (assert (SeguirLeyendoAnalisis))
)

(defrule leer_analisis_from_file
    ?f <- (SeguirLeyendoAnalisis)
    =>
    (bind ?Leido (read analisis))
    (retract ?f)
    (if (neq ?Leido EOF) then
        (assert (ValorEmpresaIbex 
            (Nombre ?Leido) 
            (Precio (read analisis))
            (PerVarDia (read analisis))
            (Capitalizacion (read analisis))
            (PER (read analisis))
            (RPD (read analisis))
            (Tamanio (read analisis))
            (PerIbex (read analisis)) 
            (EtiqPER (read analisis))
            (EtiqRPD (read analisis))
            (Sector (read analisis))
            (PerVar5Dias (read analisis))
            (Perd3consec (read analisis))
            (Perd5consec (read analisis))
            (PerVarRespSector5 (read analisis))
            (VRS5 (read analisis))
            (PerVarMen (read analisis))
            (PerVarTri (read analisis))
            (PerVarSem (read analisis))
            (PerVarAn (read analisis))
            )
        )
        (assert (SeguirLeyendoAnalisis))
    )
)

(defrule close_analisis
    =>
    (close analisis)
    ; (assert (finLeerFicheros))
)
;;;;;;;;;;;;;;; FIN BLOQUE PARA LEER DATOS DE EMPRESAS DEL IBEX ;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;; BLOQUE PARA LEER DATOS SECTORES ;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule leer_sectores
    =>
    (open "AnalisisSectores.txt" sectores)
    (assert (SeguirLeyendoSectores))
)

(defrule leer_sectores_from_file
    ?f <- (SeguirLeyendoSectores)
    =>
    (bind ?Leido (read sectores))
    (retract ?f)
    (if (neq ?Leido EOF) then
        ; cada valor de sectores tiene la forma:
        (assert (ValorSector 
            (Nombre ?Leido)
            (PerVarDia (read sectores))
            (Capitalizacion (read sectores))
            (PER (read sectores))
            (RPD (read sectores))
            (PerIbex (read sectores))
            (PerVar5Dias (read sectores))
            (Perd3consec (read sectores))
            (Perd5consec (read sectores))
            (PerVarMen (read sectores))
            (PerVarTri (read sectores))
            (PerVarSem (read sectores))
            (PerVarAn (read sectores))
            )
        )
        (assert (SeguirLeyendoSectores))
    )
)

(defrule close_sectores
    =>
    (close sectores)
    ; (assert (finLeerFicheros))
)
;;;;;;;;;;;;;;;;;; FIN BLOQUE PARA LEER DATOS SECTORES ;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BLOQUE PARA LEER LA CARTERA DEL USUARIO ;;;;;;;;;;;;;;;;;;;;
(defrule leer_cartera
    =>
    (open "Cartera.txt" cartera)
    (assert (SeguirLeyendoCartera))
)

(defrule leer_cartera_from_file
    ?f <- (SeguirLeyendoCartera)
    =>
    (bind ?Leido (read cartera))
    (retract ?f)
    (if (neq ?Leido EOF) then
        (assert (ValorCartera 
            (Empresa ?Leido) 
            (Acciones (read cartera)) 
            (ValorActual (read cartera))
            )
        )
        (assert (SeguirLeyendoCartera))
    )
)

(defrule close_cartera
    =>
    (close cartera)
    ; (assert (finLeerFicheros))
)
;;;;;;;;;;;;;;;;;; FIN BLOQUE PARA LEER CARTERA DEL USUARIO ;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BLOQUE PARA LEER EL FICHERO DE NOTICIAS ;;;;;;;;;;;;;;;;;;;;
(defrule leer_noticias
    =>
    (open "Noticias.txt" noticias)
    (assert (SeguirLeyendonoticias))
)

(defrule leer_noticias_from_file
    ?f <- (SeguirLeyendonoticias)
    =>
    (bind ?Leido (read noticias))
    (retract ?f)
    (if (neq ?Leido EOF) then
        (assert (Noticia
            (Sobre ?Leido) 
            (Tipo (read noticias)) 
            (Antiguedad (read noticias))
            )
        )
        (assert (SeguirLeyendonoticias))
    )
)

(defrule close_noticias
    =>
    (close noticias)
    (assert (finLeerFicheros))
)
;;;;;;;;;;;;;;; FIN BLOQUE PARA LEER EL FICHERO DE NOTICIAS ;;;;;;;;;;;;;;;;;;;;

;-------------------------------------------------------------------------------
; regla de transición entre el módulo de lectura de ficheros y el de deducciones
;-------------------------------------------------------------------------------
(defrule transicionLectura
    (declare (salience -10)) ; esta debe ser la última regla del módulo en ejecutarse
    ?f <- (finLeerFicheros)
    =>
    (retract ?f)
    (assert (deducirValores))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Módulo de deducir valores ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;; BLOQUE PARA DEDUCIR VALORES INESTABLES ;;;;;;;;;;;;;;;;;;;;;;

;-------------------------------------------------------------------------------
; Los valores del sector de la construcción son inestables por defecto
;-------------------------------------------------------------------------------
(defrule deducir_valores_inestables_construccion
    (deducirValores)
    (ValorEmpresaIbex 
        (Nombre ?nombre)
        (Sector Construccion)
    )
    =>
    (assert (ValorInestable ?nombre (str-cat "La empresa " ?nombre " es inestable "
        "porque su sector es la construcción")))
)

;-------------------------------------------------------------------------------
; Si la economía está bajando, los valores del sector servicios son inestables
; por defecto. Para saber si la economía está bajando, nos fijamos en el sector 
; Ibex y sus pérdidas en los últimos 5 días.
;-------------------------------------------------------------------------------
(defrule deducir_valores_inestables_servicios
    (deducirValores)
    (ValorSector
        (Nombre Ibex)
        (Perd5consec true)
    )
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (Sector Servicios)
    )
    =>
    (assert (ValorInestable ?nombre (str-cat "La empresa " ?nombre " es inestable "
        "porque la economía está bajando y su sector es el servicios")))
)

;-------------------------------------------------------------------------------
; Si hay una noticia negativa sobre un valor, éste pasa a ser inestable durante
; dos días
;-------------------------------------------------------------------------------
(defrule deducir_valores_estables_noticias_negativas_empresa
    (deducirValores)
    (Noticia
        (Sobre ?nombre)
        (Tipo Mala)
        (Antiguedad ?ant)
    )
    (ValorEmpresaIbex
        (Nombre ?nombre)
    )
    (test (<= ?ant 2)) ; la noticia debe tener dos o menos días de antigüedad
    =>
    (assert (ValorInestable ?nombre (str-cat "La empresa " ?nombre " es inestable "
        "durante dos días porque ha surgido una noticia negativa donde está implicada")))
)

;-------------------------------------------------------------------------------
; Si hay una noticia negativa sobre un sector, éste pasa a ser inestable durante
; dos días
;-------------------------------------------------------------------------------
(defrule deducir_valores_inestables_notcias_negativas_sector
    (deducirValores)
    (Noticia
        (Sobre ?sector)
        (Tipo Mala)
        (Antiguedad ?ant)
    )
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (Sector ?sector)
    )
    (test (<= ?ant 2)) ; la noticia debe tener menos de dos días de antigüedad
    =>
    (assert (ValorInestable ?nombre (str-cat "La empresa " ?nombre " es inestable "
        "durante dos días porque ha surgido una noticia negativa donde su "
        "sector, " ?sector ", está implicado")))
)

;-------------------------------------------------------------------------------
; Si hay una noticia negativa sobre la econoía, todos los valores pasan a ser
; inestables durante dos días
;-------------------------------------------------------------------------------
(defrule deducir_valores_inestables_economia
    (deducirValores)
    (Noticia
        (Sobre Ibex)
        (Tipo Mala)
        (Antiguedad ?ant)
    )
    (test (<= ?ant 2))
    =>
    (assert (ValorInestable Todos (str-cat "Al surgir una noticia negativa sobre "
        "la economía, todos los valores pasan a ser inestables durante dos días")))
)
;;;;;;;;;;;;;;;; FIN BLOQUE PARA DEDUCIR VALORES INESTABLES ;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BLOQUE PARA DEDUCIR VALORES ESTABLES ;;;;;;;;;;;;;;;;;;;;;;;

;-------------------------------------------------------------------------------
; si hay una noticia positiva sobre un valor o su sector, un valor inestable deja
; de serlo durante dos días
;-------------------------------------------------------------------------------

; Versión noticia positiva sobre un valor
(defrule deducir_valores_estables_noticias_positivas_valor
    (deducirValores)
    (Noticia
        (Sobre ?nombre)
        (Tipo Buena)
        (Antiguedad ?ant)
    )
    (ValorInestable ?nombre $?)
    ?v <- (ValorInestable ?nombre $?)
    (test (< ?ant 2))
    =>
    (retract ?v)
    ; (printout t "Al surgir una noticia positiva sobre la empresa " ?nombre 
    ;     ", su valor ha dejado de ser inestable durante dos días" crlf)
)

; Versión noticia positiva sobre un sector
(defrule deducir_valores_estables_noticias_positivas_sector
    (deducirValores)
    (Noticia
        (Sobre ?sector)
        (Tipo Buena)
        (Antiguedad ?ant)
    )
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (Sector ?sector)
    )
    (ValorInestable ?nombre $?)
    ?v <- (ValorInestable ?nombre $?)
    (test (< ?ant 2))
    =>
    (retract ?v)
    ; (printout t "Al surgir una noticia positiva sobre el sector " ?sector 
    ;     ", el valor de la empresa " ?nombre " es estable durante dos días" crlf)
)
;;;;;;;;;;;;;;;;; FIN BLOQUE PARA DEDUCIR VALORES ESTABLES ;;;;;;;;;;;;;;;;;;;;;

;-------------------------------------------------------------------------------
; regla de transición entre el bloque de deducir valores y el de detectar
; valores peligrosos
;-------------------------------------------------------------------------------
(defrule transicionDeducir
    (declare (salience -10)) ; esta debe ser la última regla del módulo en ejecutarse
    ?d <- (deducirValores)
    =>
    (retract ?d)
    (assert (detectarValores))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; Módulo de detectar valores ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; peligrosos, sobrevalorados e infravalorados ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;; BLOQUE PARA DETECTAR VALORES PELIGROSOS ;;;;;;;;;;;;;;;;;;

;-------------------------------------------------------------------------------
; Si un valor, de la cartera del usuario, es inestable y está perdiendo de forma
; continua duante los últimos 3 días es peligroso
;-------------------------------------------------------------------------------
(defrule detectar_valores_peligrosos_perd3
    (detectarValores)
    (ValorCartera (Empresa ?nombre))
    (or (ValorInestable ?nombre $?) (ValorInestable Todos $?))
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (Perd3consec true)
    )
    =>
    (assert (ValorPeligroso ?nombre (str-cat "El valor " ?nombre " es peligroso "
        "porque es inestable y lleva tres días perdiendo")))
)

;-------------------------------------------------------------------------------
; Si un valor, de la cartera del usuario, está perdiendo durante los últimos 5 días
; y la variación en esos días con respecto a la variación del sector es mayor de
; un 5%, ese valor es peligroso
;-------------------------------------------------------------------------------
(defrule detectar_valores_peligrosos_perd5
    (detectarValores)
    (ValorCartera (Empresa ?nombre))
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (Perd5consec true)
        (VRS5 true)
    )
    =>
    (assert (ValorPeligroso ?nombre (str-cat "El valor " ?nombre " es peligroso "
        "porque está perdiendo durante los últimos 5 días y la variación con "
        "respecto al sector es mayor al -5%")))
)
;;;;;;;;;;;;;;;;;; FIN BLOQUE PARA DETECTAR VALORES PELIGROSOS ;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;; BLOQUE PARA DETECTAR VALORES SOBREVALORADOS ;;;;;;;;;;;;;;;;;;

;-------------------------------------------------------------------------------
; Si el PER es Alto y el RPD bajo, la empresa está sobrevalorada
;-------------------------------------------------------------------------------
(defrule detectar_valores_sobrevalorados_general
    (detectarValores)
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (EtiqPER Alto)
        (EtiqRPD Bajo)
    )
    =>
    (assert (ValorSobrevalorado ?nombre (str-cat "El valor " ?nombre " está "
        "sobrevalorado ya que su PER es alto y su RPD es bajo")))
)

;-------------------------------------------------------------------------------
; Caso empresa pequeña: si el PER es alto entonces la empresa está sobrevalorada
;-------------------------------------------------------------------------------
(defrule detectar_valores_sobrevalorados_empresa_peque1
    (detectarValores)
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (EtiqPER Alto)
        (Tamanio PEQUENIA)
    )
    =>
    (assert (ValorSobrevalorado ?nombre (str-cat "El valor " ?nombre " está "
        "sobrevalorado porque es una empresa pequeña y tiene un PER alto")))
)

;-------------------------------------------------------------------------------
; Caso empresa pequeña: Si el PER es mediano y el RPD es bajo la empresa está
; sobrevalorada
;-------------------------------------------------------------------------------
(defrule detectar_valores_sobrevalorados_empresa_peque2
    (detectarValores)
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (EtiqPER Medio)
        (EtiqRPD Bajo)
        (Tamanio PEQUENIA)
    )
    =>
    (assert (ValorSobrevalorado ?nombre (str-cat "El valor " ?nombre " está "
        "sobrevalorado porque es una empresa pequeña, tiene un PER mediano y un "
        "RPD bajo")))
)

;-------------------------------------------------------------------------------
; Caso empresa grande: Si el RPD es bajo y el PER es mediano o alto la empresa
; está sobrevalorada
;-------------------------------------------------------------------------------
(defrule detectar_valores_sobrevalorados_empresa_grande1
    (detectarValores)
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (EtiqRPD Bajo)
        (Tamanio GRANDE)
        (EtiqPER ?per)
    )
    (test (or (eq ?per Medio) (eq ?per Alto)))
    =>
    (assert (ValorSobrevalorado ?nombre (str-cat "El valor " ?nombre " está "
        "sobrevalorado porque es una empresa grande, tiene un PER mediano o "
        "alto y un RPD bajo")))
)

;-------------------------------------------------------------------------------
; Caso empresa grande: Si el RPD es mediano y el PER es alto la empresa está 
; sobrevalorada
;-------------------------------------------------------------------------------
(defrule detectar_valores_sobrevalorados_empresa_grande2
    (detectarValores)
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (EtiqPER Alto)
        (EtiqRPD Medio)
        (Tamanio GRANDE)
    )
    =>
    (assert (ValorSobrevalorado ?nombre (str-cat "El valor " ?nombre " está "
        "sobrevalorado porque es una empresa grande, tiene un PER alto o alto "
        "y un RPD mediano")))
)
;;;;;;;;;;;;;;; FIN BLOQUE PARA DETECTAR VALORES SOBREVALORADOS ;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; BLOQUE PARA DETECTAR VALORES INFRAVALORADOS ;;;;;;;;;;;;;;;;;;;;

;-------------------------------------------------------------------------------
; Si el PER es bajo y el RPD alto, la empresa está infravalorada
;-------------------------------------------------------------------------------
(defrule detectar_valores_infravalorados1
    (detectarValores)
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (EtiqPER Bajo)
        (EtiqRPD Alto)
    )
    =>
    (assert (ValorInfravalorado ?nombre (str-cat "El valor " ?nombre " está "
        "infravalorado porque tiene un PER bajo y un RPD alto")))
)

;-------------------------------------------------------------------------------
; Si la empresa ha caído bastante (más de un 30%) en los últimos 3, 6 o 12 meses,
; ha subido pero no mucho en el último mes, y el PER es bajo, la empresa está 
; infravalorada
;-------------------------------------------------------------------------------
(defrule detectar_valores_infravalorados2
    (detectarValores)
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (PerVarTri ?tri)
        (PerVarSem ?sem)
        (PerVarAn ?an)
        (PerVarMen ?men)
        (EtiqPER Bajo)
    )
    ; La empresa ha caído más de un 30% en los últimos 3, 6 o 12 meses
    (test (or (<= ?tri -30) (<= ?sem -30) (<= ?an -30)))
    ; La empresa ha subido, pero no mucho, en el último mes
    (test (< ?men 10)) ;consideramos que no ha subido si ha subido menos de un 10%
    (test (> ?men 0))
    =>
    (assert (ValorInfravalorado ?nombre (str-cat "El valor " ?nombre " está "
        "infravalorado porque tiene un PER bajo, ha caído bastante en los "
        "últimos 3, 6 o 12 meses pero este último mes ha subido un poco")))
)

;-------------------------------------------------------------------------------
; Si la empresa es grande, el RPD es alto y el PER mediano, además no está bajando
; y se comporta mejor que su sector, la empresa está infravalorada
;-------------------------------------------------------------------------------
(defrule detectar_valores_infravalorados3
    (detectarValores)
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (Tamanio GRANDE)
        (EtiqRPD Alto)
        (EtiqPER Medio)
        (Perd5consec false) ; se considera que no está bajando si lleva 5 días sin pérdidas
        (VRS5 false) ; si la variación con respecto al sector es mayor al -5, 
                     ; la empresa va mejor que su sector
    )
    =>
    (assert (ValorInfravalorado ?nombre (str-cat "El valor " ?nombre " está "
        "infravalorado porque tiene un PER mediano, un RPD alto, no está bajando "
        "y se comporta mejor que su sector")))
)
;;;;;;;;;;;;;;; FIN BLOQUE PARA DETECTAR VALORES INFRAVALORADOS ;;;;;;;;;;;;;;;;

;-------------------------------------------------------------------------------
; Regla transición entre el módulo de detectar valores y el de obtener propuestas
;-------------------------------------------------------------------------------
(defrule transicionDetectar
    (declare (salience -10)) ; esta debe ser la última regla del módulo en ejecutarse
    ?d <- (detectarValores)
    =>
    (retract ?d)
    (assert (buscarPropuestas))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Módulo de realización de propuestas ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;; BLOQUE DE BÚSQUEDA DE PROPUESTAS ;;;;;;;;;;;;;;;;;;;;;

; RE = rendimiento esperado = revalorización anual esperada + dividendos anuales esperados

                    ; Proponer vender valores peligrosos ;

;-------------------------------------------------------------------------------
; Si una empresa es peligrosa, ha bajado el último mes y ha bajado más de un 3%
; con respecto a su sector en el último mes, proponer vender las acciones de la
; empresa. RE = 20-rpd. 
; EXPLICACIÓN: la empresa es peligrosa por...; además está entrando en tendencia 
; bajista con respecto a su sector. Según mi estimación, existe una probabilidad 
; no despreciable de que pueda caer al cabo del año un 20%, aunque produzca rpd% 
; por dividendos perderíamos un 20-rpd%.
;-------------------------------------------------------------------------------
(defrule proponer_vender_valor_peligroso
    (buscarPropuestas)
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (PerVarMen ?men)
        (Sector ?sector)
        (RPD ?rpd)
    )
    (ValorSector
        (Nombre ?sector)
        (PerVarMen ?mensector)
    )
    (ValorPeligroso ?nombre ?comentario)
    (test (< ?men 0)) ; si la empresa ha bajado en el último mes, este valor será negativo
    (test (<= (- (abs ?men) (abs ?mensector)) -3)) ; si la empresa ha bajado más de un 3% con 
                                       ;respecto al sector, este valor será menor a -3
    =>
    (assert (VenderPeligroso ?nombre (- 20 (* ?rpd 100)) (str-cat ?comentario ", además, "
        "está entrando en tendencia bajista con respecto a su sector. Según mi "
        "estimación, existe una probabilidad no despreciable de que pueda caer "
        "al cabo del año un 20%, aunque produzca rpd% por dividendos, "
        "perderíamos " (- 20 (* ?rpd 100))  "%")))
)
                    ; Proponer invertir en empresas infravaloradas ;

;-------------------------------------------------------------------------------
; Si una empresa está infravalorada y el usuario tiene dinero para invertir, 
; proponer invertir el dinero en las acciones de la empresa:
; RE = (PERMedio - PER)*100 / (5 * PER) + RDP
; EXPLICACIÓN: esta empresa está infravalorada y seguramente el PER tienda al 
; PER medio en 5 años,  con lo que se debería revalorizar un (PERMedio - PER)*100/(5*PER)
; anual a lo que habría que sumar el RPD% de beneficios por dividendos.
;-------------------------------------------------------------------------------
(defrule proponer_comprar_valor_infravalorado
    (buscarPropuestas)
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (PER ?per)
        (RPD ?rpd)
        (Sector ?sector)
    )
    (ValorSector
        (Nombre ?sector)
        (PER ?persector)
    )
    (ValorInfravalorado ?nombre ?comentario)
    (ValorCartera
        (Empresa DISPONIBLE)
        (Acciones ?acciones)
    )
    (not 
        (ValorCartera 
            (Empresa ?nombre)
        )
    )
    (test (> ?acciones 0))  ; comprobamos que el usuario tiene dinero
    =>
    (if (not (eq ?per 0)) then
        (assert (ComprarInfravalorado ?nombre (+ (/ (* 100 (- ?persector ?per)) (* 5 ?per)) (* ?rpd 100))
            (str-cat ?comentario " y seguramente el PER tienda al PER medio en 5 años, "
            "con lo que se debería revalorizar un " (/ (* 100 (- ?persector ?per)) (* 5 ?per)) "% anual "
            "a lo que habría que sumar el " (* ?rpd 100) "% de beneficios por dividendos")))
    )
)
                    ; Proponer vender valores de empresas sobrevaloradas ;

;-------------------------------------------------------------------------------
; Si una empresa de mi cartera está sobrevalorada y el rendimiento por año < 5 + precio dinero,
; proponer vender las acciones de la empresa; RE = -RPD + (PER - PERMedioSector)/(5*PER)
; EXPLICACIÓN: Esta empresa está sobrevalorada, es mejor amortizar lo invertido, ya
; que seguramente el PER tan alto deberá bajar al PER medio del sector en unos 5
; años, con lo que se debería devaluar un (PER - PERMedioSector)*100/(5*PER)
; anual, así que aunque se pieda el RPD% de beneficios por dividendos saldría rentable.
;-------------------------------------------------------------------------------
(defrule proponer_vender_valor_sobrevalorado
    (buscarPropuestas)
    (ValorCartera (Empresa ?nombre))
    (ValorEmpresaIbex
        (Nombre ?nombre)
        (PER ?per)
        (RPD ?rpd)
        (PerVarAn ?an)
        (Precio ?precio)
        (Sector ?sector)
    )
    (ValorSector
        (Nombre ?sector)
        (PER ?persector)
    )
    (ValorSobrevalorado ?nombre ?comentario)
    (test (< (+ (* ?rpd 100) ?an) (+ ?precio 5))) ; rendimiento al año < precio + 5
    =>
    (if (not (eq ?per 0)) then
        (assert (VenderSobrevalorado ?nombre (* (- (/ (* (- ?per ?persector) 100) (* 5 ?per)) (* ?rpd 100)) 100)
            (str-cat ?comentario ", es mejor amortizar lo invertido, ya que seguramente "
                "el PER tan alto deberá bajar al PER medio del sector en unos 5 años, "
                "con lo que se debería devaluar un " (/ (* (- ?per ?persector) 100) (* 5 ?per))
                "% anual, así  que aunque se pierda el " (* ?rpd 100) "% de beneficios por "
                "dividendos saldría rentable.")))
    )
)

            ; Proponer cambiar una inversión a valores más rentables ;

;-------------------------------------------------------------------------------
; Si una empresa (empresa1) no está sobrevalorada y su RPD es mayor que el 
; (revalorización por semestre + RPD + 1) de una empresa de mi cartera (empresa2)
; que no está infravalorada, proponer cambiar las acciones de una empresa por las
; de la otra, RE = (RPD empresa1 - (revalorización por semestre empresa2 + RPD empresa2 +1))
; EXPLICACIÓN: empresa1 debe tener una revalorización acorde con la evolución de
; la bolsa. Por dividendos se espera un RPD%, que es más de lo que te está dando
; empresa2, por eso te propongo cambiar los valores por los de esta otra 
; (rendimiento por año obtenido de revalorización + RPD de beneficios). Aunque se
; pague el 1% del coste del cambio te saldría rentable.
;-------------------------------------------------------------------------------
(defrule proponer_cambiar_rentable
    (buscarPropuestas)
    (ValorEmpresaIbex
        (Nombre ?empresa1)
        (RPD ?rpd1)
        (PerVarSem ?sem1)
    )
    (ValorEmpresaIbex
        (Nombre ?empresa2)
        (RPD ?rpd2)
        (PerVarSem ?sem2)
    )
    (ValorCartera (Empresa ?empresa2))
    (not (ValorCartera (Empresa ?empresa1)))
    (not (ValorInfravalorado ?empresa2 $?))
    (not (ValorSobrevalorado ?empresa1 $?))
    (test (< (+ (* ?rpd2 100) ?sem2 1) (* ?rpd1 100))) ; el 1 viene del coste de vender y comprar (0.5% por cada acción)
    =>
    (assert (Cambiar ?empresa1 ?empresa2 (- (* ?rpd1 100) (+ (* ?rpd2 100) ?sem2 1)) 
        (str-cat ?empresa1 " debe tener una revalorización acorde con la evolución "
            "de la bolsa. Por dividendos se espera un " (* ?rpd1 100) "%, que es "
            "más de lo que te está dando " ?empresa2 ", por eso te propongo cambiar "
            "los valores por los de esta otra, donde obtendrás un " (+ ?sem2 (* ?rpd2 100)) "% de beneficios. "
            "Aunque se pague el 1% del coste del cambio te saldría rentable.")))
)
;;;;;;;;;;;;;;;;;;; FIN BLOQUE DE BÚSQUEDA DE PROPUESTAS ;;;;;;;;;;;;;;;;;;;;;;;

;-------------------------------------------------------------------------------
; Transición entre el bloque de buscar propuestas y el de mostrarlas al usuario
;-------------------------------------------------------------------------------
(defrule transicion
    (declare (salience -10))
    ?b <- (buscarPropuestas)
    ?m <- (maximo ?max)
    ?n <- (numpropuestas ?nump)
    =>
    (retract ?b)
    (retract ?m)
    (retract ?n)
    (assert (maximo 0))
    (assert (numpropuestas 0))
    (assert (proponerCosas))
)

;;;;;;;;;;;;;;;;;;; BLOQUE DE REALIZACIÓN DE PROPUESTAS ;;;;;;;;;;;;;;;;;;;;;;;;

;-------------------------------------------------------------------------------
; Hechos iniciales: máximo RE encontrado y el número de propuestas realizadas
;-------------------------------------------------------------------------------
(deffacts max
    (maximo 0)
    (numpropuestas 0)
)


;-------------------------------------------------------------------------------
; regla para detectar el primer máximo RE de todas las propuestas de cambio
;-------------------------------------------------------------------------------
(defrule maximoREInicial
    (proponerCosas)
    (or (Cambiar ?e1 ?e2 ?RE ?com)
        (VenderSobrevalorado ?e ?RE ?com)
        (ComprarInfravalorado ?e ?RE ?com)
        (VenderPeligroso ?e ?RE ?com)
    )
    (maximo ?n)
    (numpropuestas ?np)
    ?m <- (maximo ?n)
    (test (> ?RE ?n))
    (test (< ?np 5))
    =>
    (assert (Propuesta ?RE ?com))
    (retract ?m)
    (assert (maximo ?RE))
)

;-------------------------------------------------------------------------------
; Regla para ir limpiando propuestas inútiles.
; Esta regla tiene mayor salience porque debe ejecutarse antes que la anterior.
; La anterior sólo sirve para encontrar una primera propuesta y ya después,
; la que debe ejecutarse es esta.
;-------------------------------------------------------------------------------
(defrule maximoRE
    (declare (salience 1))
    (proponerCosas)
    (or (Cambiar ?e1 ?e2 ?RE ?com)
        (VenderSobrevalorado ?e ?RE ?com)
        (ComprarInfravalorado ?e ?RE ?com)
        (VenderPeligroso ?e ?RE ?com)
    )
    (maximo ?n)
    ?m <- (maximo ?n)
    (numpropuestas ?np)
    (test (> ?RE ?n))
    (test (< ?np 5))
    ?p <- (Propuesta ?n ?c)
    =>
    (retract ?p)
    (retract ?m)
    (assert (maximo ?RE))
    (assert (Propuesta ?RE ?com))
)

;-------------------------------------------------------------------------------
; Una vez obtenida una propuesta, reiniciamos el máximo y borramos ese hecho.
; Para ello, según la propuesta sea de cambiar, de vender (sobrevalorado o 
; peligroso) o de comprar, se lanzará un hecho u otro.
; Versión para cambiar.
;-------------------------------------------------------------------------------
(defrule imprimirPropuestaCambiar
    (proponerCosas)
    ?p <- (Propuesta ?RE ?explicacion)
    ?n <- (numpropuestas ?np)
    ?m <- (maximo ?RE)
    ?c <- (Cambiar ?e1 ?e2 ?RE ?com)
    (test (< ?np 5))
    =>
    (printout t (+ ?np 1) " Te propongo cambiar tus acciones de " ?e2 " por las de " 
        ?e1 " ya que "  ?explicacion " RENDIMIENTO ESPERADO = " ?RE crlf)
    (retract ?c)
    (retract ?p)
    (retract ?n)
    (retract ?m)
    (assert (maximo 0))
    (assert (numpropuestas (+ ?np 1)))
    (assert (PropuestaCambiar ?e1 ?e2 ?RE (+ ?np 1)))
)

;-------------------------------------------------------------------------------
; Una vez obtenida una propuesta, reiniciamos el máximo y borramos ese hecho.
; Para ello, según la propuesta sea de cambiar, de vender (sobrevalorado o 
; peligroso) o de comprar, se lanzará un hecho u otro.
; Versión para vender.
;-------------------------------------------------------------------------------
(defrule imprimirPropuestaVender
    (proponerCosas)
    ?p <- (Propuesta ?RE ?explicacion)
    ?n <- (numpropuestas ?np)
    ?m <- (maximo ?RE)
    (or ?v <- (VenderSobrevalorado ?e ?RE ?com)
        ?v <- (VenderPeligroso ?e ?RE ?com)
    )
    (test (< ?np 5))
    =>
    (printout t (+ ?np 1) " Te propongo vender tus acciones de " ?e 
        ", ya que" ?explicacion " RENDIMIENTO ESPERADO = " ?RE crlf)
    (retract ?v)
    (retract ?p)
    (retract ?n)
    (retract ?m)
    (assert (maximo 0))
    (assert (numpropuestas (+ ?np 1)))
    (assert (PropuestaVender ?e ?RE (+ ?np 1)))
)

;-------------------------------------------------------------------------------
; Una vez obtenida una propuesta, reiniciamos el máximo y borramos ese hecho.
; Para ello, según la propuesta sea de cambiar, de vender (sobrevalorado o 
; peligroso) o de comprar infravalorado se lanzará un hecho u otro.
; Versión para comprar.
;-------------------------------------------------------------------------------
(defrule imprimirPropuestaComprar
    (proponerCosas)
    ?p <- (Propuesta ?RE ?explicacion)
    ?n <- (numpropuestas ?np)
    ?m <- (maximo ?RE)
    ?c <- (ComprarInfravalorado ?e ?RE ?com)
    (test (< ?np 5))
    =>
    (printout t (+ ?np 1) " Te propongo comprar acciones de " ?e 
        ", ya que " ?explicacion " RENDIMIENTO ESPERADO = " ?RE crlf)
    (retract ?c)
    (retract ?p)
    (retract ?n)
    (retract ?m)
    (assert (maximo 0))
    (assert (numpropuestas (+ ?np 1)))
    (assert (PropuestaComprar ?e ?RE (+ ?np 1)))
    (assert (numpropuestas (+ ?np 1)))
)

;-------------------------------------------------------------------------------
; Regla para leer la elección del usuario
;-------------------------------------------------------------------------------
(defrule preguntarPorPropuestas
    (proponerCosas)
    ?n <- (numpropuestas ?np)
    (test (<= ?np 5))
    =>
    (printout t (+ ?np 1) " No, he acabado" crlf)
    (printout t "¿Quieres aplicar alguno de estos consejos? Selecciona el número" crlf)
    (bind ?consejo (read))
    (retract ?n)
    (assert (Consejo ?consejo))
    (if (< ?np ?consejo) then
        (assert (salir))
    )
)

;-------------------------------------------------------------------------------
; Regla para salir del programa
;-------------------------------------------------------------------------------
(defrule AplicarConsejoDespedida
    (proponerCosas)
    (salir)
    ?m <- (maximo ?max)

    =>
    (retract ?m)
    (printout t "¡Hasta pronto!" crlf)
)

;-------------------------------------------------------------------------------
; Si el consejo seleccionado es uno de cambiar un valor, se disparará esta regla.
; Elimina de la cartera el antiguo valor y guarda el nuevo, con el 5% de las 
; acciones que tenía el otro
;-------------------------------------------------------------------------------
(defrule AplicarConsejoCambiar
    (proponerCosas)
    ?c <- (Consejo ?consejo)
    ?prob <- (PropuestaCambiar ?e1 ?e2 ?RE ?consejo)
    ?car <- (ValorCartera
        (Empresa ?e2)
        (Acciones ?accs)
        (ValorActual ?dineroempresa)
    )
    ?disp <- (ValorCartera
        (Empresa DISPONIBLE)
        (ValorActual ?dinero)
    )
    (ValorEmpresaIbex
        (Nombre ?e1)
        (Precio ?precio)
    )
    =>
    (retract ?car)
    (retract ?c)
    (retract ?disp)
    (retract ?prob)
    (assert (ValorCartera 
                (Empresa ?e1)
                (Acciones ?accs)
                (ValorActual (* ?accs ?precio))
            )
    )
    (printout t "Actualizada tu cartera. Añadida la empresa " ?e1 " con " ?accs " acciones" crlf)
    (printout t "Ahora tienes " (- (+ ?dinero ?dineroempresa) (* ?accs ?precio))
        " euros en tu cartera." crlf)
    ; Al vender las acciones de la empresa2, en la cartera obtendremos:
    ;      (DineroDisponible + ValorAccionEmpresa2)
    ; Al comprar las acciones de la empresa1, en la cartera obtendremos:
    ;  (DineroDisponible + ValorAccionEmpresa2) - (numAccionesEmpresa2 * PrecioAccionesEmpresa1)
    (assert (ValorCartera
                (Empresa DISPONIBLE)
                (Acciones (- (+ ?dinero ?dineroempresa) (* ?accs ?precio)))
                (ValorActual (- (+ ?dinero ?dineroempresa) (* ?accs ?precio)))
            )
    )
    (assert (borrarPropuestas))
)

;-------------------------------------------------------------------------------
; Si el consejo seleccionado es uno de vender un valor, se disparará esta regla.
; Elimina de la cartera el valor y suma a la cantidad disponible su valor actual
;-------------------------------------------------------------------------------
(defrule AplicarConsejoVender
    (proponerCosas)
    ?c <- (Consejo ?consejo)
    ?prob <- (PropuestaVender ?e ?RE ?consejo)
    ?car <- (ValorCartera
                (Empresa ?e)
                (ValorActual ?valor)
            )
    ?disp <- (ValorCartera
                (Empresa DISPONIBLE)
                (ValorActual ?dinero)
             )
    =>
    (retract ?c)
    (retract ?car)
    (retract ?disp)
    (retract ?prob)
    ; Al vender un valor sobrevalorado, el valor que obtenemos en la cartera es
    ;       (DineroQueTeniamosDeAntes + ValorDeLaAccion)
    (assert (ValorCartera
                (Empresa DISPONIBLE)
                (Acciones (+ ?dinero ?valor))
                (ValorActual (+ ?dinero ?valor))
            )
    )
    (printout t "Actualizada tu cartera. Eliminada la empresa " ?e crlf)
    (printout t "Ahora tienes " (+ ?dinero ?valor) " euros en tu cartera." crlf)
    (assert (borrarPropuestas))
)

;-------------------------------------------------------------------------------
; Si el consejo seleccionado es uno de comprar un valor, se disparará esta regla.
; Añade a la cartera el nuevo valor con un 5% del número de acciones que puede
; comprar el usuario.
;-------------------------------------------------------------------------------
(defrule AplicarConsejoComprar
    (proponerCosas)
    ?c <- (Consejo ?consejo)
    ?prob <- (PropuestaComprar ?e ?RE ?consejo)
    ?disp <- (ValorCartera
                (Empresa DISPONIBLE)
                (Acciones ?dinero)
                (ValorActual ?dinero)
            )
    (ValorEmpresaIbex
        (Nombre ?e)
        (Precio ?p)
    )
    =>
    (retract ?c)
    (retract ?disp)
    (retract ?prob)
    (printout t "Te recomiendo invertir el 5% de tus acciones, para no tener "
        "todo invertido en la misma empresa" crlf)
    (assert (ValorCartera
                (Empresa DISPONIBLE)
                (ValorActual (- ?dinero (* 0.05 ?dinero)))
                (Acciones (- ?dinero (* 0.05 ?dinero)))
            )
    )
    (assert (ValorCartera
                (Empresa ?e)
                (Acciones (/ (* 0.05 ?dinero) ?p))
                (ValorActual (* 0.05 ?dinero))
            )
    )
    (printout t "Actualizada tu cartera. Añadida la empresa " ?e 
        " con " (/ (* 0.05 ?dinero) ?p) " acciones" crlf)
    (printout t "Ahora tienes " (- ?dinero (* 0.05 ?dinero))
        " euros en tu cartera." crlf)
    (assert (borrarPropuestas))
)

(defrule borrar_propuestas
    (declare (salience 10))
    (borrarPropuestas)
    (or ?p <- (PropuestaCambiar $?)
        ?p <- (PropuestaComprar $?)
        ?p <- (PropuestaVender $?)
        ?p <- (ComprarInfravalorado $?)
        ?p <- (VenderPeligroso $?)
        ?p <- (VenderSobrevalorado $?)
        ?p <- (Cambiar $?)
        ?p <- (ValorPeligroso $?)
        ?p <- (ValorSobrevalorado $?)
        ?p <- (ValorInfravalorado $?)
    )
    =>
    (retract ?p)
    (assert (repetirCiclo))
)

(defrule repetir_ciclo
    ; (declare (salience 5))
    ?p <- (proponerCosas)
    ?r <- (repetirCiclo)
    ?b <- (borrarPropuestas)
    =>
    (retract ?r)
    (retract ?b)
    (retract ?p)
    (assert (numpropuestas 0))
    (assert (detectarValores))
    ; (assert (buscarPropuestas))
    (printout t "Borradas todas las propuestas anteriores. Buscando nuevas según tu nueva cartera..." crlf)
)
