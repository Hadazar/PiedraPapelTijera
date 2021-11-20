
;Funci�n para generar un n�mero aleatorio
(defun random(/ aleatorio tiempo entero)

  ;Se crea una variable con la fecha y hora, y otra con la parte entera de este n�mero (es decir, solo con la fecha)
  (setq tiempo(getvar "cdate"))
  (setq entero(atoi(rtos tiempo)))

  ;Se extraen los segundos de la hora
  (repeat 3
    
    (setq tiempo(- tiempo entero))
    (setq tiempo(* 100 tiempo))
    (setq entero(atoi(rtos tiempo)))
  )

  ;Se calcula el modulo 3 de dichos segundos, generando un n�mero aleatorio entre 0 y 2
  (setq aleatorio(- entero (* 3 (/ entero 3))))
)

;Funci�n para redibujar las figuras que representan la piedra, el papel y las tijeras
(defun redibujarJugada (antigua jugada / propiedades origen ninguno)

  (setq propiedades (entget antigua))
  
  ;Se cambia la propiedad que establece la figura de la jugada
  (setq ninguno 1)
  (if (= jugada 0) (progn (setq propiedades (subst (cons 2 "roca") (assoc 2 propiedades) propiedades)) (setq ninguno 0)))
  (if (= jugada 1) (progn (setq propiedades (subst (cons 2 "papel") (assoc 2 propiedades) propiedades)) (setq ninguno 0)))
  (if (= jugada 2) (progn (setq propiedades (subst (cons 2 "tijeras") (assoc 2 propiedades) propiedades)) (setq ninguno 0)))
  (if (= ninguno 1) (setq propiedades (subst (cons 2 "piedra") (assoc 2 propiedades) propiedades)))
  (entmod propiedades)
)

;Funci�n para redibujar los rostros de los robots
(defun redibujarCara (antigua cara / propiedades origen)

   (setq propiedades (entget antigua))
  
  ;Se cambia la propiedad que establece el tipo de cara
  (cond
    ((= cara 0) (setq propiedades (subst (cons 2 "cara") (assoc 2 propiedades) propiedades))) 
    ((= cara 1) (setq propiedades (subst (cons 2 "feliz") (assoc 2 propiedades) propiedades))) 
    ((= cara 2) (setq propiedades (subst (cons 2 "brava") (assoc 2 propiedades) propiedades)))
   )
  (entmod propiedades)
)

;Funci�n que genara las palabras para editar los textos bajo las caras
(defun cambiarPalabra(numero / palabra)

  (cond
    ((= numero 0) (setq palabra "Empate")) 
    ((= numero 1) (setq palabra "Ganador")) 
    ((= numero 2) (setq palabra "Perdedor"))
   )
)

;Funci�n que edita los textos bajo las caras
(defun redibujarTexto (texto palabra / propiedades)

  (setq propiedades (entget texto))
  (setq propiedades (subst (cons 1 palabra) (assoc 1 propiedades) propiedades ))
  (entmod propiedades)
)

;Esta funci�n redibuja las caras, las figuras y los textos, haciendo un llamado a las otras funciones
(defun actualizar (numero1 numero2)

  (redibujarCara cara1 numero1)
  (redibujarCara cara2 numero2)
  (redibujarJugada jugada1 aleatorio)
  (redibujarJugada jugada2 jugador1)
  (redibujarTexto maquina (cambiarPalabra numero1))
  (redibujarTexto persona (cambiarPalabra numero2))
  (setq ninguno 0)

)

;Se establecen los n�meros de victorias iniciales en cero
(setq victorias1 0)
(setq victorias2 0)

;Se abarca todo el programa dentro de un ciclo, para poder disputar varias partidas
(setq duracion (getint "Introduzca el n�mero de partidas"))

(repeat duracion

;Se cargan todos los entity name de los objetos

(setq marcador1 (handent "296"))
(setq marcador2 (handent "29D"))
(setq maquina (handent "281"))
(setq persona (handent "288"))
(setq cara1 (handent "92A"))
(setq cara2 (handent "92B"))
(setq jugada1 (handent "92C"))
(setq jugada2 (handent "92D"))
  
;Se genera un n�mero aleatorio que representa la jugada de la m�quina, y se le solicita al usuario su jugada
(setq aleatorio (random))
(setq jugador1 (getint "Por favor, introduzca su jugada ('0' Piedra, '1' Papel, '2' Tijera): "))

;Se realiza la diferencia entre las jugadas de la m�quina y el jugador, como una referencia para saber quien gan�
(setq diferencia (- aleatorio jugador1))

;Se redibujan todos los objetos dependiendo del resultado de la variable diferencia
(setq ninguno 1)

;Empate
(if (= diferencia 0)(actualizar 0 0))

;Victoria del jugador
(if (= diferencia -1)(progn (actualizar 2 1)
		       	    (setq victorias2 (+ 1 victorias2))
		       	    (redibujarTexto marcador2 (strcat "l" (rtos victorias2) "l"))
		       	    ))
;Victoria de la m�quina
(if (= diferencia -2)(progn (actualizar 1 2)
		       	    (setq victorias1 (+ 1 victorias1))
		       	    (redibujarTexto marcador1 (strcat "l" (rtos victorias1) "l"))
		       	    ))
;Victoria de la m�quina
(if (= diferencia 1)(progn (actualizar 1 2)
		       	   (setq victorias1 (+ 1 victorias1))
		       	   (redibujarTexto marcador1 (strcat "l" (rtos victorias1) "l"))
		      	   ))
;Victoria del jugador
(if (= diferencia 2)(progn (actualizar 2 1)
		       	   (setq victorias2 (+ 1 victorias2))
		       	   (redibujarTexto marcador2 (strcat "l" (rtos victorias2) "l"))
		      	   ))
;Victoria de la m�quina (caso en el que el usuario no marca ninguno de los tres n�meros v�lidos (0, 1 � 2)
(if (= ninguno 1) (progn (actualizar 1 2)
		       	 (setq victorias1 (+ 1 victorias1))
		       	 (redibujarTexto marcador1 (strcat "l" (rtos victorias1) "l"))
		    	 ))
  
)

;Se realiza una pausa para poder observar el resultado final
(getint "Pulse un numero para reiniciar el marcador")

;Se reinician los marcadores en cero
(redibujarTexto marcador1 (strcat "l0l"))
(redibujarTexto marcador2 (strcat "l0l"))