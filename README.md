# pd4inline

 ![alt text](http://www.ludoteka.com/conetau.gif)


## Proyecto

El proyecto consiste en ralizar el clásico juego del 4 en linea, en el 
cual cada jugador,  por turno, va tirando una ficha en el tablero 
desde arriba intentando formar una linea de 4 fichas consecutivas 
de su color. Ya sea, de manera vertical, horizontal o diagonal.

Nuestra idea es simular dicho tablero permitiendo jugar al 
usuario contra la PC, donde la misma tratará de ganarle 
intentando formar su jugada consecutiva, como asi tambien 
jugando de manera estrategica para bloquear la fila consecutiva 
del oponente, evitando en caso de ser posible que gane el juego.
 
En caso que el jugador o la PC logren armar las 4 fichas 
consecutivas se mostrara un mensaje al usuario, indicando si gano 
o perdio, y tambien en caso de llenarse el tablero el mensaje de 
empate


## Requisitos Ejecutar Cuatro en linea
1. Descargar [Data.Matrix](http://goo.gl/CukqnJ)
2. Instalar *Data.Matrix* 
```$ cabal install matrix-0.3.5.0.tar.gz ``` 

## Clonar Repositorio
1. ```$ git clone https://github.com/LeoPuertas/pd4inline ``` 

## Compilar y ejecutar proyecto
1. Abrir GHC
2. ```Prelude> :l CuatroEnLinea ```

## Ejecutar Cuatro en linea
- Ejecutar funcion main
      
   ``` *CuatroEnLinea> main ```
- Luego de ejecutar main aparecerá un menú como el siguiente: Donde tendra que seleccionar un modo de juego
  
>     Inicio de Juego Cuatro en Linea 
>      Modos de Juego
>       1. 1 Jugador
>       2. 2 Jugadores
>       3. Salir

Se podrá jugar contra la pc o contra otro usuario.
En cualquiera de los dos casos se jugara por turnos, ingresando la columna donde se quiere realizar la jugada.

El objetivo es completar 4 piezas alineadas en horizontal, vertical o diagonal.

Cuando se logre el objetivo aparecera un mensaje mostrando quien fue el jugador ganador.


## Imágenes

![](http://goo.gl/iaCh8E)

![](https://goo.gl/VVQPzC)

![](https://goo.gl/ssGmhq)

![](https://goo.gl/XGWNA3)