# Programacion-Declarativa

Programa en Haskell que genera un intérprete imperativo sencillo.

Características del lenguaje:
Solo se consideran variables de tipo entero, que no hace falta declarar.
Hay tres tipos de instrucciones:

- Instrucción de asignación: x := e, donde x es una variable y e es una expresión aritmética.
Una expresión aritmética está formada por enteros, variables, sumas, restas y multiplicaciones
de otras expresiones.

- Instrucción condicional: Cond b p p', donde b es una expresión booleana y p, p' son programas.
Las expresiones booleanas son: comparaciones (=; <; >;<=;>=) de expresiones aritméticas,
negaciones, conjunciones y disyunciones de otras expresiones booleanas.

- Instrucción de bucle: While b p, donde b es una expresión booleana y p es un programa.
El efecto de ejecutar una instrucción es modificar el estado (valor actual) de las variables, de acuerdo
con la forma estándar de entender el significado de las instrucciones.

Un programa no es más que una secuencia de instrucciones que se ejecutan, claro, en secuencia.
El estado inicial de la ejecución de un programa no se determina en el propio programa, sino que
se supone dado de manera adicional al comenzar la ejecución.

El resultado final de la ejecución de un programa es el valor que tenga al final una variable distinguida
de nombre R.


Objetivo a implementar:
Tipos (o alias de tipos) para representar adecuadamente instrucciones, programas, expresiones
aritméticas, expresiones booleanas, estados, etc.

- Una función ejecuta:: Programa -> Estado -> Integer que proporcione el resultado de
la ejecución de un programa a partir de un estado inicial. Por supuesto, pueden ser necesarias
algunas funciones auxiliares.

- Sendas funciones de aridad cero factorial y s0 (para ejemplos) cuyos valores sean, respectivamente:
    - el programa para el cálculo del factorial escrito arriba, de acuerdo con la representación
      determinada por los tipos que se hayan definido,
    - el estado inicial en el que la variable X vale 3 y las demás no tienen valor definido.
