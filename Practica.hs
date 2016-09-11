{-Carmen Acosta Morales
---PRACTICA DE PROGRAMACIÓN DECLARATIVA-}
{-Para realizar la interacción con menú por consola basta con poner usuario en el terminal y 
aparecen todas las opciones las cuales van indicando los pasos a seguir.-}
import System.IO
import Data.Char

{-Función que recibe una lista de cifrado y un nombre de función y 
aplica la función al fichero cuyo nombre mete el usuario por consola. 
La función deberá ser una función de codificación o decodificación-}
cifradoLista:: [(Char,Char)]-> ([Char] -> Int -> [(Char,Char)]->[Char]) -> IO()
cifradoLista lista funcion = do 
                    putStr ("Escríbeme el nombre del fichero \n") 
                    file <- getLine
                    ys <- readFile file 
                    putStr (funcion ys (length lista -1) lista)

{-Lo mismo que la función de cifradoLista salvo que solo realiza las codificaciones y 
decodificaciones sobre la codificación estandar y solo es usable con las funciones de
"decodificar" y "codificar" mas rudimentarias (las dos del final).-}
cifradoEstandar :: (String -> [Char])-> IO()
cifradoEstandar funcion = do 
                    putStr ("Escríbeme el nombre del fichero \n") 
                    xs <- getLine 
                    ys <- readFile xs 
                    putStr (funcion ys)

{-Se le pide al usuario el nombre del fichero a analizar y calcula el número de veces
que aparece cada letra, ordenado de forma decreciente por el número de apariciones-}
estadisticaLetras:: IO()
estadisticaLetras = do 
                    putStr ("Escríbeme el nombre del fichero \n") 
                    xs <- getLine 
                    ys <- readFile xs 
                    putStrLn("********************")
                    putStrLn ("--Orden aparición   "++ "Letra   " ++ "Numero de apariciones.")
                    mostrar(quickSort(estadLetras [] ys)) 0 (length ys)

{-Se le pide al usuario el nombre del fichero a analizar y calcula el número de veces
que aparece cada palabra, ordenadolo de forma decreciente por el número de apariciones-}
estadisticaPalabras:: IO()
estadisticaPalabras = do 
                    putStr ("Escríbeme el nombre del fichero \n") 
                    xs <- getLine 
                    ys <- readFile xs 
                    putStrLn("********************")
                    putStrLn ("--Orden aparición   "++ "Palabra   " ++ "Numero de apariciones.")
                    mostrarPalabras(quickSort(estadPalabras [] (words ys))) 0 

{-Función principal de interacción con el usuario usando un menú de interacción-}
usuario:: IO()
usuario = do 
          putStrLn("___________________________________________________________")
          putStrLn("***********************************************************")
          putStrLn("-----------------------------------------------------------")
          putStrLn("¿Que acción te gustaría realizar?")
          putStrLn("1- Codificar archivo de texto con codificación estandar.")
          putStrLn("2- Decodificar archivo de texto con codificación estandar.")
          putStrLn("3- Sacar estadísticas de letras de un archivo de texto.")
          putStrLn("4- Sacar estadísticas de palabras de un archivo de texto.")
          putStrLn("5- Crear lista de codificación y codificar.")
          putStrLn("Cualquier otro número para salir.")
          line <- getInt
          if line == 1 then do cifradoLista estandar codiMejorado 
                               usuario
                       else if line == 2 then do cifradoLista estandar decoMejorado 
                                                 usuario
                                         else if line == 3 then do estadisticaLetras
                                                                   usuario
                                                           else if line == 4 then do estadisticaPalabras
                                                                                     usuario
                                                                             else if line == 5 then do crearCodi []
                                                                                                       usuario
                                                                                               else return()
{-Función que se encarga de implementar la opción 5 del menú. Se le pide al usuario la codificación 
que desea realizar reiteradamente hasta que este indica que no quiere realizar más cambios.-}
crearCodi::[(Char,Char)]-> IO()
crearCodi lista =do putStrLn("Introduzca la letra que desea encriptar:")
                    c <- getChar
                    putStr("\n")
                    putStrLn("Introduzca la letra por la que desea encriptarla:")
                    d <- getChar
                    putStr("\n")
                    putStrLn("¿Desea introducir más cambios (s/n)?")
                    s <- getChar
                    putStr("\n")
                    if s == 's' then crearCodi ([(toUpper c, toUpper d)]++[(c,d)]++lista)
                                else opcionCodDecod ([(toUpper c, toUpper d)]++[(c,d)]++lista)

{-Una vez que el usuario no quiere realizar más inserciones de cambios se le pregunta
si desea encriptar o desencriptar un texto y el nombre del archivo que contiene el texto-}
opcionCodDecod:: [(Char,Char)] -> IO()
opcionCodDecod lista = do putStrLn("¿Qué desea, codificar un texto (1) o decodificarlo(2)?")
                          i <- getInt
                          if i == 1 then do cifradoLista lista codiMejorado 
                                    else if i == 2 then do cifradoLista lista decoMejorado 
                                                   else do putStrLn("Error opción no valida.")
                                                           opcionCodDecod lista

{-Función que lee un entero metido por el usuario por consola-}
getInt::  IO Int
getInt = do line <- getLine
            return (read line::Int)

{-Función que contiene una lista que sirve tanto para la codificación como para la decodificación.
Usado en las funciones más genéricas codiMejorado y decoMejorado. Se utilizará el orden b=a para el cifrado y a=b 
para el descifrado, siendo (a,b) elementos cualesquiera de la lista-}
estandar:: [(Char, Char)]
estandar = [('e','k'),('l', 'y'),('a','o'), ('o','v'),('d','l'),('t','q'),('u','p'),('s','r'),
            ('m','x'),('n','w'),('g','i'),('q','t'),('y','b'),('i','g'),('j','f'),('v','e'),('r','s'),
            ('c','m'),('p','u'),('b','n'),('f','j'),('x','c'),('z','a'),('E','K'),('L', 'Y'),('A','O'), ('O','V'),('D','L'),('T','Q'),('U','P'),('S','R'),
            ('M','X'),('N','W'),('G','I'),('Q','T'),('Y','B'),('I','G'),('J','F'),('V','E'),('R','S'),
            ('C','M'),('P','U'),('B','N'),('F','J'),('X','C'),('Z','A')]

{-Función de decodificación de una lista de caracteres a partir de una lista de codificación dada.
Consta de 3 elementos de entrada: la lista a decodificar, la posición en la que nos encontramos
decodificando en dicha lista en este momento y la lista de codificación.-}
decoMejorado:: [Char] -> Int -> [(Char, Char)] -> [Char]
decoMejorado [] _ lista= []
decoMejorado(ch:xs) a lista
     | a < 0 = ch:decoMejorado xs (length lista -1) lista
     | ch ==  (snd (lista !! a)) = (fst(lista !! a)):decoMejorado xs (length lista -1) lista
     | otherwise = decoMejorado (ch:xs) (a-1) lista


{-Función de codificación de una lista de caracteres a partir de una lista de codificación dada.
Consta de 3 elementos de entrada: la lista a codificar, la posición en la que nos encontramos
codificando en dicha lista en este momento y la lista de codificación.-}
codiMejorado:: [Char]-> Int -> [(Char, Char)] -> [Char]
codiMejorado [] _ lista= []
codiMejorado (ch:xs) a lista
     | a < 0 = ch:codiMejorado xs (length lista -1) lista
     | ch ==  (fst (lista !! a)) = (snd(lista !! a)):codiMejorado xs (length lista -1) lista
     | otherwise = codiMejorado (ch:xs) (a-1) lista   

{-Función que muestra por pantalla la posición, la letra y el numero de apariciones 
de la misma, siendo la posición el lugar que ocupa la letra en la lista de apariciones 
ordenada de mayor a menor-}
mostrar:: [(Char,Int)]-> Int -> Int-> IO()
mostrar xs i n
          | i == ((length xs)-1) = putChar '.' 
          | otherwise = do putStr("\t")
                           putStr(show $ ((+) i 1)) 
                           putStr("\t\t")
                           putChar(fst a)
                           putStr("\t") 
                           putStr(show (snd a))
                           putChar('\n')
                           mostrar xs (i+1) n
                           where a = xs !! i 

{-Función que muestra por pantalla la posición, la palabra y el numero de apariciones 
de la misma, siendo la posición el lugar que ocupa la palabra en la lista de apariciones 
ordenada de mayor a menor-}
mostrarPalabras:: [(String,Int)]-> Int -> IO()
mostrarPalabras xs i 
          | i == ((length xs)-1) = putChar '.' 
          | otherwise = do putStr("\t")
                           putStr(show $ ((+) i 1)) 
                           putStr("\t\t")
                           putStr(fst a)
                           putStr("\t\t") 
                           putStr(show (snd a)) 
                           putChar('\n')
                           mostrarPalabras xs (i+1)
                           where a = xs !! i 

{-Función que, dada una lista de tuplas de dos elementos siendo el primer elemento un Char y 
el segundo el número de apariciones de ese char (un Int) y un Char (a), incrementa el número
de apariciones de a en la lista de tuplas. Si a no estaba en la lista de tuplas lo añade 
con apariciones 1 -}
findLetra:: [(Char,Int)] -> Char -> [(Char,Int)]
findLetra [] a 
         | a == ' ' || a == '.' || a == '\n' || a == '\r' || a == ':'  || a == ',' = [] 
         | otherwise = [(a,1)]
findLetra ((m,s):xs) a 
                | m == a = (a, s+1):xs
                | otherwise = (m,s):(findLetra xs a)


{-Función que, dada una lista de tuplas de dos elementos (siendo el primer elemento un String y 
el segundo el número de apariciones de ese String) y un String (a), incrementa el número
de apariciones de a en la lista de tuplas. Si a no estaba en la lista de tuplas lo añade 
con apariciones 1 -}
findPalabra:: [(String,Int)] -> String -> [(String,Int)]
findPalabra [] a = [(a,1)]
findPalabra ((m,n):xs) a
                | m == a = (a, n+1):xs
                | otherwise = (m,n):(findPalabra xs a)

{-Función que, dada una lista de tuplas de dos elementos (xs) y una lista de String (ys),
devuelve una lista de tuplas de dos elementos, siendo el primer elemento 
la palabra que aparece en la lista y el segundo el numero de apariciones.-}
estadPalabras:: [(String,Int)] -> [String] -> [(String, Int)]
estadPalabras xs [] = xs
estadPalabras xs (y:ys) = estadPalabras (findPalabra xs y) ys


{-Función que, dada una lista de tuplas de dos elementos (xs) y una lista de Char (ys),
devuelve una lista de tuplas de dos elementos, siendo el primer elemento 
la letra que aparece en la lista y el segundo el numero de apariciones.-}
estadLetras:: [(Char,Int)] -> [Char] -> [(Char,Int)]
estadLetras xs [] = xs
estadLetras xs (y:ys) = estadLetras (findLetra xs y) ys

{-Función que dada una lista de tuplas de dos elementos (a,Int), la ordena de forma decreciente
por el segundo elemento de las tuplas (Int)-}
quickSort::[(a,Int)]->[(a,Int)]
quickSort [] = []
quickSort (x:xs) = quickSort(menores) ++ [x] ++ quickSort(mayores)
          where
              menores = [y | y <-xs, snd y > snd x]
              mayores = [z | z <-xs, snd z <= snd x]
               

{-Función de decodificación de un fichero. Se trata de una función
concreta de decodificación, no una decodificación automática-}
decodificar:: [Char] -> [Char]
decodificar [] = []
decodificar (ch:xs) 
     | ch == 'k' = 'e':decodificar xs
     | ch == 'y' = 'l':decodificar xs
     | ch == 'o' = 'a':decodificar xs
     | ch == 'v' = 'o':decodificar xs
     | ch == 'l' = 'd':decodificar xs
     | ch == 'q' = 't':decodificar xs
     | ch == 'p' = 'u':decodificar xs
     | ch == 'r' = 's':decodificar xs
     | ch == 'x' = 'm':decodificar xs
     | ch == 'w' = 'n':decodificar xs
     | ch == 'i' = 'g':decodificar xs
     | ch == 't' = 'q':decodificar xs
     | ch == 'b' = 'y':decodificar xs
     | ch == 'g' = 'i':decodificar xs
     | ch == 'f' = 'j':decodificar xs
     | ch == 'e' = 'v':decodificar xs
     | ch == 's' = 'r':decodificar xs
     | ch == 'm' = 'c':decodificar xs
     | ch == 'u' = 'p':decodificar xs
     | ch == 'n' = 'b':decodificar xs
     | ch == 'j' = 'f':decodificar xs
     | ch == 'c' = 'x':decodificar xs
     | ch == 'a' = 'z':decodificar xs
     | ch == 'K' = 'E':decodificar xs
     | ch == 'Y' = 'L':decodificar xs
     | ch == 'O' = 'A':decodificar xs
     | ch == 'V' = 'O':decodificar xs
     | ch == 'L' = 'D':decodificar xs
     | ch == 'Q' = 'T':decodificar xs
     | ch == 'P' = 'U':decodificar xs
     | ch == 'R' = 'S':decodificar xs
     | ch == 'X' = 'M':decodificar xs
     | ch == 'W' = 'N':decodificar xs
     | ch == 'I' = 'G':decodificar xs
     | ch == 'T' = 'Q':decodificar xs
     | ch == 'B' = 'Y':decodificar xs
     | ch == 'G' = 'I':decodificar xs
     | ch == 'F' = 'J':decodificar xs
     | ch == 'E' = 'V':decodificar xs
     | ch == 'S' = 'R':decodificar xs
     | ch == 'M' = 'C':decodificar xs
     | ch == 'U' = 'P':decodificar xs
     | ch == 'N' = 'B':decodificar xs
     | ch == 'J' = 'F':decodificar xs
     | ch == 'C' = 'X':decodificar xs
     | ch == 'A' = 'Z':decodificar xs
     | otherwise = ch:decodificar xs


{-Función de codificación de un fichero. Se trata de una función
concreta de codificación, no una codificación automática-}
codificar:: [Char] -> [Char]
codificar [] = []
codificar (ch:xs) 
     | ch == 'e' = 'k':codificar xs
     | ch == 'l' = 'y':codificar xs
     | ch == 'a' = 'o':codificar xs
     | ch == 'o' = 'v':codificar xs
     | ch == 'd' = 'l':codificar xs
     | ch == 't' = 'q':codificar xs
     | ch == 'u' = 'p':codificar xs
     | ch == 's' = 'r':codificar xs
     | ch == 'm' = 'x':codificar xs
     | ch == 'n' = 'w':codificar xs
     | ch == 'g' = 'i':codificar xs
     | ch == 'q' = 't':codificar xs
     | ch == 'y' = 'b':codificar xs
     | ch == 'i' = 'g':codificar xs
     | ch == 'j' = 'f':codificar xs
     | ch == 'v' = 'e':codificar xs
     | ch == 'r' = 's':codificar xs
     | ch == 'c' = 'm':codificar xs
     | ch == 'p' = 'u':codificar xs
     | ch == 'b' = 'n':codificar xs
     | ch == 'f' = 'j':codificar xs
     | ch == 'x' = 'c':codificar xs
     | ch == 'z' = 'a':codificar xs
     | ch == 'E' = 'K':codificar xs
     | ch == 'L' = 'Y':codificar xs
     | ch == 'A' = 'O':codificar xs
     | ch == 'O' = 'V':codificar xs
     | ch == 'D' = 'L':codificar xs
     | ch == 'T' = 'Q':codificar xs
     | ch == 'U' = 'P':codificar xs
     | ch == 'S' = 'R':codificar xs
     | ch == 'M' = 'X':codificar xs
     | ch == 'N' = 'W':codificar xs
     | ch == 'G' = 'I':codificar xs
     | ch == 'Q' = 'T':codificar xs
     | ch == 'Y' = 'B':codificar xs
     | ch == 'I' = 'G':codificar xs
     | ch == 'J' = 'F':codificar xs
     | ch == 'V' = 'E':codificar xs
     | ch == 'R' = 'S':codificar xs
     | ch == 'C' = 'M':codificar xs
     | ch == 'P' = 'U':codificar xs
     | ch == 'B' = 'N':codificar xs
     | ch == 'F' = 'J':codificar xs
     | ch == 'X' = 'C':codificar xs
     | ch == 'Z' = 'A':codificar xs
     | otherwise = ch:codificar xs
