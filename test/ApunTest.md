Para poner a prueba a el compilador utilizaremos una técnica denominada
*Test de Caja Negra*. Es decir, traremos al compilador como una función
de la cual no conocemos su implementación, y por lo que, lo trataremos
como una función `comp` que a dado un código fuente `A` nos retorna un
binario `comp = BA` como resultado. Ejecutaremos el binario resultante,
`BA`, y analizaremos el resultado. Si el resultado es el que esperamos,
el compilador ha *superado* esa prueba, y sino lo ha *fallado*.

Utilizaremos una libraría del compilador
[GHC](https://www.haskell.org/ghc/) para realizar tipo de pruebas
llamada [Haskell Test Framework
(HTF)](https://github.com/skogsbaer/HTF). En nuestro caso, utilizaremos
un fragmento de la librería donde haremos pruebas de caja negra de
código de fuente que se compilan y se ejecutan de manera unitaria.

Debido a que el compilador tiene varios modos de compilación definiremos
un archivo indicando como se ejecutan las pruebas, aunque las pruebas
**son las mismas para todos los modos**, y además, **todos los modos
esbozan el mismo comportamiento**.

Para sumar nuestra bateria de pruebas al proyecto de
[stack](https://docs.haskellstack.org/en/stable/README/) lo que haremos
es definir un nuevo modulo indicando cual será el archivo de cabecera
que se ejecutará para cada uno de los modos del compilador.

A modo de ejemplo podemos ver como se pueden lanzar las pruebas de
`Bytecode`:

``` yaml
tests:
  # Bytecode Test Suite
  Bytecode:
    main: Byte.hs
    source-dirs: test
    ghc-options:
      - -main-is Byte
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
    dependencies:
      - compiladores
      - HTF
  # Virtual Machine Test Suite
  C:
    main: C.hs
    source-dirs: test
    ghc-options:
      - -main-is C
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
    dependencies:
      - compiladores
      - HTF
```

Los archivos `VM.hs, Byte.hs` estarán ubicados en una nueva carpeta
`test`, mientras que los casos de prueba estarán en `test/testCases/`
Esto nos permite lanzar las diferentes baterías de prueba desde la
terminal de la siguiente forma:

-   Para probar la compilación a *Bytecode*

    ``` bash
    > stack test :Bytecode
    ```

-   Para probar la compilación a *C*

    ``` bash
    > stack test :C
    ```

    La compilación a *C* es en varios pasos, y para esto, utilizamos un
    script `compile_and_run_c.sh` que pueden encontrar en la carpeta
    *Test*.

Para listar todo las posibles baterías de prueba podemos utilizar el
comando:

``` bash
> stack ide targets
```

Donde los casos de prueba serán los que figuren como
`compiladores:test:...`.

Dentro de cada archivo de prueba `VM.hs, Byte.hs` encontrarán como se
define la ejecución de las pruebas. Para esto se utiliza una función que
provee la catedra definida en el archivo [file:Spec.hs](Spec.hs) :

``` haskell
runTestWith :: String -> IO ()
runTestWith script = do
  bbts <- blackBoxTests "test/cases" script ".fd4" bbTArg
  htfMain ([makeTestSuite "bbts" bbts])
  where
    bbTArg = defaultBBTArgs
      { bbtArgs_stdoutSuffix = ".fd4.out"
      , bbtArgs_stderrSuffix = ".fd4.err"
      }
```

Donde lo que hace es simplemente ejecutar un script `S` cuya dirección
se la entrega como argumento a la función `runTestWith`. La función
`runTestWith` lo que hará es ejecutar el script `S` en cada uno de los
archivos terminados en `.fd4` que se encuentren en la carpeta
`test/correctos` y comparará el resultado de la salida estandar con los
archivos con mismo nombre pero terminados en `.fd4.out` y `.fd4.err`.

Se recomienda fuertemente incluir nuevos casos y en lo posible incluir
casos que sean erroneos, casos que se sepa que deban fallar.
