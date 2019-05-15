import
scala.collection.mutable.ArrayBuffer

/**
  *  Objeto que resuelve el problema de situar N reinas en un tablero de ajedrez
  *  de NxN sin que se amenacen entre ellas.
  *
  *  Una reina amenaza a otra si está en la misma fila, columna o diagonal.
  *
  *  Los parámetros que definen al objeto son:
  *
  *  type fila = Int
  *  type columna = Int
  */
object N_Reinas {
  type fila = Int
  type columna = Int

  /**
    * Función principal que contiene a las anidadas y que sirve de soporte para realizar
    * el bactraking necesario para buscar las distitnas soluciones
    *
    * @param n número de reinas a colocar
    * @return List[(fila, columna)]
    */
  def reinas(n: Int): Set[List[(fila, columna)]] = {
    /**
      * Método que va analizando las soluciones posibles. Si es una solución viable
      * se encarga de añadirla a la lista y sigue buscando la siguiente.
      *
      * @param k número de reinas a colocar
      * @return (fila, columna) Posición en la que se propone colocar a la Reina
      */
    def colocarReina(k: Int): Set[List[(fila, columna)]] = {
      //Ya no quedan mas reinas para colocar en el tablero
      if (k < 0) {
        Set(List())
      } else {
        for {
          //Colocar cada una de las reinas
          reinas <- colocarReina(k - 1)
          columna <- 0 until n
          reina = (k, columna)
          if factible(reina, reinas)
        } yield reina :: reinas
      }
    }

    /**
      *  Método que analiza si es factible colocar la reina en la posición que ha definido
      *  el método colocarReina
      *
      * @param reina (fila, columna)
      * @param reinas List[(fila, columna)]
      * @return List[(fila, columna)]
      */
    def factible(reina: (fila, columna), reinas: List[(fila, columna)]): Boolean = {
      reinas.forall { reinaOk =>
        reinaOk._1 != reina._1 && reinaOk._2 != reina._2 &&
          (Math.abs(reinaOk._1 - reina._1) != Math.abs(reinaOk._2 - reina._2))
      }
    }
      colocarReina(n - 1)
  }


  /**
    * Método encargado de imprimir por consola las soluciones. Lo hace recorriendo
    * la lista de soluciones almacenada por el método colocarReina
    *
    * @param n número de reinas
    * @param soluciones Set[List[(fila, columna)]
    */
  def verSoluciones(n: Int, soluciones: Set[List[(fila, columna)]]) = {
      for (reinas <- soluciones) {
        println("-----------------------------------------")
        for (fila <- 0 until n) {
          val buffer = new ArrayBuffer[String]()
          for (columna <- 0 until n) {
            if (reinas.contains((fila, columna))) {
              buffer.append("1")
            } else {
              buffer.append("*")
            }
          }
          println(buffer.mkString(" "))
        }
      }
    }

    def main(args: Array[String]) {
      val soluciones = reinas(8)
      verSoluciones(8, soluciones)
      println(s"Numero de soluciones encontradas: ${soluciones.size}")
    }
}

