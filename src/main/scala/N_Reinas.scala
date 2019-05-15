import
scala.collection.mutable.ArrayBuffer

object N_Reinas {
  type fila = Int
  type columna = Int

  def reinas(n: Int): Set[List[(fila, columna)]] = {
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

      def factible(reina: (fila, columna), reinas: List[(fila, columna)]): Boolean = {
        reinas.forall { reinaOk =>
          reinaOk._1 != reina._1 && reinaOk._2 != reina._2 &&
            (Math.abs(reinaOk._1 - reina._1) != Math.abs(reinaOk._2 - reina._2))
        }
      }
      colocarReina(n - 1)
    }

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

