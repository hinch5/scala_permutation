package permutation

object Main {
  def main(args: Array[String]): Unit = {
    val stringSet = List("asdf", "ghas", "asd");
    val positions = List(2, 1, 3);
    val permutation = new Permutation(stringSet, positions);
    println(permutation.getSuperString);
    println(permutation.i(2));
    val longSet = List(1, 2, 3, 4, 5, 6);
    val longPositions = List(3, 4, 5, 2, 1, 6);
    val longPerm = new Permutation(longSet, longPositions);
    val transpositions = longPerm.getRepresentation;
    println(transpositions.length);
    transpositions.foreach(println);
  }
}
