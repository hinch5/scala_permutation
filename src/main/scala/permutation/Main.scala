package permutation

object Main {
  def main(args: Array[String]): Unit = {
    val stringSet = List("asdf", "ghas");
    val positions = List(2, 1);
    val permutation = new Permutation(stringSet, positions);
    println(permutation.getSuperString);
    println(permutation.i(2));
    val longSet = List(1, 2, 3, 4, 5, 6);
    val
  }
}
