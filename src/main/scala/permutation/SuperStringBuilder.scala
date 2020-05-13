package permutation

abstract class SuperStringBuilder[T] {
  def sum(a: T, b: T) : T;
  def length(a: T): Int;
  def substring(a: T, start: Int): T;
  def substring(a: T, start: Int, end: Int): T;
}

object SuperStringBuilder {
  implicit object str extends SuperStringBuilder[String] {
    override def sum(a: String, b: String) : String = a + b;

    override def length(a: String): Int = a.length;

    override def substring(a: String, start: Int, end: Int): String = a.substring(start, end);

    override def substring(a: String, start: Int): String = a.substring(start);
  }
}
