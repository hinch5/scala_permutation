package permutation

import java.util.stream.IntStream

class Permutation[T](pset: List[T], pperm: List[Int]) {
  validatePerm(pset, pperm);
  val set: List[T] = pset;
  val perm: List[Int] = pperm;
  val appliedRes: List[T] = this.apply();

  def i(i: Int): T = this.appliedRes(i - 1);

  private[this] def getCycle(start: Int, current: Int): List[Int] = {
    if (current == start) {
      List(start);
    } else {
      current :: this.getCycle(start, this.perm(current - 1));
    }
  }

  private[this] def firstUnused(used: Set[Int]): Int = this.perm.find(p => !(used contains p)).get

  private[this] def getDisjointCycles(used: Set[Int]): List[List[Int]] = {
    if (used.size == this.set.length) {
      Nil;
    } else {
      val unused = firstUnused(used);
      val cycle = this.getCycle(unused, this.perm(unused - 1));
      cycle :: getDisjointCycles(used ++ cycle.toSet);
    }
  }

  private[this] def permutationsFromCycle(cycle: List[Int]): List[Permutation[T]] = {
    val idPerm = IntStream.rangeClosed(1, this.set.length).toArray.toList;
    cycle
      .slice(1, cycle.length)
      .map(elem => new Permutation[T](this.set, idPerm.updated(cycle(0) - 1, elem).updated(elem - 1, cycle(0))));
  }

  def getRepresentation: List[Permutation[T]] = {
    val cycles = this.getDisjointCycles(Set());
    cycles
      .filter(c => c.length > 1)
      .flatMap(c => permutationsFromCycle(c));
  }

  private[this] def findPrefix(current: T, toMatch: T, length: Int)(implicit builder: SuperStringBuilder[T]): Int = {
    if (length == 0) {
      0;
    } else {
      if (builder.substring(toMatch, 0, length)
        .equals(builder.substring(current, builder.length(current) - length))) {
        length;
      } else {
        this.findPrefix(current, toMatch, length - 1);
      }
    }
  }

  def getSuperString(implicit builder: SuperStringBuilder[T]): T = {
    this.appliedRes.reduceLeft((total, current) => {
      if (builder.contains(total, current)) {
        total;
      }
      val prefixLen = this.findPrefix(total, current,
        builder.length(current) min builder.length(total));
      builder.sum(total, builder.substring(current, prefixLen));
    });
  }

  def superStringSize(implicit builder: SuperStringBuilder[T]): Int = builder.length(this.getSuperString)

  def multTransposition(a: Int, b: Int): Permutation[T] = {
    if (a < 1 || b < 1 || a > this.perm.length || b > this.perm.length) {
      throw new RuntimeException("invalid transposition");
    }
    val first = this.perm.indexOf(a);
    val second = this.perm.indexOf(b);
    val newPerm = this.perm.updated(first, this.perm(second)).updated(second, this.perm(first));
    new Permutation[T](this.set, newPerm);
  }

  private[this] def checkUnique[U](toCheck: List[U]) = {
    val valueSet = scala.collection.mutable.Set[U]();
    toCheck.foreach(value => {
      if (valueSet(value)) {
        throw new RuntimeException("non unique permutation element");
      }
      valueSet += value;
    })
  }

  private[this] def validatePerm(pset: List[T], pperm: List[Int]) = {
    pperm.foreach(pos => {
      if (pos > pperm.length) {
        throw new RuntimeException("permutation index illegal");
      }
    });
    checkUnique(pset);
    checkUnique(pperm);
  }

  private[this] def revert(): List[List[Int]] = {
    val cycles = this.getDisjointCycles(Set[Int]());
    cycles.map(c => c.reverse);
  }

  private[this] def apply(): List[T] = {
    this.perm.map(p => p - 1) map this.set;
  }

  override def toString: String = {
    String.format("set: [%s]\npermutation:\n%s\n%s",
      this.set.map(s => s.toString).reduceLeft((total, current) => total + "," + current),
      IntStream.rangeClosed(1, this.set.length).toArray.map(n => String.format("%3d", n)).reduceLeft((total, current) => total + " " + current),
      this.perm.map(n => String.format("%3d", n)).reduceLeft((total, current) => total + " " + current)
    );
  }
}
