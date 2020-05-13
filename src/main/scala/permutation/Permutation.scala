package permutation

class Permutation[T](pset: List[T], pperm: List[Int]) {
  validatePerm(pset, pperm);
  val set: List[T] = pset;
  val perm: List[Int] = pperm;

  def i(i: Int): T = this.set(this.perm(i-1))

  private[this] def getCycle(start: Int, current: Int): List[Int] = current match {
    case (start) => List(start);
    case (_) => current :: this.getCycle(start, this.perm(current));
  }

  private[this] def firstUnused(used: Set[Int]): Int = this.perm.find(p => !(used contains p)).get

  private[this] def getDisjointCycles(used: Set[Int]): List[List[Int]] = used.size match {
    case (this.set.length) => Nil;
    case (_) =>
      val unused = firstUnused(used);
      val cycle = this.getCycle(unused, this.perm(unused));
      cycle :: getDisjointCycles(used ++ cycle.toSet);
  }

  private[this] def permutationsFromCycle(cycle: List[Int]): List[Permutation[T]] = {
    cycle
      .slice(1, cycle.length)
      .map(elem => new Permutation[T](List(this.set(cycle(0)), this.set(elem)), List(1, 2)));
  }

  def getRepresentation: List[Permutation[T]] = {
    val cycles = this.getDisjointCycles(Set());
    cycles
      .filter(c => c.length > 1)
      .flatMap(c => permutationsFromCycle(c));
  }

  private[this] def findPrefix[T: SuperStringBuilder](current: T, toMatch: T, length: Int): Int = length match {
    case (0) => 0;
    case (_) => {
      if (implicitly.substring(toMatch, length)
        .equals(implicitly.substring(current, implicitly.length(current) - length))) {
        length;
      } else {
        this.findPrefix(current, toMatch, length - 1);
      }
    }
  }

  def getSuperString[T: SuperStringBuilder]: T = {
    this.set.reduceLeft((total, current) => {
      val prefixLen = this.findPrefix(total,
        current,
        implicitly.length(current) min implicitly.length(total));
      implicitly.sum(total, implicitly.substring(current, prefixLen));
    });
  }

  def superStringSize[T: SuperStringBuilder]: Int = implicitly.length(this.getSuperString)

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
}
