import scala.util.Random

/**
  * Created by rvenkataraman on 18/07/16.
  */
object IntermediateProblems extends App {

  def flatten(list: List[Any]): List[Any] = list flatMap {
    case l: List[_] => flatten(l)
    case e => List(e)
  }

  def compress[T](list: List[T]) = list.foldRight[List[T]](List[T]()) {
    case (c, ls) if (ls.isEmpty || ls.head != c) => c :: ls
    case (_, ls) => ls
  }

  def pack[T](list: List[T]): List[List[T]] = list match {
    case Nil => Nil
    case x :: xs => {
      val p = list span (_ == x)
      p._1 :: pack(p._2)
    }
  }

  def encode[T](list: List[T]) = pack(list) map (l => (l.size, l.head))

  def encodeModified[T](list: List[T]) = pack(list) map {
    case l if l.size > 1 => (l.size, l.head)
    case l => l.head
  }

  def decode[T](list: List[(Int, T)]) = list flatMap (e => List.fill(e._1)(e._2))

  def encodeDirect[T](list: List[T]): List[(Int, T)] = list match {
    case Nil => Nil
    case x :: xs => {
      val p = list span (_ == x)
      (p._1.size, x) :: encodeDirect(p._2)
    }
  }

  def duplicate[T](list: List[T]) = list flatMap (x => List.fill(2)(x))

  def duplicateN[T](n: Int, list: List[T]) = list flatMap (x => List.fill(n)(x))

  def drop[T](n: Int, list: List[T]): List[T] = list match {
    case Nil => Nil
    case x => {
      val s = x.splitAt(n - 1)
      if (!s._2.isEmpty) s._1 ::: drop(n, s._2.tail)
      else s._1
    }
  }

  def split[T](n: Int, list: List[T]) = {
    val s = list.splitAt(n)
    (s._1, s._2)
  }

  def slice[T](i: Int, k: Int, list: List[T]) = {
    val s = list.splitAt(i)
    if (!s._2.isEmpty) s._2.splitAt(k - i)._1
    else s._1
  }

  def rotate[T](n: Int, list: List[T]): List[T] = {
    if (n < 0) rotate(list.size + n, list)
    else {
      val s = list.splitAt(n)
      s._2 ::: s._1
    }
  }

  def removeAt[T](n: Int, list: List[T]) = {
    val s = list.splitAt(n)
    (s._1 ::: s._2.tail, list(n))
  }

  def insertAt[T](elem: T, n: Int, list: List[T]) = {
    val s = list.splitAt(n)
    s._1 ::: List(elem) ::: s._2
  }

  def range(i: Int, j: Int): List[Int] = if (i <= j) List(i) ::: range(i + 1, j) else Nil

  def randomSelect[T](n: Int, list: List[T]): List[T] = {
    if (n == 0) Nil
    else {
      val s = removeAt(Random.nextInt(list.size), list)
      List(s._2) ::: randomSelect(n - 1, s._1)
    }
  }

  def lotto(n: Int, m: Int) = randomSelect(n, range(1, m))

  def randomPermute[T](list: List[T]) = {
    val r = randomSelect(list.size/2, list)
    list.filterNot(r.toSet) ::: r
  }

  println(flatten(List(List(1, 1), 2, List(3, List(5, 8))))) //List(1, 1, 2, 3, 5, 8)
  println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))) //List('a, 'b, 'c, 'a, 'd, 'e)
  println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))) //List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))) //List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  println(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))) //List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  println(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))) //List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  println(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))) //List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  println(duplicate(List('a, 'b, 'c, 'c, 'd))) //List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  println(duplicateN(3, List('a, 'b, 'c, 'c, 'd))) //List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  println(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))) //List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  println(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))) //(List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  println(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))) //List('d, 'e, 'f, 'g)
  println(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))) //List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  println(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))) //List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  println(removeAt(1, List('a, 'b, 'c, 'd))) //(List('a, 'c, 'd),'b)
  println(insertAt('new, 1, List('a, 'b, 'c, 'd))) //List('a, 'new, 'b, 'c, 'd)
  println(range(4, 9)) //List(4, 5, 6, 7, 8, 9)
  println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))) //List('e, 'd, 'a)
  println(lotto(6, 49)) //List(23, 1, 17, 33, 21, 37)
  println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))) //List('b, 'a, 'd, 'c, 'e, 'f)
}
