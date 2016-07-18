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
    case x::xs => {
      val p = list span (_ == x)
      p._1 :: pack(p._2)
    }
    case x => List(x)
  }

  def encode[T](list: List[T]) = pack(list) map(l => (l.size, l.head))

  println(flatten(List(List(1, 1), 2, List(3, List(5, 8))))) //List(1, 1, 2, 3, 5, 8)
  println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))) //List('a, 'b, 'c, 'a, 'd, 'e)
  println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))) //List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))) //List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
}
