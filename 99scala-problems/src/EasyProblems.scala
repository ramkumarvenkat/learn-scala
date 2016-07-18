/**
  * Created by rvenkataraman on 18/07/16.
  */
object EasyProblems extends App {

  def last(list: List[Int]) = {
    list.last
  }

  def penultimate(list: List[Int]) = {
    if (list.size >= 2) list(list.size - 2)
  }

  def nth(n: Int, list: List[Int]) = {
    list(n)
  }

  def length(list: List[Int]) = {
    list.size
  }

  def reverse(list: List[Int]) = {
    list.reverse
  }

  def isPalindrome(list: List[Int]) = {
    list == list.reverse
  }

  println(last(List(1, 1, 2, 3, 5, 8))) //8
  println(penultimate(List(1, 1, 2, 3, 5, 8))) //5
  println(nth(2, List(1, 1, 2, 3, 5, 8))) //2
  println(length(List(1, 1, 2, 3, 5, 8))) //6
  println(reverse(List(1, 1, 2, 3, 5, 8))) //List(8, 5, 3, 2, 1, 1)
  println(isPalindrome(List(1, 2, 3, 2, 1))) //true
  println(isPalindrome(List(1, 2, 3, 2, 2))) //false
}

