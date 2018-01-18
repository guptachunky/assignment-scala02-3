import org.apache.log4j.Logger

class ParameterOperations {

  def lengthList[A](list: List[A]): Int = {
    val yieldList = for (element <- list)
      yield 1
    yieldList.foldRight(0)((m: Int, n: Int) => m + n)
  }

  def hasSubsequence[A](mainList: List[A], sub: List[A]): Boolean = {

    def check[A](mainList: List[A], sub: List[A]): Boolean = {
      sub match {
        case head :: tail if (head == mainList.head) => check(mainList.tail, tail)
        case head :: tail if (head != mainList.head) => false
        case _ => true
      }
    }

    mainList match {
      case head :: tail if (head == sub.head) => {
        check(tail, sub.tail) match {
          case true => true
          case false => hasSubsequence(tail, sub)
        }
      }
      case head :: tail if (head != sub.head) => hasSubsequence(tail, sub)
      case head :: Nil => true
      case _ => false
    }
  }

  def concatList[A](list1: List[A], list2: List[A]): List[A] = {
    list2 match {
      case head :: tail => concatList(list1 :+ head, tail)
      case _ => list1
    }
  }

}

object CheckOperations extends App {

  val log = Logger.getLogger(this.getClass)
  val one = 1
  val two = 5
  val three = 6
  val four = 1
  val five = 2
  val six = 1
  val seven = 3
  val eight = 5
  val nine = 9
  val list = List(one, two, three, four, five, six, seven, eight, nine)
  val sub = List(four, five, six, seven)
  val operationsObj = new ParameterOperations
  log.info(operationsObj.lengthList(list) + "\n")
  log.info(operationsObj.hasSubsequence(list, sub) + "\n")
  log.info(operationsObj.concatList(list, sub) + "\n")
}