package com.course.immutable

import java.util.NoSuchElementException
import scala.annotation.tailrec

trait MyList[+A] {

  def ::[B >: A](element: B): MyList[B]

  def head: A

  def last: A

  def headOpt: Option[A]

  def lastOpt: Option[A]

  def tail: MyList[A]

  def isEmpty: Boolean

  def size: Long

  def indexOf[B >: A](element: B): Long

  def indexOfPredicate[B >: A](predicate: A => Boolean): Option[Long]

  def map[B](f: A => B): MyList[B]

  def filter(predicate: A => Boolean): MyList[A]

  def count(predicate: A => Boolean): Int

  def find(predicate: A => Boolean): Option[A]

  def exists(predicate: A => Boolean): Boolean

  def take(n: Int): MyList[A]

  def takeRight(n: Int): MyList[A]

  def isDefinedAt[B >: A](element: B): Boolean

  def cons[B >: A](element: B): MyList[B]

  def append[B >: A](that: MyList[B]): MyList[B]

  def reverse: MyList[A]

  def zip[B](that: MyList[B]): MyList[(A, B)]

  def zipWithIndex: MyList[(A, Long)]

  def collect[B](pfun: PartialFunction[A, B]): MyList[B]

  def foldLeft[B](z: B)(operator: (B, A) => B): B

  def foldRight[B](z: B)(operator: (A, B) => B): B

  def replace[B >: A](index: Long, element: B): MyList[B]
}

case object Nil extends MyList[Nothing] {

  def ::[B >: Nothing](element: B): MyList[B] = new ::(element, Nil)

  override def head: Nothing = throw new NoSuchElementException

  override def last: Nothing = throw new NoSuchElementException

  override def headOpt: Option[Nothing] = Option.empty

  override def lastOpt: Option[Nothing] = Option.empty

  override def tail: MyList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def size: Long = 0

  override def indexOf[B >: Nothing](element: B): Long = -1

  override def indexOfPredicate[B >: Nothing](predicate: Nothing => Boolean): Option[Long] = Option.empty

  override def map[B](f: Nothing => B): MyList[B] = Nil

  override def filter(predicate: Nothing => Boolean): MyList[Nothing] = Nil

  override def count(predicate: Nothing => Boolean): Int = 0

  override def find(predicate: Nothing => Boolean): Option[Nothing] = Option.empty

  override def exists(predicate: Nothing => Boolean): Boolean = false

  override def take(n: Int): MyList[Nothing] = throw new NoSuchElementException

  override def takeRight(n: Int): MyList[Nothing] = throw new NoSuchElementException

  override def isDefinedAt[B >: Nothing](element: B): Boolean = false

  override def cons[B >: Nothing](element: B): MyList[B] = new ::(element, Nil)

  override def append[B >: Nothing](that: MyList[B]): MyList[B] = that

  override def reverse: MyList[Nothing] = Nil

  override def zip[B](that: MyList[B]): MyList[(Nothing, B)] = Nil

  override def zipWithIndex: MyList[(Nothing, Long)] = Nil

  override def collect[B](pfun: PartialFunction[Nothing, B]): MyList[B] = Nil

  override def foldLeft[B](z: B)(operator: (B, Nothing) => B): B = throw new NoSuchElementException

  override def foldRight[B](z: B)(operator: (Nothing, B) => B): B = throw new NoSuchElementException

  override def replace[A](index: Long, element: A): MyList[A] = throw new NoSuchElementException
}

final case class ::[+A](head: A, tail: MyList[A]) extends MyList[A] {

  override def ::[B >: A](element: B): MyList[B] = new ::(element, this)

  override def last: A = {

    @tailrec
    def recLast(list: MyList[A]): A = {
      if (list.tail.isEmpty) {
        list.head
      } else recLast(list.tail)
    }

    recLast(this)
  }

  override def headOpt: Option[A] = Option(head)

  override def lastOpt: Option[A] = Option(last)

  override def isEmpty: Boolean = false

  override def size: Long = {

    @tailrec
    def recSize(list: MyList[A], acc: Long): Long =
      list match {
        case _ :: tl => recSize(tl, acc + 1)
        case Nil => acc
      }

    recSize(this, 0)
  }

  override def map[B](f: A => B): MyList[B] = new ::(f(head), tail.map(f))

  override def filter(predicate: A => Boolean): MyList[A] = {
    if (predicate(head)) new ::(head, tail.filter(predicate))
    else tail.filter(predicate)
  }

  override def count(predicate: A => Boolean): Int = {

    @tailrec
    def recCount(list: MyList[A], acc: Int = 0): Int = {
      if (!list.tail.isEmpty) {
        if (predicate(list.head)) recCount(list.tail, acc + 1)
        else recCount(list.tail, acc)
      }
      else acc
    }

    recCount(this)
  }

  override def find(predicate: A => Boolean): Option[A] = {

    @tailrec
    def recFind(list: MyList[A]): Option[A] = {
      list match {
        case hd :: tl =>
          if (predicate(hd)) Option(list.head)
          else recFind(tl)
        case Nil => Option.empty
      }
    }

    recFind(this)
  }

  override def exists(predicate: A => Boolean): Boolean = {

    @tailrec
    def recExists(list: MyList[A]): Boolean =
      if (!list.tail.isEmpty) {
        if (predicate(list.head)) true
        else recExists(list.tail)
      } else false

    recExists(this)
  }

  override def indexOfPredicate[B >: A](predicate: A => Boolean): Option[Long] = {

    @tailrec
    def recIndexOf(list: MyList[A], accum: Long = 0): Option[Long] =
      list match {
        case hd :: tl =>
          if (predicate(hd)) Some(accum)
          else recIndexOf(tl, accum + 1)
        case Nil => None
      }

    recIndexOf(this)
  }

  override def indexOf[B >: A](element: B): Long = {

    @tailrec
    def recIndexOf(list: MyList[A], accum: Long): Long = {
      list match {
        case hd :: tl =>
          if (hd == element) accum
          else recIndexOf(tl, accum + 1)
        case Nil => -1
      }
    }

    recIndexOf(this, 0)
  }

  override def isDefinedAt[B >: A](element: B): Boolean = {

    @tailrec
    def recIsDefinedAt(list: MyList[A]): Boolean =
      if (!list.tail.isEmpty) {
        if (list.head == element) true
        else if (list.tail.head == element) true
        else recIsDefinedAt(list.tail)
      } else false

    recIsDefinedAt(this)
  }

  override def cons[B >: A](element: B): MyList[B] = new ::(element, this)

  override def take(n: Int): MyList[A] = {

    @tailrec
    def recTake(oldList: MyList[A], newList: MyList[A], n: Int): MyList[A] =
      if (n == 0) newList
      else
        oldList match {
          case hd :: tl => recTake(tl, hd :: newList, n - 1)
          case Nil => newList
        }

    recTake(this, Nil, n).reverse
  }

  override def takeRight(n: Int): MyList[A] = {
    val lastElements = this.reverse
    lastElements.take(n).reverse
  }

  override def append[B >: A](that: MyList[B]): MyList[B] = {

    @tailrec
    def recAppend(appendedList: MyList[B], oldList: MyList[B], newList: MyList[B]): MyList[B] =
      if (that.isEmpty) this
      else
        oldList match {
          case hd :: tl => recAppend(appendedList, tl, hd :: newList)
          case Nil => appendedList match {
            case hd :: tl => recAppend(tl, Nil, hd :: newList)
            case Nil => newList
          }
        }

    recAppend(that, this, Nil).reverse
  }

  override def reverse: MyList[A] = {

    @tailrec
    def recReverse(oldList: MyList[A], newList: MyList[A]): MyList[A] =
      oldList match {
        case hd :: tl => recReverse(tl, hd :: newList)
        case Nil => newList
      }

    recReverse(this, Nil)
  }

  override def zip[B](that: MyList[B]): MyList[(A, B)] = {

    @tailrec
    def recZip(zipList: MyList[B], oldList: MyList[A], newList: MyList[(A, B)]): MyList[(A, B)] =
      oldList match {
        case headOldList :: tailOldList => zipList match {
          case headZipList :: tailZipList => recZip(tailZipList, tailOldList, new ::((headOldList, headZipList), newList))
          case Nil => newList
        }
        case Nil => newList
      }

    recZip(that, this, Nil).reverse
  }

  override def zipWithIndex: MyList[(A, Long)] = {
    @tailrec
    def recZipWithIndex(list: MyList[A], indexList: MyList[(A, Long)], acc: Long): MyList[(A, Long)] =
      list match {
        case hd :: tl => recZipWithIndex(tl, new ::((hd, acc), indexList), acc + 1)
        case Nil => indexList
      }

    recZipWithIndex(this, Nil, 0).reverse
  }

  override def collect[B](pfun: PartialFunction[A, B]): MyList[B] = {

    @tailrec
    def recCollect(currentList: MyList[A], resultList: MyList[B]): MyList[B] =
      currentList match {
        case hd :: tl => recCollect(tl, new ::(pfun(hd), resultList))
        case Nil => resultList
      }

    recCollect(this, Nil).reverse
  }

  override def foldLeft[B](z: B)(operator: (B, A) => B): B = {

    @tailrec
    def recFoldLeft(list: MyList[A], acc: B): B =
      list match {
        case hd :: tl => recFoldLeft(tl, operator(acc, hd))
        case Nil => acc
      }

    recFoldLeft(this, z)
  }

  override def foldRight[B](z: B)(operator: (A, B) => B): B = {

    @tailrec
    def recFoldRight(list: MyList[A], acc: B): B =
      list match {
        case hd :: tl => recFoldRight(tl, operator(hd, acc))
        case Nil => acc
      }

    recFoldRight(this.reverse, z)
  }

  override def replace[B >: A](index: Long, element: B): MyList[B] = {

    @tailrec
    def recIndexOf(list: MyList[A], updatedList: MyList[B], acc: Long): MyList[B] = {
      list match {
        case hd :: tl =>
          if (acc == index) recIndexOf(tl, element :: updatedList, acc + 1)
          else recIndexOf(tl, hd :: updatedList, acc + 1)
        case Nil => updatedList
      }
    }

    recIndexOf(this, Nil, 0).reverse
  }
}

