package com.course.immutable

import scala.annotation.tailrec

case class MyMap[K, V](buckets: Vector[MyList[(K, V)]]) {

  private val initialCapacity = 15

  def +(pair: (K, V)): MyMap[K, V] = {
    val idx = indexFor(pair._1)

    if (buckets.isEmpty) init + pair
    else {
      val chain = buckets(idx)
      chain.indexOfPredicate(element => element._1 == pair._1) match {
        case -1 => MyMap(buckets.updated(idx, chain.cons(pair)))
        case i => MyMap(buckets.updated(idx, chain.replace(i, pair)))
      }
    }
  }

  def size: Long = {

    @tailrec
    def recSize(buckets: Vector[MyList[(K, V)]], acc: Long): Long =
      buckets match {
        case hd +: tl => recSize(tl, acc + hd.size)
        case _ => acc
      }

    recSize(buckets, 0)
  }

  def isEmpty: Boolean = {
    if (this.size == 0) true
    else false
  }

  def get(key: K): Option[V] = {
    val idx = indexFor(key)
    buckets(idx)
      .find(element => element._1 == key)
      .map(element => element._2)
  }

  def -(key: K): MyMap[K, V] = {
    val idx = indexFor(key)
    val updatedMap = buckets(idx).filter(element => element._1 != key)
    MyMap(buckets.updated(idx, updatedMap))
  }

  private def init: MyMap[K, V] = {
    MyMap(Vector.fill(initialCapacity)(Nil))
  }

  private def indexFor(key: K): Int = {
    key.hashCode() & buckets.length
  }
}

object MyMap {

  def apply[K, V](elements: (K, V)*): MyMap[K, V] =
    populate(elements.toList)

  private def populate[K, V](elements: List[(K, V)]): MyMap[K, V] = {

    @tailrec
    def recPopulate(el: List[(K, V)], resultMap: MyMap[K, V]): MyMap[K, V] =
      el match {
        case hd +: tl => recPopulate(tl, resultMap.+(hd._1, hd._2))
        case _ => resultMap
      }

    recPopulate(elements, MyMap(Vector.empty))
  }
}

