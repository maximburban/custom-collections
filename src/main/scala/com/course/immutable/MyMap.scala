package com.course.immutable

import scala.annotation.tailrec

case class MyMap[K, V](buckets: Vector[MyList[(K, V)]]) {

  private val initialCapacity = 15

  def +(pair: (K, V)): MyMap[K, V] = add(pair)

  def add(pair: (K, V)): MyMap[K, V] = {
    val (key, value) = pair
    val idx = indexFor(key)

    if (buckets.isEmpty) init + pair
    else {
      val chain = buckets(idx)
      chain.indexOfPredicate { case (savedKey, savedValue) => savedKey == key } match {
        case None => MyMap(buckets.updated(idx, chain.cons(pair)))
        case Some(i) => MyMap(buckets.updated(idx, chain.replace(i, pair)))
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

  def isEmpty: Boolean = this.size == 0

  def get(key: K): Option[V] = {
    val idx = indexFor(key)
    buckets(idx)
      .find { case (savedKey, savedValue) => savedKey == key }
      .map { case (savedKey, savedValue) => savedValue }
  }

  def -(key: K): MyMap[K, V] = remove(key)

  def remove(key: K): MyMap[K, V] = {
    val idx = indexFor(key)
    val updatedMap = buckets(idx).filter { case (savedKey, savedValue) => savedKey != key }
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
        case (key, value) +: tl => recPopulate(tl, resultMap.+(key, value))
        case _ => resultMap
      }

    recPopulate(elements, MyMap(Vector.empty))
  }
}

