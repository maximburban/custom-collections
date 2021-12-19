package com.course.immutable

import org.scalatest.funsuite.AnyFunSuite

class MyMapSpec extends AnyFunSuite {

  test("should create map from elements") {
    val map = MyMap("one" -> 111, "two" -> 222, "three" -> 333)

    val result = map.isEmpty

    assert(!result)
  }

  test("should create empty map from elements") {
    val map = MyMap()

    val result = map.isEmpty

    assert(result)
  }

  test("should get element from map by key") {
    val map = MyMap("one" -> 111, "two" -> 222, "three" -> 333, "one" -> 11111)

    val result = map.get("two")

    assert(result == Option(222))
  }

  test("should remove element from map") {
    val map = MyMap("one" -> 111, "two" -> 222, "three" -> 333, "one" -> 11111)
    val expectedMap = MyMap("one" -> 11111, "two" -> 222)

    val result = map - "three"

    assert(result == expectedMap)
  }

  test("should return size of map") {
    val map = MyMap("one" -> 111, "two" -> 222, "three" -> 333)

    val result = map.size

    assert(result == 3)
  }
}
