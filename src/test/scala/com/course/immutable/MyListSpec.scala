package com.course.immutable

import org.scalatest.funsuite.AnyFunSuite

class MyListSpec extends AnyFunSuite {

  test("should create list from elements") {
    val list = 1 :: 4 :: 8 :: Nil

    assert(!list.isEmpty)
  }

  test("should create empty list") {
    val list = Nil

    assert(list.isEmpty)
  }

  test("should filter list") {
    val list = 1 :: 4 :: 8 :: 22 :: Nil
    val expect = 8 :: 22 :: Nil

    val result = list.filter(element => element > 4)

    assert(result == expect)
  }

  test("should map list") {
    val list = "one" :: "two" :: "three" :: "four" :: Nil
    val expect = 3 :: 3 :: 5 :: 4 :: Nil

    val result = list.map(element => element.length)

    assert(result == expect)
  }

  test("should count elements by expression") {
    val list = "one" :: "two" :: "three" :: "four" :: Nil

    val result = list.count(element => element.length == 3)

    assert(result == 2)
  }

  test("should find element by expression") {
    val list = "one" :: "two" :: "three" :: "four" :: Nil

    val resultPositive = list.find(element => element.startsWith("th"))
    val resultNegative = list.find(element => element.startsWith("the"))

    assert(resultPositive == Option("three"))
    assert(resultNegative == Option.empty)

  }

  test("should check if element exists") {
    val list = "one" :: "two" :: "three" :: "four" :: Nil

    val resultPositive = list.exists(element => element.startsWith("th"))
    val resultNegative = list.exists(element => element.startsWith("the"))

    assert(resultPositive)
    assert(!resultNegative)
  }

  test("should return last element") {
    val list = "one" :: "two" :: "three" :: "four" :: Nil

    val result = list.last

    assert(result == "four")
  }

  test("should return optional last element") {
    val list = 1 :: 4 :: 8 :: 22 :: Nil

    val result = list.lastOpt

    assert(result == Option(22))
  }

  test("should return size of list") {
    val list = 1 :: 4 :: 8 :: 22 :: Nil

    val result = list.size

    assert(result == 4)
  }

  test("should return index of element") {
    val list = 1 :: 4 :: 8 :: 22 :: Nil

    val result = list.indexOf(22)

    assert(result == 3)
  }

  test("should return index of predicate element") {
    val list = 1 :: 4 :: 8 :: 22 :: Nil

    val result = list.indexOfPredicate((element: Int) => element > 3)

    assert(result == Option(1))
  }

  test("should return true if element is defined") {
    val list = 1 :: 4 :: 8 :: 22 :: Nil

    val resultPositive = list.isDefinedAt(22)
    val resultNegative = list.isDefinedAt(222)

    assert(resultPositive)
    assert(!resultNegative)
  }

  test("should take first 3 elements") {
    val list = 1 :: 4 :: 8 :: 22 :: Nil
    val expectedList = 1 :: 4 :: Nil

    val result = list.take(2)

    assert(result == expectedList)
  }

  test("should add element to list") {
    val list = 33 :: 56 :: 8 :: 22 :: Nil
    val expectedList = 78 :: 33 :: 56 :: 8 :: 22 :: Nil

    val result = list.cons(78)

    assert(result == expectedList)
  }

  test("should append list to list") {
    val list1 = 33 :: 56 :: Nil
    val list2 = 78 :: 67 :: Nil
    val expectedList = 33 :: 56 :: 78 :: 67 :: Nil

    val result = list1.append(list2)

    assert(result == expectedList)
  }

  test("should reverse list") {
    val list = 67 :: 78 :: 56 :: 33 :: Nil
    val expectedList = 33 :: 56 :: 78 :: 67 :: Nil

    val result = list.reverse

    assert(result == expectedList)
  }

  test("should take last elements from list") {
    val list = "one" :: "two" :: "three" :: "four" :: Nil
    val expectedList = "three" :: "four" :: Nil

    val result = list.takeRight(2)

    assert(result == expectedList)
  }

  test("should create zip from lists") {
    val list1 = 67 :: 78 :: 56 :: 33 :: Nil
    val list2 = "one" :: "two" :: "three" :: "four" :: Nil
    val list3 = "one" :: "two" :: Nil
    val expectedList1 = (67, "one") :: (78, "two") :: (56, "three") :: (33, "four") :: Nil
    val expectedList2 = (67, "one") :: (78, "two") :: Nil

    val result1 = list1.zip(list2)
    val result2 = list1.zip(list3)

    assert(result1 == expectedList1)
    assert(result2 == expectedList2)
  }

  test("should create zip with index from list") {
    val list = "one" :: "two" :: "three" :: "four" :: Nil
    val expectedList = ("one", 0) :: ("two", 1) :: ("three", 2) :: ("four", 3) :: Nil

    val result = list.zipWithIndex

    assert(result == expectedList)
  }

  test("should collect list") {
    val list = 1 :: 4 :: 8 :: 22 :: Nil
    val expectedList = 3 :: 12 :: 24 :: 66 :: Nil

    val result = list.collect(element => element * 3)

    assert(result == expectedList)
  }

  test("should test foldLeft method and print result") {
    val list = 1 :: 2 :: 3 :: 4 :: 5 :: Nil
    val result = list.foldLeft(0) { (acc, currNum) =>
      val sum = acc + currNum
      println(s"FoldLeft: acc($acc) + currNum($currNum) = $sum ")
      sum
    }
    println(result)

    assert(result == 15)
  }

  test("should test foldRight method and print result") {
    val list = 1 :: 2 :: 3 :: 4 :: 5 :: Nil

    val result = list.foldRight(0) { (currNum, acc) =>
      val sum = acc + currNum
      println(s"FoldRight: acc($acc) + currNum($currNum) = $sum")
      sum
    }
    println(result)

    assert(result == 15)
  }

  test("should replace element with index") {
    val list = "one" :: "two" :: "three" :: "four" :: Nil
    val expectedList = "one" :: "two" :: "ten" :: "four" :: Nil

    val result = list.replace(2, "ten")

    assert(result == expectedList)
  }
}
