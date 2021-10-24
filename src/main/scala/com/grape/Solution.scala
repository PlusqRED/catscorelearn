package com.grape

import scala.annotation.tailrec

object Solution {

  def returnCommonPrefix(left: String, right: String): String =
    left
      .zip(right)
      .takeWhile(t => t._1 == t._2)
      .map(_._1.toString)
      .foldLeft(new StringBuilder())(_.append(_))
      .toString()

  @tailrec
  def longestCommonPrefixReq(strs: Array[String]): String = {
    if (strs.length == 1) return strs(0)
    longestCommonPrefixReq(
      strs
        .combinations(2)
        .map(t => returnCommonPrefix(t(0), t(1)))
        .toArray
    )
  }

  def longestCommonPrefix(strs: Array[String]): String = {
    if (strs.length == 0 || strs.exists(_.isEmpty)) return ""
    if (strs.length == 1) return strs(0)
    longestCommonPrefixReq(strs)
  }

  def main(args: Array[String]): Unit = {
    assert(
      longestCommonPrefix(Array("cir", "car")) == "c",
      longestCommonPrefix(Array("cir", "car"))
    )
    assert(
      longestCommonPrefix(Array("car", "car", "cag")) == "ca",
      longestCommonPrefix(Array("car", "car", "cag"))
    )
    assert(
      longestCommonPrefix(Array("abcd", "abc")) == "abc",
      longestCommonPrefix(Array("abcd", "abc"))
    )
    assert(
      longestCommonPrefix(Array("ab", "abc", "abcd", "abcde")) == "ab",
      longestCommonPrefix(Array("ab", "abc", "abcd", "abcde"))
    )
    assert(
      longestCommonPrefix(Array("aba", "abb", "abcd", "abcd")) == "ab",
      longestCommonPrefix(Array("aba", "abb", "abcd", "abcd"))
    )
    assert(
      longestCommonPrefix(Array("", "abb", "abcd", "abcd")) == "",
      longestCommonPrefix(Array("", "abb", "abcd", "abcd"))
    )
    assert(
      longestCommonPrefix(Array("aba", "abb", "abcd", "a")) == "a",
      longestCommonPrefix(Array("aba", "abb", "abcd", "a"))
    )
    assert(
      longestCommonPrefix(Array("aba", "abab", "ababc", "abae")) == "aba",
      longestCommonPrefix(Array("aba", "abab", "ababc", "abae"))
    )
    assert(longestCommonPrefix(Array()) == "", longestCommonPrefix(Array()))
    assert(
      longestCommonPrefix(Array("abcde")) == "abcde",
      longestCommonPrefix(Array("abcde"))
    )
    assert(
      longestCommonPrefix(Array("a", "b", "c", "b")) == "",
      longestCommonPrefix(Array("a", "b", "c", "b"))
    )
  }
}
