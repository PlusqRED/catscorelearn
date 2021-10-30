package com.grape

import scala.annotation.tailrec

object AmazonTask {
  def firstNonRepeatingCharacter(str: String): Char = {
    firstNonRepeatingCharacterReq(str, str.tail, 0)
  }

  @tailrec
  def firstNonRepeatingCharacterReq(
      original: String,
      str: String,
      chIndex: Int
  ): Char = if (chIndex == original.length) '_'
  else if (str.contains(original.charAt(chIndex)))
    firstNonRepeatingCharacterReq(
      original,
      original.patch(chIndex + 1, "", 1),
      chIndex + 1
    )
  else original.charAt(chIndex)

  def main(args: Array[String]): Unit = {
    println(firstNonRepeatingCharacter("aaabcccdeeef"))
    println(firstNonRepeatingCharacter("abcbad"))
    println(firstNonRepeatingCharacter("abcabcabc"))
  }
}
