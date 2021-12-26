import scala.annotation.tailrec
import scala.io.Source
//import Day16._

val in = Source.fromFile(getClass.getResource("/inputs/16_input.txt").getFile)
val inputs = in.getLines().toList.head

object Day16 {
  case class Message(version: Int, typeId: Int, value: Long) {
    def updateValue(new_value: Long): Message = Message(version, typeId, new_value)
  }
  case class Transmission(bits: String, messages: List[Message], operators: List[List[Long] => Long], sum_versions: Int)

  def parseHexStringToBinary(hex_string: String): List[String] = {
    hex_string.toList.map(x => Integer.parseInt(x.toString, 16).toBinaryString).map(x => ("000" + x).takeRight(4))
  }

  def getVersion(tm: Transmission): Transmission = {
    val version = Integer.parseInt(tm.bits.take(3),2)
    Transmission(tm.bits.drop(3), Message(version,-1,-1) :: tm.messages, tm.operators, tm.sum_versions + version)
  }

  def getOperator(tm: Transmission): Transmission = {
    val op: List[Long] => Long = tm.messages.head.typeId match {
      case 0 => x => x.sum
      case 1 => x => x.product
      case 2 => x => x.min
      case 3 => x => x.max
      case 5 => x => if (x.last > x.head) 1 else 0
      case 6 => x => if (x.last < x.head) 1 else 0
      case 7 => x => if (x.last == x.head) 1 else 0
    }
    Transmission(tm.bits, tm.messages, op :: tm.operators, tm.sum_versions)
  }

  def getType(tm: Transmission): Transmission = {
    val version = tm.messages.head.version
    val typeId = Integer.parseInt(tm.bits.take(3), 2)
    val new_tm = Transmission(tm.bits.drop(3), Message(version, typeId, -1) :: tm.messages.tail, tm.operators, tm.sum_versions)

    if (typeId == 4) {
      new_tm
    } else {
      getOperator(new_tm)
    }
  }

  def getType4Body(bits: List[String]): (List[String], Int) = {
    bits.take(5) match {
      case "1" :: xs =>
        val temp = getType4Body(bits.drop(5))
        (xs ::: temp._1, 5 + temp._2)
      case "0" :: xs => (xs, 5)
    }
  }

  def evaluateOperator(tm: Transmission): Transmission = {
    val (value_messages, other_messages) = tm.messages.span(_.value != -1)
    val op = tm.operators.head
    val evaluated = op(value_messages.map(_.value))
    Transmission(tm.bits, other_messages.head.updateValue(evaluated) :: other_messages.tail, tm.operators.tail, tm.sum_versions)
  }


  def operator15(tm: Transmission): Transmission = {
    val subpacket_length = Integer.parseInt(tm.bits.take(15), 2)
    val prep_tm = Transmission(tm.bits.drop(15), tm.messages, tm.operators, tm.sum_versions)

    @tailrec
    def iter(accum: Int, tm: Transmission): Transmission = {
      if (prep_tm.bits.length - tm.bits.length == subpacket_length) {
        evaluateOperator(tm)
      } else {
        iter(accum + 1, processTransmission(tm))
      }
    }
    iter(0, prep_tm)
  }

  def operator11(tm: Transmission): Transmission = {
    val subpacket_count = Integer.parseInt(tm.bits.take(11), 2)
    val prep_tm = Transmission(tm.bits.drop(11), tm.messages, tm.operators, tm.sum_versions)

    @tailrec
    def iter(accum: Int, tm: Transmission): Transmission = {
      if (accum == subpacket_count) {
        evaluateOperator(tm)
      } else {
        iter(accum + 1, processTransmission(tm))
      }
    }
    iter(0, prep_tm)
  }

  def getBody(tm: Transmission): Transmission = {
    if (tm.messages.head.typeId == 4) {
      val (body, size) = getType4Body(tm.bits.toList.map(_.toString))
      val value = java.lang.Long.parseLong(body.reduce(_ + _),2)
      Transmission(
        tm.bits.drop(size),
        Message(tm.messages.head.version, tm.messages.head.typeId, value) :: tm.messages.tail, tm.operators, tm.sum_versions)
    } else {
      val bits = tm.bits.tail
      val new_tm = Transmission(bits, tm.messages, tm.operators, tm.sum_versions)
      tm.bits.head match {
        case '0' => operator15(new_tm)
        case '1' => operator11(new_tm)
      }
    }
  }

  def processTransmission(tm: Transmission): Transmission = {
    (getVersion _ andThen getType andThen getBody)(tm)
  }
}

import Day16._
val binary_inputs = parseHexStringToBinary(inputs)
val transmission = processTransmission(Transmission(binary_inputs.reduce(_ + _), List(), List(), 0))
val result_part1 = transmission.sum_versions
val result_part2 = transmission.messages.head.value