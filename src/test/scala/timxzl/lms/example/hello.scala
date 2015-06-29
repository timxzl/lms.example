package timxzl.lms.example

import java.math.BigInteger

import scala.language.implicitConversions

import java.io.PrintWriter

import org.scalatest.FunSuite

import scala.virtualization.lms.common._

import scala.js.exp.JSExp
import scala.js.gen.js.{GenPrimitiveOps, GenJS}


trait BigInts extends Base {
  def infix_+(x: Rep[BigInteger], y: Rep[BigInteger]): Rep[BigInteger]
  def infix_intValue(v: Rep[BigInteger]): Rep[Int]

  implicit def unit(x: Int): Rep[BigInteger]
  implicit def unit(x: BigInteger): Rep[BigInteger]
  implicit def toBigInt(v: Rep[Int]): Rep[BigInteger]
}

object BigIntHelper {
  def withinInt(x: BigInteger): Boolean = {
    BigInteger.valueOf(x.intValue()).equals(x)
  }
}

trait BigIntExp extends BaseExp with BigInts {
  import BigIntHelper._

  def unit(v: Int) = toAtom(ConstIntBig(v))
  def unit(v: BigInteger) = Const(v)
  def toBigInt(v: Rep[Int]) = v match {
    case Const(x) => toAtom(ConstIntBig(x))
    case _ => toAtom(ToBigInt(v))
  }

  case class ConstIntBig(v: Int) extends Def[BigInteger]
  case class ToBigInt(v: Rep[Int]) extends Def[BigInteger]
  case class Plus(a: Rep[BigInteger], b: Rep[BigInteger]) extends Def[BigInteger]
  case class ToIntValue(v: Rep[BigInteger]) extends Def[Int]

  object ConstBigInt {
    def apply(x: BigInteger): Exp[BigInteger] = {
      if (withinInt(x)) {
        toAtom(ConstIntBig(x.intValue()))
      } else {
        Const(x)
      }
    }
    def unapply(x: Exp[BigInteger]): Option[BigInteger] = x match {
      case Const(v: BigInteger) => Some(v)
      case Def(ConstIntBig(v: Int)) => Some(BigInteger.valueOf(v))
      case _ => None
    }
  }

  def infix_+(x: Exp[BigInteger], y: Exp[BigInteger]): Exp[BigInteger] = (x, y) match {
    case (ConstBigInt(a), ConstBigInt(b)) => {
      val sum = a.add(b)
      if (withinInt(sum)) toAtom(ConstIntBig(sum.intValue())) else Const(sum)
    }
    case (ConstBigInt(a), Def(Plus(ConstBigInt(b), c))) => infix_+( ConstBigInt(a.add(b)) , c )
    case (_, ConstBigInt(a)) => infix_+(y, x)
    case _ => toAtom(Plus(x, y))
  }

  def infix_intValue(v: Exp[BigInteger]) = v match {
    case Def(ConstIntBig(x)) => Const(x)
    case Def(ToBigInt(x)) => x
    case _ => toAtom(ToIntValue(v))
  }
}

trait BigIntGen extends ScalaGenBase {
  val IR: BigIntExp
  import IR._
  override def quote(x: Exp[Any]) = x match {
    case Const(v: BigInteger) =>
      "new java.math.BigInteger(\"" + v.toString + "\")"
    case _ => super.quote(x)
  }
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ConstIntBig(v) =>
      emitValDef(sym, "java.math.BigInteger.valueOf(" + v + ")")
    case ToBigInt(v) =>
      emitValDef(sym, "java.math.BigInteger.valueOf(" + quote(v) + ")")
    case ToIntValue(v) =>
      emitValDef(sym, quote(v) + ".intValue()")
    case Plus(x, y) =>
      emitValDef(sym, quote(x) + ".add(" + quote(y) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait GenJSBigInt extends GenPrimitiveOps {
  val IR: BigIntExp with PrimitiveOpsExp
  import IR._

  override def quote(x: Exp[Any]) = x match {
    case Const(v: BigInteger) =>
      //FIXME
      "new bigint(\"" + v.toString() + "\")"
    case _ => super.quote(x)
  }
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ConstIntBig(v) =>
      emitValDef(sym, v.toString)
    case ToBigInt(v) =>
      emitValDef(sym, quote(v))
    case ToIntValue(v) =>
      emitValDef(sym, quote(v))
    case Plus(x, y) =>
      emitValDef(sym, quote(x) + ".add(" + quote(y) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

class HelloTest extends FunSuite {

  test("power") {
    trait Power extends PrimitiveOps with BooleanOps with OrderingOps with LiftNumeric with LiftBoolean with IfThenElse with Equal {

      def power(b: Rep[Int], x: Int): Rep[Int] = {
        if (x == 0) 1
        else b * power(b, x - 1)
      }

      def power2(b: Rep[Int], x: Int): Rep[Int] = {
        if (x == 0) 1
        else {
          val t = power2(b, x / 2)
          if (x % 2 == 0) t * t else t * t * b
        }
      }

      def snippet(b: Rep[Int]) =
        power2(b, 6)

      def foo(a: Rep[Int], b: Rep[Int]): Rep[Int] = {
        if (a > 5) {
          2
        } else {
          b
        }
      }
    }

    val scalaSnippet = new DslDriver[Int,Int] with Power
    info(scalaSnippet.code)
    assert(scalaSnippet.eval(2) === 64)

    val jsSource = new java.io.StringWriter()
    val printer = new PrintWriter(jsSource)
    val program = new Power with JSExp
    val gen = new GenJS { val IR: program.type = program }
    gen.emitSource(program.snippet, "power", printer)
    gen.emitSource2(program.foo, "foo", printer)
    info(jsSource.toString)
  }

  test("BigInt") {


    trait BigProg extends PrimitiveOps with BooleanOps with OrderingOps with LiftNumeric with LiftBoolean with IfThenElse with Equal with BigInts {
      def snippet(a: Rep[BigInteger]) = {
        val x = a + a
        val b = a.intValue
        val c = b + toBigInt(8)
        3 + x + 5 + c
      }
    }

    abstract class MyDriver[A: Manifest, B: Manifest] extends DslDriver[A, B] with DslImpl with DslExp with BigIntExp { q =>
      override val codegen = new DslGen with BigIntGen {
        val IR: q.type = q
      }
    }

    val sSnip = new MyDriver[BigInteger, BigInteger] with BigProg
    info(sSnip.code)
    val two = BigInteger.valueOf(2)
    val result = BigInteger.valueOf(3+2+2+5+2+8)
    assert(result == sSnip.eval(two))

    val jSrc = new java.io.StringWriter()
    val printer = new PrintWriter(jSrc)
    val program = new BigProg with JSExp with BigIntExp
    val gen = new GenJS with GenJSBigInt { val IR: program.type = program }
    gen.emitSource(program.snippet, "snippet", printer)
    info(jSrc.toString)
  }
}

