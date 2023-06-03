package warnings

import Warning.{DeprecatedFieldUsed, RecommendedFieldMissing}
import zio._
import zio.stm.{STM, TRef}

import scala.reflect.ClassTag

trait WarningAcc[+A] {
  def raiseWarning[A1 >: A](a: A1): UIO[Unit]
  def getAllWarnings: UIO[Chunk[Any]]
}

object WarningAcc {

  def raiseWarning[A](a: A)(implicit aa: Tag[WarningAcc[A]]): ZIO[WarningAcc[A], Nothing, Unit] =
    ZIO.serviceWithZIO[WarningAcc[A]](_.raiseWarning(a))

  def getWarnings[A: ClassTag](implicit aa: Tag[WarningAcc[A]]): ZIO[WarningAcc[A], Nothing, Chunk[A]] =
    ZIO.serviceWithZIO[WarningAcc[A]](_.getAllWarnings.map(_.collect { case a: A => a}))

  def warningsLayer: ULayer[WarningAccImpl] = ZLayer.fromZIO(TRef.make(Chunk.empty[Any]).commit.map(WarningAccImpl))

  def layer[A]: ZLayer[WarningAccImpl, Nothing, WarningAcc[A]] =
    ZLayer.service[WarningAccImpl].map(_.asInstanceOf[ZEnvironment[WarningAcc[A]]])
}

case class WarningAccImpl(w: TRef[Chunk[Any]]) extends WarningAcc[Any] {

  override def raiseWarning[A](a: A): UIO[Unit] = w.getAndUpdate(_.prepended(a)).commit.unit

  override def getAllWarnings: UIO[Chunk[Any]] = w.get.commit
}
//
//zio.transformEnvironment(R => R1)
//ZIO[R, .., ..] => ZIO[R1, .., ..]

object WarningCovariant extends ZIOAppDefault {

  import WarningAcc._

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    (for {
      _ <- raiseWarning(RecommendedFieldMissing("one"))
      _ <- raiseWarning(DeprecatedFieldUsed("two"))
      _ <- raiseWarning(RecommendedFieldMissing("three"))
      _ <- raiseWarning(DeprecatedFieldUsed("four"))
      _ <- getWarnings[RecommendedFieldMissing].debug("RecommendedFieldMissing")
      _ <- getWarnings[DeprecatedFieldUsed].debug("DeprecatedFieldUsed")
      _ <- getWarnings[Warning].debug("All warnings")
    } yield ()).provide(
      layer[RecommendedFieldMissing],
      layer[DeprecatedFieldUsed],
      warningsLayer,
    )
}
