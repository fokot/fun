package warnings

import Warning.{DeprecatedFieldUsed, RecommendedFieldMissing}
import zio.Console.printLine
import zio._
import zio.stm.{STM, TRef}

trait WarningAccInv[A] {
  def raiseWarning(a: A): UIO[Unit]
  def getWarnings: UIO[Chunk[A]]
}

object WarningAccInv {

  def raiseWarning[A](a: A)(implicit aa: Tag[WarningAccInv[A]]): ZIO[WarningAccInv[A], Nothing, Unit] =
    ZIO.serviceWithZIO[WarningAccInv[A]](_.raiseWarning(a))

  def getWarnings[A](implicit aa: Tag[WarningAccInv[A]]): ZIO[WarningAccInv[A], Nothing, Chunk[A]] =
    ZIO.serviceWithZIO[WarningAccInv[A]](_.getWarnings)

  def layer[A](implicit aa: Tag[WarningAccInv[A]]): ULayer[WarningAccInv[A]] =
    ZLayer.fromZIO(TRef.make(Chunk.empty[A]).commit.map(WarningAccInvImpl(_): WarningAccInv[A]))

//  implicit class ZIOWarningUnifier[R, E, A](zio: ZIO[R, E, A]) {
//    def unifyWarnings[W3, W1 <: W3, W2 <: W3](implicit ev: R <:< WarningAccInv[W1] with WarningAccInv[W2]) =
//    zio.provideSome
//  }
}

case class WarningAccInvImpl[A](w: TRef[Chunk[A]]) extends WarningAccInv[A] {
  override def raiseWarning(a: A): UIO[Unit] = w.getAndUpdate(_.prepended(a)).commit.unit

  override def getWarnings: UIO[Chunk[A]] = w.get.commit
}

object WarningInvariant extends ZIOAppDefault {

  import WarningAccInv._

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    (for {
      _ <- printLine("Start")
      _ <- raiseWarning(RecommendedFieldMissing("one"))
      _ <- raiseWarning(DeprecatedFieldUsed("two"))
      _ <- raiseWarning(RecommendedFieldMissing("three"))
      _ <- raiseWarning(DeprecatedFieldUsed("four"))
      _ <- getWarnings[RecommendedFieldMissing].debug("RecommendedFieldMissing")
      _ <- getWarnings[DeprecatedFieldUsed].debug("DeprecatedFieldUsed")
//      _ <- unifyWarnings
      _ <- getWarnings[Warning].debug("DeprecatedFieldUsed")
    } yield ()).provide(
      layer[RecommendedFieldMissing],
      layer[DeprecatedFieldUsed],
      layer[Warning]
    )
}
