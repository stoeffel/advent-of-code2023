package aoc

class TestSuite extends munit.FunSuite:
  def assertRight[A, B](either: Either[A, B], expected: B): Unit =
    either match
      case Right(value) => assertEquals(value, expected)
      case Left(error)  => fail(s"Expected Right($expected), got Left($error)")

  def assertLeft[A, B](either: Either[A, B]): Unit =
    either match
      case Right(value) => fail(s"Expected Left, got Right($value)")
      case Left(_)      => assert(true)
