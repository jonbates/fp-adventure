package adventure

/**
 * The world's simplest possible implementation of the IO monad.
 * A pure way to represent effectful operations as a thunk.
 */
case class IO[A](unsafePerformIO: () => A) {
  def map[B](ab: A => B): IO[B] =
    IO(() => ab(unsafePerformIO()))
  def flatMap[B](afb: A => IO[B]): IO[B] =
    IO(() => afb(unsafePerformIO()).unsafePerformIO())
  def tryIO(ta: Throwable => A): IO[A] =
    IO(() => IO.tryIO(unsafePerformIO()).unsafePerformIO() match {
      case Left(t) => ta(t)
      case Right(a) => a
    })
}

object IO {
  def point[A](a: => A): IO[A] = IO(() => a)

  def tryIO[A](a: => A): IO[Either[Throwable, A]] =
    IO(() => try { Right(a) } catch { case t : Throwable => Left(t) })

  def textToIO(text: List[String]): IO[Unit] =
    text.foldLeft[IO[Unit]](IO.point(())) {
      case (io, str) =>
        for {
          _ <- io
          _ <- Console.putStrLn(str)
        } yield ()
    }
}

object Console {
  /**
   * Writes a line of text to the console.
   */
  def putStrLn(line: String): IO[Unit] = IO(unsafePerformIO = () => println(line))
  def putStr(line: String): IO[Unit] = IO(unsafePerformIO = () => print(line))

  /**
   * Reads a line of text from the console.
   */
  def getStrLn: IO[String] = IO(unsafePerformIO = () => scala.io.StdIn.readLine())
}
