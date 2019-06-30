package util

final case class State[S, A](run: S => (A, S)) {

  def apply(s: S): (A, S) = run(s)

  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, sNew) = run(s)
    (f(a), sNew)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, sNew) = run(s)
    f(a)(sNew)
  })

  def filter(p: A => Boolean): State[S, A] = this

  def >>|[B](f: A => B): State[S, B] = map(f)

  def >>=[B](f: A => State[S, B]): State[S, B] = flatMap(f)

  def >>[B](f: State[S, B]): State[S, B] = State(s => {
    val (_, sNew) = run(s)
    f(sNew)
  })

  def &[B](f: State[S, B]): State[S, (A, B)] = State(s => {
    val (a, s2) = run(s)
    val (b, s3) = f(s2)
    ((a, b), s3)
  })

}

object State {

  def get[S]: State[S, S] = State(s => (s, s))

  def put[S](s: S): State[S, Unit] = State(_ => ((), s))

  def pure[S, A](a: A): State[S, A] = State(s => (a, s))

}

