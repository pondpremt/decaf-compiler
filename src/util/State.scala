package util

case class State[S, A](run: S => (A, S)) extends (S => (A, S)) {

  def apply(s: S): (A, S) = run(s)

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, t) = run(s)
    f(a)(t)
  })

  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, t) = run(s)
    (f(a), t)
  })

}

object State {

  def pure[S, A](a: A): State[S, A] = State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def put[S](s: S): State[S, Unit] = State(_ => ((), s))

}
