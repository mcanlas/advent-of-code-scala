package com.htmlism

package object adventofcode:
  implicit class PipelineOps[A](x: A):
    def andThen[B](f: A => B): B =
      f(x)
