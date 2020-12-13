package finalCads

import io.threadcso._
object Example1
{ /** generate a process that outputs text every quarter second· */
def periodically(text: String): PROC = proc { var count: Int = 0
while (count<10) { Console.print(s"${text}.${count}␣")
  sleep(seconds(0.25))
count += 1 }
}
/** Generate a composite parallel process and run it twice in sequence */
def main(args: Array[String]): Unit =
{ val example: PROC = || (for (arg←args) yield periodically(arg))
      example(); println; example()
   }
}