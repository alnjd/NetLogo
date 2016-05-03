// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim.etc

import org.nlogo.agent.Turtle
import org.nlogo.core.Syntax
import org.nlogo.nvm.{ Command, Context }

class _pendown extends Command {


  switches = true
  override def perform(context: Context) {
    context.agent.asInstanceOf[Turtle].penMode(Turtle.PEN_DOWN)
    context.ip = next
  }
}