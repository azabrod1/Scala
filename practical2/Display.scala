package practical2

import java.awt._


class Display(N: Int, var pic : Array[Boolean]) extends Frame {
  // Define some constants
  private val blockSize = 8
  private val padding   = 1
  private val gridSize  = blockSize+2*padding
  
  // Set up the display
  private val pane = new ScrollPane() 
  pane.setSize(N*gridSize, N*gridSize)
  private val board = new Board()
  pane.add(board)
  this.add(pane, "Center")
  this.pack()
  this.setVisible(true)
  this.setTitle("Life")
  this.setSize(N*gridSize, N*gridSize)
  
  // Fill in all the squares
  def draw = {
    for (i <- 0 until N){
        for (j<-0 until N){
            if (pic(N*i+j)) board.drawAt(j,i) else board.blankAt(j,i)
        }
    }
  }
  
  
  //We return the array that was used to color the board while replacing it with a new one
  def replaceBoard(replacement : Array[Boolean]) : Array[Boolean] = {
    
    val toReturn = this.pic;
    this.pic = replacement;
    
     toReturn
    
  }

  override def paint(g: Graphics) = draw

  class Board extends Component{
    // Define colours
    val backgroundColor = Color.gray.brighter
    val blockColor      = Color.blue

    // Paint the square at (x,y) in colour c
    private def paintAt(x: Int, y: Int, c: Color) = {    
      val g = getGraphics()
      g.setColor(c)
      g.fillRect(x*gridSize+padding, y*gridSize+padding, blockSize, blockSize)
    }

    // Draw a piece
    def drawAt(x: Int, y: Int) = paintAt(x, y, blockColor)

    // Draw a blank square
    def blankAt(x: Int, y: Int) = paintAt(x, y, backgroundColor)
  }

}

