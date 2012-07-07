/**
 * チェスのボード.
 */
case class Bord(word:Array[String]) {

  /**
   * 高さ.
   */
  val height = word.size

  /**
   * 幅.
   */
  val width = word(0).size

  /**
   * 位置がボードの範囲内にあるかどうか.
   */
  def isWithin(p:Pos) = 
    p.x >= 1 && p.x <= width && p.y >= 1 && p.y <= width

  /**
   * 幅×高さの全ての位置のリスト.
   */
  def allArea:List[Pos] =
    (for(x <- 1 to width; y <- 1 to height) yield Pos(x, y)).toList

  /**
   * 指定された位置のリストに対するキーワードを返す.
   */
  def keyword(l:List[Pos]):String = l.map(p => word(p.y - 1)(p.x - 1)).foldLeft("") { (x, y) => x + y }
}


/**
 * チェスの位置.
 */
case class Pos(x:Int, y:Int) extends Ordered[Pos] {

  def compare(that: Pos) = {
    val a = x * y
    val b = that.x * that.y

    if (a > b || (a == b && x > that.x))
      1
    else if (a == b && x == that.x)
      0
    else
      -1
  }
  
  override def toString():String = "(" + x + "," + y + ")"
}
/**
 * クイーンの駒.
 */
case class Queen(bord:Bord, pos:Pos) {

  /**
   * 指定された方向へ動かせる位置のリストを返す。
   * @param dx X軸の方向 -1:左 1:右 0:NONE
   * @param dy Y軸の方向 -1:上 1:下 0:NONE
   * @return 一方向へ動かせる位置のリスト
   */
  private def moveable(dx:Int, dy:Int):List[Pos] = {

    assert(dx >= -1 && dx <= 1)
    assert(dy >= -1 && dy <= 1)

    // 1方向進ませる
    val newp = Pos(pos.x + dx, pos.y + dy)

    if (bord.isWithin(pos)) {

      newp :: Queen(bord, newp).moveable(dx, dy)
    } else {

      Nil
    }
  }

  /**
   * クイーンが進める位置のリストを返す。
   * @param p クイーンの位置
   * @return クイーンが進める位置のリスト
   */
  def moveable():List[Pos] = pos :: 
    moveable(-1,  0) ++ // 左
    moveable(-1, -1) ++ // 左上
    moveable( 0, -1) ++ // 上
    moveable( 1, -1) ++ // 右上
    moveable( 1,  0) ++ // 右
    moveable( 1,  1) ++ // 右下
    moveable( 0,  1) ++ // 下
    moveable(-1,  1)    // 左下
}


/**
 * Nクイーン問題。
 */
object NQueenProblem extends App {

  val bord = Bord(Array("lhikoav"
                      , "rqsczlp"
                      , "uwalnfo"
                      , "tykajeh"
                      , "ahitsyd"
                      , "efoptxn"
                      , "ruzwyve"))
    
  // 7 × 7のボード
  val queen1 = Queen(bord, Pos(3, 3))   // クイーン1
  val queen2 = Queen(bord, Pos(4, 5))   // クイーン2

  /**
   * areaで指定された位置の中で、クイーンが置ける位置のリスト
   * を複数パターン返す。
   */
  def ansers(area:List[Pos]):List[List[Pos]] = area match {

    case Nil => Nil
    case _   => area.flatMap { p =>
      val anss = ansers(area filterNot (Queen(bord, p).moveable contains))
      anss match {
        case Nil => List(List(p))
        case _   => anss.map(ans => p :: ans)
      }
    }
  }

  val firstArea = bord.allArea filterNot 
                    (queen1.moveable contains) filterNot 
                    (queen2.moveable contains)
  val anss = ansers(firstArea).map(l => queen1.pos :: queen2.pos :: l)
 
  val x = anss.map( x => x.sorted ).toSet.toList.sortWith(_.size > _.size)
  x.map { l =>
    println(bord.keyword(l) + " : " + l.mkString(", "))
    
  }
}

