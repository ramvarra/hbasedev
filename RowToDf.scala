import scala.collection.mutable.ArrayBuffer


/**
 * Convert a row (Seq of cell) to a ArrayBuffer of values, with missing columns padded as nulls
 * @param row: A IndexedSequence containing cells. Each cell is of format (Key, Value)
 * @param allColsMap: Map of each column => its index position in allColumns
 * @param fillMissing: Filler value to use for missing columns. Default null
 * @return ArrayBuffer containing values aligned to their column positions.
 */
def convertRowToArrayOfValues(row: IndexedSeq[(String, String)], allColsMap: Map[String, Int], fillMissing: String=null): ArrayBuffer[String] = {
    val retRow: ArrayBuffer[String] = ArrayBuffer.fill(allColsMap.size)(fillMissing)
    row.foreach(cell => {
      val (colName, value) = cell
      allColsMap.get(colName) match {
        case Some(colIx) => retRow(colIx) = value
        case None => System.err.println(s"Invalid Column name in a cell: $colName")
      }      
    })
    retRow
}
/**
 * Convert Seq of rows to DataFrame (Seq of ArrayBuffers)
 * @param rawData: Seq of rows. Each row is IndexedSeq of cells. Cell = (ColName, Value_
 * @param allCols: IndexedSeq of all colunns expected in the DataFrame.
 * @return Seq of ArrayBuffers.
 */
def convertRowsToDf(rawData: Seq[IndexedSeq[(String, String)]], allCols: IndexedSeq[String]): Seq[ArrayBuffer[String]] = {
  val allColsMap = allCols.zipWithIndex.toMap
  rawData.map(row => convertRowToArrayOfValues(row, allColsMap))
}


/**
 * Test Driver
 */
// Generate simulated raw data List of (cell_name, value) Lists
val charToCell =  (c: Char) => {(c.toString, s"VAL_${c}_${c-'A'+1}")}
val rawData = Seq("ACDG", "ABEG", "ABCDG", "ABCDEFG").map(r => r.map(charToCell))
// Full list of columns needed in our dataFrame
val allCols = "ABCDEFG".map(_.toString)

println(s"All cols = $allCols")

val df = convertRowsToDf(rawData, allCols)

df
