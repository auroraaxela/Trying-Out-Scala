import java.util.ArrayList;
import scala.io.Source;
import java.io.{FileReader,FileNotFoundException,IOException};
import util.control.Breaks._;

object TryNewLang {

	val filename = "geodata.txt"
	val start = "%data"
	var dataList: Array[dataNode] = new Array[dataNode](3091);

	try{
		breakable{
			for(line <- Source.fromFile(filename).getLines()){
				if( line == start ) 
				{  break  }
			}
		}
	
		for(line <- Source.fromFile(filename).getLines()){
			var pieces = line.split(",")
			var piece0 = pieces(0);
			var piece1 = pieces(1).toInt;
			var piece2 = pieces(2).toInt;
			var piece3 = pieces(3).toInt;
			var piece4 = pieces(4).toInt;
			var piece5 = pieces(5).toInt;
			val tempItem = new dataNode(piece0, piece1, piece2, piece3, piece4, piece5);
			dataList :+ tempItem;
		}	
	}
	catch{
		case ex: FileNotFoundException => println("Could not find file.")
		case ex: IOException => println("IOException trying to read file.")
	}
	
	println("System ready. ");
	
}


class dataNode( val countryIn: String, val exportIn: Int, val tradeBalIn: Int, val yearIn: Int,
				val popIn: Int, val geoAreaIn: Int){
		val country = countryIn;
		val export = exportIn;
		val tradeBal = tradeBalIn;
		val year = yearIn;
		val pop = popIn;
		val area = geoAreaIn;		
		
		def getCountry() : String = {
		return country; 
		}
		
		def getExp() : Int = {
		return export;
		}
		
		def getBal() : Int = {
		return tradeBal;
		}
		
		def getYear() : Int = {
		return year;
		}
		
		def getPop() : Int = {
		return pop;
		}
		
		def getArea() : Int = {
		return area;
		}		
}