/* author:  daemoniclegend  */


import java.util.ArrayList;
import scala.io.Source;
import scala.io._;
import scala.io.StdIn.readInt;
import java.io.{FileReader,FileNotFoundException,IOException};
import scala.util.control.Breaks._;


object TryNewLang {
	val filename = "geodata.txt"
	val start = "%data"
	var dataList: Array[dataNode] = new Array[dataNode](0);
	var header = true;
	var counter = 0;


	def main(args: Array[String]) { 
		try{
			for(line <- Source.fromFile(filename).getLines()){
				if(header == true){
					if( line.equalsIgnoreCase("%data")) header = false;
				}
				else{
				var pieces = line.split(",")
				var piece0: String = pieces(0);
				var piece1: Int = pieces(1).toInt;
				var piece2: Int = pieces(2).toInt;
				var piece3: Int = pieces(3).toInt;
				var piece4: Int = pieces(4).toInt;
				var piece5: Int = pieces(5).toInt;
				val tempItem = new dataNode(piece0, piece1, piece2, piece3, piece4, piece5);

				dataList = dataList ++ Array(tempItem);
				}
			}
		println("It tingles so you know it's working! ");		
		}
		catch{
			case ex: FileNotFoundException => println("Could not find file.")
			case ex: IOException => println("IOException trying to read file.")
		}
		println();
		println("Please select one of the following options: ")
		println("1. Top 5 exporting countries")
		println("2. Worst 5 exporting countries")	
		println("3. 5 best trade balances")
		println("4. 5 worst trade balances")
		println("5. 5 best export/balance of trade ratios")
		println("6. 5 worst export/balance of trade ratios")
		println("7. 5 largest populations")
		
		var selection = readInt();
		
		println("Please enter a year between 1986 and 2006: ")
		var yearSelect = readInt();
		
		if( selection == 1){
			topExport(yearSelect)
		}
		else if( selection == 2 ){
			bottomExport(yearSelect)
		}
		else if( selection == 3 ){
			bestBal(yearSelect)
		}
		else if( selection == 4 ){
			worstBal(yearSelect)
		}
		else if( selection == 5 ){
			bestRatio(yearSelect)
		}
		else if( selection == 6 ){
			worstRatio(yearSelect)
		}
		else if( selection == 7 ){
			popSize(yearSelect)
		}
		else{ println("not valid") }
	}


	def topExport(yearIn: Int): Unit ={
		val yearSelected = yearIn;
		var index = 0;
		var tempArray: Array[dataNode] = new Array[dataNode](0);
		for( index <- 0 to 3090){
			var currentItem: dataNode = dataList(index);
			var test = currentItem.getYear();
			if(test == yearSelected){
				tempArray = currentItem +: tempArray;
			}
		}
		index = 0;
		for(index <- 0 to 5){
			var innerIndex = 0;
			var currentMax: Int = 0;
			var location = 0;
			for(innerIndex <- index to ( tempArray.size - 1)){
				if( tempArray(innerIndex).getExp() > currentMax){
					currentMax = tempArray(innerIndex).getExp();
					location = innerIndex;
				}
			}
			var tempA = tempArray(location);
			var tempB = tempArray(index);
			tempArray(location) = tempB;
			tempArray(index) = tempA;
		}
		println("Results for year: " + yearSelected)
		println("#1: " + tempArray(0).getCountry() + " " + tempArray(0).getExp())
		println("#2: " + tempArray(1).getCountry() + " " + tempArray(1).getExp())
		println("#3: " + tempArray(2).getCountry() + " " + tempArray(2).getExp())
		println("#4: " + tempArray(3).getCountry() + " " + tempArray(3).getExp())
		println("#5: " + tempArray(4).getCountry() + " " + tempArray(4).getExp())
	}/*END TOPEXPORT */


	def bottomExport(yearIn: Int): Unit ={
		val yearSelected = yearIn;
		var index = 0;
		var tempArray: Array[dataNode] = new Array[dataNode](0);
		for( index <- 0 to 3090){
			var currentItem: dataNode = dataList(index);
			var test = currentItem.getYear();
			if(test == yearSelected){
				tempArray = currentItem +: tempArray;
			}
		}
		index = 0;
		for(index <- 0 to 5){
			var innerIndex = 0;
			var currentMax: Int = 1000000000;
			var location = 0;
			for(innerIndex <- index to ( tempArray.size - 1)){
				if( tempArray(innerIndex).getExp() < currentMax){
					currentMax = tempArray(innerIndex).getExp();
					location = innerIndex;
				}
			}
			var tempA = tempArray(location);
			var tempB = tempArray(index);
			tempArray(location) = tempB;
			tempArray(index) = tempA;	
		}
		println("Results for year: " + yearSelected)
		println("#1: " + tempArray(0).getCountry() + " " + tempArray(0).getExp())
		println("#2: " + tempArray(1).getCountry() + " " + tempArray(1).getExp())
		println("#3: " + tempArray(2).getCountry() + " " + tempArray(2).getExp())
		println("#4: " + tempArray(3).getCountry() + " " + tempArray(3).getExp())
		println("#5: " + tempArray(4).getCountry() + " " + tempArray(4).getExp())
	}


	def bestBal(yearIn: Int): Unit ={
		val yearSelected = yearIn;
		var index = 0;
		var tempArray: Array[dataNode] = new Array[dataNode](0);
		for( index <- 0 to 3090){
			var currentItem: dataNode = dataList(index);
			var test = currentItem.getYear();
			if(test == yearSelected){
				tempArray = currentItem +: tempArray;
			}
		}
		index = 0;
		for(index <- 0 to 5){
			var innerIndex = 0;
			var currentMax: Int = 0;
			var location = 0;
			for(innerIndex <- index to ( tempArray.size - 1)){
				if( tempArray(innerIndex).getBal() > currentMax){
					currentMax = tempArray(innerIndex).getBal();
					location = innerIndex;
				}
			}
			var tempA = tempArray(location);
			var tempB = tempArray(index);
			tempArray(location) = tempB;
			tempArray(index) = tempA;	
		}
		println("Results for year: " + yearSelected)
		println("#1: " + tempArray(0).getCountry() + " " + tempArray(0).getBal())
		println("#2: " + tempArray(1).getCountry() + " " + tempArray(1).getBal())
		println("#3: " + tempArray(2).getCountry() + " " + tempArray(2).getBal())
		println("#4: " + tempArray(3).getCountry() + " " + tempArray(3).getBal())
		println("#5: " + tempArray(4).getCountry() + " " + tempArray(4).getBal())
	} /*END BESTBAL*/


	def worstBal(yearIn: Int): Unit ={
		val yearSelected = yearIn;
		var index = 0;
		var tempArray: Array[dataNode] = new Array[dataNode](0);
		for( index <- 0 to 3090){
			var currentItem: dataNode = dataList(index);
			var test = currentItem.getYear();
			if(test == yearSelected){
				tempArray = currentItem +: tempArray;
			}
		}
		index = 0;
		for(index <- 0 to 5){
			var innerIndex = 0;
			var currentMax: Int = 1000000000;
			var location = 0;
			for(innerIndex <- index to ( tempArray.size - 1)){
				if( tempArray(innerIndex).getBal() < currentMax){
					currentMax = tempArray(innerIndex).getBal();
					location = innerIndex;
				}
			}
			var tempA = tempArray(location);
			var tempB = tempArray(index);
			tempArray(location) = tempB;
			tempArray(index) = tempA;	
		}
		println("Results for year: " + yearSelected)
		println("#1: " + tempArray(0).getCountry() + " " + tempArray(0).getBal())
		println("#2: " + tempArray(1).getCountry() + " " + tempArray(1).getBal())
		println("#3: " + tempArray(2).getCountry() + " " + tempArray(2).getBal())
		println("#4: " + tempArray(3).getCountry() + " " + tempArray(3).getBal())
		println("#5: " + tempArray(4).getCountry() + " " + tempArray(4).getBal())
	} /*END WORSTBAL*/


	def bestRatio(yearIn: Int): Unit ={
		val yearSelected = yearIn;
		var index = 0;
		var tempArray: Array[dataNode] = new Array[dataNode](0);
		for( index <- 0 to 3090){
			var currentItem: dataNode = dataList(index);
			var test = currentItem.getYear();
			if(test == yearSelected){
				tempArray = currentItem +: tempArray;
			}
		}
		index = 0;
		for(index <- 0 to 5){
			var innerIndex = 0;
			var currentMax: Int = 0;
			var location = 0;
			for(innerIndex <- index to ( tempArray.size - 1)){
				if( tempArray(innerIndex).getRatio() > currentMax){
					currentMax = tempArray(innerIndex).getRatio();
					location = innerIndex;
				}
			}
			var tempA = tempArray(location);
			var tempB = tempArray(index);
			tempArray(location) = tempB;
			tempArray(index) = tempA;	
		}
		println("Results for year: " + yearSelected)
		println("#1: " + tempArray(0).getCountry() + " " + tempArray(0).getRatio())
		println("#2: " + tempArray(1).getCountry() + " " + tempArray(1).getRatio())
		println("#3: " + tempArray(2).getCountry() + " " + tempArray(2).getRatio())
		println("#4: " + tempArray(3).getCountry() + " " + tempArray(3).getRatio())
		println("#5: " + tempArray(4).getCountry() + " " + tempArray(4).getRatio())
	} /*END BESTRATIO */


	def worstRatio(yearIn: Int): Unit ={
		val yearSelected = yearIn;
		var index = 0;
		var tempArray: Array[dataNode] = new Array[dataNode](0);
		for( index <- 0 to 3090){
			var currentItem: dataNode = dataList(index);
			var test = currentItem.getYear();
			if(test == yearSelected){
				tempArray = currentItem +: tempArray;
			}
		}
		index = 0;
		for(index <- 0 to 5){
			var innerIndex = 0;
			var currentMax: Int = 1000000000;
			var location = 0;
			for(innerIndex <- index to ( tempArray.size - 1)){
				if( tempArray(innerIndex).getRatio() < currentMax){
					currentMax = tempArray(innerIndex).getRatio();
					location = innerIndex;
				}
			}
			var tempA = tempArray(location);
			var tempB = tempArray(index);
			tempArray(location) = tempB;
			tempArray(index) = tempA;	
		}
		println("Results for year: " + yearSelected)
		println("#1: " + tempArray(0).getCountry() + " " + tempArray(0).getRatio())
		println("#2: " + tempArray(1).getCountry() + " " + tempArray(1).getRatio())
		println("#3: " + tempArray(2).getCountry() + " " + tempArray(2).getRatio())
		println("#4: " + tempArray(3).getCountry() + " " + tempArray(3).getRatio())
		println("#5: " + tempArray(4).getCountry() + " " + tempArray(4).getRatio())	
	} /* END WORSTRATIO */


	def popSize(yearIn: Int): Unit ={
		val yearSelected = yearIn;
		var index = 0;
		var tempArray: Array[dataNode] = new Array[dataNode](0);
		for( index <- 0 to 3090){
			var currentItem: dataNode = dataList(index);
			var test = currentItem.getYear();
			if(test == yearSelected){
				tempArray = currentItem +: tempArray;
			}
		}
		index = 0;
		for(index <- 0 to 5){
			var innerIndex = 0;
			var currentMax: Int = 0;
			var location = 0;
			for(innerIndex <- index to ( tempArray.size - 1)){
				if( tempArray(innerIndex).getPop() > currentMax){
					currentMax = tempArray(innerIndex).getPop();
					location = innerIndex;
				}
			}
			var tempA = tempArray(location);
			var tempB = tempArray(index);
			tempArray(location) = tempB;
			tempArray(index) = tempA;	
		}
		println("Results for year: " + yearSelected)
		println("#1: " + tempArray(0).getCountry() + " " + tempArray(0).getPop())
		println("#2: " + tempArray(1).getCountry() + " " + tempArray(1).getPop())
		println("#3: " + tempArray(2).getCountry() + " " + tempArray(2).getPop())
		println("#4: " + tempArray(3).getCountry() + " " + tempArray(3).getPop())
		println("#5: " + tempArray(4).getCountry() + " " + tempArray(4).getPop())
	}
	
}


class dataNode( val countryIn: String, val exportIn: Int, val tradeBalIn: Int, val yearIn: Int,
				val popIn: Int, val geoAreaIn: Int){
		val country = countryIn;
		val export = exportIn;
		val tradeBal = tradeBalIn;
		val year = yearIn;
		val pop = popIn;
		val area = geoAreaIn;	
		var ratio = export/tradeBal;
		
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
		 def getRatio() : Int = {
			return ratio
		}
}