
import scala.io.Source
import java.io._

trait FileInformation
{
  def readFile(directory:Array[java.io.File]):List[(String,String)]
  def readContent(files:List[(String,String)]): List[(String,List[String])]
  def countingWord(information:List[(String,List[String])]):List[(String,Map[String,Int])]
  def writeContent(information:List[(String,Map[String,Int])]):String
}

class FileProcessing extends FileInformation {
  
  def readFile(directory:Array[java.io.File]):List[(String,String)]=
  {
    val files=for{i<-directory
		    val file= i.toString
	            val index = file.lastIndexOf("/")
	            val filename= file.substring(index+1,file.length)
	          } yield (filename.toString(),file)
		
        files.toList   // returning list of file names and their address
   }
  
  
  
  def readContent(files:List[(String,String)]): List[(String,List[String])]=
  {

      val information= files.map{x=>
                            val filename=x._1
                            val address = x._2
                            val content= for{line<-Source.fromFile(address).getLines()}yield line
		            (filename,content.toList) }

                            information   // returning list that contains file name and data of file
  }
  
  
  def countingWord(information:List[(String,List[String])]):List[(String,Map[String,Int])]=
  {
    val info= information.map{x=>(x._1,x._2.map(_.toLowerCase()))} // converting into small characters so that "Hello" and "hello" seems to be equal
    val mapper=info.map{x=>	
	                   val filename=x._1
		           val containt=x._2
		           val data = containt.map{data=> (data split "\\W+").toList } // splitting line to words
                           val count=data.flatten.groupBy((word:String)=>word).mapValues(_.length)
		          (filename,count) }
        
                           mapper// returning file name and wordcount of each file
  }
  
  def writeContent(information:List[(String,Map[String,Int])]):String=
  {

      information.map{x=>	
                          val filename=x._1
		          val content=x._2
		          val writer=new PrintWriter(new File("/home/shubham/Desktop/new/"+filename)) 
                          content.map{data=> writer.write((data._1)+"-> "+(data._2)+" \n")}// wirting data of file to another file of same name
		          writer.close()}
      
                         "data has been written successfully" // returning status
  }
  
}

object WordCount extends App
{
  try{
       val directory = new java.io.File("/home/shubham/Desktop/charmy/").listFiles.filter(_.getName.endsWith(".txt"))
       if(directory.isEmpty) throw new IOException("directory in null")
       else
       {
             val obj = new FileProcessing
	     val files=obj.readFile(directory)
	     val information=obj.readContent(files)
	     val mapper=obj.countingWord(information)
	     val status=obj.writeContent(mapper)
	     println(status)
       }
    }
  catch{
    
        case ioe:IOException=>println(ioe.getMessage)
        case ex:Exception=>   println(ex.getMessage)
    
  }
	   
}
 
