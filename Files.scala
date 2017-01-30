import java.lang.String
import scala.io.Source
import java.io._


trait FileParsing
{
   def readFile(directory:Array[java.io.File]):List[(String,String)]
   def readContent(files:List[(String,String)]):List[(String,List[String])]
   def capitalization(information:List[(String,List[String])]): List[(String,List[String])]
   def writeContent(information:List[(String,List[String])]):String
}


class Directory extends FileParsing
{
  
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


    def capitalization(information: List[(String,List[String])]): List[(String,List[String])]=
    {
         information.map{x=>(x._1,x._2.map(_.toUpperCase()))} // capitalize the data of every file
    }

    def writeContent(information:List[(String,List[String])]):String=
    {
       information.map{x=>	
                           val filename=x._1
		           val content=x._2
		           val writer=new PrintWriter(new File("/home/shubham/Desktop/shub/"+filename)) 
                           content.map{data=> writer.write(data+"\n")} // wirting data of file to another file of same name
	                   writer.close()
                       }
       
            "data has been written successfully" // returning status
     }
}

object Files extends App
{
  try
  {
	  val directory = new java.io.File("/home/shubham/Desktop/charmy/").listFiles.filter(_.getName.endsWith(".txt"))
	  if(directory.isEmpty) throw new IOException("directory is null") else
	  {
	      val obj = new Directory
	      val files=obj.readFile(directory)
	      val information=obj.readContent(files)
	      val capital=obj.capitalization(information)
	      val status=obj.writeContent(capital)
	      println(status)
	  }
  }
  
  catch{
    
          case ioe:IOException=>println(ioe.getMessage)
          case ex:Exception=>   println(ex.getMessage)
    
    
      }
}


