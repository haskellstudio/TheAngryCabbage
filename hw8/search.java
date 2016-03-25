import java.util.Scanner;
import java.util.ArrayList;
import java.util.*;
import java.io.*;
import java.net.*;

public class search {
    

    public static void main(String[] args) throws Exception {
	Map<String, ArrayList<Integer>> words = new HashMap<String, ArrayList<Integer>>();
	//Map<String, Integer> words = new HashMap<String, Integer>();
	URL url = new URL("http://homes.soic.indiana.edu/classes/spring2016/csci/c343-yye/docu.txt");
	Scanner in = new Scanner(url.openStream());
	int line = 0;
	ArrayList<Integer> lineNumber = new ArrayList
	
	while(in.hasNext()) {
	    String str = in.nextLine();
	    line++;
	    String[] parts = str.split(" ");
	    
	    for(int i = 0; i < parts.length; i++) {
		if(words.containsKey(parts[i])) {
		    words.put(parts[i], words.get(parts[i]).add(line));
		    }else{
		    words.put(parts[i], lineNumber.add(line));
		    }
	    }
	}
	System.out.println(words);
	System.out.println();
	System.out.println(line);
    }
}
