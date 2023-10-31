import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.io.BufferedReader;
import java.io.FileReader;

import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.ANTLRFileStream;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import javax.swing.*;
import org.antlr.v4.runtime.tree.ParseTree;


@SuppressWarnings("deprecation")
public class Main {
	
	static Map<Integer, String> dictMap = new HashMap<Integer, String>();
	
//	public static void main(String[] args) throws IOException, InterruptedException {
//		ANTLRFileStream input = new ANTLRFileStream(args[0]);
//        ANTLRInputStream ip = new ANTLRInputStream(input.toString());
//        JavaLexer lex = new JavaLexer(ip); 
//        
//        
//        Token token;
//        System.out.println("٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭");
//        
//        BufferedReader reader;
//        try {
//        	reader = new BufferedReader(new FileReader("JavaLexer.tokens"));
//			String line = reader.readLine();
//
//			while (line != null) {
//				line = reader.readLine();
//				String[] arr = line.split("=");
//				Integer valInteger = Integer.parseInt(arr[1]);
//				if(dictMap.containsKey(valInteger)) {
//					break;
//				}
//				dictMap.put(valInteger, arr[0]);
//			}
//		} catch (IOException e) {
//			e.printStackTrace();
//		}
//        while ((token = lex.nextToken()).getType()!= -1) 
//        	
//        {
//             System.out.println("line " + token.getLine()+ ":" + token.getStartIndex() + " token < "
//                                 + token.getType() + ", "+token.getText() + " >");
//             System.out.println("Token Type : "+dictMap.get(token.getType()));
//             System.out.println("٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭٭");
//             //Thread.sleep(300);
//        }
//	}
	
	public static void main(String[] args) {
		ANTLRInputStream input = new ANTLRInputStream(args[0]);
		Java8Lexer lexer = new Java8Lexer(input);
		CommonTokenStream tokenStream = new CommonTokenStream(lexer);
		Java8Parser java8Parser = new Java8Parser(tokenStream);
						
	}
}