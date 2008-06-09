// Copyright (c) 2007 Aemon Cannon, aemoncannon -at- gmail -dot- com
//
// This file is part of flyparse-mode
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

package emacs.flyparse.as3;

import emacs.flyparse.*;
import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;
import java.io.*;
import java.util.Collection;
import java.util.Vector;
import java.util.regex.*;

public class AS3Driver{
    
    /* A helper for debugging. */	 
    public static void printAllTokens(Lexer lex){
	CommonToken t  = (CommonToken)lex.nextToken();
	while(t.getType() != Token.EOF){
	    System.out.println(t);
	    t = (CommonToken)lex.nextToken();
	}
	lex.reset();
    }


    public static void main(String[] args) throws Exception {
	BufferedWriter bout;
	if(args[0].equals("-f")){ 
	    if(args.length == 3){// -f input output
		bout = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[2])), 512);
	    }
	    else{ // -f input 
		bout = new BufferedWriter(new OutputStreamWriter(System.out), 512);
	    }
	    processSingleFile(new File(args[1]), bout);
	}
	else if(args[0].equals("-l") && args.length >= 3) { // -l output [dir dir dir....]
	    bout = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1])), 512);
	    Vector<File> files = new Vector<File>();
	    for(int i = 2; i < args.length; i++){
		files.addAll(FileWalker.listFiles(new File(args[i]), Pattern.compile("\\.as$"), true));
	    }
	    processAll(files, bout);
	}
    }


    private static void processSingleFile(File file, BufferedWriter bout) throws Exception{
	try{
	    bout.write("(setq tree '");
	    writeTreeForFile(file, bout);
	    bout.write(")");
	}
	catch(IOException e){
	}
	bout.flush();
    }


    private static void processAll(Vector<File> files, BufferedWriter bout) throws Exception{
	try{
	    for(File file : files){
		bout.write("(puthash \"");
		bout.write(file.getCanonicalPath());
		bout.write("\" '");
		writeTreeForFile(file, bout);
		bout.write(" flyparse-loading-tree-cache)\n");
	    }
	}
	catch(IOException e){
	}
	bout.flush();
    }


    private static void writeTreeForFile(File file, BufferedWriter bout)  throws Exception {
	SanitizedFileStream chars = new SanitizedFileStream(file.getPath());
	AS3Lexer lex = new AS3Lexer(chars);
	CommonTokenStream tokens = new CommonTokenStream(lex);
	AS3Parser parser = new AS3Parser(tokens);
	parser.setTreeAdaptor(new FlyparseTreeAdaptor());
	AS3Parser.compilationUnit_return ret = parser.compilationUnit();
	FlyparseTree tree = (FlyparseTree)ret.getTree();
	tree.prepareTree();	
	tree.writeTo(bout);
    }
    
}