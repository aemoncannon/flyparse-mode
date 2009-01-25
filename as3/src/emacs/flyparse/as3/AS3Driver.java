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

public class AS3Driver extends ADriver{

    public static void main(String[] args) throws Exception {
	BufferedWriter bout;
	ADriver driver = new AS3Driver();
	if(args[0].equals("-f")){ 
	    if(args.length == 3){// -f input output
		bout = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[2])), 512);
	    }
	    else{ // -f input 
		bout = new BufferedWriter(new OutputStreamWriter(System.out), 512);
	    }
	    driver.processSingle(new File(args[1]), bout);
	}
	else if(args[0].equals("-l") && args.length >= 3) { // -l output [dir dir dir....]
	    bout = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1])), 512);
	    Vector<File> files = new Vector<File>();
	    for(int i = 2; i < args.length; i++){
		files.addAll(FileWalker.listFiles(new File(args[i]), Pattern.compile("[\\\\/][A-Z][a-zA-Z0-9]+\\.as$"), true));
	    }
	    driver.processAll(files, bout);
	}
    }


    protected void processFile(File file, BufferedWriter bout)  throws Exception{
	SanitizedFileStream chars = new SanitizedFileStream(file.getPath());
	AS3Lexer lex = new AS3Lexer(chars);
	CommonTokenStream tokens = new CommonTokenStream(lex);
	AS3Parser parser = new AS3Parser(tokens);
	parser.setTreeAdaptor(new FlyparseTreeAdaptor());
 	try{
	    AS3Parser.compilationUnit_return ret = parser.compilationUnit();
	    FlyparseTree tree = (FlyparseTree)ret.getTree();
	    tree.prepareTree();	
	    tree.writeTo(bout);
 	}
 	catch(Exception e){
	    throw e;
 	}
    }

}