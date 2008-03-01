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


package emacs.flyparse.css;

import emacs.flyparse.*;
import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;
import java.io.*;

public class CSSDriver{
    
    public static void main(String[] args) throws Exception {
	ANTLRFileStream chars = new ANTLRFileStream(args[0]);
        CSSLexer lex = new CSSLexer(chars);
       	CommonTokenStream tokens = new CommonTokenStream(lex);
        CSSParser parser = new CSSParser(tokens);
	parser.setTreeAdaptor(new FlyparseTreeAdaptor());
	CSSParser.stylesheet_return ret = parser.stylesheet();
	BufferedWriter bout;
	if(args.length > 1){
	    bout = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1])), 512);
	}
	else{
	    bout = new BufferedWriter(new OutputStreamWriter(System.out), 512);
	}
	FlyparseTree tree = (FlyparseTree)ret.getTree();
	tree.prepareTree();
	try{
	    bout.write("(setq tree '");
	    tree.writeTo(bout);
	    bout.write(")");
	}
	catch(IOException e){
	}
	bout.flush();
    }
    
}