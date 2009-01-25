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

package emacs.flyparse;

import emacs.flyparse.*;
import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;
import java.io.*;
import java.util.Collection;
import java.util.Vector;
import java.util.regex.*;

public abstract class ADriver{
    
    /* A helper for debugging. */	 
    public void printAllTokens(Lexer lex){
	CommonToken t  = (CommonToken)lex.nextToken();
	while(t.getType() != Token.EOF){
	    System.out.println(t);
	    t = (CommonToken)lex.nextToken();
	}
	lex.reset();
    }


    public void processSingle(File file, BufferedWriter bout) throws Exception{
	try{
	    bout.write("(setq tree '");
	    processFile(file, bout);
	    bout.write(")");
	}
	catch(IOException e){
	}
	bout.flush();
    }


    public void processAll(Vector<File> files, BufferedWriter bout) throws Exception{
	Pattern p = Pattern.compile("\\\\");
	int count = 0;
	for(File file : files){
	    Matcher matcher = p.matcher(file.getCanonicalPath());
	    String path = matcher.replaceAll("/");
	    bout.write("(puthash \"");
	    bout.write(path);
	    bout.write("\" '");
	    processFile(file, bout);
	    bout.write(" flyparse-loading-tree-cache)\n");
	    count++;
	}
	System.out.println("Parsed " + count + " files.");
	bout.flush();
    }


    protected abstract void processFile(File file, BufferedWriter bout)  throws Exception;

    
}