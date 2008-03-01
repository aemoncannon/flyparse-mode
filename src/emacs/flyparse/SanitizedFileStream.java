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

import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;
import java.util.*;
import java.util.regex.*;
import java.io.*;

public class SanitizedFileStream extends ANTLRStringStream {
    protected String fileName;
    
    public SanitizedFileStream(String fileName) throws IOException {
	this(fileName, null);
    }
    
    public SanitizedFileStream(String fileName, String encoding) throws IOException {
	this.fileName = fileName;
	load(fileName, encoding);
    }
    
    public void load(String fileName, String encoding) throws IOException {
	if ( fileName == null ) {
	    return;
	}
	File f = new File(fileName);
	int size = (int)f.length();
	data = new char[size];
	LineNumberReader isr = new LineNumberReader(new FileReader(fileName));
	try{
	    int i = 0;
	    int ch = isr.read();
	    while(ch > 0){
		data[i] = (char)ch;
		i++;
		ch = isr.read();
	    }
	    super.n = i;
	}
	finally {
	    isr.close();
	}
    }
    
    public String getSourceName() {
	return fileName;
    }
}


