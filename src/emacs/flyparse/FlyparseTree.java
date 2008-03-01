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

public class FlyparseTree extends CommonTree implements Comparable{
    final int EMACS_BUFFER_OFFSET = 1;
    private int cacheStartCharIndex = -1;
    private int cacheStopCharIndex = -1;
    
    public FlyparseTree(Token t){
	super(t);
    }
    
    /* Remove all the empty non-terminals from the tree. As they are not anchored
       in tokens(tokens with buffer location information) they are useless for our 
       purposes. 
       
       This method is called once, after the tree is generated, and before it is
       output. */
    public void prepareTree(){
	if(this.isNonTerminal() && (this.children != null)){
	    for (Iterator<FlyparseTree> it = this.children.iterator(); it.hasNext();){
		FlyparseTree t = it.next();
		t.prepareTree();
		if (t.isNonTerminal() && t.isEmpty()){
		    it.remove();
		}
	    }
	}
    }
    
    public int compareTo(Object o){
	FlyparseTree other = (FlyparseTree)o;
	if(this.getStartCharIndex() < other.getStartCharIndex()){
	    return 1;
	}
	else if(this.getStartCharIndex() > other.getStartCharIndex()){
	    return -1;
	}
	else{
	    return 0;
	}
    }

    public void writeTo(Writer writer){
	this.writeTo(writer, 0);
    }
    
    public boolean isTerminal(){
	return this.isEmpty() && !(this.isTokenImaginary((CommonToken)this.token));
    }
    
    public boolean isNonTerminal(){
	return (this.isTokenImaginary((CommonToken)this.token));
    }
    
    public boolean isEmptyNonTerminal(){
	return this.isNonTerminal() && this.isEmpty();
    }
    
    private boolean isTokenImaginary(CommonToken t){
	return (t.getStartIndex() < 0) || (t.getStopIndex() < 0);
    }
    
    public boolean isEmpty(){
	return this.getChildCount() == 0;
    }
    
    
    /* Write a description of this tree and its children
       with positions calculated as offsets to buf. */
    public void writeTo(Writer w, int baseOffset){
	try{
	    if (this.isTerminal()){
		w.write('(');
		w.write(this.toString(baseOffset));
		w.write(')');
	    }
	    else if(this.isNonTerminal()){
		// Otherwise....
		w.write('(');
		w.write(this.toString(baseOffset));
		w.write(' ');
		int offset = this.getStartCharIndex();
		for (int i = 0; children!=null && i < children.size(); i++) {
		    FlyparseTree t = (FlyparseTree) children.get(i);
		    w.write(' ');
		    t.writeTo(w, offset);
		    offset = t.getStopCharIndex();
		}
		w.write(')');
	    }
	}
	catch(IOException e){
	}
    }
    
    
    
    /* Return a description of this tree with positions
       calculated as offsets. */
    public String toString(int baseOffset){
	String tokenText = this.token.getText().replace("\\", "\\\\").replace("\"", "\\\"");
	int begOffset = Math.max(0, (this.getStartCharIndex() - baseOffset));
	int endOffset = Math.max(begOffset, (this.getStopCharIndex() - baseOffset));
	return "\"" + tokenText + "\" (" + begOffset + " " + endOffset + ")";
    }
    
    
    // Get the index into the character stream at which
    // this tree starts.
    private int getStartCharIndex(){
	if(cacheStartCharIndex > -1){
	    return cacheStartCharIndex;
	}
	else if(this.isTerminal()){
	    cacheStartCharIndex =  ((CommonToken)this.token).getStartIndex() + EMACS_BUFFER_OFFSET;
	    return cacheStartCharIndex;
	}
	else if(!this.isEmpty()){
	    cacheStartCharIndex =  ((FlyparseTree)this.children.get(0)).getStartCharIndex();
	    return cacheStartCharIndex;
	}
	else{
	    cacheStartCharIndex = -1;
	    return cacheStartCharIndex;
	}
    }
    
    
    // Get the index into the character stream at which
    // this tree stops. 
    private int getStopCharIndex(){
	if(cacheStopCharIndex > -1){
	    return cacheStopCharIndex;
	}
	else if (this.isTerminal()){
	    cacheStopCharIndex =  ((CommonToken)this.token).getStopIndex() + EMACS_BUFFER_OFFSET;
	    return cacheStopCharIndex;
	}
	else if(!this.isEmpty()){
	    cacheStopCharIndex =  this.inferStopCharIndexFromChildren();
	    return cacheStopCharIndex;
	}
	else{
	    cacheStopCharIndex = -1;
	    return cacheStopCharIndex;
	}
    }
    
    
    private int inferStopCharIndexFromChildren(){
	if(this.isEmpty()){
	    return ((CommonToken)this.token).getStopIndex() + EMACS_BUFFER_OFFSET;
	}
	else{
	    FlyparseTree lastChild = (FlyparseTree)this.children.get(this.children.size() - 1);
	    return lastChild.inferStopCharIndexFromChildren();
	}
    }
    
}