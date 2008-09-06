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

public class FlyparseTreeAdaptor extends CommonTreeAdaptor {
    
    public Object create(Token payload) {
	return new FlyparseTree(payload);
    }
    
    /** Tell me how to create a token for use with imaginary token nodes.
     *  For example, there is probably no input symbol associated with imaginary
     *  token DECL, but you need to create it as a payload or whatever for
     *  the DECL node as in ^(DECL type ID).
     *
     *  If you care what the token payload objects' type is, you should
     *  override this method and any other createToken variant.
     */
    public Token createToken(int tokenType, String text) {
	CommonToken c =  new CommonToken(tokenType, text);
	c.setStartIndex(-1);
	c.setStopIndex(-1);
	return c;
    }
    
    
    /** Tell me how to create a token for use with imaginary token nodes.
     *  For example, there is probably no input symbol associated with imaginary
     *  token DECL, but you need to create it as a payload or whatever for
     *  the DECL node as in ^(DECL type ID).
     *
     *  This is a variant of createToken where the new token is derived from
     *  an actual real input token.  Typically this is for converting '{'
     *  tokens to BLOCK etc...  You'll see
     *
     *    r : lc='{' ID+ '}' -> ^(BLOCK[$lc] ID+) ;
     *
     *  If you care what the token payload objects' type is, you should
     *  override this method and any other createToken variant.
     */
    public Token createToken(Token fromToken) {
	return new CommonToken(fromToken);
    }


    public Object errorNode(TokenStream input, Token start, Token stop,
			    RecognitionException e)
    {
	FlyparseErrorNode t = new FlyparseErrorNode(input, start, stop, e);
	return t;
    }
    

    
}
