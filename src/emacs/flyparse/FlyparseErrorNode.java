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

public class FlyparseErrorNode extends FlyparseTree{
    
    public FlyparseErrorNode(TokenStream input, Token start, Token stop, RecognitionException e){
	super(start);
    }
    
}