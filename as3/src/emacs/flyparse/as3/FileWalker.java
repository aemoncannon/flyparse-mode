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

import java.io.File;
import java.io.FilenameFilter;
import java.util.Collection;
import java.util.Vector;
import java.util.regex.*;

public class FileWalker
{ 


    public static File[] listFilesAsArray(File directory, Pattern filter, boolean recurse){
	Collection<File> files = listFiles(directory, filter, recurse);
	File[] arr = new File[files.size()];
	return files.toArray(arr);
    }


    public static Collection<File> listFiles(File directory, Pattern filter, boolean recurse){
	Vector<File> files = new Vector<File>();
	File[] entries = directory.listFiles();
	for (File entry : entries) {
	    Matcher matcher = filter.matcher(entry.getPath());
	    if (matcher.find()){
		files.add(entry);
	    }
	    if (recurse && entry.isDirectory()){
		files.addAll(listFiles(entry, filter, recurse));
	    }
	}
	return files;		
    }


  
}
