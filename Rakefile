#! ruby

# Copyright (c) 2007 Aemon Cannon, aemoncannon -at- gmail -dot- com
#
# This file is part of flyparse-mode
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

require 'fileutils'
require 'date'

ARCHIVE_NAME = "flyparse_#{Date.today}.zip"

task :clean => [] do
  FileUtils.rm_rf Dir.glob('bin/*')
end

task :compile_common => [] do
  system "javac src/emacs/flyparse/FlyparseTreeAdaptor.java src/emacs/flyparse/FlyparseTree.java src/emacs/flyparse/SanitizedFileStream.java -d bin"
end

task :as3 => [] do
  Dir.chdir("as3"){
    system "rake.bat"
  }
end

task :css => [] do
  Dir.chdir("css"){
    system "rake.bat"
  }
end

task :javascript => [] do
  Dir.chdir("javascript"){
    system "rake.bat"
  }
end

task :all_languages => [:as3, :css, :javascript] do
end

task :make_jar => [] do
  Dir.chdir("bin"){
    system "jar cf ../lib/flyparse_parsers.jar emacs"
    if $?.success?; puts "Created jar successfully."; end
  }
end

task :make_archive => [:make_jar] do
  FileUtils.rm_f Dir.glob('./*.zip')
  archive_files = ["COPYING", "README", "flyparse-mode.el", 
                   "lib/flyparse_parsers.jar", 
                   "as3/as3-flyparse-extensions.el",
                   "css/css-flyparse-extensions.el",
                   "javascript/javascript-flyparse-extensions.el"]

  system "7z a -r -tZip -x!*.svn #{ARCHIVE_NAME} #{archive_files.join(" ")}"
  if $?.success? and File.exist? ARCHIVE_NAME
    puts "Created deployment archive successfully."
  else
    puts "Failed to create deployment archive."
  end
end

task :test => [] do
  Dir.chdir("as3"){
    system "rake.bat test"
  }
  Dir.chdir("css"){
    system "rake.bat test"
  }
  Dir.chdir("javascript"){
    system "rake.bat test"
  }
end

task :deploy => [:clean, :compile_common, :all_languages, :make_archive] do
end


task :default => [:deploy]






