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

# Just like File.expand_path, but for windows systems it
# capitalizes the drive name and ensures backslashes are used
def normalize_path(path, *dirs)
  path = File.expand_path(path, *dirs)
  if PLATFORM =~ /mswin/
    path.gsub!('/', '\\').gsub!(/^[a-zA-Z]+:/) { |s| s.upcase }
  else
    path
  end
end

JAVA_CLASSPATH_DELIM = PLATFORM =~ /mswin/ ? ";" : ":"
JAVA_CLASSPATH = ["bin", "lib/antlr-2.7.7.jar", "lib/antlr-3.1.jar", "lib/antlr-runtime-3.1.jar", "lib/stringtemplate-3.2.jar"]
ENV["CLASSPATH"] = (ENV["CLASSPATH"].to_s + 
                    JAVA_CLASSPATH_DELIM + 
                    JAVA_CLASSPATH.collect{|ea| normalize_path(ea) }.join(JAVA_CLASSPATH_DELIM))
ARCHIVE_NAME = "flyparse_#{Date.today}.zip"
RAKE = PLATFORM =~ /mswin/ ? "rake.bat" : "rake"
LANGUAGES = ["as3", "css", "javascript"]
JAR_TARGET = "flyparse-parsers.jar"
JAVAC_OPTIONS = [] # -g (for debugging)


COMMON_TARGET = "bin/.common"
FLYPARSE_CORE_SOURCE = FileList["src/**/*.java"]
file COMMON_TARGET => FLYPARSE_CORE_SOURCE do
  sh "javac #{JAVAC_OPTIONS.join(" ")} #{FLYPARSE_CORE_SOURCE.join(" ")} -d bin"
  touch COMMON_TARGET
end


task :all_languages => [] do
  LANGUAGES.each{|l|
    Dir.chdir(l.to_s){
      sh "#{RAKE}"
    }
  }
end

file JAR_TARGET => [:build] do
  Dir.chdir("bin"){
    sh "jar cf ../lib/#{JAR_TARGET} emacs"
  }
end


task :make_archive => [:make_jar] do
  FileUtils.rm_f Dir.glob('./*.zip')
  archive_files = ["COPYING", "README", "flyparse-mode.el", 
                   "lib/flyparse-parsers.jar", 
                   "as3/as3-flyparse-extensions.el",
                   "css/css-flyparse-extensions.el",
                   "javascript/javascript-flyparse-extensions.el"]

  sh "7z a -r -tZip -x!*.svn #{ARCHIVE_NAME} #{archive_files.join(" ")}"
  if $?.success? and File.exist? ARCHIVE_NAME
    puts "Created deployment archive successfully."
  else
    puts "Failed to create deployment archive."
  end
end


task :test => [] do
  LANGUAGES.each{|l|
    Dir.chdir(l){
      sh "#{RAKE} test"
    }
  }
end


task :deploy => [:build, :make_archive] do
end


task :build => [:clean, COMMON_TARGET, :all_languages] do
end


task :default => [JAR_TARGET]


task :clean => [] do
  rm_f COMMON_TARGET
  FileUtils.rm_rf Dir.glob('bin/*')
  LANGUAGES.each{|l|
    Dir.chdir(l){
      sh "#{RAKE} clean"
    }
  }
end




