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

require 'find'
require 'platform'
require 'popen4'

def run_tests(dir, testp, cmd_func, ignore_error = nil)
  Find.find(dir) { | path |
    if testp[path]
      puts "\n********* #{path} ***********"
      status = POpen4::popen4(cmd_func[path]) { | stdout, stderr, stdin, pid |
        stdout.readlines
        while o = stderr.gets
          if (not ignore_error) or (not ignore_error[o])
            STDOUT.write o
          end
        end
      }
      if status != 0
        puts "Exit Status: #{status}"
      else
        puts "OK"
      end
    end
  }
end
