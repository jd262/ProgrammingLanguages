# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.14

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/local/Cellar/cmake/3.14.3/bin/cmake

# The command to remove a file.
RM = /usr/local/Cellar/cmake/3.14.3/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/jacobdarabaris/Downloads/hw7

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/jacobdarabaris/Downloads/hw7

# Include any dependencies generated for this target.
include CMakeFiles/clox.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/clox.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/clox.dir/flags.make

CMakeFiles/clox.dir/main.c.o: CMakeFiles/clox.dir/flags.make
CMakeFiles/clox.dir/main.c.o: main.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/jacobdarabaris/Downloads/hw7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object CMakeFiles/clox.dir/main.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/clox.dir/main.c.o   -c /Users/jacobdarabaris/Downloads/hw7/main.c

CMakeFiles/clox.dir/main.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/clox.dir/main.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/jacobdarabaris/Downloads/hw7/main.c > CMakeFiles/clox.dir/main.c.i

CMakeFiles/clox.dir/main.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/clox.dir/main.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/jacobdarabaris/Downloads/hw7/main.c -o CMakeFiles/clox.dir/main.c.s

CMakeFiles/clox.dir/chunk.c.o: CMakeFiles/clox.dir/flags.make
CMakeFiles/clox.dir/chunk.c.o: chunk.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/jacobdarabaris/Downloads/hw7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building C object CMakeFiles/clox.dir/chunk.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/clox.dir/chunk.c.o   -c /Users/jacobdarabaris/Downloads/hw7/chunk.c

CMakeFiles/clox.dir/chunk.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/clox.dir/chunk.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/jacobdarabaris/Downloads/hw7/chunk.c > CMakeFiles/clox.dir/chunk.c.i

CMakeFiles/clox.dir/chunk.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/clox.dir/chunk.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/jacobdarabaris/Downloads/hw7/chunk.c -o CMakeFiles/clox.dir/chunk.c.s

CMakeFiles/clox.dir/memory.c.o: CMakeFiles/clox.dir/flags.make
CMakeFiles/clox.dir/memory.c.o: memory.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/jacobdarabaris/Downloads/hw7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building C object CMakeFiles/clox.dir/memory.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/clox.dir/memory.c.o   -c /Users/jacobdarabaris/Downloads/hw7/memory.c

CMakeFiles/clox.dir/memory.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/clox.dir/memory.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/jacobdarabaris/Downloads/hw7/memory.c > CMakeFiles/clox.dir/memory.c.i

CMakeFiles/clox.dir/memory.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/clox.dir/memory.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/jacobdarabaris/Downloads/hw7/memory.c -o CMakeFiles/clox.dir/memory.c.s

CMakeFiles/clox.dir/debug.c.o: CMakeFiles/clox.dir/flags.make
CMakeFiles/clox.dir/debug.c.o: debug.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/jacobdarabaris/Downloads/hw7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building C object CMakeFiles/clox.dir/debug.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/clox.dir/debug.c.o   -c /Users/jacobdarabaris/Downloads/hw7/debug.c

CMakeFiles/clox.dir/debug.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/clox.dir/debug.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/jacobdarabaris/Downloads/hw7/debug.c > CMakeFiles/clox.dir/debug.c.i

CMakeFiles/clox.dir/debug.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/clox.dir/debug.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/jacobdarabaris/Downloads/hw7/debug.c -o CMakeFiles/clox.dir/debug.c.s

CMakeFiles/clox.dir/value.c.o: CMakeFiles/clox.dir/flags.make
CMakeFiles/clox.dir/value.c.o: value.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/jacobdarabaris/Downloads/hw7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building C object CMakeFiles/clox.dir/value.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/clox.dir/value.c.o   -c /Users/jacobdarabaris/Downloads/hw7/value.c

CMakeFiles/clox.dir/value.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/clox.dir/value.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/jacobdarabaris/Downloads/hw7/value.c > CMakeFiles/clox.dir/value.c.i

CMakeFiles/clox.dir/value.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/clox.dir/value.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/jacobdarabaris/Downloads/hw7/value.c -o CMakeFiles/clox.dir/value.c.s

CMakeFiles/clox.dir/vm.c.o: CMakeFiles/clox.dir/flags.make
CMakeFiles/clox.dir/vm.c.o: vm.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/jacobdarabaris/Downloads/hw7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Building C object CMakeFiles/clox.dir/vm.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/clox.dir/vm.c.o   -c /Users/jacobdarabaris/Downloads/hw7/vm.c

CMakeFiles/clox.dir/vm.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/clox.dir/vm.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/jacobdarabaris/Downloads/hw7/vm.c > CMakeFiles/clox.dir/vm.c.i

CMakeFiles/clox.dir/vm.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/clox.dir/vm.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/jacobdarabaris/Downloads/hw7/vm.c -o CMakeFiles/clox.dir/vm.c.s

# Object files for target clox
clox_OBJECTS = \
"CMakeFiles/clox.dir/main.c.o" \
"CMakeFiles/clox.dir/chunk.c.o" \
"CMakeFiles/clox.dir/memory.c.o" \
"CMakeFiles/clox.dir/debug.c.o" \
"CMakeFiles/clox.dir/value.c.o" \
"CMakeFiles/clox.dir/vm.c.o"

# External object files for target clox
clox_EXTERNAL_OBJECTS =

clox: CMakeFiles/clox.dir/main.c.o
clox: CMakeFiles/clox.dir/chunk.c.o
clox: CMakeFiles/clox.dir/memory.c.o
clox: CMakeFiles/clox.dir/debug.c.o
clox: CMakeFiles/clox.dir/value.c.o
clox: CMakeFiles/clox.dir/vm.c.o
clox: CMakeFiles/clox.dir/build.make
clox: CMakeFiles/clox.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/jacobdarabaris/Downloads/hw7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Linking C executable clox"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/clox.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/clox.dir/build: clox

.PHONY : CMakeFiles/clox.dir/build

CMakeFiles/clox.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/clox.dir/cmake_clean.cmake
.PHONY : CMakeFiles/clox.dir/clean

CMakeFiles/clox.dir/depend:
	cd /Users/jacobdarabaris/Downloads/hw7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/jacobdarabaris/Downloads/hw7 /Users/jacobdarabaris/Downloads/hw7 /Users/jacobdarabaris/Downloads/hw7 /Users/jacobdarabaris/Downloads/hw7 /Users/jacobdarabaris/Downloads/hw7/CMakeFiles/clox.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/clox.dir/depend
