(*
 * This file is the entry point for oed and the main file for building the
 * binary. It parses the command line arguments and begins the main loop of the
 * program.
 * Currently no command line parsing is supported.
 *)

Main.run @@ FileBuffer.make None
