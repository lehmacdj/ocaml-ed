type filename =
  | File of string (** either a file *)
  | Command of string (** a command to read / write from *)
  | ThisFile (** nothing; the current file *)
;;

type text = string list
;;

type regex = string
;;

type address =
  | FirstLine (** the first line in the buffer *)
  | Current (** the current line *)
  | Line of int (** the line numbered line *)
  | LastLine (** the last line in the file *)
  | ForwardSearch of regex (** the first line from the current line that matches the regex *)
  | BackwardSearch of regex (** the previous line from the current line that matches the regex *)
  | Offset of address * int (** offset from specified address *)
;;

(**
 * A pair of address used as a parameter type for some methods
 *)
type address_range = address * address
;;

(**
 * The first two addresses of any command are the address to execute the
 * command on. If there are more the remaining addresses are commands
 *)
type command =
  | Append of address * text
  | Change of address_range * text
  | Delete of address_range
  | Edit of filename
  | EditForce of filename
  | SetFile of filename
  | Global of address_range * regex * command
  | GlobalInteractive of address_range * regex
  | HelpToggle
  | Help
  | Insert of address * text
  | Join of address_range
  | List of address_range
  | Move of address_range * address
  | Number of address_range
  | Print of address_range
  | PromptToggle
  | Quit
  | QuitForce
  | Read of address * filename
  | Substitute of address_range * regex * string
  | Transfer of address_range * address
  | ConverseGlobal of address_range * regex * command
  | ConverseGlobalInteractive of address_range * regex
  | Write of address_range * filename
  | WriteAppend of address_range * filename
  | Scroll of address * int
  | LineNumber of address
  | Goto of address
;;

(** Type of the suffix of a command *)
type suffix =
  | PrintS
  | ListS
  | NumberS
  | NoSuffix
;;

(**
 * Type of output to display to the console.
 *)
type output =
  | Nothing (** prints nothing *)
  | EdError of string option (* an error with an optional description *)
  | ByteCount of int (** the byte count of a file loaded or written *)
  | Text of string (** some text from the buffer to output *)
  | PathName of string (** a pathname , e.g. after changing the filename *)
;;
