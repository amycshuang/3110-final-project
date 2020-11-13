open Graphics

type  g_context = {
  mutable bcol : Graphics.color;
  mutable fcol : Graphics.color;
  mutable font :  string;
  mutable font_size : int;
  mutable lw : int;
  mutable x : int;
  mutable y : int
}

let default_font = "fixed"
let default_font_size = 12
let make_default_context () =
  { bcol = Graphics.white; fcol = Graphics.black;
    font = default_font;  
    font_size = default_font_size;
    lw = 1;
    x = 0; y = 0;}

let get_gc_bcol gc  = gc.bcol 
let get_gc_fcol gc  = gc.fcol 
let get_gc_font  gc  = gc.font
let get_gc_font_size  gc  = gc.font_size
let get_gc_lw gc  = gc.lw
let get_gc_cur gc = (gc.x,gc.y)

let set_gc_bcol gc c = gc.bcol <- c
let set_gc_fcol gc c = gc.fcol <- c
let set_gc_font  gc f = gc.font <- f
let set_gc_font_size  gc s = gc.font_size <- s
let set_gc_lw gc i = gc.lw <- i
let set_gc_cur gc (a,b) = gc.x<- a; gc.y<-b

let use_gc gc = 
  Graphics.set_color (get_gc_fcol gc);
  Graphics.set_font (get_gc_font gc);
  Graphics.set_text_size (get_gc_font_size gc);
  Graphics.set_line_width (get_gc_lw gc);
  let (a,b) = get_gc_cur gc in Graphics.moveto a b

type rich_event = 
    MouseDown | MouseUp | MouseDrag | MouseMove
  | MouseEnter | MouseExit | Exposure 
  | GotFocus | LostFocus | KeyPress | KeyRelease

type opt_val = Copt of Graphics.color | Sopt of string 
             | Iopt of int | Bopt of bool;; 
type lopt = (string * opt_val) list ;;

exception OptErr

let theInt lo name default = 
  try
    match List.assoc name lo with 
      Iopt i -> i 
    | _  -> raise OptErr
  with Not_found -> default

let theBool lo name default = 
  try
    match List.assoc name lo with 
      Bopt b -> b 
    | _  -> raise OptErr
  with Not_found -> default

let set_gc gc lopt = 
  set_gc_bcol gc (theColor lopt "Background" (get_gc_bcol gc));
  set_gc_fcol gc (theColor lopt "Foreground" (get_gc_fcol gc));
  set_gc_font gc (theString lopt "Font" (get_gc_font gc));
  set_gc_font_size gc (theInt lopt "FontSize" (get_gc_font_size gc));
  set_gc_lw gc (theInt lopt "LineWidth" (get_gc_lw gc))


