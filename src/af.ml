open Cil
module E = Errormsg

let (||>) : 'a -> ('a -> 'b) -> 'b = fun a -> fun f -> f a

let af_func_name : string ref = ref ""

class afVisitor = object(self)
	inherit nopCilVisitor

  val mutable at_func = false

  method vfunc (f : fundec) =
    at_func <- (match !af_func_name with
    | "" -> true
    | _ ->
      let re = Str.regexp_string !af_func_name in
      try ignore (Str.search_forward re f.svar.vname 0); true
      with Not_found ->
        false
        (* f.svar.vname = "main" *)
      );
    DoChildren

  method vinst st = 
	  let df_fun = Cil.emptyFunction "su_stub" in
	  let df_monitor_call =
      Call(None,Lval(Var df_fun.svar,NoOffset),[Cil.integer (!currentLoc.line)], !currentLoc) in
	  if at_func = false then
      DoChildren
    else
      ChangeTo([df_monitor_call ; st])
end;;

let handle_f (vis : cilVisitor) (f : fundec) : unit =
  visitCilFunction vis f ||> ignore

let file_handler (vis : cilVisitor) : (file -> unit) =
  fun f -> ((List.iter (fun f ->
    match f with
    | GFun(fd, _) -> handle_f vis fd
    | _ -> ()
  ) f.globals) ||> ignore)

let feature : featureDescr = 
  { fd_name = "af";
    fd_enabled = ref false;
    fd_description = "af";
    fd_extraopt = [
			("--af-func", Arg.Set_string af_func_name, " Set the function name");
    ];
    fd_doit = (file_handler (new afVisitor));
    fd_post_check = true;
  }