open Cil
module E = Errormsg

open Caututils

let af_func_name : string ref = ref ""

let stub_with_location l =
  let df_fun = Cil.emptyFunction "su_stub" in
  Call(None,Lval(Var df_fun.svar,NoOffset),[Cil.integer (l.line)], l)

class afVisitor = object(self)
	inherit nopCilVisitor

  val mutable at_func = false

  method vfunc (f : fundec) =
    at_func <- (match !af_func_name with
    | "" -> true
    | _ ->
      let re = Str.regexp_string !af_func_name in
      try ignore (Str.search_forward re f.svar.vname 0); true
      with Not_found -> false
        (* f.svar.vname = "main" *)
      );
    DoChildren

  (* method vinst st =
	  let df_monitor_call =
      stub_with_location !currentLoc in
	  if at_func = false then
      DoChildren
    else
      ChangeTo([df_monitor_call ; st]) *)

  method vblock bk =
    let stub_with_stmt st l = [mkStmtOneInstr (stub_with_location l); st;] in
    let rec inst_with_stmt insts =
      List.fold_left (fun l i -> l @ [stub_with_location (get_instrLoc i);i]) [] insts in
    let with_succ_stub st =
      match st.skind with
      | Instr(insts) -> [ {st with skind=Instr (inst_with_stmt insts)} ]
      | _ -> stub_with_stmt st (get_stmtLoc st.skind) in
    let adder stmts = List.fold_left (fun l s -> l @ with_succ_stub s) [] stmts in
    ChangeDoChildrenPost({bk with bstmts = adder bk.bstmts}, fun x -> x)
end;;

let handle_f (vis : cilVisitor) (f : fundec) : unit =
  visitCilFunction vis f |> ignore

let file_handler (vis : cilVisitor) : (file -> unit) =
  fun f -> ((List.iter (fun f ->
    match f with
    | GFun(fd, _) -> handle_f vis fd
    | _ -> ()
  ) f.globals) |> ignore)

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