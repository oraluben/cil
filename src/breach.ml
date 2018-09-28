open Cil

open Bf
open Caututils

let branch_flag_perfix : string ref = ref "_br_"
let check_flag : bool ref = ref true
let main_tail_stub : string ref = ref "check_stub"

let int_t = uintType

let mut_block_append (br:int) (b: block) : unit =
  let _var = makeVarinfo true (Printf.sprintf "%s%d" !branch_flag_perfix br) uintType in
  let stmt = mkStmtOneInstr(Set (var _var, 
                           (BinOp(PlusA, Lval(var _var), Const(CInt64(Int64.one,IUInt,None)), uintType)),
                           locUnknown)) in
  b.bstmts <- b.bstmts @ [stmt]

let flag_init = {init=Some(SingleInit(Const(CInt64(Int64.zero,IUInt,None))))}

let rec gen_glob_list ?(start=1) br =
  if start < 1 || start > br then []
  else
    let _var = makeVarinfo true (Printf.sprintf "%s%d" !branch_flag_perfix start) uintType in
    [GVar(_var, flag_init, locFirstLine)] @ gen_glob_list ~start:(start+1) br

class brReachVisitor = object(self)
	inherit countVisitor

  val mutable at_main : bool = false

  method vfunc fd =
    at_main <- (match fd.svar.vname with
      | "main" -> true;
      | _ when fd.svar.vname = !main_tail_stub ->
        (Printf.sprintf "fin_stub duplicate with %s @ %d" fd.svar.vname fd.svar.vdecl.line) |> failwith;
      | _ -> false);
    DoChildren

  method vblock b =
    if !check_flag || not at_main then DoChildren
    else
      let _main_tail_stub = Cil.emptyFunction !main_tail_stub in
      let _stmt = mkStmtOneInstr(Call(None,Lval(Var _main_tail_stub.svar,NoOffset),[], locUnknown)) in
      ChangeDoChildrenPost(b, fun b ->
      b.bstmts <- List.fold_left (fun l s ->
        let _l = match s.skind with
        | Return(_) -> l @ [_stmt]
        | _ -> l in
        _l @ [s]
      ) [] b.bstmts;
      b)

  method vstmt (s : stmt) =
    match s.skind with
    | If(e, b1, b2, l) ->
      (
        self#add 2;
        if not !check_flag then begin
          mut_block_append (self#count - 1) b1;
          mut_block_append (self#count    ) b2;
        end;
      )
    | _ -> (); |> ignore;
    DoChildren

  method vglob g =
    let re = Str.regexp (Printf.sprintf "^%s[0-9]+$" !branch_flag_perfix) in
    let check_g_dup (g:string) =
      Str.string_match re g 0 in
    match g with
    | GVar(v,_,l) when check_g_dup v.vname -> (Printf.sprintf "branch_flag duplicate with %s @ %d" v.vname l.line) |> failwith;
    | _ -> SkipChildren
end;;

let fhandler (f : file) : unit =
  let visitor = new brReachVisitor in
  let _f = file_counter visitor "" 0 in
  (
    _f f;
    f.globals <- (gen_glob_list visitor#count) @ f.globals;
  )
  

let feature : featureDescr =
  { fd_name = "br_reach";
    fd_enabled = ref false;
    fd_description = "branch_reachability";
    fd_extraopt = [
			("--br-flag-perfix", Arg.Set_string branch_flag_perfix, " perfix for flag variable.");
			("--br-transform", Arg.Clear check_flag, " do run transform, default only verify transform.");
    ];
    fd_doit = fhandler;
    fd_post_check = true;
  }