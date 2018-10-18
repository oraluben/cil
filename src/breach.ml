open Cil

open Bf
open Caututils

let branch_flag_perfix : string ref = ref "_br_"
let check_flag : bool ref = ref true
let single_flag : bool ref = ref false
let main_tail_stub : string ref = ref "check_stub"

let int_t = intType

let mut_block_append (br:int) ?(minus=false) (b: block) : unit =
  let _var = makeVarinfo true (Printf.sprintf "%s%d" !branch_flag_perfix (if !single_flag then 1 else br)) int_t in
  let stmt = mkStmtOneInstr(Set (var _var,
                           (BinOp(
                            (if minus then MinusA else PlusA),
                            Lval(var _var),
                            Const(CInt64(Int64.one,IInt,None)),
                            int_t)),
                           locUnknown)) in
  b.bstmts <- b.bstmts @ [stmt]

let flag_init = {init=Some(SingleInit(Const(CInt64(Int64.zero,IInt,None))))}

let rec gen_glob_list ?(start=1) br =
  if start < 1 || start > br then []
  else
    let _var = makeVarinfo true (Printf.sprintf "%s%d" !branch_flag_perfix start) int_t in
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
      let _stub_instr = Call(None,Lval(Var _main_tail_stub.svar,NoOffset),[], locUnknown) in
      let _stmt = mkStmtOneInstr(_stub_instr) in
      ChangeDoChildrenPost(b, fun b ->
      b.bstmts <- List.fold_left (fun l s ->
        let _l = match s.skind with
        | Return(_) -> l @ [_stmt]
        | _ -> l in
        let _s = match s.skind with
        | Instr(_l) ->
          let instr_with_stub (l:instr list) =
            mkStmt(Instr(List.fold_left (fun l i -> (match i with
              | Call(_,Lval(Var(_l),_),_,_) when _l.vname = "exit" ->
                l @ [_stub_instr]
              | _ -> l)
            @ [i]) [] l)) in
          instr_with_stub _l
        | _ -> s in
        _l @ [_s]
      ) [] b.bstmts;
      b)

  method vstmt (s : stmt) =
    match s.skind with
    | If(e, b1, b2, l) ->
      (
        self#add 2;
        if not !check_flag then begin
          mut_block_append (self#count - 1)                                           b1;
          mut_block_append (self#count) ~minus:(if !single_flag then true else false) b2;
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
    f.globals <- (gen_glob_list (if !single_flag then 1 else visitor#count)) @ f.globals;
  )
  

let feature : featureDescr =
  { fd_name = "br_reach";
    fd_enabled = ref false;
    fd_description = "branch_reachability";
    fd_extraopt = [
			("--br-flag-perfix", Arg.Set_string branch_flag_perfix, " perfix for flag variable.");
			("--br-transform", Arg.Clear check_flag, " do run transform, default only verify transform.");
			("--br-single-flag", Arg.Set single_flag, " use one flag to track all branch.");
    ];
    fd_doit = fhandler;
    fd_post_check = true;
  }
