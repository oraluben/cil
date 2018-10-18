open Cil
module E = Errormsg

open Af
open Caututils

let branch_target : int ref = ref 0
let stub_type : string ref = ref "label"

let block_replacement (b : block) : block =
  if !stub_type = "label" then
    let ls : stmt = {
      labels = [ Label("ERROR", locUnknown, true) ];
      skind = Instr []; sid = -1; succs = []; preds = [] } in
    let s : stmt = {
      labels = [ Label("ERROR", locUnknown, true) ];
      skind = Goto(ref ls, locUnknown);
      sid = -1; succs = []; preds = []} in
    { bstmts = [s];
      battrs = []; }
  else if !stub_type = "assert" then
    let assert_function = Cil.emptyFunction "assert" in
    { bstmts = [Call(None,Lval(Var assert_function.svar,NoOffset),[Cil.integer (0)], locUnknown) |> mkStmtOneInstr];
      battrs = []; }
  else Printf.sprintf "do not support %s" !stub_type |> failwith

class bVisitor = object(self)
  inherit countVisitor

  method vstmt (s : stmt) =
    match s.skind with
    | If(e, b1, b2, l) -> (
        self#add 2;
        if !branch_target = 0 || self#count < !branch_target then
          DoChildren
        else if self#count - 1 > !branch_target then SkipChildren
        else
          let _b1 = if self#count - 1 = !branch_target then block_replacement b1 else b1 in
          let _b2 = if self#count     = !branch_target then block_replacement b2 else b2 in
          ChangeTo {s with skind = If(e, _b1, _b2, l);}
      )
    | _ -> DoChildren
end;;

let feature : featureDescr = 
  { fd_name = "af_branch";
    fd_enabled = ref false;
    fd_description = "af_branch";
    fd_extraopt = [
      ("--br-target", Arg.Set_int branch_target, " Set the branch need to be replaced with stub.");
      ("--br-stub", Arg.Set_string stub_type, " Set stub type, could be \"label\" or \"assert\"");
    ];
    fd_doit = (file_counter (new bVisitor) "" !branch_target);
    fd_post_check = true;
  }