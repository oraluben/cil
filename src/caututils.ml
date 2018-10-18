open Cil

let id = fun a -> a
(* let (||>) : 'a -> ('a -> 'b) -> 'b = fun a -> fun f -> f a *)

let locFirstLine = { locUnknown with line = 0 }

class countVisitor = object(self)
  inherit nopCilVisitor

  val mutable _count : int = 0
  method count = _count
  method add (i : int) = _count <- _count + i
end

let print_block (b : block) =
  ignore (List.map (fun stmt -> Pretty.printf "%a@!" d_stmt stmt) b.bstmts)


let mkFunStmt s = mkStmtOneInstr(Call(None,Lval(Var (Cil.emptyFunction s).svar,NoOffset),[], locUnknown))

let file_counter (vis : countVisitor) (str : string) (br_target : int) : (file -> unit) = 
  fun f -> (
      List.iter (fun f ->
          match f with
          | GFun(fd, _) -> visitCilFunction (vis :> cilVisitor) fd |> ignore
          | _ -> visitCilGlobal (vis :> cilVisitor) f |> ignore
        ) f.globals;
      if br_target > vis#count then
        Printf.sprintf "branch target is invalid: %d of %d" br_target vis#count |> failwith;
      if br_target == 0 then
        Printf.printf "%s(%d)" str vis#count;
    )
