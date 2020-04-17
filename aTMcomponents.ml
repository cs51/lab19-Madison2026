(*
                Component Behaviors of an ATM Machine

The functions here represent the component behaviors that an ATM
machine can take, including: prompting for and acquiring from the
customer some information (choosing an action or entering an account
id or an amount); presenting information to the customer; dispensing
cash.

Implementation of these behaviors is likely to require some database
of accounts, each with an id number, a customer name, and a current
balance.
 *)

(* Customer account identifiers *)
type id = int ;;

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;; 

(*....................................................................
 Initializing database of accounts
*)

(* A specification of a customer name and initial balance for
   initializing the account database *)
type account_spec = {name : string; id : id; balance : int} ;;

(* initialize accts -- Establishes a database of accounts, each with a
   name, aribtrary id, and balance. The names and balances are
   initialized as per the `accts` provided. *)
let initialize =
  let accounts = ref [] in
  fun accts -> 
    match accts with
    | [] -> ()
    | hd :: tl -> accounts := hd :: !accounts ;;

(*....................................................................
 Acquiring information from the customer
*)

(* acquire_id () -- Requests from the ATM customer and returns an id
   (akin to entering one's ATM card), by prompting for an id number
   and reading an id from stdin. *)
val acquire_id =
  Printf.print_string "Enter customer id: " ; 
    let i = read_int () in
    i ;;

(* acquire_amount () -- Requests from the ATM customer and returns an
   amount by prompting for an amount and reading an int from stdin. *)
val acquire_amount =
  let () = print_string "Enter amount: " in 
    let i = read_int () in
    i ;;

(* acquire_act () -- Requests from the user and returns an action to
   be performed, as a value of type action *)
let acquire_act =
  let () = print_string "Enter action: (B) Balance (-) Withdraw (+) Deposit (=) Done (X) Exit: " in 
    let act = read_line () in
    match act with
    | "B" -> Balance
    | "-" -> Withdraw (acquire_amount ())
    | "+" -> Deposit (acquire_amount ())
    | "=" -> Next
    | "X" -> Finished
    | _ -> raise (Failure "Invalid action");;

(*....................................................................
  Querying and updating the account database

  These functions all raise Not_found if there is no account with the
  given id. 
 *)
  
(* get_balance id -- Returns the balance for the customer account with
   the given id. *)
let get_balance (id' : id) : int = 
   let lst = !accounts in
   let rec get_helper i_d acct_lst = 
   match acct_lst with 
   | [] -> raise (Failure "invalid id")
   | hd :: tl -> if hd.id = i_d then hd.balance
                 else get_helper i_d tl in
   get_helper id' lst ;;

(* get_name id -- Returns the name associated with the customer
   account with the given id. *)
let get_name (id' : id) : string = 
   let lst = !accounts in
   let rec get_helper i_d acct_lst = 
   match acct_lst with 
   | [] -> raise (Failure "invalid id")
   | hd :: tl -> if hd.id = i_d then hd.name
                 else get_helper i_d tl in
   get_helper id' lst ;;

(* update_balance id amount -- Modifies the balance of the customer
   account with the given id,setting it to the given amount. *)
let update_balance i amount = 
  match !accounts with
  | [] -> ()
  | hd :: tl -> if hd.id = id then 
                   let t = List.filter (fun x -> not (x.id = i)) !accounts 
                   accounts := {name = hd.name; id = i; balance = amount} :: t ;;

(*....................................................................
  Presenting information and cash to the customer
 *)
  
(* present_message message -- Presents to the customer (on stdout) the
   given message followed by a newline. *)
let present_message (s : string) : unit =    
  print_string s;    
  print_newline ;;

(* deliver_cash amount -- Dispenses the given amount of cash to the
   customer (really just prints to stdout a message to that
   effect). *)
let deliver_cash i =
  let amnt = ref 1 in 
  let cash = ref 0 in 
  let left = ref 0 in 
  (*let rec change i =
    if i / 100 > 0 then cash := i / 100; 
      left := i % 100; 
      amnt := 100  
    else if i / 50 > 0 then cash := i / 50; 
      left := i % 50; 
      amnt := 50 
    else if i / 20 > 0 then cash := i / 20; 
      left := i % 20; 
      amnt := 20 
    else left := i;
         amnt := 1 in  *)
    print_string("Here's your cash: "); 
  for n = 0 to (i / 20) 
    do
        print_string("[ 20 @ 20 ]") 
    done; 
  print_string(" and" ^ string_of_int(i mod 20) " more") ;;





