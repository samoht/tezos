(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Node_logging

let replay ~singleprocess (config : Node_config_file.t) block =
  (let open Block_validator_process in
  let store_root = Node_data_version.store_dir config.data_dir in
  let context_root = Node_data_version.context_dir config.data_dir in
  let protocol_root = Node_data_version.protocol_dir config.data_dir in
  let genesis = config.blockchain_network.genesis in
  let validator_env =
    {
      genesis;
      user_activated_upgrades =
        config.blockchain_network.user_activated_upgrades;
      user_activated_protocol_overrides =
        config.blockchain_network.user_activated_protocol_overrides;
    }
  in
  if singleprocess then
    State.init ~store_root ~context_root genesis
    >>=? fun (state, mainchain_state, context_index, history_mode) ->
    init validator_env (Internal context_index)
    >>=? fun validator_process ->
    return (validator_process, state, mainchain_state, history_mode)
  else
    init
      validator_env
      (External
         {
           data_dir = config.data_dir;
           context_root;
           protocol_root;
           process_path = Sys.executable_name;
           sandbox_parameters = None;
         })
    >>=? fun validator_process ->
    let commit_genesis =
      Block_validator_process.commit_genesis validator_process
    in
    State.init ~store_root ~context_root ~commit_genesis genesis
    >>=? fun (state, mainchain_state, _context_index, history_mode) ->
    return (validator_process, state, mainchain_state, history_mode))
  >>=? fun (validator_process, _state, mainchain_state, _) ->
  let block_alias = Block_services.to_string block in
  Block_directory.get_block mainchain_state block
  >>= function
  | None ->
      failwith "block not found"
  | Some block -> (
      State.Block.all_operations block
      >>= fun operations ->
      let header = State.Block.header block in
      State.Block.predecessor block
      >>= function
      | None ->
          failwith "block predecessor not found"
      | Some predecessor ->
          let t0 = Unix.gettimeofday () in
          lwt_log_notice
            "Validating block %s (%a) once again."
            block_alias
            Block_hash.pp
            (State.Block.hash block)
          >>= fun () ->
          Block_validator_process.apply_block
            validator_process
            ~predecessor
            header
            operations
          >>=? fun _result ->
          lwt_log_notice "Done in %g seconds." (Unix.gettimeofday () -. t0)
          >>= fun () ->
          Block_validator_process.close validator_process
          >>= fun () -> return () )

let run ?verbosity ~singleprocess (config : Node_config_file.t) block =
  Node_data_version.ensure_data_dir config.data_dir
  >>=? fun () ->
  Lwt_lock_file.create
    ~unlink_on_exit:true
    (Node_data_version.lock_file config.data_dir)
  >>=? fun () ->
  (* Main loop *)
  let log_cfg =
    match verbosity with
    | None ->
        config.log
    | Some default_level ->
        {config.log with default_level}
  in
  Internal_event_unix.init
    ~lwt_log_sink:log_cfg
    ~configuration:config.internal_events
    ()
  >>= fun () ->
  Updater.init (Node_data_version.protocol_dir config.data_dir) ;
  lwt_log_notice "Starting the Tezos node..."
  >>= fun () -> Lwt_exit.(wrap_and_exit @@ replay ~singleprocess config block)

let process verbosity singleprocess block args =
  let verbosity =
    let open Internal_event in
    match verbosity with [] -> None | [_] -> Some Info | _ -> Some Debug
  in
  let run =
    Node_shared_arg.read_and_patch_config_file args
    >>=? fun config ->
    Lwt_lock_file.is_locked (Node_data_version.lock_file config.data_dir)
    >>=? function
    | false ->
        run ?verbosity ~singleprocess config block
    | true ->
        failwith "Data directory is locked by another process"
  in
  match Lwt_main.run run with
  | Ok () ->
      (* 2 means that we exit by a signal that was handled *)
      `Ok ()
  | Error err ->
      `Error (false, Format.asprintf "%a" pp_print_error err)

module Term = struct
  let verbosity =
    let open Cmdliner in
    let doc =
      "Increase log level. Using $(b,-v) is equivalent to using \
       $(b,TEZOS_LOG='* -> info'), and $(b,-vv) is equivalent to using \
       $(b,TEZOS_LOG='* -> debug')."
    in
    Arg.(
      value & flag_all
      & info ~docs:Node_shared_arg.Manpage.misc_section ~doc ["v"])

  let block =
    let open Cmdliner in
    let doc = "The block to replay." in
    let block =
      Arg.conv
        ( (fun s ->
            match Tezos_shell_services.Block_services.parse_block s with
            | Ok r ->
                Ok r
            | Error _ ->
                Error (`Msg "cannot decode block argument")),
          fun ppf b ->
            Format.fprintf
              ppf
              "%s"
              (Tezos_shell_services.Block_services.to_string b) )
    in
    Arg.(
      value
      & pos 0 block (`Head 0)
      & info
          ~docs:Node_shared_arg.Manpage.misc_section
          ~doc
          ~docv:"<level>|<block_hash>|<alias>"
          [])

  let singleprocess =
    let open Cmdliner in
    let doc =
      "When enabled, it deactivates block validation using an external \
       process. Thus, the validation procedure is done in the same process as \
       the node and might not be responding when doing extensive I/Os."
    in
    Arg.(
      value & flag
      & info ~docs:Node_shared_arg.Manpage.misc_section ~doc ["singleprocess"])

  let term =
    Cmdliner.Term.(
      ret
        ( const process $ verbosity $ singleprocess $ block
        $ Node_shared_arg.Term.args ))
end

module Manpage = struct
  let command_description =
    "The $(b,replay) command is meant to replay a previously validated block \
     for debugging purposes."

  let description = [`S "DESCRIPTION"; `P command_description]

  let debug =
    let log_sections =
      String.concat
        " "
        (TzString.Set.elements (Internal_event.get_registered_sections ()))
    in
    [ `S "DEBUG";
      `P
        ( "The environment variable $(b,TEZOS_LOG) is used to fine-tune what \
           is going to be logged. The syntax is \
           $(b,TEZOS_LOG='<section> -> <level> [ ; ...]') where section is \
           one of $(i," ^ log_sections
        ^ ") and level is one of $(i,fatal), $(i,error), $(i,warn), \
           $(i,notice), $(i,info) or $(i,debug). A $(b,*) can be used as a \
           wildcard in sections, i.e. $(b, client* -> debug). The rules are \
           matched left to right, therefore the leftmost rule is highest \
           priority ." ) ]

  let examples =
    [ `S "EXAMPLE";
      `I ("$(b,Replay block at level twelve)", "$(mname) replay 12") ]

  let man =
    description @ Node_shared_arg.Manpage.args @ debug @ examples
    @ Node_shared_arg.Manpage.bugs

  let info =
    Cmdliner.Term.info ~doc:"Replay a previously validated block" ~man "replay"
end

let cmd = (Term.term, Manpage.info)
