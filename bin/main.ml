open Cmdliner

(* Report a failed package action the way opam does (see OpamSolution.display_error) and return the exit code *)
let display_error action pkg = function
  | (OpamStd.Sys.Exit _ | Sys.Break) as e -> raise e
  | e ->
      let msg =
        match e with
        | Failure s -> s
        | OpamSystem.Process_error r -> OpamProcess.string_of_result r
        | e -> Printexc.to_string e
      in
      OpamConsole.header_error "while %s %s" action (OpamPackage.to_string pkg) "%s" msg;
      1

let init_opam verbose_level debug_level =
  OpamSystem.init ();
  (* The *Config.init functions read OPAM* environment variables from a table that these two calls populate *)
  OpamArg.preinit_opam_env_variables ();
  OpamArg.init_opam_env_variabes OpamCLIVersion.Sourced.current;
  let yes = Some (Some true) in
  let confirm_level = Some `unsafe_yes in
  let verbose_level = if verbose_level = [] then None else Some (List.length verbose_level) in
  let debug_level = if debug_level = [] then None else Some (List.length debug_level) in
  OpamCoreConfig.init ?yes ?confirm_level ?verbose_level ?debug_level ();
  OpamFormatConfig.init ();
  OpamRepositoryConfig.init ();
  OpamStateConfig.init ();
  OpamClientConfig.init ()

(* Run [f] on the first switch, with the write lock, converting opam's exit requests into an exit code *)
let with_switch_state f =
  try
    let gt = OpamGlobalState.load `Lock_none in
    let switches = OpamGlobalState.switches gt in
    (* I'd rather do this: let switch = OpamStateConfig.get_current_switch_from_cwd (OpamFilename.cwd ()) in *)
    let switch = List.hd switches in
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamSwitchState.with_ `Lock_write gt ~switch f
  with OpamStd.Sys.Exit n -> n

let source_dir st nv =
  let opam = OpamSwitchState.opam st nv in
  let raw = OpamSwitchState.source_dir st nv in
  match OpamFile.OPAM.url opam with
  | None -> raw
  | Some url -> OpamFilename.SubPath.(raw /? OpamFile.URL.subpath url)

let parse_package st name =
  match OpamPackage.of_string_opt name with
  | None -> OpamConsole.error_and_exit `Bad_arguments "Invalid package %S, expected NAME.VERSION" name
  | Some pkg when OpamSwitchState.opam_opt st pkg = None ->
      OpamConsole.error_and_exit `Not_found "%s" (OpamSwitchState.not_found_message st (OpamSolution.eq_atom_of_package pkg))
  | Some pkg -> pkg

let install_package ?(depexts_only = false) st pkg test doc dev_setup =
      let name = OpamPackage.to_string pkg in
      let src = source_dir st pkg in
      let build_dir = OpamPath.Switch.build_dir st.switch_global.root st.switch in
      let build_dir = OpamFilename.SubPath.(build_dir / of_string name) in
      (* Checking depexts queries the system package manager for every depext in the switch, so skip it
         when this package has none *)
      if not (OpamSysPkg.Set.is_empty (OpamSwitchState.depexts st pkg)) then begin
        match OpamSwitchState.depexts_unavailable st pkg with
        | Some missing ->
            let names = List.rev_map OpamSysPkg.to_string (OpamSysPkg.Set.elements missing) in
            let pkgs = match names with
              | [p] -> " '" ^ p ^ "'"
              | ps -> "s " ^ OpamStd.Format.pretty_list (List.rev_map (Printf.sprintf "'%s'") ps)
            in
            OpamConsole.error
              "Package %s depends on the unavailable system package%s. You can use \
               `--no-depexts' to attempt installation anyway.%s"
              name pkgs
              (OpamStd.Option.map_default (Printf.sprintf "\n%s.") ""
                 (OpamSysInteract.repo_enablers ~env:st.switch_global.global_variables st.switch_global.config));
            OpamStd.Sys.exit_because `Not_found
        | None ->
            ignore (OpamSolution.install_depexts ~confirm:false ~pkg_to_install:(OpamPackage.Set.singleton pkg) ~pkg_installed:st.installed st)
      end;
      if depexts_only then 0 else
      OpamAction.download_package st pkg |> OpamProcess.Job.run |> function
      | Some (_, e) ->
          Printf.printf "download failed... %s\n" e;
          1
      | None -> (
          OpamAction.prepare_package_source st pkg src |> OpamProcess.Job.run |> function
          | Some exn -> display_error "preparing sources for" pkg exn
          | None -> (
              OpamFilename.copy_dir ~src ~dst:build_dir;
              OpamProcess.Job.catch (fun e -> OpamProcess.Job.Op.Done (Some e)) (fun () -> OpamAction.build_package st ~test ~doc ~dev_setup build_dir pkg)
              |> OpamProcess.Job.run |> function
              | Some exn -> display_error "compiling" pkg exn
              | None -> (
                  OpamProcess.Job.catch (fun e -> OpamProcess.Job.Op.Done (OpamCompat.Either.Right e)) (fun () -> OpamAction.install_package st ~test ~doc ~dev_setup ~build_dir pkg)
                  |> OpamProcess.Job.run |> function
                  | Right exn -> display_error "installing" pkg exn
                  | Left conf ->
                      let conf_files =
                        let add_conf conf = OpamPackage.Name.Map.add pkg.name conf st.conf_files in
                        OpamStd.Option.map_default add_conf st.conf_files conf in
                      ignore (OpamSwitchAction.add_to_installed { st with conf_files } pkg);
                      0)))

(* Returns the exit code and the switch state after removal *)
let uninstall_package (st : OpamStateTypes.rw OpamStateTypes.switch_state) pkg =
      if not (OpamPackage.Set.mem pkg st.installed) then (
        OpamConsole.note "%s is not installed." (OpamPackage.to_string pkg);
        (0, st))
      else
        let open OpamProcess.Job.Op in
        let job () =
          (if OpamAction.removal_needs_download st pkg then (
             (* The remove: commands run from the package source, recreated under the switch's remove directory *)
             let d = OpamPath.Switch.remove st.switch_global.root st.switch pkg in
             OpamFilename.rmdir d;
             let src = source_dir st pkg in
             if OpamFilename.exists_dir src then OpamFilename.copy_dir ~src ~dst:d else OpamFilename.mkdir d;
             OpamAction.prepare_package_source st pkg d)
           else Done None)
          @@+ fun _ -> OpamAction.remove_package st pkg
        in
        let rc =
          OpamProcess.Job.catch (fun e -> Done (Some e)) (fun () -> job () @@| fun () -> None)
          |> OpamProcess.Job.run |> function
          | Some exn -> display_error "removing" pkg exn
          | None -> 0
        in
        (* Like opam, deregister the package even if its removal reported errors *)
        let st = OpamSwitchAction.remove_from_installed st pkg in
        OpamAction.cleanup_package_artefacts st pkg;
        (rc, st)

let build ?depexts_only verbose_level debug_level name test doc dev_setup =
  init_opam verbose_level debug_level;
  with_switch_state (fun st -> install_package ?depexts_only st (parse_package st name) test doc dev_setup)

let uninstall verbose_level debug_level name =
  init_opam verbose_level debug_level;
  with_switch_state (fun st -> fst (uninstall_package st (parse_package st name)))

let reinstall verbose_level debug_level name test doc dev_setup =
  init_opam verbose_level debug_level;
  with_switch_state (fun st ->
      let pkg = parse_package st name in
      match uninstall_package st pkg with
      | 0, st -> install_package st pkg test doc dev_setup
      | rc, _ -> rc)

let package_name = Arg.(value & pos 0 string "" & info ~docv:"PACKAGE-NAME" ~doc:"The name and version of the package in the format pkg.x.y.z." [])

let verbose_level = Arg.(value & flag_all & info [ "v"; "verbose" ] ~docv:"VERBOSELEVEL" ~doc:"Increase the log level. Use several times to increase, e.g. '-vv'")

let debug_level = Arg.(value & flag_all & info [ "d"; "debug" ] ~docv:"DEBUGLEVEL" ~doc:"Increase the debug level. Use several times to increase, e.g. '-dd'")

let with_test = Arg.(value & flag & info [ "t"; "with-test" ] ~docv:"WITH-TEST" ~doc:"Build and run package tests")

let with_doc = Arg.(value & flag & info [ "with-doc" ] ~docv:"WITH-DOC" ~doc:"Build package documentation")

let with_dev_setup = Arg.(value & flag & info [ "with-dev-setup" ] ~docv:"WITH-DEV-SETUP" ~doc:"Run package dev setup")

let uninstall_flag = Arg.(value & flag & info [ "uninstall" ] ~doc:"Uninstall the package instead of installing it. Dependent packages are left as they are.")

let reinstall_flag = Arg.(value & flag & info [ "reinstall" ] ~doc:"Uninstall the package first if it is installed, then install it.")

let depexts_only_flag =
  Arg.(
    value & flag
    & info [ "depexts-only" ]
        ~doc:
          "Install the package's system dependencies, then stop, before its sources are fetched or anything of its \
           own is run.  The step that needs the network, so a caller can run it with the network and the rest \
           without.")

(* --depexts-only is a run of its own, so it takes neither --uninstall nor
   --reinstall.  Those two keep what they always did together. *)
let main verbose_level debug_level name test doc dev_setup remove re depexts_only =
  if depexts_only && (remove || re) then `Error (true, "--depexts-only cannot be used with --uninstall or --reinstall")
  else
    `Ok
      (if depexts_only then build ~depexts_only verbose_level debug_level name test doc dev_setup
       else if remove then uninstall verbose_level debug_level name
       else if re then reinstall verbose_level debug_level name test doc dev_setup
       else build verbose_level debug_level name test doc dev_setup)

let cmd =
  let doc = "Installs an opam package in the default switch. No checks, no questions asked." in
  let info = Cmd.info "day10-install" ~version:"%%VERSION%%" ~doc in
  Cmd.v info
    Term.(
      ret
        (const main $ verbose_level $ debug_level $ package_name $ with_test $ with_doc $ with_dev_setup $ uninstall_flag $ reinstall_flag
       $ depexts_only_flag))

let () = exit (Cmd.eval' cmd)
