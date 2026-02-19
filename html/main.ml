let () =
  let file = Sys.argv.(1) in
  let base = Filename.remove_extension (Filename.basename file) in
  let out = base ^ ".html" in
  let cmd =
    Printf.sprintf
      "pant --markdown %s | pandoc -s --css=style.css --metadata title=%s -o %s"
      (Filename.quote file) (Filename.quote base) (Filename.quote out)
  in
  exit (Sys.command cmd)
