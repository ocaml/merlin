let _ : _ = Repro2_main.Topic_name.of_topic

(* U:
     Repro2_main.Topic_name.of_topic
       ->  D: Topic.t [Repro2_types__!.Topic.t]  -?-> D: Topic
     Repro2_main.Topic_name
       -> D: Repro2_types!.Topic_name
              -> Subst: Topic_name -> Repro2_types__Topic_name  (? check)
       -> U: Repro2_types!.Topic_name
              -> D: Topic_name [Repro2_types__!.Topic_name]
              -> U: Topic_name [Repro2_types__!.Topic_name]
                    -> D: Topic_name.t [? check]
     Repro2_main
       -> D: Repro2_main.Topic [? path]     Repro2_main.Topic_name [? path]
            -> Subst: Repro2_main.Topic -> Repro2_types!.Topic
            -> Subst: Repro2_main.Topic_name -> Repro2_types!.Topic_name

   Repro2_types!.Topic -> Repro2_types__!.Topic

    Initial: Repro2_types__!.Topic.t
    Canon: Topic!.t


    We are missing the
    Repro2_types__!.Topic -> [Repro2_types__Topic] subtitution ??
*)
