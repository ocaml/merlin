
(rule
 (alias completion-application_context)(enabled_if (>= %{ocaml_version} 4.09.0))
 (package merlin)
 (deps (:t ./test-dirs/completion/application_context.t)
       (source_tree ./test-dirs/completion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/completion
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias completion-application_context)))

(rule
 (alias completion-expansion)
 (package merlin)
 (deps (:t ./test-dirs/completion/expansion.t)
       (source_tree ./test-dirs/completion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/completion
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias completion-expansion)))

(rule
 (alias completion-infix)
 (package merlin)
 (deps (:t ./test-dirs/completion/infix.t)
       (source_tree ./test-dirs/completion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/completion
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias completion-infix)))

(rule
 (alias completion-parenthesize_from_411)(enabled_if (>= %{ocaml_version} 4.11.0))
 (package merlin)
 (deps (:t ./test-dirs/completion/parenthesize_from_411.t)
       (source_tree ./test-dirs/completion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/completion
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias completion-parenthesize_from_411)))

(alias
 (name completion-parenthesize_pre_411)(enabled_if (< %{ocaml_version} 4.11.0))
 (package merlin)
 (deps (:t ./test-dirs/completion/parenthesize_pre_411.t)
       (source_tree ./test-dirs/completion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/completion
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias completion-parenthesize_pre_411)))

(rule
 (alias config-check-check-config)
 (package merlin)
 (deps (:t ./test-dirs/config/check/check-config.t)
       (source_tree ./test-dirs/config/check)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/config/check
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias config-check-check-config)))

(rule
 (alias config-dot-merlin-reader-erroneous-config)
 (package merlin)
 (deps (:t ./test-dirs/config/dot-merlin-reader/erroneous-config.t)
       (source_tree ./test-dirs/config/dot-merlin-reader)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/config/dot-merlin-reader
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias config-dot-merlin-reader-erroneous-config)))

(rule
 (alias config-dot-merlin-reader-quoting)
 (package merlin)
 (deps (:t ./test-dirs/config/dot-merlin-reader/quoting.t)
       (source_tree ./test-dirs/config/dot-merlin-reader)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/config/dot-merlin-reader
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias config-dot-merlin-reader-quoting)))

(rule
 (alias config-flags-invalid)
 (package merlin)
 (deps (:t ./test-dirs/config/flags/invalid.t)
       (source_tree ./test-dirs/config/flags)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/config/flags
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias config-flags-invalid)))

(rule
 (alias config-flags-nolabels)
 (package merlin)
 (deps (:t ./test-dirs/config/flags/nolabels.t)
       (source_tree ./test-dirs/config/flags)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/config/flags
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias config-flags-nolabels)))

(rule
 (alias config-flags-unsafe)
 (package merlin)
 (deps (:t ./test-dirs/config/flags/unsafe.t)
       (source_tree ./test-dirs/config/flags)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/config/flags
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias config-flags-unsafe)))

(rule
 (alias config-path-expansion)
 (package merlin)
 (deps (:t ./test-dirs/config/path-expansion.t)
       (source_tree ./test-dirs/config)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/config
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias config-path-expansion)))

(rule
 (alias deprecation-run)
 (package merlin)
 (deps (:t ./test-dirs/deprecation/run.t)
       (source_tree ./test-dirs/deprecation)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/deprecation
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias deprecation-run)))

(rule
 (alias destruct-complete)
 (package merlin)
 (deps (:t ./test-dirs/destruct/complete.t)
       (source_tree ./test-dirs/destruct)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/destruct
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias destruct-complete)))

(rule
 (alias destruct-create)
 (package merlin)
 (deps (:t ./test-dirs/destruct/create.t)
       (source_tree ./test-dirs/destruct)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/destruct
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias destruct-create)))

(rule
 (alias destruct-errors)
 (package merlin)
 (deps (:t ./test-dirs/destruct/errors.t)
       (source_tree ./test-dirs/destruct)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/destruct
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias destruct-errors)))

(rule
 (alias destruct-errors_pre_408)(enabled_if (< %{ocaml_version} 4.08.0))
 (package merlin)
 (deps (:t ./test-dirs/destruct/errors_pre_408.t)
       (source_tree ./test-dirs/destruct)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/destruct
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias destruct-errors_pre_408)))

(rule
 (alias destruct-errors_since_408)(enabled_if (>= %{ocaml_version} 4.08.0))
 (package merlin)
 (deps (:t ./test-dirs/destruct/errors_since_408.t)
       (source_tree ./test-dirs/destruct)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/destruct
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias destruct-errors_since_408)))

(rule
 (alias destruct-from_val)
 (package merlin)
 (deps (:t ./test-dirs/destruct/from_val.t)
       (source_tree ./test-dirs/destruct)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/destruct
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias destruct-from_val)))

(rule
 (alias destruct-issue596)
 (package merlin)
 (deps (:t ./test-dirs/destruct/issue596.t)
       (source_tree ./test-dirs/destruct)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/destruct
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias destruct-issue596)))

(rule
 (alias destruct-refine)
 (package merlin)
 (deps (:t ./test-dirs/destruct/refine.t)
       (source_tree ./test-dirs/destruct)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/destruct
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias destruct-refine)))

(rule
 (alias errors-alerts-test-pre-408)(enabled_if (< %{ocaml_version} 4.08.0))
 (package merlin)
 (deps (:t ./test-dirs/errors/alerts/test-pre-408.t)
       (source_tree ./test-dirs/errors/alerts)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/errors/alerts
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias errors-alerts-test-pre-408)))

(rule
 (alias errors-alerts-test)(enabled_if (>= %{ocaml_version} 4.08.0))
 (package merlin)
 (deps (:t ./test-dirs/errors/alerts/test.t)
       (source_tree ./test-dirs/errors/alerts)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/errors/alerts
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias errors-alerts-test)))

(rule
 (alias errors-error-in-constrained-env-test)
 (package merlin)
 (deps (:t ./test-dirs/errors/error-in-constrained-env/test.t)
       (source_tree ./test-dirs/errors/error-in-constrained-env)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/errors/error-in-constrained-env
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias errors-error-in-constrained-env-test)))

(rule
 (alias errors-inconsistent-assumptions-test)(enabled_if (< %{ocaml_version} 4.08.0))
 (package merlin)
 (deps (:t ./test-dirs/errors/inconsistent-assumptions/test.t)
       (source_tree ./test-dirs/errors/inconsistent-assumptions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/errors/inconsistent-assumptions
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias errors-inconsistent-assumptions-test)))

(rule
 (alias errors-inconsistent-assumptions-test_408)(enabled_if (>= %{ocaml_version} 4.08.0))
 (package merlin)
 (deps (:t ./test-dirs/errors/inconsistent-assumptions/test_408.t)
       (source_tree ./test-dirs/errors/inconsistent-assumptions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/errors/inconsistent-assumptions
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias errors-inconsistent-assumptions-test_408)))

(rule
 (alias errors-typing-after-parsing-test)
 (package merlin)
 (deps (:t ./test-dirs/errors/typing-after-parsing/test.t)
       (source_tree ./test-dirs/errors/typing-after-parsing)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/errors/typing-after-parsing
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias errors-typing-after-parsing-test)))

(rule
 (alias locate-ambiguity-not-in-env)
 (package merlin)
 (deps (:t ./test-dirs/locate/ambiguity/not-in-env.t)
       (source_tree ./test-dirs/locate/ambiguity)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/ambiguity
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-ambiguity-not-in-env)))

(rule
 (alias locate-ambiguity-rebinding)
 (package merlin)
 (deps (:t ./test-dirs/locate/ambiguity/rebinding.t)
       (source_tree ./test-dirs/locate/ambiguity)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/ambiguity
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-ambiguity-rebinding)))

(rule
 (alias locate-context-detection-environment_on_open)(enabled_if (>= %{ocaml_version} 4.08.0))
 (package merlin)
 (deps (:t ./test-dirs/locate/context-detection/environment_on_open.t)
       (source_tree ./test-dirs/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/context-detection
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-context-detection-environment_on_open)))

(rule
 (alias locate-context-detection-environment_on_open_bad)(enabled_if (< %{ocaml_version} 4.08.0))
 (package merlin)
 (deps (:t ./test-dirs/locate/context-detection/environment_on_open_bad.t)
       (source_tree ./test-dirs/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/context-detection
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-context-detection-environment_on_open_bad)))

(rule
 (alias locate-context-detection-field)
 (package merlin)
 (deps (:t ./test-dirs/locate/context-detection/field.t)
       (source_tree ./test-dirs/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/context-detection
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-context-detection-field)))

(rule
 (alias locate-context-detection-from_a_pattern)
 (package merlin)
 (deps (:t ./test-dirs/locate/context-detection/from_a_pattern.t)
       (source_tree ./test-dirs/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/context-detection
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-context-detection-from_a_pattern)))

(rule
 (alias locate-context-detection-label)
 (package merlin)
 (deps (:t ./test-dirs/locate/context-detection/label.t)
       (source_tree ./test-dirs/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/context-detection
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-context-detection-label)))

(rule
 (alias locate-context-detection-mod_constr)
 (package merlin)
 (deps (:t ./test-dirs/locate/context-detection/mod_constr.t)
       (source_tree ./test-dirs/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/context-detection
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-context-detection-mod_constr)))

(rule
 (alias locate-context-detection-test)
 (package merlin)
 (deps (:t ./test-dirs/locate/context-detection/test.t)
       (source_tree ./test-dirs/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/context-detection
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-context-detection-test)))

(rule
 (alias locate-functors-all_local)
 (package merlin)
 (deps (:t ./test-dirs/locate/functors/all_local.t)
       (source_tree ./test-dirs/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/functors
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-functors-all_local)))

(rule
 (alias locate-functors-from_application)
 (package merlin)
 (deps (:t ./test-dirs/locate/functors/from_application.t)
       (source_tree ./test-dirs/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/functors
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-functors-from_application)))

(rule
 (alias locate-functors-generative)
 (package merlin)
 (deps (:t ./test-dirs/locate/functors/generative.t)
       (source_tree ./test-dirs/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/functors
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-functors-generative)))

(rule
 (alias locate-functors-included)
 (package merlin)
 (deps (:t ./test-dirs/locate/functors/included.t)
       (source_tree ./test-dirs/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/functors
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-functors-included)))

(rule
 (alias locate-functors-missed_shadowing)
 (package merlin)
 (deps (:t ./test-dirs/locate/functors/missed_shadowing.t)
       (source_tree ./test-dirs/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/functors
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-functors-missed_shadowing)))

(rule
 (alias locate-functors-nested_applications)
 (package merlin)
 (deps (:t ./test-dirs/locate/functors/nested_applications.t)
       (source_tree ./test-dirs/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/functors
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-functors-nested_applications)))

(rule
 (alias locate-includes-test)
 (package merlin)
 (deps (:t ./test-dirs/locate/includes/test.t)
       (source_tree ./test-dirs/locate/includes)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/includes
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-includes-test)))

(rule
 (alias locate-issue802-test)
 (package merlin)
 (deps (:t ./test-dirs/locate/issue802/test.t)
       (source_tree ./test-dirs/locate/issue802)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/issue802
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-issue802-test)))

(rule
 (alias locate-issue845-test)
 (package merlin)
 (deps (:t ./test-dirs/locate/issue845/test.t)
       (source_tree ./test-dirs/locate/issue845)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/issue845
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-issue845-test)))

(rule
 (alias locate-issue949-run)
 (package merlin)
 (deps (:t ./test-dirs/locate/issue949/run.t)
       (source_tree ./test-dirs/locate/issue949)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/issue949
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-issue949-run)))

(rule
 (alias locate-local-definitions-issue798)
 (package merlin)
 (deps (:t ./test-dirs/locate/local-definitions/issue798.t)
       (source_tree ./test-dirs/locate/local-definitions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/local-definitions
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-local-definitions-issue798)))

(rule
 (alias locate-local-definitions-issue806)
 (package merlin)
 (deps (:t ./test-dirs/locate/local-definitions/issue806.t)
       (source_tree ./test-dirs/locate/local-definitions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/local-definitions
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-local-definitions-issue806)))

(rule
 (alias locate-looping-substitution-test)
 (package merlin)
 (deps (:t ./test-dirs/locate/looping-substitution/test.t)
       (source_tree ./test-dirs/locate/looping-substitution)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/looping-substitution
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-looping-substitution-test)))

(rule
 (alias locate-mutually-recursive-issue973)
 (package merlin)
 (deps (:t ./test-dirs/locate/mutually-recursive/issue973.t)
       (source_tree ./test-dirs/locate/mutually-recursive)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/mutually-recursive
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-mutually-recursive-issue973)))

(rule
 (alias locate-non-local-ignore-kept-locs-test)
 (package merlin)
 (deps (:t ./test-dirs/locate/non-local/ignore-kept-locs/test.t)
       (source_tree ./test-dirs/locate/non-local/ignore-kept-locs)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/non-local/ignore-kept-locs
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-non-local-ignore-kept-locs-test)))

(rule
 (alias locate-non-local-preference-test)
 (package merlin)
 (deps (:t ./test-dirs/locate/non-local/preference/test.t)
       (source_tree ./test-dirs/locate/non-local/preference)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/non-local/preference
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-non-local-preference-test)))

(rule
 (alias locate-partial-cmt-test)
 (package merlin)
 (deps (:t ./test-dirs/locate/partial-cmt/test.t)
       (source_tree ./test-dirs/locate/partial-cmt)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/partial-cmt
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-partial-cmt-test)))

(rule
 (alias locate-reconstruct-identifier-newlines)
 (package merlin)
 (deps (:t ./test-dirs/locate/reconstruct-identifier/newlines.t)
       (source_tree ./test-dirs/locate/reconstruct-identifier)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/reconstruct-identifier
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-reconstruct-identifier-newlines)))

(rule
 (alias locate-reconstruct-identifier-off_by_one)
 (package merlin)
 (deps (:t ./test-dirs/locate/reconstruct-identifier/off_by_one.t)
       (source_tree ./test-dirs/locate/reconstruct-identifier)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/reconstruct-identifier
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-reconstruct-identifier-off_by_one)))

(rule
 (alias locate-sig-substs-basic)
 (package merlin)
 (deps (:t ./test-dirs/locate/sig-substs/basic.t)
       (source_tree ./test-dirs/locate/sig-substs)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate/sig-substs
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-sig-substs-basic)))

(rule
 (alias locate-type-run)
 (package merlin)
 (deps (:t ./test-dirs/locate-type/run.t)
       (source_tree ./test-dirs/locate-type)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/locate-type
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-type-run)))

(rule
 (alias misc-external-arity)
 (package merlin)
 (deps (:t ./test-dirs/misc/external-arity.t)
       (source_tree ./test-dirs/misc)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/misc
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias misc-external-arity)))

(rule
 (alias misc-load_path)
 (package merlin)
 (deps (:t ./test-dirs/misc/load_path.t)
       (source_tree ./test-dirs/misc)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/misc
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias misc-load_path)))

(rule
 (alias motion-jump)
 (package merlin)
 (deps (:t ./test-dirs/motion/jump.t)
       (source_tree ./test-dirs/motion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/motion
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias motion-jump)))

(rule
 (alias motion-phrase)
 (package merlin)
 (deps (:t ./test-dirs/motion/phrase.t)
       (source_tree ./test-dirs/motion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/motion
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias motion-phrase)))

(rule
 (alias no-escape-test)
 (package merlin)
 (deps (:t ./test-dirs/no-escape/test.t)
       (source_tree ./test-dirs/no-escape)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/no-escape
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias no-escape-test)))

(rule
 (alias occurrences-basic)
 (package merlin)
 (deps (:t ./test-dirs/occurrences/basic.t)
       (source_tree ./test-dirs/occurrences)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/occurrences
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias occurrences-basic)))

(rule
 (alias occurrences-issue827)
 (package merlin)
 (deps (:t ./test-dirs/occurrences/issue827.t)
       (source_tree ./test-dirs/occurrences)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/occurrences
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias occurrences-issue827)))

(rule
 (alias outline-run)
 (package merlin)
 (deps (:t ./test-dirs/outline/run.t)
       (source_tree ./test-dirs/outline)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/outline
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias outline-run)))

(rule
 (alias polarity-search-older)(enabled_if (< %{ocaml_version} 4.07.0))
 (package merlin)
 (deps (:t ./test-dirs/polarity-search/older.t)
       (source_tree ./test-dirs/polarity-search)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/polarity-search
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias polarity-search-older)))

(rule
 (alias polarity-search-recent)(enabled_if (>= %{ocaml_version} 4.07.0))
 (package merlin)
 (deps (:t ./test-dirs/polarity-search/recent.t)
       (source_tree ./test-dirs/polarity-search)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/polarity-search
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias polarity-search-recent)))

(rule
 (alias pp-simple-pp)
 (package merlin)
 (deps (:t ./test-dirs/pp/simple-pp.t)
       (source_tree ./test-dirs/pp)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/pp
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias pp-simple-pp)))

(rule
 (alias recovery-test)(enabled_if (>= %{ocaml_version} 4.04.0))
 (package merlin)
 (deps (:t ./test-dirs/recovery/test.t)
       (source_tree ./test-dirs/recovery)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/recovery
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias recovery-test)))

(rule
 (alias short-paths-test)(enabled_if (>= %{ocaml_version} 4.10.0))
 (package merlin)
 (deps (:t ./test-dirs/short-paths/test.t)
       (source_tree ./test-dirs/short-paths)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/short-paths
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias short-paths-test)))

(rule
 (alias short-paths-test_pre_410)(enabled_if (< %{ocaml_version} 4.10.0))
 (package merlin)
 (deps (:t ./test-dirs/short-paths/test_pre_410.t)
       (source_tree ./test-dirs/short-paths)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/short-paths
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias short-paths-test_pre_410)))

(rule
 (alias type-enclosing-constructors_and_paths-cons)
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/constructors_and_paths/cons.t)
       (source_tree ./test-dirs/type-enclosing/constructors_and_paths)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/constructors_and_paths
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-constructors_and_paths-cons)))

(rule
 (alias type-enclosing-misc-let)
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/misc/let.t)
       (source_tree ./test-dirs/type-enclosing/misc)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/misc
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-misc-let)))

(rule
 (alias type-enclosing-misc-letop)(enabled_if (>= %{ocaml_version} 4.08.0))
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/misc/letop.t)
       (source_tree ./test-dirs/type-enclosing/misc)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/misc
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-misc-letop)))

(rule
 (alias type-enclosing-modules-alias)
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/modules/alias.t)
       (source_tree ./test-dirs/type-enclosing/modules)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/modules
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-modules-alias)))

(rule
 (alias type-enclosing-modules-issue1003)
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/modules/issue1003.t)
       (source_tree ./test-dirs/type-enclosing/modules)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/modules
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-modules-issue1003)))

(rule
 (alias type-enclosing-modules-module_type)
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/modules/module_type.t)
       (source_tree ./test-dirs/type-enclosing/modules)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/modules
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-modules-module_type)))

(rule
 (alias type-enclosing-modules-not-in-env)
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/modules/not-in-env.t)
       (source_tree ./test-dirs/type-enclosing/modules)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/modules
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-modules-not-in-env)))

(rule
 (alias type-enclosing-objects-test_correct_pre_408)(enabled_if (< %{ocaml_version} 4.08.0))
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/objects/test_correct_pre_408.t)
       (source_tree ./test-dirs/type-enclosing/objects)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/objects
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-objects-test_correct_pre_408)))

(rule
 (alias type-enclosing-objects-test_wrong_from_408)(enabled_if (>= %{ocaml_version} 4.08.0))
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/objects/test_wrong_from_408.t)
       (source_tree ./test-dirs/type-enclosing/objects)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/objects
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-objects-test_wrong_from_408)))

(rule
 (alias type-enclosing-records-issue864)
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/records/issue864.t)
       (source_tree ./test-dirs/type-enclosing/records)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/records
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-records-issue864)))

(rule
 (alias type-enclosing-records-record)
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/records/record.t)
       (source_tree ./test-dirs/type-enclosing/records)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/records
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-records-record)))

(rule
 (alias type-enclosing-recovery-issue1109_from_406)(enabled_if (>= %{ocaml_version} 4.06.0))
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/recovery/issue1109_from_406.t)
       (source_tree ./test-dirs/type-enclosing/recovery)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/recovery
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-recovery-issue1109_from_406)))

(rule
 (alias type-enclosing-recovery-issue1109_pre_406)(enabled_if (< %{ocaml_version} 4.06.0))
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/recovery/issue1109_pre_406.t)
       (source_tree ./test-dirs/type-enclosing/recovery)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/recovery
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-recovery-issue1109_pre_406)))

(rule
 (alias type-enclosing-types-alias)
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/types/alias.t)
       (source_tree ./test-dirs/type-enclosing/types)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/types
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-types-alias)))

(rule
 (alias type-enclosing-types-gadt_correct_pre_408)(enabled_if (< %{ocaml_version} 4.08.0))
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/types/gadt_correct_pre_408.t)
       (source_tree ./test-dirs/type-enclosing/types)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/types
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-types-gadt_correct_pre_408)))

(rule
 (alias type-enclosing-types-gadt_wrong_from_408)(enabled_if (>= %{ocaml_version} 4.08.0))
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/types/gadt_wrong_from_408.t)
       (source_tree ./test-dirs/type-enclosing/types)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/types
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-types-gadt_wrong_from_408)))

(rule
 (alias type-enclosing-types-types)
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/types/types.t)
       (source_tree ./test-dirs/type-enclosing/types)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/types
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-types-types)))

(rule
 (alias type-enclosing-types-variants)
 (package merlin)
 (deps (:t ./test-dirs/type-enclosing/types/variants.t)
       (source_tree ./test-dirs/type-enclosing/types)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-enclosing/types
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-types-variants)))

(rule
 (alias type-expr-test)
 (package merlin)
 (deps (:t ./test-dirs/type-expr/test.t)
       (source_tree ./test-dirs/type-expr)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/type-expr
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-expr-test)))

(rule
 (alias typer-cache-current-level-test)
 (package merlin)
 (deps (:t ./test-dirs/typer-cache/current-level/test.t)
       (source_tree ./test-dirs/typer-cache/current-level)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/typer-cache/current-level
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias typer-cache-current-level-test)))

(rule
 (alias typer-cache-load_path-test)
 (package merlin)
 (deps (:t ./test-dirs/typer-cache/load_path/test.t)
       (source_tree ./test-dirs/typer-cache/load_path)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/typer-cache/load_path
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias typer-cache-load_path-test)))

(rule
 (alias typer-cache-stamps-test)
 (package merlin)
 (deps (:t ./test-dirs/typer-cache/stamps/test.t)
       (source_tree ./test-dirs/typer-cache/stamps)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/typer-cache/stamps
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias typer-cache-stamps-test)))

(rule
 (alias warnings-backtrack)
 (package merlin)
 (deps (:t ./test-dirs/warnings/backtrack.t)
       (source_tree ./test-dirs/warnings)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server}
       %{bin:dot-merlin-reader})
 (action
   (chdir ./test-dirs/warnings
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias warnings-backtrack)))
