
(alias
 (name completion-expansion)
 (deps (:t ./test-dirs/completion/expansion.t)
       (source_tree ./test-dirs/completion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/completion
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias completion-expansion)))

(alias
 (name completion-parenthesize)
 (deps (:t ./test-dirs/completion/parenthesize.t)
       (source_tree ./test-dirs/completion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/completion
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias completion-parenthesize)))

(alias
 (name config-flags-invalid)
 (deps (:t ./test-dirs/config/flags/invalid.t)
       (source_tree ./test-dirs/config/flags)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/config/flags
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias config-flags-invalid)))

(alias
 (name config-flags-nolabels)
 (deps (:t ./test-dirs/config/flags/nolabels.t)
       (source_tree ./test-dirs/config/flags)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/config/flags
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias config-flags-nolabels)))

(alias
 (name config-flags-unsafe)
 (deps (:t ./test-dirs/config/flags/unsafe.t)
       (source_tree ./test-dirs/config/flags)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/config/flags
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias config-flags-unsafe)))

(alias
 (name config-path-expansion)
 (deps (:t ./test-dirs/config/path-expansion.t)
       (source_tree ./test-dirs/config)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/config
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias config-path-expansion)))

(alias
 (name deprecation-run)
 (deps (:t ./test-dirs/deprecation/run.t)
       (source_tree ./test-dirs/deprecation)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/deprecation
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias deprecation-run)))

(alias
 (name destruct-basic)
 (deps (:t ./test-dirs/destruct/basic.t)
       (source_tree ./test-dirs/destruct)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/destruct
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias destruct-basic)))

(alias
 (name errors-inconsistent-assumptions-test)(enabled_if (< %{ocaml_version} 4.08.0))
 (deps (:t ./test-dirs/errors/inconsistent-assumptions/test.t)
       (source_tree ./test-dirs/errors/inconsistent-assumptions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/errors/inconsistent-assumptions
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias errors-inconsistent-assumptions-test)))

(alias
 (name errors-inconsistent-assumptions-test_408)(enabled_if (>= %{ocaml_version} 4.08.0))
 (deps (:t ./test-dirs/errors/inconsistent-assumptions/test_408.t)
       (source_tree ./test-dirs/errors/inconsistent-assumptions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/errors/inconsistent-assumptions
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias errors-inconsistent-assumptions-test_408)))

(alias
 (name errors-typing-after-parsing-test)
 (deps (:t ./test-dirs/errors/typing-after-parsing/test.t)
       (source_tree ./test-dirs/errors/typing-after-parsing)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/errors/typing-after-parsing
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias errors-typing-after-parsing-test)))

(alias
 (name file-cache-test)
 (deps (:t ./test-dirs/file-cache/test.t)
       (source_tree ./test-dirs/file-cache)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/file-cache
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias file-cache-test)))

(alias
 (name locate-ambiguity-not-in-env)
 (deps (:t ./test-dirs/locate/ambiguity/not-in-env.t)
       (source_tree ./test-dirs/locate/ambiguity)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/ambiguity
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-ambiguity-not-in-env)))

(alias
 (name locate-ambiguity-rebinding)
 (deps (:t ./test-dirs/locate/ambiguity/rebinding.t)
       (source_tree ./test-dirs/locate/ambiguity)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/ambiguity
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-ambiguity-rebinding)))

(alias
 (name locate-context-detection-environment_on_open)(enabled_if (>= %{ocaml_version} 4.08.0))
 (deps (:t ./test-dirs/locate/context-detection/environment_on_open.t)
       (source_tree ./test-dirs/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/context-detection
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-context-detection-environment_on_open)))

(alias
 (name locate-context-detection-environment_on_open_bad)(enabled_if (< %{ocaml_version} 4.08.0))
 (deps (:t ./test-dirs/locate/context-detection/environment_on_open_bad.t)
       (source_tree ./test-dirs/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/context-detection
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-context-detection-environment_on_open_bad)))

(alias
 (name locate-context-detection-field)
 (deps (:t ./test-dirs/locate/context-detection/field.t)
       (source_tree ./test-dirs/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/context-detection
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-context-detection-field)))

(alias
 (name locate-context-detection-from_a_pattern)
 (deps (:t ./test-dirs/locate/context-detection/from_a_pattern.t)
       (source_tree ./test-dirs/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/context-detection
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-context-detection-from_a_pattern)))

(alias
 (name locate-context-detection-label)
 (deps (:t ./test-dirs/locate/context-detection/label.t)
       (source_tree ./test-dirs/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/context-detection
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-context-detection-label)))

(alias
 (name locate-context-detection-mod_constr)
 (deps (:t ./test-dirs/locate/context-detection/mod_constr.t)
       (source_tree ./test-dirs/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/context-detection
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-context-detection-mod_constr)))

(alias
 (name locate-context-detection-test)
 (deps (:t ./test-dirs/locate/context-detection/test.t)
       (source_tree ./test-dirs/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/context-detection
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-context-detection-test)))

(alias
 (name locate-functors-all_local)
 (deps (:t ./test-dirs/locate/functors/all_local.t)
       (source_tree ./test-dirs/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/functors
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-functors-all_local)))

(alias
 (name locate-functors-from_application)
 (deps (:t ./test-dirs/locate/functors/from_application.t)
       (source_tree ./test-dirs/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/functors
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-functors-from_application)))

(alias
 (name locate-functors-generative)
 (deps (:t ./test-dirs/locate/functors/generative.t)
       (source_tree ./test-dirs/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/functors
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-functors-generative)))

(alias
 (name locate-functors-included)
 (deps (:t ./test-dirs/locate/functors/included.t)
       (source_tree ./test-dirs/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/functors
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-functors-included)))

(alias
 (name locate-functors-missed_shadowing)
 (deps (:t ./test-dirs/locate/functors/missed_shadowing.t)
       (source_tree ./test-dirs/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/functors
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-functors-missed_shadowing)))

(alias
 (name locate-functors-nested_applications)
 (deps (:t ./test-dirs/locate/functors/nested_applications.t)
       (source_tree ./test-dirs/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/functors
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-functors-nested_applications)))

(alias
 (name locate-includes-test)
 (deps (:t ./test-dirs/locate/includes/test.t)
       (source_tree ./test-dirs/locate/includes)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/includes
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-includes-test)))

(alias
 (name locate-issue802-test)
 (deps (:t ./test-dirs/locate/issue802/test.t)
       (source_tree ./test-dirs/locate/issue802)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/issue802
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-issue802-test)))

(alias
 (name locate-issue845-test)
 (deps (:t ./test-dirs/locate/issue845/test.t)
       (source_tree ./test-dirs/locate/issue845)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/issue845
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-issue845-test)))

(alias
 (name locate-issue949-run)
 (deps (:t ./test-dirs/locate/issue949/run.t)
       (source_tree ./test-dirs/locate/issue949)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/issue949
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-issue949-run)))

(alias
 (name locate-local-definitions-issue798)
 (deps (:t ./test-dirs/locate/local-definitions/issue798.t)
       (source_tree ./test-dirs/locate/local-definitions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/local-definitions
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-local-definitions-issue798)))

(alias
 (name locate-local-definitions-issue806)
 (deps (:t ./test-dirs/locate/local-definitions/issue806.t)
       (source_tree ./test-dirs/locate/local-definitions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/local-definitions
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-local-definitions-issue806)))

(alias
 (name locate-looping-substitution-test)
 (deps (:t ./test-dirs/locate/looping-substitution/test.t)
       (source_tree ./test-dirs/locate/looping-substitution)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/looping-substitution
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-looping-substitution-test)))

(alias
 (name locate-mutually-recursive-issue973)
 (deps (:t ./test-dirs/locate/mutually-recursive/issue973.t)
       (source_tree ./test-dirs/locate/mutually-recursive)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/mutually-recursive
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-mutually-recursive-issue973)))

(alias
 (name locate-non-local-ignore-kept-locs-test)
 (deps (:t ./test-dirs/locate/non-local/ignore-kept-locs/test.t)
       (source_tree ./test-dirs/locate/non-local/ignore-kept-locs)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/non-local/ignore-kept-locs
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-non-local-ignore-kept-locs-test)))

(alias
 (name locate-non-local-preference-test)
 (deps (:t ./test-dirs/locate/non-local/preference/test.t)
       (source_tree ./test-dirs/locate/non-local/preference)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/non-local/preference
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-non-local-preference-test)))

(alias
 (name locate-partial-cmt-test)
 (deps (:t ./test-dirs/locate/partial-cmt/test.t)
       (source_tree ./test-dirs/locate/partial-cmt)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/partial-cmt
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-partial-cmt-test)))

(alias
 (name locate-reconstruct-identifier-newlines)
 (deps (:t ./test-dirs/locate/reconstruct-identifier/newlines.t)
       (source_tree ./test-dirs/locate/reconstruct-identifier)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/reconstruct-identifier
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-reconstruct-identifier-newlines)))

(alias
 (name locate-reconstruct-identifier-off_by_one)
 (deps (:t ./test-dirs/locate/reconstruct-identifier/off_by_one.t)
       (source_tree ./test-dirs/locate/reconstruct-identifier)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/reconstruct-identifier
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-reconstruct-identifier-off_by_one)))

(alias
 (name locate-sig-substs-basic)
 (deps (:t ./test-dirs/locate/sig-substs/basic.t)
       (source_tree ./test-dirs/locate/sig-substs)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/locate/sig-substs
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias locate-sig-substs-basic)))

(alias
 (name misc-external-arity)
 (deps (:t ./test-dirs/misc/external-arity.t)
       (source_tree ./test-dirs/misc)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/misc
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias misc-external-arity)))

(alias
 (name misc-load_path)
 (deps (:t ./test-dirs/misc/load_path.t)
       (source_tree ./test-dirs/misc)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/misc
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias misc-load_path)))

(alias
 (name motion-jump)
 (deps (:t ./test-dirs/motion/jump.t)
       (source_tree ./test-dirs/motion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/motion
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias motion-jump)))

(alias
 (name motion-phrase)
 (deps (:t ./test-dirs/motion/phrase.t)
       (source_tree ./test-dirs/motion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/motion
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias motion-phrase)))

(alias
 (name no-escape-test)
 (deps (:t ./test-dirs/no-escape/test.t)
       (source_tree ./test-dirs/no-escape)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/no-escape
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias no-escape-test)))

(alias
 (name occurrences-basic)
 (deps (:t ./test-dirs/occurrences/basic.t)
       (source_tree ./test-dirs/occurrences)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/occurrences
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias occurrences-basic)))

(alias
 (name occurrences-issue827)
 (deps (:t ./test-dirs/occurrences/issue827.t)
       (source_tree ./test-dirs/occurrences)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/occurrences
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias occurrences-issue827)))

(alias
 (name polarity-search-older)(enabled_if (< %{ocaml_version} 4.07.0))
 (deps (:t ./test-dirs/polarity-search/older.t)
       (source_tree ./test-dirs/polarity-search)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/polarity-search
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias polarity-search-older)))

(alias
 (name polarity-search-recent)(enabled_if (>= %{ocaml_version} 4.07.0))
 (deps (:t ./test-dirs/polarity-search/recent.t)
       (source_tree ./test-dirs/polarity-search)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/polarity-search
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias polarity-search-recent)))

(alias
 (name pp-simple-pp)
 (deps (:t ./test-dirs/pp/simple-pp.t)
       (source_tree ./test-dirs/pp)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/pp
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias pp-simple-pp)))

(alias
 (name recovery-test)(enabled_if (>= %{ocaml_version} 4.04.0))
 (deps (:t ./test-dirs/recovery/test.t)
       (source_tree ./test-dirs/recovery)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/recovery
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias recovery-test)))

(alias
 (name short-paths-test)
 (deps (:t ./test-dirs/short-paths/test.t)
       (source_tree ./test-dirs/short-paths)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/short-paths
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias short-paths-test)))

(alias
 (name type-enclosing-letop)(enabled_if (>= %{ocaml_version} 4.08.0))
 (deps (:t ./test-dirs/type-enclosing/letop.t)
       (source_tree ./test-dirs/type-enclosing)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/type-enclosing
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-letop)))

(alias
 (name type-enclosing-module_type)
 (deps (:t ./test-dirs/type-enclosing/module_type.t)
       (source_tree ./test-dirs/type-enclosing)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/type-enclosing
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-enclosing-module_type)))

(alias
 (name type-expr-test)
 (deps (:t ./test-dirs/type-expr/test.t)
       (source_tree ./test-dirs/type-expr)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/type-expr
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias type-expr-test)))

(alias
 (name warnings-backtrack)
 (deps (:t ./test-dirs/warnings/backtrack.t)
       (source_tree ./test-dirs/warnings)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (chdir ./test-dirs/warnings
     (setenv MERLIN %{exe:merlin-wrapper}
     (setenv OCAMLC %{ocamlc}
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
(alias (name runtest) (deps (alias warnings-backtrack)))
