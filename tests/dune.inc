
(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)(enabled_if (< %{ocaml_version} 4.08.0))
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

(alias
 (name runtest)(enabled_if (>= %{ocaml_version} 4.08.0))
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)(enabled_if (>= %{ocaml_version} 4.08.0))
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

(alias
 (name runtest)(enabled_if (< %{ocaml_version} 4.08.0))
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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

(alias
 (name runtest)(enabled_if (< %{ocaml_version} 4.07.0))
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

(alias
 (name runtest)(enabled_if (>= %{ocaml_version} 4.07.0))
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

(alias
 (name runtest)
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

(alias
 (name runtest)(enabled_if (>= %{ocaml_version} 4.04.0))
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

(alias
 (name runtest)
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

(alias
 (name runtest)(enabled_if (>= %{ocaml_version} 4.08.0))
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

(alias
 (name runtest)
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

(alias
 (name runtest)
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
