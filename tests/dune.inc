
(alias
 (name runtest)
 (deps (:t ./misc/external-arity.t)
       (source_tree ./misc)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./misc
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./misc/load_path.t)
       (source_tree ./misc)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./misc
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (>= %{ocaml_version} 4.08.0))
 (deps (:t ./type-enclosing/letop.t)
       (source_tree ./type-enclosing)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./type-enclosing
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (>= %{ocaml_version} 4.04.0))
 (deps (:t ./recovery/test.t)
       (source_tree ./recovery)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./recovery
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./file-cache/test.t)
       (source_tree ./file-cache)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./file-cache
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./warnings/backtrack.t)
       (source_tree ./warnings)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./warnings
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./short-paths/test.t)
       (source_tree ./short-paths)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./short-paths
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (< %{ocaml_version} 4.07.0))
 (deps (:t ./polarity-search/older.t)
       (source_tree ./polarity-search)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./polarity-search
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (>= %{ocaml_version} 4.07.0))
 (deps (:t ./polarity-search/recent.t)
       (source_tree ./polarity-search)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./polarity-search
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./type-expr/test.t)
       (source_tree ./type-expr)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./type-expr
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (< %{ocaml_version} 4.08.0))
 (deps (:t ./errors/inconsistent-assumptions/test.t)
       (source_tree ./errors/inconsistent-assumptions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./errors/inconsistent-assumptions
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (>= %{ocaml_version} 4.08.0))
 (deps (:t ./errors/inconsistent-assumptions/test_408.t)
       (source_tree ./errors/inconsistent-assumptions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./errors/inconsistent-assumptions
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./errors/typing-after-parsing/test.t)
       (source_tree ./errors/typing-after-parsing)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./errors/typing-after-parsing
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./motion/phrase.t)
       (source_tree ./motion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./motion
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./motion/jump.t)
       (source_tree ./motion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./motion
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/local-definitions/issue798.t)
       (source_tree ./locate/local-definitions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/local-definitions
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/local-definitions/issue806.t)
       (source_tree ./locate/local-definitions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/local-definitions
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/ambiguity/rebinding.t)
       (source_tree ./locate/ambiguity)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/ambiguity
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/ambiguity/not-in-env.t)
       (source_tree ./locate/ambiguity)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/ambiguity
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/mutually-recursive/issue973.t)
       (source_tree ./locate/mutually-recursive)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/mutually-recursive
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/issue845/test.t)
       (source_tree ./locate/issue845)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/issue845
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/non-local/preference/test.t)
       (source_tree ./locate/non-local/preference)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/non-local/preference
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/non-local/ignore-kept-locs/test.t)
       (source_tree ./locate/non-local/ignore-kept-locs)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/non-local/ignore-kept-locs
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/includes/test.t)
       (source_tree ./locate/includes)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/includes
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/reconstruct-identifier/off_by_one.t)
       (source_tree ./locate/reconstruct-identifier)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/reconstruct-identifier
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/reconstruct-identifier/newlines.t)
       (source_tree ./locate/reconstruct-identifier)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/reconstruct-identifier
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/functors/generative.t)
       (source_tree ./locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/functors
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/functors/missed_shadowing.t)
       (source_tree ./locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/functors
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/functors/all_local.t)
       (source_tree ./locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/functors
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/functors/included.t)
       (source_tree ./locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/functors
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/functors/from_application.t)
       (source_tree ./locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/functors
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/functors/nested_applications.t)
       (source_tree ./locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/functors
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/partial-cmt/test.t)
       (source_tree ./locate/partial-cmt)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/partial-cmt
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/looping-substitution/test.t)
       (source_tree ./locate/looping-substitution)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/looping-substitution
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/sig-substs/basic.t)
       (source_tree ./locate/sig-substs)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/sig-substs
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/issue802/test.t)
       (source_tree ./locate/issue802)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/issue802
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/context-detection/test.t)
       (source_tree ./locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/context-detection
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/context-detection/label.t)
       (source_tree ./locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/context-detection
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/context-detection/from_a_pattern.t)
       (source_tree ./locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/context-detection
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/context-detection/mod_constr.t)
       (source_tree ./locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/context-detection
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./locate/context-detection/field.t)
       (source_tree ./locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/context-detection
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (>= %{ocaml_version} 4.08.0))
 (deps (:t ./locate/context-detection/environment_on_open.t)
       (source_tree ./locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/context-detection
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (< %{ocaml_version} 4.08.0))
 (deps (:t ./locate/context-detection/environment_on_open_bad.t)
       (source_tree ./locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./locate/context-detection
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./destruct/basic.t)
       (source_tree ./destruct)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./destruct
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./config/path-expansion.t)
       (source_tree ./config)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./config
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./config/flags/invalid.t)
       (source_tree ./config/flags)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./config/flags
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./config/flags/nolabels.t)
       (source_tree ./config/flags)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./config/flags
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./config/flags/unsafe.t)
       (source_tree ./config/flags)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./config/flags
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./no-escape/test.t)
       (source_tree ./no-escape)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./no-escape
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./pp/simple-pp.t)
       (source_tree ./pp)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./pp
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./completion/expansion.t)
       (source_tree ./completion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./completion
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./completion/parenthesize.t)
       (source_tree ./completion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./completion
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./occurrences/issue827.t)
       (source_tree ./occurrences)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./occurrences
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t ./occurrences/basic.t)
       (source_tree ./occurrences)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir ./occurrences
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
