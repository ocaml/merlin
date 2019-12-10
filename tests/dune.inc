
(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/misc/external-arity.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/misc)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/misc
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/misc/load_path.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/misc)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/misc
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (>= %{ocaml_version} 4.08.0))
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/type-enclosing/letop.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/type-enclosing)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/type-enclosing
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (>= %{ocaml_version} 4.04.0))
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/recovery/test.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/recovery)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/recovery
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/file-cache/test.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/file-cache)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/file-cache
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/warnings/backtrack.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/warnings)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/warnings
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/short-paths/test.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/short-paths)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/short-paths
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (< %{ocaml_version} 4.07.0))
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/polarity-search/older.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/polarity-search)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/polarity-search
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (>= %{ocaml_version} 4.07.0))
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/polarity-search/recent.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/polarity-search)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/polarity-search
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/type-expr/test.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/type-expr)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/type-expr
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (< %{ocaml_version} 4.08.0))
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/errors/inconsistent-assumptions/test.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/errors/inconsistent-assumptions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/errors/inconsistent-assumptions
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (>= %{ocaml_version} 4.08.0))
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/errors/inconsistent-assumptions/test_408.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/errors/inconsistent-assumptions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/errors/inconsistent-assumptions
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/errors/typing-after-parsing/test.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/errors/typing-after-parsing)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/errors/typing-after-parsing
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/motion/phrase.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/motion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/motion
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/motion/jump.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/motion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/motion
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/local-definitions/issue798.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/local-definitions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/local-definitions
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/local-definitions/issue806.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/local-definitions)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/local-definitions
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/ambiguity/rebinding.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/ambiguity)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/ambiguity
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/ambiguity/not-in-env.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/ambiguity)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/ambiguity
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/mutually-recursive/issue973.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/mutually-recursive)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/mutually-recursive
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/issue845/test.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/issue845)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/issue845
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/non-local/preference/test.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/non-local/preference)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/non-local/preference
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/non-local/ignore-kept-locs/test.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/non-local/ignore-kept-locs)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/non-local/ignore-kept-locs
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/includes/test.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/includes)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/includes
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/reconstruct-identifier/off_by_one.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/reconstruct-identifier)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/reconstruct-identifier
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/reconstruct-identifier/newlines.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/reconstruct-identifier)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/reconstruct-identifier
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/functors/generative.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/functors
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/functors/missed_shadowing.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/functors
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/functors/all_local.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/functors
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/functors/included.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/functors
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/functors/from_application.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/functors
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/functors/nested_applications.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/functors)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/functors
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/partial-cmt/test.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/partial-cmt)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/partial-cmt
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/looping-substitution/test.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/looping-substitution)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/looping-substitution
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/sig-substs/basic.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/sig-substs)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/sig-substs
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/issue802/test.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/issue802)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/issue802
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/context-detection/test.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/context-detection
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/context-detection/label.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/context-detection
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/context-detection/from_a_pattern.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/context-detection
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/context-detection/mod_constr.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/context-detection
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/context-detection/field.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/context-detection
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (>= %{ocaml_version} 4.08.0))
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/context-detection/environment_on_open.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/context-detection
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)(enabled_if (< %{ocaml_version} 4.08.0))
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/locate/context-detection/environment_on_open_bad.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/locate/context-detection)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/locate/context-detection
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/destruct/basic.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/destruct)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/destruct
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/config/path-expansion.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/config)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/config
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/config/flags/invalid.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/config/flags)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/config/flags
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/config/flags/nolabels.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/config/flags)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/config/flags
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/config/flags/unsafe.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/config/flags)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/config/flags
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/no-escape/test.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/no-escape)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/no-escape
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/pp/simple-pp.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/pp)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/pp
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/completion/expansion.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/completion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/completion
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/completion/parenthesize.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/completion)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/completion
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/occurrences/issue827.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/occurrences)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/occurrences
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))

(alias
 (name runtest)
 (deps (:t /home/trefis/repos/merlin/_build/default/tests/occurrences/basic.t)
       (source_tree /home/trefis/repos/merlin/_build/default/tests/occurrences)
       %{bin:ocamlmerlin}
       %{bin:ocamlmerlin-server})
 (action
   (setenv MERLIN %{exe:merlin-wrapper}
   (setenv OCAMLC %{ocamlc}
     (chdir /home/trefis/repos/merlin/_build/default/tests/occurrences
       (progn
         (run %{bin:mdx} test --syntax=cram %{t})
         (diff? %{t} %{t}.corrected)))))))
