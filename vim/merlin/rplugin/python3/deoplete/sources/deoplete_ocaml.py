# -*- coding: utf-8 -*-

from .base import Base
import re
import subprocess
import json
import pprint


DEBUG = False

def concat_map(f, args):
    return [item for arg in args for item in f(arg)]


class Source(Base):

    def __init__(self, vim):
        Base.__init__(self, vim)

        self.name = 'ocaml'
        self.mark = '[ocaml]'
        self.filetypes = ['ocaml']
        self.rank = 1000
        self.input_pattern = r'[^\s\'"]*'
        self.current = vim.current
        self.vim = vim
        self.debug_enabled = False
        self.complete_query_re = re.compile(r'[^\s\'"()]*$')

    def _is_set(self, name, default=False):
        if not self.vim.eval('exists("%s")' % name):
            return default
        return not (self.vim.eval(name) in ["", 0, "0", "false"])

    def _list_if_set(self, name):
        return self.vim.eval('exists("{0}") ? {0} : []'.format(name))

    def on_init(self, context): # called by deoplete
        self.merlin_completion_with_doc = self._is_set("merlin_completion_with_doc")
        self.merlin_binary = self.vim.eval("merlin#SelectBinary()")
        self.merlin_binary_flags = self.vim.eval('g:merlin_binary_flags')
        self.buffer_merlin_flags = self._list_if_set('b:merlin_flags')
        self.merlin_extensions = concat_map(lambda ext: ("-extension",ext), self._list_if_set("b:merlin_extensions"))
        self.merlin_packages = concat_map(lambda pkg: ("-package",pkg), self._list_if_set("b:merlin_packages"))
        self.merlin_dot_merlins = concat_map(lambda dm: ("-dot-merlin",dm), self._list_if_set("b:merlin_dot_merlins"))

        if self._is_set("g:merlin_debug"):
            log_errors = ["-log-file", "-"]
        else:
            log_errors = []
        self.merlin_log_errors = log_errors

    def get_complete_position(self, context): # called by deoplete
        m = re.search(self.complete_query_re, context["input"])
        return m.start() if m else None

    def _get_complete_query(self, context):
        m = re.search(self.complete_query_re, context["input"])
        return m.group() if m else None

    def gather_candidates(self, context): # called by deoplete
        prefix = self._get_complete_query(context) or ""
        filename = context["bufpath"]
        position = "{}:{}".format(context["position"][1], context["position"][2])
        lines = self.vim.current.buffer[:]
        input = "\n".join(lines).encode("utf8")

        cmd = (
            [
                self.merlin_binary,
                "server",
                "complete-prefix",
                "-prefix", prefix,
                "-position", position,
                "-filename", filename,
                "-doc", "y" if self.merlin_completion_with_doc else "n",
            ] +
            self.merlin_extensions +
            self.merlin_packages +
            self.merlin_dot_merlins +
            self.merlin_log_errors +
            self.merlin_binary_flags +
            self.buffer_merlin_flags
        )

        process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                stdin=subprocess.PIPE,
                shell=False)

        (output, errors) = process.communicate(input=input)

        if errors:
            buf = int(self.vim.eval("bufnr('*merlin-log*',1)"))
            self.vim.buffers[buf].append(errors.split(b'\n'))

        try:
            result_json = json.loads(output)
            value = result_json["value"]
            entries = value["entries"]
        except Exception as e:
            entries = []

            if DEBUG:
                pprint.pprint(e, open("/tmp/deoplete-ocaml-exn.log", "a"))

        if DEBUG:
            pprint.pprint(errors, open("/tmp/deoplete-ocaml-merlin-errors.log", "a"))
            pprint.pprint(entries, open("/tmp/deoplete-ocaml-entries.log", "a"))
            pprint.pprint(context, open("/tmp/deoplete-ocaml-context.log", "a"))
            pprint.pprint(cmd, open("/tmp/deoplete-ocaml-cmd.log", "a"))

        complete_entries = [
            {
                "word": prefix + e["name"],
                "abbr": e["name"],
                "kind": e["desc"],
                "info": e["name"] + " : " + e["desc"] + "\n" + e["info"].strip(),
                "dup": 1,
            }
            for e in entries
        ]

        return complete_entries
