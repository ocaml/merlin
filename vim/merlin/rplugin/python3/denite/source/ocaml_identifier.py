# ============================================================================
# FILE: ocaml_identifier.py
# AUTHOR: ELLIOTTCABLE <me@ell.io>
# License: MIT license
# ============================================================================

# Shamelessly cribbed from `denite-rails`, because I can't figure out Python module-importing:
#    <https://github.com/5t111111/denite-rails/blob/master/rplugin/python3/denite/source/rails.py>
import os
import site

from .base import Base

class Source(Base):

    def __init__(self, vim):
        super().__init__(vim)
        self.vim = vim
        self.name = 'ocaml_identifier'
        self.kind = 'file'

    def on_init(self, context):
        context['__cursor_pos'] = self.vim.current.window.cursor

    def gather_candidates(self, context):
        # Copied out of `merlin.fmtpos()`, since I don't have access to that module here
        pos = "{0}:{1}".format(context['__cursor_pos'][0], context['__cursor_pos'][1])

        # An even-more-hacky round-trip thru VimScript — see `merlin#ListIdentifiers` in
        # `merlin.vim` — to solve an issue with how Denite exposes Vim's Python interface.
        identifiers = self.vim.call('merlin#ListIdentifiers', pos)

        return [ {
            'word': identifier,
        } for identifier in identifiers ]
