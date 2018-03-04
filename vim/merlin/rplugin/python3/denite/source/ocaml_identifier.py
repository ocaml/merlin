# ============================================================================
# FILE: ocaml_identifier.py
# AUTHOR: ELLIOTTCABLE <me@ell.io>
# License: MIT license
# ============================================================================

import os
import site

from .base import Base as BaseSource
from ..kind.base import Base as BaseKind

class Source(BaseSource):

    def __init__(self, vim):
        super().__init__(vim)

        self.vim = vim
        self.name = 'ocaml_identifier'
        self.kind = Kind(vim)

    def on_init(self, context):
        pos = self.vim.current.window.cursor
        context['__cursor_pos'] = "{0}:{1}".format(pos[0], pos[1])

    def gather_candidates(self, context):
        # An even-more-hacky round-trip thru VimScript — see `merlin#ListIdentifiers` in
        # `merlin.vim` — to solve an issue with how Denite exposes Vim's Python interface.
        identifiers = self.vim.call('merlin#ListIdentifiers', context['__cursor_pos'])

        candidates = []
        for ident in identifiers:
            if ident['desc']:
                candidates.append({
                    'word': ident['name'],
                    'abbr': '▷ %-12s %s : %s' % (ident['kind'], ident['name'], ident['desc']),
                    'source__identifier': ident['name'],
                })

            else:
                candidates.append({
                    'word': ident['name'],
                    'abbr': '▷ %-12s %s' % (ident['kind'], ident['name']),
                    'source__identifier': ident['name'],
                })

        return candidates


class Kind(BaseKind):

    def __init__(self, vim):
        super().__init__(vim)

        self.name = 'ocaml_identifier'
        self.default_action = 'open'
        self.persist_actions = []

    # Attempting to use Merlin's split-opening logic and settings, instead of Denite's? I think?
    def action_open(self, context):
        # XXX: This abandons multiply-selected targets, though ...
        target = context['targets'][0]
        self.vim.command('echom "' + repr(context['source']) + '"')
        self.vim.call('merlin#Locate',
                      target['source__identifier'], target['source_context']['__cursor_pos'])
