# ============================================================================
# FILE: ocaml_identifier.py
# AUTHOR: ELLIOTTCABLE <me@ell.io>
# License: MIT license
# ============================================================================

# Shamelessly cribbed from `denite-rails`, because I can't figure out Python module-importing:
#    <https://github.com/5t111111/denite-rails/blob/master/rplugin/python3/denite/source/rails.py>
import os
import site

path_to_this_dir = os.path.abspath(os.path.dirname(__file__))
path_to_merlin = os.path.join(path_to_this_dir, '../../../../autoload')
site.addsitedir(path_to_merlin)

import merlin

from .base import Base
#from denite.util import globruntime

class Source(Base):

    def __init__(self, vim):
        super().__init__(vim)
        self.vim = vim
        self.name = 'ocaml_identifier'
        self.kind = 'file'

    def on_init(self, context):
        context['__cursor_pos'] = self.vim.current.window.cursor

    def gather_candidates(self, context):
        try:
            pos = (context['__cursor_pos'][0], context['__cursor_pos'][1])
            merlin.vimprint(str(pos)) # FIXME
            l = merlin.command("expand-prefix", "-types", "y", "-prefix", '',
                               "-position", merlin.fmtpos(pos))

            return [ {
                'word': prop['name'],
                'abbr': 'hi! ' + prop['name'],

                # FIXME: Copy-pasted, won't actually work. Need to figure out printing from within
                # Python, and find documentation for Merlin's output format (wtf how is there none)
                'action__path': self.vim.call(
                    'fnamemodify', self.vim.call('bufname', '%'), ':p'),
                'action__line': linenr,
                'action__col': col,
            } for name in l['entries'] ]

        except merlin.MerlinExc as e:
            merlin.try_print_error(e)
