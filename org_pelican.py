""" org-pelican extensions

This pelican plugins add support to copy .org file to output folder.

"""
from __future__ import unicode_literals, print_function

from pelican import signals
from pelican.generators import Generator

import os
import shutil
import logging

logger = logging.getLogger(__name__)

class OrgFileGenerator(Generator):
    """Copy xxx.org.html to the output path with xx.html"""

    def __init__(self, *args, **kwargs):
        super(OrgFileGenerator, self).__init__(*args, **kwargs)

    def strip_suffix(self, filename):
        return os.path.splitext(filename)[0]

    def copy_file(self, src, dst):
        dst_dirs = os.path.dirname(dst)
        if not os.path.exists(dst_dirs):
            os.makedirs(dst_dirs)

        shutil.copy(src, dst)

    def copy_org_html(self, obj):
        if obj.source_path.endswith('.html') or obj.source_path.endswith('.md'):
            filename = self.strip_suffix(obj.source_path)

            if os.path.isfile(filename + '.org'):
                # print("-+++ --------> " + filename + '.org')
                # print("slug ->>> " + obj.slug)
                print("url ->>> " + self.strip_suffix(obj.url))
                print("output_path " + self.output_path)
                self.copy_file(filename + '.org', os.path.join( self.output_path, self.strip_suffix(obj.url) + '.org'))

            # Copy .org file with .html (which generate by org-pelican)

    def generate_context(self):
        pass

    def generate_output(self, writer=None):
        # we don't use the writer passed as argument here
        # since we write our own files
        logger.info('Copy xxx.org.html files....')

        for article in self.context['articles']:
            self.copy_org_html(article)

        for page in self.context['pages']:
            self.copy_org_html(page)

def get_generators(generators):
    return OrgFileGenerator

def register():
    signals.get_generators.connect(get_generators)