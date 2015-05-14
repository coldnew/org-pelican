"""org-pelican extensions

This pelican plugins add extra support for org-pelican such as copy .org sources
to output folder.

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

    def publish(self, src, dst):
        dst_dirs = os.path.dirname(dst)
        if not os.path.exists(dst_dirs):
            os.makedirs(dst_dirs)

        shutil.copy(src, dst)

    def copy_org_html(self, obj):
        if obj.source_path.endswith('.html') or obj.source_path.endswith('.md'):
            filename = self.strip_suffix(obj.source_path)

            org_file = filename + '.org'
            org_dist =  self.strip_suffix(obj.url) + '.org'

            # Copy .org file with .html (which generate by org-pelican)
            if os.path.isfile(org_file):
                self.publish(org_file, os.path.join( self.output_path, org_dist))

    def generate_context(self):
        pass

    def generate_output(self, writer=None):
        # we don't use the writer passed as argument here
        # since we write our own files
        logger.info('Copy .org source files....')

        for article in self.context['articles']:
            self.copy_org_html(article)

        for page in self.context['pages']:
            self.copy_org_html(page)

def get_generators(generators):
    return OrgFileGenerator

def register():
    signals.get_generators.connect(get_generators)